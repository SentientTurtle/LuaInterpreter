use std::rc::Rc;
use std::cell::{RefCell, Ref};
use crate::types::value::LuaValue;
use crate::error::ArgumentError;
use std::fmt;
use crate::constants::types::LUA_INT;
use std::fmt::{Display, Formatter, Debug};
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use crate::types::value::table::table_impl::TableImpl;

mod table_impl {
    use std::collections::HashMap;
    use crate::types::value::{LuaValue, LuaValueFullEq};
    use std::collections::hash_map::RandomState;

    /// Lua table implementation
    /// 'array' contains the array-part of the LuaTable
    /// 'map' contains the hash-part of the LuaTable
    /// 'key_array' is a mirror of the map's keys, used to speed up iteration
    #[derive(Debug)]
    pub struct TableImpl {
        pub array: Vec<LuaValue>,
        map: HashMap<LuaValueFullEq, LuaValue>,
        key_array: Vec<LuaValueFullEq>,
    }

    impl TableImpl {
        pub fn new() -> TableImpl {
            TableImpl {
                array: vec![],
                map: HashMap::new(),
                key_array: vec![],
            }
        }

        pub fn with_capacity(array_capacity: usize, hash_capacity: usize) -> TableImpl {
            TableImpl {
                map: HashMap::with_capacity(hash_capacity),
                array: Vec::with_capacity(array_capacity),
                key_array: Vec::with_capacity(hash_capacity),
            }
        }

        pub(super) fn map(&self) -> &HashMap<LuaValueFullEq, LuaValue, RandomState> {
            &self.map
        }

        pub(super) fn map_insert(&mut self, key: LuaValueFullEq, value: LuaValue) -> Option<LuaValue> {
            let prev_value = self.map.insert(key.clone(), value);
            if prev_value.is_none() {
                for value in self.key_array.iter_mut() {
                    if value.inner == LuaValue::NIL {
                        *value = key;
                        return prev_value;
                    }
                }
                self.key_array.push(key);
            }
            prev_value
        }

        pub(super) fn map_remove(&mut self, key: &LuaValueFullEq) -> Option<LuaValue> {
            let prev_value = self.map.remove(&key);
            if prev_value.is_some() {
                for value in self.key_array.iter_mut() {
                    if value == key {
                        *value = LuaValue::NIL.try_full_eq().expect("LuaValue::NIL is full-eq")
                    }
                }
            }
            prev_value
        }

        pub(super) fn map_key_at_iter_index(&self, index: &mut usize) -> Option<&LuaValueFullEq> {
            while let Some(key) = self.key_array.get(*index) {
                *index += 1;
                if key.inner == LuaValue::NIL {
                    continue;
                } else {
                    return Some(key);
                }
            }
            None
        }
    }
}

#[derive(Clone)]
pub struct LuaTable {
    inner: Rc<(RefCell<TableImpl>, RefCell<Option<LuaTable>>)>,   // Where inner.0 = TableImpl, and inner.1 = table's metatable
}

impl LuaTable {
    pub fn empty() -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl::new()), RefCell::from(None)))
        }
    }

    pub fn with_capacity(array_capacity: usize, hash_capacity: usize) -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl::with_capacity(array_capacity, hash_capacity)), RefCell::from(None)))
        }
    }

    /// `table::get` but for Into<LuaValue> keys
    pub fn raw_get_into<K: Into<LuaValue>>(&self, key: K) -> Result<LuaValue, ArgumentError> {
        self.raw_get(&key.into())
    }

    // Note to self: _DO NOT ENTER KEY/VALUE REFCELLS IN THIS METHOD_
    pub fn raw_get(&self, key: &LuaValue) -> Result<LuaValue, ArgumentError> {
        if key == &LuaValue::NIL { return Ok(LuaValue::NIL); }

        let (inner, _) = &*self.inner;
        let table = inner.borrow_mut();
        match key {
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i > 0 && ((i as usize - 1) < table.array.len()) }).unwrap_or(false) => {
                let index = num.as_int().unwrap() as usize - 1;
                if let Some(val) = table.array.get(index) {
                    if val != &LuaValue::NIL {
                        return Ok(val.clone());
                    }
                }
            }
            _ => {
                let key = key.clone().try_full_eq().map_err(|_| { ArgumentError::TableKeyIsNaN })?;
                if let Some(val) = table.map().get(&key) {
                    if val != &LuaValue::NIL {
                        return Ok(val.clone());
                    }
                }
            }
        };
        Ok(LuaValue::NIL)
    }

    pub fn raw_set<K: Into<LuaValue>, V: Into<LuaValue>>(&self, key: K, value: V) -> Result<(), ArgumentError> {
        self.set(key, value)    // TODO: Wait what?
    }

    // Note to self: _DO NOT ENTER KEY/VALUE REFCELLS IN THIS METHOD_
    // TODO: Ensure tables are a DAG
    pub fn set<K: Into<LuaValue>, V: Into<LuaValue>>(&self, key: K, value: V) -> Result<(), ArgumentError> {
        let key = key.into();
        let value = value.into();

        if key == LuaValue::NIL {
            return Err(ArgumentError::TableKeyIsNil);
        }
        let key_eq = key.clone().try_full_eq().map_err(|_| ArgumentError::TableKeyIsNaN)?;
        let (inner, _) = &*self.inner;
        let table = &mut *inner.borrow_mut();

        match key {
            // In existing array-part
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i > 0 && ((i as usize - 1) < table.array.len()) }).unwrap_or(false) => {
                let index = num.as_int().unwrap() as usize - 1; // We already checked num is greater than 0
                if value == LuaValue::NIL && index == table.array.len() - 1 {
                    table.array.truncate(table.array.len() - 1);
                    while let Some(LuaValue::NIL) = table.array.last() {
                        table.array.truncate(table.array.len() - 1);
                    }
                } else {
                    *table.array.get_mut(index).unwrap() = value;
                }
            }
            // Append to array-part
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i > 0 && ((i as usize - 1) == table.array.len()) }).unwrap_or(false) => {
                if value != LuaValue::NIL {
                    if table.map().contains_key(&key_eq) {
                        table.map_remove(&key_eq);
                    }
                    table.array.push(value)
                } else if table.map().contains_key(&key_eq) {
                    table.map_remove(&key_eq);
                    // mem::replace( // TODO: Wait why was this here?
                    //     dest,
                    //     value,
                    // );
                }
            }
            _ => {
                if value == LuaValue::NIL {
                    table.map_remove(&key_eq);
                } else {
                    table.map_insert(key_eq, value);
                }
            }
        };
        Ok(())
    }

    pub fn metatable(&self) -> Option<LuaTable> {
        self.inner.1.borrow().clone()
    }

    pub fn set_metatable(&self, metatable: LuaTable) {
        self.inner.1.replace(Some(metatable));
    }

    // Note: Does not invoke metamethod, TODO: Implement in vm
    // TODO: Length is technically defined as "N where N != nil && N+1 = nil"
    pub fn len(&self) -> usize {
        let (inner, _) = &*self.inner;
        let tab = inner.borrow();
        tab.array.len() + tab.map().len()
    }

    pub fn iter(&self) -> TableIterator {
        TableIterator::from(self)
    }

    pub fn next(&self, index: &LuaValue) -> Option<(LuaValue, LuaValue)> {  // TODO: replace with something less horrendously slow
        let table = &self.inner.0.borrow();
        let mut iter = table.map().iter()
            .map(|k| (k.0.inner.clone(), k.1))
            .chain(table.array.iter().enumerate().map(|kv| (LuaValue::from((kv.0 + 1) as LUA_INT), kv.1)));
        if index != &LuaValue::NIL {
            while let Some(kv) = iter.next() {
                if &kv.0 == index { break; };
            }
        }
        return iter.next().map(|kv| (kv.0, kv.1.clone()));
    }
}

impl LuaType for LuaTable {
    const CONTAINER_NAME: &'static str = "table";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaTable {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::TABLE(table) = value.clone().into() {
            Some(table)
        } else {
            None
        }
    }
}

impl AsLuaPointer for LuaTable {
    fn as_lua_pointer(&self) -> usize {
        ref_to_pointer(self.inner.as_ref())
    }
}

impl Debug for LuaTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "table:{:X}", self.as_lua_pointer()) // TODO: This leaks the memory address, needs to be fixed
    }
}

impl Display for LuaTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        // let inner = self.inner.0.borrow_mut();
        // write!(f, "table:{:?},{:?}", inner.map, inner.array)
        write!(f, "table:{:X}", self.as_lua_pointer()) // TODO: This leaks the memory address, needs to be fixed
    }
}

impl PartialEq for LuaTable {
    fn eq(&self, other: &Self) -> bool {
        self.as_lua_pointer() == other.as_lua_pointer()
    }
}


pub struct TableIterator<'a> {
    inner: &'a LuaTable,
    table_impl: Ref<'a, TableImpl>,
    in_map: bool,
    index: usize,
}

impl<'a> Iterator for TableIterator<'a> {
    type Item = (LuaValue, LuaValue);

    fn next(&mut self) -> Option<Self::Item> {
        if self.in_map {
            match self.table_impl.map_key_at_iter_index(&mut self.index) {
                None => {
                    self.in_map = false;
                    self.index = 0;
                    self.next()
                }
                Some(key) => {
                    Some(
                        (
                            key.inner.clone(),
                            self.table_impl.map()
                                .get(key)
                                .expect("key retrieved from map!")
                                .clone()
                        )
                    )
                }
            }
        } else {
            let value = self.table_impl.array.get(self.index)
                .map(|value| (LuaValue::from(self.index), value.clone()));
            self.index += 1;
            value
        }
    }
}

impl TableIterator<'_> {
    pub fn from(table: &LuaTable) -> TableIterator {
        TableIterator {
            inner: table,
            table_impl: table.inner.0.borrow(),
            in_map: true,
            index: 0,
        }
    }
}