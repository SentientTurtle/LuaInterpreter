use std::rc::Rc;
use std::collections::HashMap;
use std::cell::RefCell;
use crate::types::value::{LuaValue, LuaValueFullEq};
use crate::error::ArgumentError;
use std::fmt;
use crate::constants::types::LUA_INT;
use std::fmt::{Display, Formatter};
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use nom::lib::std::fmt::Debug;

#[derive(Debug)]
struct TableImpl {
    map: HashMap<LuaValueFullEq, LuaValue>,
    array: Vec<LuaValue>,
}

#[derive(Clone)]
pub struct LuaTable {
    inner: Rc<(RefCell<TableImpl>, RefCell<Option<LuaTable>>)>   // Where inner.0 = TableImpl, and inner.1 = table's metatable
}

impl LuaTable {
    pub fn empty() -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl {
                map: HashMap::new(),
                array: vec![],
            }), RefCell::from(None)))
        }
    }

    pub fn with_capacity(array_capacity: usize, hash_capacity: usize) -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl {
                map: HashMap::with_capacity(hash_capacity),
                array: Vec::with_capacity(array_capacity),
            }), RefCell::from(None)))
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
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i >= 0 && ((i as usize) < table.array.len()) }).unwrap_or(false) => {
                let index = num.as_int().unwrap() as usize;
                if let Some(val) = table.array.get(index) {
                    if val != &LuaValue::NIL {
                        return Ok(val.clone());
                    }
                }
            }
            _ => {
                let key = key.clone().try_full_eq().map_err(|_| { ArgumentError::TableKeyIsNaN })?;
                if let Some(val) = table.map.get(&key) {
                    if val != &LuaValue::NIL {
                        return Ok(val.clone());
                    }
                }
            }
        };
        Ok(LuaValue::NIL)
    }

    pub fn raw_set<K: Into<LuaValue>, V: Into<LuaValue>>(&self, key: K, value: V) -> Result<(), ArgumentError> {
        self.set(key, value)
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
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i >= 0 && ((i as usize) < table.array.len()) }).unwrap_or(false) => {
                let index = num.as_int().unwrap() as usize;
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
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i >= 0 && ((i as usize) == table.array.len()) }).unwrap_or(false) => {
                if value != LuaValue::NIL {
                    if let Some(_) = table.map.get(&key_eq) {
                        table.map.remove(&key_eq);
                    }
                    table.array.push(value)
                } else if table.map.contains_key(&key_eq) {
                    table.map.remove(&key_eq);
                    // mem::replace( // TODO: Wait why was this here?
                    //     dest,
                    //     value,
                    // );
                }
            }
            _ => {
                if value == LuaValue::NIL {
                    table.map.remove(&key_eq);
                } else {
                    table.map.insert(key_eq, value);
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
        tab.array.len() + tab.map.len()
    }

    pub fn next(&self, index: &LuaValue) -> Option<(LuaValue, LuaValue)> {  // TODO: replace with something less stupidly slow
        let table = &self.inner.0.borrow();
        let mut iter = table.map.iter()
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
