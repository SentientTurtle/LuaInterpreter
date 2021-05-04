use crate::types::value::number::LuaNumber;
use crate::types::value::string::LuaString;
use crate::types::value::userdata::UserData;
use crate::types::value::function::{LuaFunction};
use crate::types::value::thread::LuaThread;
use crate::types::value::table::LuaTable;
use crate::error::ArgumentError;
use crate::constants::types::{LUA_FLOAT, LUA_INT};
use std::ops::Not;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Shl, Shr, Neg};
use std::cmp::Ordering;
use crate::types::{AsLuaPointer, LuaType, CoerceFrom};

pub mod number;
pub mod string;
pub mod table;
pub mod userdata;
pub mod function;
pub mod thread;

pub struct TypeMetatables {
    pub(crate) boolean: Option<LuaTable>,
    pub(crate) number: Option<LuaTable>,
    pub(crate) string: Option<LuaTable>,
    pub(crate) function: Option<LuaTable>,
    pub(crate) thread: Option<LuaTable>,
}

#[derive(Debug, Clone)]
pub enum LuaValue {
    NIL,
    BOOLEAN(bool),
    NUMBER(LuaNumber),
    STRING(LuaString),
    USERDATA(UserData),
    FUNCTION(LuaFunction),
    THREAD(LuaThread),
    TABLE(LuaTable),
}

impl LuaValue {
    pub const CONST_NIL: LuaValue = LuaValue::NIL;
}

impl LuaType for LuaValue {
    const CONTAINER_NAME: &'static str = "value";

    fn type_name(&self) -> &'static str {
        match self {
            LuaValue::NIL => super::Nil.type_name(),
            LuaValue::BOOLEAN(b) => b.type_name(),
            LuaValue::NUMBER(n) => n.type_name(),
            LuaValue::STRING(s) => s.type_name(),
            LuaValue::USERDATA(u) => u.type_name(),
            LuaValue::FUNCTION(f) => f.type_name(),
            LuaValue::THREAD(t) => t.type_name(),
            LuaValue::TABLE(t) => t.type_name(),
        }
    }
}

impl Default for LuaValue {
    fn default() -> Self {
        LuaValue::NIL
    }
}

impl From<bool> for LuaValue {
    fn from(b: bool) -> Self {
        LuaValue::BOOLEAN(b)
    }
}

impl From<LUA_INT> for LuaValue {
    fn from(i: LUA_INT) -> Self {
        LuaValue::NUMBER(LuaNumber::INT(i))
    }
}

impl From<LUA_FLOAT> for LuaValue {
    fn from(f: LUA_FLOAT) -> Self {
        LuaValue::NUMBER(LuaNumber::FLOAT(f))
    }
}

impl From<usize> for LuaValue {
    fn from(i: usize) -> Self {
        LuaValue::NUMBER(LuaNumber::from(i))
    }
}

impl From<&str> for LuaValue {
    fn from(string: &str) -> Self {
        LuaValue::STRING(LuaString::UNICODE(Rc::from(string)))
    }
}

impl From<&[u8]> for LuaValue {
    fn from(string: &[u8]) -> Self {
        LuaValue::STRING(LuaString::from(string))
    }
}

impl From<String> for LuaValue {
    fn from(s: String) -> Self {
        LuaValue::STRING(LuaString::UNICODE(Rc::from(s)))
    }
}

impl From<Box<[u8]>> for LuaValue {
    fn from(b: Box<[u8]>) -> Self {
        LuaValue::STRING(LuaString::BINARY(Rc::from(b)))
    }
}

impl From<LuaNumber> for LuaValue {
    fn from(n: LuaNumber) -> Self {
        LuaValue::NUMBER(n)
    }
}

impl From<LuaString> for LuaValue {
    fn from(s: LuaString) -> Self {
        LuaValue::STRING(s)
    }
}

impl From<UserData> for LuaValue {
    fn from(u: UserData) -> Self {
        LuaValue::USERDATA(u)
    }
}

impl From<LuaFunction> for LuaValue {
    fn from(f: LuaFunction) -> Self {
        LuaValue::FUNCTION(f)
    }
}

impl From<LuaThread> for LuaValue {
    fn from(t: LuaThread) -> Self {
        LuaValue::THREAD(t)
    }
}

impl From<LuaTable> for LuaValue {
    fn from(t: LuaTable) -> Self {
        LuaValue::TABLE(t)
    }
}

// TODO: Remove
// unsafe fn transmute<T: 'static, U: 'static>(e: T) -> U { // Regular mem::transmute does a typecheck before filling in the generic parameters
//     debug_assert_eq!(TypeId::of::<T>(), TypeId::of::<U>());                 // TODO: Maybe replace with a regular assert
//     debug_assert_eq!(std::mem::size_of::<T>(), std::mem::size_of::<U>());
//     let copy = std::mem::transmute_copy(&e);    // Does this have to be transmute_copy?
//     // TODO: Ensure no destructor for `e` is ran
//     std::mem::forget(e);
//     copy
// }

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaValue {
    fn coerce(value: &T) -> Option<Self> {
        Some(T::clone(value).into())
    }
}

impl LuaValue {
    pub fn nil() -> &'static LuaValue {
        &LuaValue::CONST_NIL
    }
}

// TODO: Make note that these don't invoke metamethods in documentation
macro_rules! delegate_math_to_luanumber {
    ($trait:ident, $func:ident, $op:tt) => {
        impl $trait for &LuaValue {
            type Output = Result<LuaValue, ArgumentError>;
            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaValue::from((LuaNumber::coerce_from(self)? $op LuaNumber::coerce_from(rhs)?)?))
            }
        }
    };
    ($trait:ident, $func:ident, unary: $op:tt) => {
        impl $trait for &LuaValue {
            type Output = Result<LuaValue, ArgumentError>;
            fn $func(self) -> Self::Output {
                Ok(LuaValue::from(($op LuaNumber::coerce_from(self)? )?))
            }
        }
    };
    (func: $func:ident) => {
        impl LuaValue {
            pub fn $func(&self, rhs: &Self) -> Result<LuaValue, ArgumentError> {
                Ok(LuaValue::from(LuaNumber::coerce_from(self)?.$func(LuaNumber::coerce_from(rhs)?)?))
            }
        }
    };
}

delegate_math_to_luanumber!(Add, add, +);
delegate_math_to_luanumber!(Sub, sub, -);
delegate_math_to_luanumber!(Mul, mul, *);
delegate_math_to_luanumber!(Div, div, /);
delegate_math_to_luanumber!(Rem, rem, %);
delegate_math_to_luanumber!(func: pow);
delegate_math_to_luanumber!(func: idiv);
delegate_math_to_luanumber!(BitAnd, bitand, &);
delegate_math_to_luanumber!(BitOr, bitor, |);
delegate_math_to_luanumber!(BitXor, bitxor, ^);
delegate_math_to_luanumber!(Shl, shl, <<);
delegate_math_to_luanumber!(Shr, shr, >>);
delegate_math_to_luanumber!(Neg, neg, unary: -);

impl LuaValue {
    pub fn bnot(&self) -> Result<LuaValue, ArgumentError> {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)?.bnot()?))
    }
}

impl Not for &LuaValue {
    type Output = Result<LuaValue, ArgumentError>;

    fn not(self) -> Self::Output {
        Ok(LuaValue::from(!bool::coerce_from(self)?))
    }
}

impl LuaValue {
    pub fn len(&self) -> Result<LuaValue, ArgumentError> {
        match self {
            LuaValue::STRING(s) => Ok(LuaValue::NUMBER(LuaNumber::from(s.len()))),
            LuaValue::TABLE(t) => Ok(LuaValue::NUMBER(LuaNumber::from(t.len()))),
            n @ _ => Err(ArgumentError::InvalidType { expected: "String or table", found: self.type_name() })
        }
    }

    pub fn concat(values: &[&LuaValue]) -> Result<LuaValue, ArgumentError> {
        let mut new_len = 0usize;
        for value in values {
            if let LuaValue::STRING(s) = value {
                new_len = (new_len.checked_add(s.len())).ok_or(ArgumentError::ConcatenationTooLarge)?
            } else {
                return Err(ArgumentError::InvalidType { expected: "string", found: value.type_name() });
            }
        }
        let mut buffer: Vec<u8> = Vec::with_capacity(new_len); // TODO: Fix potential for huge icky allocations
        for value in values {
            if let LuaValue::STRING(s) = value {
                buffer.extend_from_slice(s.as_bytes());
            } else {
                unreachable!()  // We already checked that each value is a ::STRING in the above loop
            }
        }
        Ok(LuaValue::STRING(LuaString::from(buffer.into_boxed_slice())))
    }

    pub fn get_metatable<'a, 'b: 'a>(&'a self, metatables: &'b TypeMetatables) -> Option<LuaTable> {
        match self {
            LuaValue::NIL => None,
            LuaValue::BOOLEAN(_) => metatables.boolean.clone(),
            LuaValue::NUMBER(_) => metatables.number.clone(),
            LuaValue::STRING(_) => metatables.string.clone(),
            LuaValue::USERDATA(userdata) => userdata.metatable(),
            LuaValue::FUNCTION(_) => metatables.function.clone(),
            LuaValue::TABLE(table) => table.metatable(),
            LuaValue::THREAD(_) => metatables.thread.clone(),
        }
    }

    // TODO: rename these to `index`/`setindex`/`call`?
    /// Handles the `__index` metamethod
    pub fn index_with_metatable(&self, key: &LuaValue, metatables: &TypeMetatables) -> Result<LuaValue, ArgumentError> {
        if let LuaValue::TABLE(table) = self {
            match table.raw_get(key) {
                Ok(LuaValue::NIL) => (),    // Fallthrough to metatable lookup
                val @ Ok(_) | val @ Err(_) => return val
            }
        }

        if let Some(metatable) = self.get_metatable(metatables) {
            if let Ok(value) = metatable.raw_get_into("__index") {
                if value != LuaValue::NIL {
                    return value.index_with_metatable(key, metatables)   // TODO: Ensure metatable ownership is a DAG
                }
            }
        }

        Ok(LuaValue::NIL)
    }

    /// Handles the `__newindex` metamethod
    pub fn write_index_with_metatable(&self, key: LuaValue, value: LuaValue, metatables: &TypeMetatables) -> Result<(), ArgumentError> {
        if let LuaValue::TABLE(table) = self {
            match table.raw_get(&key) {
                Ok(LuaValue::NIL) => (),    // Fallthrough to metatable lookup
                Ok(_) | Err(_) => {
                    return table.raw_set(key, value);
                }
            }
        }

        if let Some(metatable) = self.get_metatable(metatables) {
            if let Ok(metavalue) = metatable.raw_get_into("__newindex") {
                return match metavalue {   // If __newindex is a function, call it. Else, attempt to index it.
                    LuaValue::FUNCTION(function) => {
                        unimplemented!()
                    }
                    table @ _ => {
                        table.write_index_with_metatable(key, value, metatables)
                    }
                }
            }
        }

        Ok(())
    }

    /// Handles the `__call` metamethod
    pub fn prep_call_with_metatable(self, metatables: &TypeMetatables) -> Option<LuaFunction> {
        match self {
            LuaValue::FUNCTION(function) => Some(function),
            _ => {
                if let Some(meta) = self.get_metatable(metatables) {
                    if let Ok(value) = meta.raw_get_into("__call") {
                        if value != LuaValue::NIL {
                            value.prep_call_with_metatable(metatables)
                        } else {
                            None
                        }
                    } else {
                        unreachable!("Raw table gets with &str may not error!")
                    }
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn non_nil(&self) -> Option<&Self> {
        match self {
            LuaValue::NIL => None,
            _ => Some(self)
        }
    }

    pub(crate) fn not_nil(self) -> Option<Self> {
        match self {
            LuaValue::NIL => None,
            _ => Some(self)
        }
    }
}

impl PartialEq<bool> for LuaValue {
    fn eq(&self, other: &bool) -> bool {
        bool::coerce(self).expect("Coercing to bool should always succeed!") == *other
    }
}

impl PartialEq for LuaValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaValue::NIL, LuaValue::NIL) => true,
            (LuaValue::BOOLEAN(lhs), LuaValue::BOOLEAN(rhs)) => lhs == rhs,
            (LuaValue::NUMBER(lhs), LuaValue::NUMBER(rhs)) => lhs == rhs,
            (LuaValue::STRING(lhs), LuaValue::STRING(rhs)) => lhs == rhs,
            (LuaValue::USERDATA(lhs), LuaValue::USERDATA(rhs)) => lhs == rhs,
            (LuaValue::FUNCTION(lhs), LuaValue::FUNCTION(rhs)) => lhs == rhs,
            (LuaValue::THREAD(lhs), LuaValue::THREAD(rhs)) => lhs == rhs,
            (LuaValue::TABLE(lhs), LuaValue::TABLE(rhs)) => lhs == rhs,
            _ => false
        }
    }
}

impl PartialOrd for LuaValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LuaValue::NUMBER(lhs), LuaValue::NUMBER(rhs)) => lhs.partial_cmp(rhs),
            (LuaValue::STRING(lhs), LuaValue::STRING(rhs)) => lhs.partial_cmp(rhs),
            _ => None
        }
    }
}

/// LuaValue struct that implements (full) Eq
#[derive(Debug, Clone)]
struct LuaValueFullEq {
    inner: LuaValue
}

impl LuaValue {
    fn try_full_eq(self) -> Result<LuaValueFullEq, ()> {
        if self == self {
            return Ok(LuaValueFullEq { inner: self });
        } else {
            Err(())
        }
    }
}

impl Eq for LuaValueFullEq {}

impl PartialEq for LuaValueFullEq {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Hash for LuaValueFullEq {
    fn hash<H: Hasher>(&self, state: &mut H) {  // TODO: Possibly implement specific hashing algorithm
        match &self.inner {
            LuaValue::NIL => unreachable!(),    // NIL is an invalid key, and thus should never be hashed
            LuaValue::BOOLEAN(b) => state.write_u8(if *b { 1 } else { 0 }),
            LuaValue::NUMBER(n) => {
                match n {
                    LuaNumber::INT(int) => state.write_i64(*int),
                    LuaNumber::FLOAT(float) => {
                        if (*float as LUA_INT) as LUA_FLOAT == *float {
                            state.write_i64(*float as LUA_INT)
                        } else {
                            state.write(&float.to_le_bytes())
                        }
                    },
                }
            }
            LuaValue::STRING(s) => state.write(s.as_bytes()),
            LuaValue::USERDATA(u) => state.write_usize(u.as_lua_pointer()),
            LuaValue::FUNCTION(f) => state.write_usize(f.as_lua_pointer()),
            LuaValue::THREAD(t) => unimplemented!(),
            LuaValue::TABLE(t) => state.write_usize(t.as_lua_pointer()),
        }
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LuaValue::NIL => write!(f, "NIL"),
            LuaValue::BOOLEAN(b) => write!(f, "{}", b),
            LuaValue::NUMBER(n) => write!(f, "{}", n),
            LuaValue::STRING(s) => write!(f, "{}", s),
            LuaValue::USERDATA(u) => write!(f, "{}", u),
            LuaValue::FUNCTION(function) => write!(f, "{}", function),
            LuaValue::THREAD(thread) => write!(f, "{}", thread),
            LuaValue::TABLE(table) => write!(f, "{}", table),
        }
    }
}
