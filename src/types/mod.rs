use crate::constants::types::{LUA_FLOAT, LUA_INT};
use crate::error::ArgumentError;
use crate::types::value::LuaValue;
use crate::types::value::number::LuaNumber;

pub mod varargs;
pub mod parameters;
pub mod value;
pub mod upvalue;
pub mod locvar;

/// Trait for LuaTypes
pub trait LuaType: 'static {
    /// Equivalent Lua term for the rust type; E.g. "value" for LuaValue.
    /// Not to be confused with the lua "type()" function
    /// Must be unique!
    const CONTAINER_NAME: &'static str;

    /// The concrete type name of a value, regardless of it's current Rust type. Usually the name of the LuaValue variant
    /// Equivalent to the lua "type()" function
    fn type_name(&self) -> &'static str {
        Self::CONTAINER_NAME
    }
}

impl LuaType for bool {
    const CONTAINER_NAME: &'static str = "boolean";
}

impl LuaType for LUA_INT {
    const CONTAINER_NAME: &'static str = "int";
}

impl LuaType for LUA_FLOAT {
    const CONTAINER_NAME: &'static str = "float";
}

/// Conversion from rust references to "Lua Pointers"; Following Lua's equality and identity rules.
///
/// Used internally by the interpreter to handle equality and for debug information
pub trait AsLuaPointer {
    fn as_lua_pointer(&self) -> usize;
}

/// Utility function to convert a reference to a usize pointer, primarily for use in `AsLuaPointer` implementations
#[inline(always)]
pub(self) fn ref_to_pointer<T: ?Sized>(rf: &T) -> usize {
    rf as *const T as *const () as usize
}

pub trait CoerceFrom<T: Into<LuaValue> + Clone>: LuaType + Sized {
    fn coerce(value: &T) -> Option<Self>;

    fn coerce_from(value: &T) -> Result<Self, ArgumentError> {
        Self::coerce(value).ok_or(ArgumentError::CannotCoerce { expected: Self::CONTAINER_NAME, found: T::into(T::clone(value)).type_name() })
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for bool {
    fn coerce(value: &T) -> Option<Self> {
        Some(match value.clone().into() {
            LuaValue::NIL => false,
            LuaValue::BOOLEAN(b) => b,
            _ => true
        })
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LUA_INT {
    fn coerce(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::NUMBER(LuaNumber::INT(int)) => Some(int),
            LuaValue::NUMBER(LuaNumber::FLOAT(float)) => if (float as LUA_INT) as LUA_FLOAT == float { Some(float as LUA_INT) } else { None },
            LuaValue::STRING(s) => s.try_utf8().ok().and_then(|s| s.parse::<LUA_INT>().ok()),
            _ => None
        }
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LUA_FLOAT {
    fn coerce(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::NUMBER(LuaNumber::INT(int)) => Some(int as LUA_FLOAT),
            LuaValue::NUMBER(LuaNumber::FLOAT(float)) => Some(float),
            LuaValue::STRING(s) => s.try_utf8().ok().and_then(|s| s.parse::<LUA_FLOAT>().ok()),
            _ => None
        }
    }
}


/// Zero-sized type to represent Lua's "nil" value in generic constructs
pub struct Nil;

impl LuaType for Nil {
    const CONTAINER_NAME: &'static str = "nil";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for Nil {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::NIL = value.clone().into() {
            Some(Nil)
        } else {
            None
        }
    }
}