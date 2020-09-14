use crate::types::value::LuaValue;
use crate::error::ArgumentError;
use crate::types::{LuaType, CoerceFrom};
use crate::util::Union2;

pub trait LuaParameters {
    // TODO: replace with index call?
    fn get_value(&self, index: usize) -> Option<&LuaValue>;

    fn get_value_or_nil(&self, index: usize) -> &LuaValue {
        self.get_value(index).unwrap_or(LuaValue::nil())
    }

    fn try_coerce<T: CoerceFrom<LuaValue>>(&self, index: usize) -> Result<T, ArgumentError> where T: LuaType {
        let value = self.get_value(index).unwrap_or(&LuaValue::NIL);
        T::coerce_from(value)
            .map_err(|argerr| {
                if let ArgumentError::CannotCoerce { expected, found } = argerr {
                    ArgumentError::InvalidArgument {
                        expected: expected.to_string(),
                        found,
                        index,
                    }
                } else {
                    unreachable!("coerce_from should never return an error variant other than ArgumentError::CannotCoerce")
                }
            })
    }

    /// Returns None if self[index] is empty
    fn opt_coerce<T: CoerceFrom<LuaValue>>(&self, index: usize) -> Option<Result<T, ArgumentError>> where T: LuaType {
        let value = self.get_value(index);
        value.map(|value| T::coerce_from(value)
            .map_err(|argerr| {
                if let ArgumentError::CannotCoerce { expected, found } = argerr {
                    ArgumentError::InvalidArgument {
                        expected: expected.to_string(),
                        found,
                        index,
                    }
                } else {
                    unreachable!("coerce_from should never return an error variant other than ArgumentError::CannotCoerce")
                }
            }))
    }

    /// Returns None if self[index] is empty or nil
    fn opt_coerce_coalesce_nil<T: CoerceFrom<LuaValue>>(&self, index: usize) -> Option<Result<T, ArgumentError>> where T: LuaType {
        let value = self.get_value(index);
        value.and_then(LuaValue::non_nil)
            .map(|value| T::coerce_from(value)
            .map_err(|argerr| {
                if let ArgumentError::CannotCoerce { expected, found } = argerr {
                    ArgumentError::InvalidArgument {
                        expected: expected.to_string(),
                        found,
                        index,
                    }
                } else {
                    unreachable!("coerce_from should never return an error variant other than ArgumentError::CannotCoerce")
                }
            }))
    }

    fn try_coerce_any<T: CoerceFrom<LuaValue>, U: CoerceFrom<LuaValue>>(&self, index: usize) -> Result<Union2<T, U>, ArgumentError> where T: LuaType, U: LuaType {
        let value = self.get_value(index).unwrap_or(&LuaValue::NIL);
        T::coerce_from(value)
            .map(Union2::first)
            .or_else(|_| U::coerce_from(value).map(Union2::second))
            .map_err(|_| ArgumentError::InvalidArgument {
                expected: format!("{} or {}", T::CONTAINER_NAME, U::CONTAINER_NAME),
                found: value.type_name(),
                index
            })
    }
}

// TODO: By changing `NativeFunction` to use trait objects we can support more types as parameters for further syntax sugar, though at a significant performance cost
impl LuaParameters for [LuaValue] {
    fn get_value(&self, index: usize) -> Option<&LuaValue> {
        self.get(index)
    }
}
