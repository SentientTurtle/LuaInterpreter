use crate::types::value::LuaValue;
use crate::error::ArgumentError;
use crate::types::{LuaType, CoerceFrom};

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
                        expected,
                        found,
                        index,
                    }
                } else {
                    unreachable!("try_coerce should never return an error variant other than ArgumentError::CannotCoerce")
                }
            })
    }
}

// TODO: By changing `NativeFunction` to use trait objects we can support more types as parameters for further syntax sugar, though at a significant performance cost
impl LuaParameters for [LuaValue] {
    fn get_value(&self, index: usize) -> Option<&LuaValue> {
        self.get(index)
    }
}
