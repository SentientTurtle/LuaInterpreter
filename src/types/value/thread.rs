use std::fmt::{Display, Formatter};
use std::fmt;
use crate::types::{LuaType, CoerceFrom};
use crate::types::value::LuaValue;

#[derive(Debug, Clone)]
pub struct LuaThread {
    // TODO: Implement "threads"/coroutines
}

impl LuaType for LuaThread {
    const CONTAINER_NAME: &'static str = "thread";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaThread {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::THREAD(thread) = value.clone().into() {
            Some(thread)
        } else {
            None
        }
    }
}

impl Display for LuaThread {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

impl PartialEq for LuaThread {
    fn eq(&self, other: &Self) -> bool {
        unimplemented!()
    }
}
