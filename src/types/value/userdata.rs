use crate::types::value::table::LuaTable;
use std::any::Any;
use std::rc::Rc;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use crate::types::value::LuaValue;

#[derive(Debug)]
struct UserDataImpl {
    metatable: Option<LuaTable>,
    pub value: Box<dyn Any>,
}

#[derive(Clone, Debug)]
pub struct UserData {
    inner: Rc<UserDataImpl>,
}

impl LuaType for UserData {
    const CONTAINER_NAME: &'static str = "userdata";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for UserData {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::USERDATA(userdata) = value.clone().into() {
            Some(userdata)
        } else {
            None
        }
    }
}

impl AsLuaPointer for UserData {
    fn as_lua_pointer(&self) -> usize {
        ref_to_pointer(self.inner.as_ref())
    }
}

impl UserData {
    pub fn new<T: 'static>(value: T) -> UserData {
        UserData {
            inner: Rc::new(UserDataImpl { metatable: None, value: Box::new(value) })
        }
    }

    pub fn metatable(&self) -> Option<LuaTable> {
        self.inner.metatable.clone()
    }   // TODO: Why clone?

    pub fn downcast<T: 'static>(&self) -> Option<&T> {
        (*self.inner.value).downcast_ref::<T>()
    }
}

impl Display for UserData {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "USERDATA@{}", self.as_lua_pointer()) // TODO: This leaks the memory address; Should be hashed or something
    }
}

impl PartialEq for UserData {
    fn eq(&self, other: &Self) -> bool {
        self.as_lua_pointer() == other.as_lua_pointer()
    }
}
