use crate::types::value::string::LuaString;
use crate::constants::types::HOST_INT;

// TODO: Figure out what this is for
#[derive(Debug)]
pub struct LocVar {
    pub name: Option<LuaString>,
    pub startpc: HOST_INT,
    pub endpc: HOST_INT,
}

impl LocVar {
    pub fn new(name: Option<LuaString>, startpc: HOST_INT, endpc: HOST_INT) -> Self {
        LocVar { name, startpc, endpc }
    }
}
