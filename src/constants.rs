use std::mem;
use crate::types::LuaNumber;

//noinspection RsTypeAliasNaming
pub mod types {
    pub type LUA_INT = i64;
    pub type LUA_FLOAT = f64;
}

pub const LUA_SIGNATURE: &[u8; 4] = b"\x1bLua";
pub const LUA_CONV_DATA: &[u8; 6] = b"\x19\x93\r\n\x1a\n";
pub const LUA_SYSTEM_PARAMETER: [u8; 5] = [
    mem::size_of::<i32>() as u8,    // Int size in bytes
    8,                              // Object size ("size_t"), size in bytes
    mem::size_of::<u32>() as u8,    // Instruction size in bytes
    mem::size_of::<LUA_INT>() as u8,    // Integer size in bytes
    mem::size_of::<f64>() as u8     // Float size in bytes
]; // Can't reference LuaNumber enum variants yet for size_of usage
pub const LUA_INT: u64 = 0x5678;
pub const LUA_NUM: f64 = 370.5;