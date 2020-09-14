use std::mem;
use types::{HOST_INT, HOST_OBJECT_SIZE, LUA_FLOAT, LUA_INT, LUA_INSTRUCTION};
use crate::types::value::number::LuaNumber;

/// Raw types
#[allow(non_camel_case_types)]
pub mod types {
    pub type HOST_BYTE = i8;
    pub type _HOST_SHORT = i16;
    pub type HOST_INT = i32;
    pub type _HOST_LONG = i64;
    pub type HOST_OBJECT_SIZE = usize;
    pub type LUA_INT = i64;
    pub type LUA_INT_UNSIGNED = u64;
    pub type LUA_FLOAT = f64;
    pub type LUA_INSTRUCTION = u32;


    static _ASSERTIONS: () = {   // TODO: Perhaps wrap in a macro
        if std::mem::size_of::<LUA_INT>() != std::mem::size_of::<LUA_INT_UNSIGNED>() {
            panic!("Signed and Unsigned LUA_INT must have equal size in memory!")
        };
    };
}

#[allow(unused)]
pub mod typetag {
    pub const TNIL: u8 = 0;
    pub const TBOOLEAN: u8 = 1;
    pub const TLIGHTUSERDATA: u8 = 2;
    pub const TNUMBER: u8 = 3;
    pub const TSTRING: u8 = 4;
    pub const TTABLE: u8 = 5;
    pub const TFUNCTION: u8 = 6;
    pub const TUSERDATA: u8 = 7;
    pub const TTHREAD: u8 = 8;

    pub const TSHORTSTRING: u8 = TSTRING | (0 << 4);
    pub const TLONGSTRING: u8 = TSTRING | (1 << 4);

    pub const TFLOAT: u8 = TNUMBER | (0 << 4);
    pub const TINTEGER: u8 = TNUMBER | (1 << 4);
}

pub const LUA_VERSION: u8 = 0x53;
pub const LUA_FORMAT: u8 = 0x00;

pub const LUA_SIGNATURE: &[u8; 4] = b"\x1bLua";
pub const LUA_CONV_DATA: &[u8; 6] = b"\x19\x93\r\n\x1a\n";
pub const LUA_SYSTEM_PARAMETER: [u8; 5] = [
    mem::size_of::<HOST_INT>() as u8,           // Int size in bytes
    mem::size_of::<HOST_OBJECT_SIZE>() as u8,   // Object size ("size_t"), size in bytes
    mem::size_of::<LUA_INSTRUCTION>() as u8,    // Instruction size in bytes
    mem::size_of::<LUA_INT>() as u8,            // Integer size in bytes
    mem::size_of::<LUA_FLOAT>() as u8           // Float size in bytes
];
// Can't reference LuaNumber enum variants yet for size_of usage
pub const LUA_CHECK_INTEGER: LuaNumber = LuaNumber::INT(0x5678);
pub const LUA_CHECK_FLOATING: LuaNumber = LuaNumber::FLOAT(370.5);
pub const LUA_FIELDS_PER_FLUSH: usize = 50;

pub mod opcodes {
    pub const MOVE: u8 = 0;
    pub const LOADK: u8 = 1;
    pub const LOADKX: u8 = 2;
    pub const LOADBOOL: u8 = 3;
    pub const LOADNIL: u8 = 4;
    pub const GETUPVAL: u8 = 5;
    pub const GETTABUP: u8 = 6;
    pub const GETTABLE: u8 = 7;
    pub const SETTABUP: u8 = 8;
    pub const SETUPVAL: u8 = 9;
    pub const SETTABLE: u8 = 10;
    pub const NEWTABLE: u8 = 11;
    pub const SELF: u8 = 12;
    pub const ADD: u8 = 13;
    pub const SUB: u8 = 14;
    pub const MUL: u8 = 15;
    pub const MOD: u8 = 16;
    pub const POW: u8 = 17;
    pub const DIV: u8 = 18;
    pub const IDIV: u8 = 19;
    pub const BAND: u8 = 20;
    pub const BOR: u8 = 21;
    pub const BXOR: u8 = 22;
    pub const SHL: u8 = 23;
    pub const SHR: u8 = 24;
    pub const UNM: u8 = 25;
    pub const BNOT: u8 = 26;
    pub const NOT: u8 = 27;
    pub const LEN: u8 = 28;
    pub const CONCAT: u8 = 29;
    pub const JMP: u8 = 30;
    pub const EQ: u8 = 31;
    pub const LT: u8 = 32;
    pub const LE: u8 = 33;
    pub const TEST: u8 = 34;
    pub const TESTSET: u8 = 35;
    pub const CALL: u8 = 36;
    pub const TAILCALL: u8 = 37;
    pub const RETURN: u8 = 38;
    pub const FORLOOP: u8 = 39;
    pub const FORPREP: u8 = 40;
    pub const TFORCALL: u8 = 41;
    pub const TFORLOOP: u8 = 42;
    pub const SETLIST: u8 = 43;
    pub const CLOSURE: u8 = 44;
    pub const VARARG: u8 = 45;
    pub const EXTRAARG: u8 = 46;

    // TODO: Possibly remove
    pub fn _name(opcode: u8) -> &'static str {
        match opcode {
            MOVE => "MOVE",
            LOADK => "LOADK",
            LOADKX => "LOADKX",
            LOADBOOL => "LOADBOOL",
            LOADNIL => "LOADNIL",
            GETUPVAL => "GETUPVAL",
            GETTABUP => "GETTABUP",
            GETTABLE => "GETTABLE",
            SETTABUP => "SETTABUP",
            SETUPVAL => "SETUPVAL",
            SETTABLE => "SETTABLE",
            NEWTABLE => "NEWTABLE",
            SELF => "SELF",
            ADD => "ADD",
            SUB => "SUB",
            MUL => "MUL",
            MOD => "MOD",
            POW => "POW",
            DIV => "DIV",
            IDIV => "IDIV",
            BAND => "BAND",
            BOR => "BOR",
            BXOR => "BXOR",
            SHL => "SHL",
            SHR => "SHR",
            UNM => "UNM",
            BNOT => "BNOT",
            NOT => "NOT",
            LEN => "LEN",
            CONCAT => "CONCAT",
            JMP => "JMP",
            EQ => "EQ",
            LT => "LT",
            LE => "LE",
            TEST => "TEST",
            TESTSET => "TESTSET",
            CALL => "CALL",
            TAILCALL => "TAILCALL",
            RETURN => "RETURN",
            FORLOOP => "FORLOOP",
            FORPREP => "FORPREP",
            TFORCALL => "TFORCALL",
            TFORLOOP => "TFORLOOP",
            SETLIST => "SETLIST",
            CLOSURE => "CLOSURE",
            VARARG => "VARARG",
            EXTRAARG => "EXTRAARG",
            _ => "[UNKNOWN OPCODE]"
        }
    }
}