use std::mem;
use types::{LUA_FLOAT, LUA_INT, LUA_INSTRUCTION};
use crate::types::value::number::LuaNumber;

/// Raw types
#[allow(non_camel_case_types)]
pub mod types {
    pub type HOST_BYTE = u8;
    pub type HOST_SIGNED_BYTE = i8;
    pub type _HOST_SHORT = i16;
    pub type HOST_INT = i32;
    pub type _HOST_LONG = i64;
    pub type HOST_OBJECT_SIZE = usize;
    pub type LUA_INT = i64;
    pub type LUA_INT_UNSIGNED = u64;
    pub type LUA_FLOAT = f64;


    // TODO: Move to a different module
    #[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
    pub struct LUA_INSTRUCTION(u32);

    pub struct UnpackedInstruction {
        pub opcode: u8,
        pub a: usize,
        pub b: usize,
        pub sb: isize,
        pub c: usize,
        pub sc: isize,
        pub bx: usize,
        pub sbx: isize,
        pub ax: usize,
        pub sj: isize,
        pub k: u8,
    }

    impl From<u32> for LUA_INSTRUCTION {    // TODO: Refactor to accept a byte-array if only loader/dumper uses this function
        fn from(bytes: u32) -> Self {
            LUA_INSTRUCTION(bytes)
        }
    }

    impl LUA_INSTRUCTION {
        pub fn opcode(self) -> u8 {
            self.unpack().opcode
        }

        pub fn opcode_name(self) -> &'static str {
            use crate::constants::opcodes;  // TODO: Move
            opcodes::opcode_name(self.unpack().opcode)
        }

        pub fn unpack(self) -> UnpackedInstruction {
            let opcode: u8 = (self.0 & 0b111_1111) as u8;
            let a = (self.0 >> 7) as usize & 0b1111_1111;
            let b = (self.0 >> (7 + 8 + 1)) as usize & 0b1111_1111;
            let sb = ((self.0 >> (7 + 8 + 1)) as usize & 0b1111_1111) as isize - ((2isize.pow(8) / 2) - 1);
            let c = (self.0 >> (7 + 8 + 8 + 1)) as usize & 0b1111_1111;
            let sc = ((self.0 >> (7 + 8 + 8 + 1)) as usize & 0b1111_1111) as isize - ((2isize.pow(8) / 2) - 1);
            let bx = (self.0 >> (7 + 8)) as usize & !(!0 << 17); // 17 bits
            let sbx = bx as isize - ((2isize.pow(17) / 2) - 1);
            let ax = (self.0 >> 7) as usize & !(!0 << 25); // 25 bits
            let sj = ax as isize - ((2isize.pow(25) / 2) - 1);

            let k = (self.0 >> (7 + 8)) as u8 & 0b1;

            UnpackedInstruction {
                opcode,
                a,
                b,
                sb,
                c,
                sc,
                bx,
                sbx,
                ax,
                sj,
                k,
            }
        }

        pub fn as_bytes(self) -> u32 {  // TODO: See From<u32> for LUA_INSTRUCTION
            self.0
        }
    }


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

    pub const VFALSE: u8 = TBOOLEAN | (0 << 4);
    pub const VTRUE: u8 = TBOOLEAN | (1 << 4);

    pub const VFLOAT: u8 = TNUMBER | (1 << 4);
    pub const VINTEGER: u8 = TNUMBER | (0 << 4);

    pub const VSHORTSTRING: u8 = TSTRING | (0 << 4);
    pub const VLONGSTRING: u8 = TSTRING | (1 << 4);
}

pub const LUA_VERSION: u8 = 0x54;
pub const LUA_FORMAT: u8 = 0x00;

pub const LUA_SIGNATURE: &[u8; 4] = b"\x1bLua";
pub const LUA_CONV_DATA: &[u8; 6] = b"\x19\x93\r\n\x1a\n";
pub const LUA_SYSTEM_PARAMETER: [u8; 3] = [
    mem::size_of::<LUA_INSTRUCTION>() as u8,    // Instruction size in bytes
    mem::size_of::<LUA_INT>() as u8,            // Integer size in bytes
    mem::size_of::<LUA_FLOAT>() as u8           // Float size in bytes
];
// Can't reference LuaNumber enum variants yet for size_of usage
pub const LUA_CHECK_INTEGER: LuaNumber = LuaNumber::INT(0x5678);
pub const LUA_CHECK_FLOATING: LuaNumber = LuaNumber::FLOAT(370.5);

macro_rules! def_opcodes {
    ($($name:ident = $id:expr);+;) => {
        pub mod opcodes {
            $(pub const $name: u8 = $id;)+

            pub fn opcode_name(code: u8) -> &'static str {
                match code {
                    $($id => stringify!($name),)+
                    _ => "[UNKNOWN OPCODE]"
                }
            }
        }
    };
}

def_opcodes!(
    MOVE = 0;
    LOADI = 1;
    LOADF = 2;
    LOADK = 3;
    LOADKX = 4;
    LOADFALSE = 5;
    LOADFALSESKIP = 6;
    LOADTRUE = 7;
    LOADNIL = 8;

    GETUPVAL = 9;
    SETUPVAL = 10;

    GETTABUP = 11;
    GETTABLE = 12;
    GETI = 13;
    GETFIELD = 14;

    SETTABUP = 15;
    SETTABLE = 16;
    SETI = 17;
    SETFIELD = 18;

    NEWTABLE = 19;

    SELF = 20;

    ADDI = 21;
    ADDK = 22;
    SUBK = 23;
    MULK = 24;
    MODK = 25;
    POWK = 26;
    DIVK = 27;
    IDIVK = 28;
    BANDK = 29;
    BORK = 30;
    BXORK = 31;
    SHLI = 32;
    SHRI = 33;

    ADD = 34;
    SUB = 35;
    MUL = 36;
    MOD = 37;
    POW = 38;
    DIV = 39;
    IDIV = 40;
    BAND = 41;
    BOR = 42;
    BXOR = 43;
    SHL = 44;
    SHR = 45;

    MMBIN = 46;
    MMBINI = 47;
    MMBINK = 48;

    UNM = 49;
    BNOT = 50;
    NOT = 51;
    LEN = 52;

    CONCAT = 53;

    CLOSE = 54;
    TBC = 55;
    JMP = 56;
    EQ = 57;
    LT = 58;
    LE = 59;

    EQK = 60;
    EQI = 61;
    LTI = 62;
    LEI = 63;
    GTI = 64;
    GEI = 65;

    TEST = 66;
    TESTSET = 67;

    CALL = 68;
    TAILCALL = 69;

    RETURN = 70;
    RETURN0 = 71;
    RETURN1 = 72;

    FORLOOP = 73;
    FORPREP = 74;

    TFORPREP = 75;
    TFORCALL = 76;
    TFORLOOP = 77;

    SETLIST = 78;

    CLOSURE = 79;

    VARARG = 80;

    VARARGPREP = 81;

    EXTRAARG = 82;
);