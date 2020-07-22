use crate::constants::types::LUA_INSTRUCTION;

pub struct RegOrConstant {
    is_constant: bool,
    index: u8,
}

macro_rules! op_def {
    (@param_type a) => {u8};
    (@param_type b) => {u16};
    (@param_type c) => {u16};
    (@param_type ax) => {u32};
    (@param_type bx) => {u32};
    (@param_type sbx) => {i32};
    (@param_type b_rk) => {RegOrConstant};
    (@param_type c_rk) => {RegOrConstant};

    (@param_parse a $instruction:ident) => {($instruction >> 6) as u8};
    (@param_parse b $instruction:ident) => {($instruction >> 23) as u16 & 0b1_1111_1111};
    (@param_parse c $instruction:ident) => {($instruction >> 14) as u16 & 0b1_1111_1111};
    (@param_parse ax $instruction:ident) => {($instruction >> 6) as u32 & 0b11_1111_1111_1111_1111_1111_1111};
    (@param_parse bx $instruction:ident) => {($instruction >> 14) as u32 & 0b11_1111_1111_1111_1111};
    (@param_parse sbx $instruction:ident) => {(($instruction >> 14) as u32 & 0b11_1111_1111_1111_1111) as i32 - 0x1FFFF};
    (@param_parse b_rk $instruction:ident) => {RegOrConstant { is_constant: ($instruction >> 31) & 1 == 1, index: ($instruction >> 23) as u8}};
    (@param_parse c_rk $instruction:ident) => {RegOrConstant { is_constant: ($instruction >> 22) & 1 == 1, index: ($instruction >> 14) as u8}};

    ($($op_name:ident($($op_param:ident),*) = $op_code:literal),+) => {
        pub enum Operation {
            $($op_name{$( $op_param: op_def!(@param_type $op_param) ),*}),+,
            UnknownOperand(u8)
        }

        impl Operation {
            pub fn parse_instruction(instruction: LUA_INSTRUCTION) -> Operation {
                match instruction & 0b0011_1111 {
                    $(
                        $op_code => Operation::$op_name {$( $op_param: op_def!(@param_parse $op_param instruction) ),*}
                    ),+,
                    opcode @ _ => Operation::UnknownOperand(opcode as u8)
                }
            }
        }
    };
}

op_def!(
    MOVE(a, b) = 0,
    LOADK(bx) = 1,
    LOADKX(a) = 2,
    LOADBOOL(a, b, c) = 3,
    LOADNIL(a, b) = 4,
    GETUPVAL(a, b) = 5,

    GETTABUP(a, b, c_rk) = 6,
    GETTABLE(a, b, c_rk) = 7,

    SETTABUP(a, b_rk, c_rk) = 8,
    SETUPVAL(a, b) = 9,
    SETTABLE(a, b_rk, c_rk) = 10,

    NEWTABLE(a, b, c) = 11,

    SELF(a, b, c_rk) = 12,

    ADD(a, b_rk, c_rk) = 13,
    SUB(a, b_rk, c_rk) = 14,
    MUL(a, b_rk, c_rk) = 15,
    MOD(a, b_rk, c_rk) = 16,
    POW(a, b_rk, c_rk) = 17,
    DIV(a, b_rk, c_rk) = 18,
    IDIV(a, b_rk, c_rk) = 19,
    BAND(a, b_rk, c_rk) = 20,
    BOR(a, b_rk, c_rk) = 21,
    BXOR(a, b_rk, c_rk) = 22,
    SHL(a, b_rk, c_rk) = 23,
    SHR(a, b_rk, c_rk) = 24,
    UNM(a, b) = 25,
    BNOT(a, b) = 26,
    NOT(a, b) = 27,
    LEN(a, b) = 28,

    CONCAT(a, b, c) = 29,

    JMP(a, sbx) = 30,
    EQ(a, b_rk, c_rk) = 31,
    LT(a, b_rk, c_rk) = 32,
    LE(a, b_rk, c_rk) = 33,

    TEST(a, c) = 34,
    TESTSET(a, b, c) = 35,

    CALL(a, b, c) = 36,
    TAILCALL(a, b, c) = 37,
    RETURN(a, b) = 38,

    FORLOOP(a, sbx) = 39,
    FORPREP(a, sbx) = 40,

    TFORCALL(a, c) = 41,
    TFORLOOP(a, sbx) = 42,

    SETLIST(a, b, c) = 43,

    CLOSURE(a, bx) = 44,

    VARARG(a, b) = 45,
    EXTRAARG(ax) = 46
);