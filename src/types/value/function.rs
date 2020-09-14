use crate::constants;
use std::fmt::{Formatter, Debug, Display};
use std::fmt;
use crate::types::value::string::LuaString;
use crate::constants::types::{HOST_INT, LUA_INSTRUCTION};
use crate::types::value::LuaValue;
use crate::types::upvalue::{UpvalueDesc, Upvalue};
use std::rc::Rc;
use crate::types::locvar::LocVar;
use std::cell::RefCell;
use crate::vm::ExecutionState;
use crate::types::varargs::Varargs;
use crate::error::TracedError;
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use crate::constants::LUA_FIELDS_PER_FLUSH;

pub struct Prototype {
    // TODO: Extract debug info to it's own type
    pub source_string: Option<LuaString>,
    pub first_line_defined: HOST_INT,
    pub last_line_defined: HOST_INT,
    pub param_count: u8,
    pub is_vararg: u8,
    pub max_stack_size: u8,
    pub code: Vec<LUA_INSTRUCTION>,
    pub constants: Vec<LuaValue>,
    pub upvalues: Vec<UpvalueDesc>,
    pub functions: Vec<Rc<Prototype>>,
    pub lineinfo: Vec<HOST_INT>,
    pub locvars: Vec<LocVar>,
    pub upvaluenames: Vec<Option<LuaString>>,
}

impl Prototype {
    pub fn from_parts(
        source_string: Option<LuaString>,
        first_line_defined: HOST_INT,
        last_line_defined: HOST_INT,
        param_count: u8,
        is_vararg: u8,
        max_stack_size: u8,
        code: Vec<LUA_INSTRUCTION>,
        constants: Vec<LuaValue>,
        upvalues: Vec<UpvalueDesc>,
        functions: Vec<Rc<Prototype>>,
        lineinfo: Vec<HOST_INT>,
        locvars: Vec<LocVar>,
        upvaluenames: Vec<Option<LuaString>>,
    ) -> Self {
        Prototype {
            source_string,
            first_line_defined,
            last_line_defined,
            param_count,
            is_vararg,
            max_stack_size,
            code,
            constants,
            upvalues,
            functions,
            lineinfo,
            locvars,
            upvaluenames,
        }
    }
}

//noinspection ALL
impl Display for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        for (index, instruction) in self.code.iter().enumerate() {
            let opcode: u8 = (instruction & 0b111111) as u8;
            let a = (instruction >> 6) as usize & 0b1111_1111;
            let b = (instruction >> 23) as usize & 0b1_1111_1111;
            let c = (instruction >> 14) as usize & 0b1_1111_1111;
            let bx = (instruction >> 14) as usize & 0b11_1111_1111_1111_1111;
            let sbx = ((instruction >> 14) as usize & 0b11_1111_1111_1111_1111) as isize - ((2isize.pow(18) / 2)-1);
            let b_rk = if b >> 8 == 1 { format!("K({})", self.constants.get(b & 0b0_1111_1111).unwrap_or(&LuaValue::NIL)) } else { format!("R({})", b & 0b0_1111_1111) };
            let c_rk = if c >> 8 == 1 { format!("K({})", self.constants.get(c & 0b0_1111_1111).unwrap_or(&LuaValue::NIL)) } else { format!("R({})", c & 0b0_1111_1111) };

            use constants::opcodes::*;
            let instruction_string = match opcode {
                MOVE => format!("MOVE\tR({}) = R({})", a, b),
                LOADK => format!("LOADK\tR({}) = K({})", a, self.constants.get(bx).unwrap_or(&LuaValue::NIL)),
                LOADKX => format!("LOADKX\tR({}) = ExtraArg", a),
                LOADBOOL => format!("LOADBOOL\tR({}) = {} {}", a, b != 0, if c != 0 { "SKIP" } else { "NO SKIP" }),
                LOADNIL => format!("LOADNIL\tR({}..={}) = NIL", a + 1, a + b),
                GETUPVAL => format!("GETUPVAL\tR({}) = Upvalue({})", a, b),
                GETTABUP => format!("GETTABUP\tR({}) = Upvalue({})[{}]", a, b, c_rk),
                GETTABLE => format!("GETTABLE\tR({}) = R({})[{}]", a, b, c_rk),
                SETTABUP => format!("SETTABUP\tUpvalue({})[{}] = {}", a, b_rk, c_rk),
                SETUPVAL => format!("SETUPVAL\tUpvalue({}) = R({})", b, a),
                SETTABLE => format!("SETTABLE\tR({})[{}] = {}", a, b_rk, c_rk),
                NEWTABLE => format!("NEWTABLE\tR({}) = {{}} (#arr={}, #hash={})", a, b, c),
                SELF => format!("SELF\tR({}) = R({}); R({}) = R({})[{}]", a + 1, b, a, b, c_rk),
                ADD => format!("ADD\tR({}) = {} + {}", a, b_rk, c_rk),
                SUB => format!("SUB\tR({}) = {} - {}", a, b_rk, c_rk),
                MUL => format!("MUL\tR({}) = {} * {}", a, b_rk, c_rk),
                MOD => format!("MOD\tR({}) = {} % {}", a, b_rk, c_rk),
                POW => format!("POW\tR({}) = {} ^ {}", a, b_rk, c_rk),
                DIV => format!("DIV\tR({}) = {} / {}", a, b_rk, c_rk),
                IDIV => format!("IDIV\tR({}) = {} // {}", a, b_rk, c_rk),
                BAND => format!("BAND\tR({}) = {} & {}", a, b_rk, c_rk),
                BOR => format!("BOR\tR({}) = {} | {}", a, b_rk, c_rk),
                BXOR => format!("XBOR\tR({}) = {} ~ {}", a, b_rk, c_rk),
                SHL => format!("SHL\tR({}) = {} << {}", a, b_rk, c_rk),
                SHR => format!("SHR\tR({}) = {} >> {}", a, b_rk, c_rk),
                UNM => format!("UNM\tR({}) = -R({})", a, b),
                BNOT => format!("BNOT\tR({}) = ~R({})", a, b),
                NOT => format!("NOT\tR({}) = not R({})", a, b),
                LEN => format!("LEN\tR({}) = #R({})", a, b),
                CONCAT => format!("CONCAT\tR({}) = R({}) .. ... .. R({})", a, b, c),
                JMP => format!("JMP\tto {} ({:+})", (index as isize) + sbx + 2, sbx),   // Offset the target index by 2; One as the display indices start at 1 whereas the opcode-array starts at 0, and one to accomodate the fact that lua-jumps occur after the program counter has been incremented.
                EQ => format!("EQ\tif {} {} {} then skip", b_rk, if a == 0 { "==" } else { "!=" }, c_rk),
                LT => format!("LT\tif {} {} {} then skip", b_rk, if a == 0 { "<" } else { ">=" }, c_rk),
                LE => format!("LE\tif {} {} {} then skip", b_rk, if a == 0 { "<=" } else { ">" }, c_rk),
                TEST => format!("TEST\tif R({}) as bool != {} then skip", a, if c == 0 { "false" } else { "true" }),
                TESTSET => format!("TESTSET\tif R({}) as bool != {} then skip else R({})=R({})", b, if c == 0 { "false" } else { "true" }, a, b),
                CALL => {
                    let parameters = if b == 0 {
                        format!("R({}..=top)", a + 1)
                    } else if b == 1 {
                        format!("")
                    } else {
                        format!("R({}..={})", a + 1, a + b - 1)
                    };
                    let result = if c == 0 {
                        format!("R({}..=TOP) = ", a)
                    } else if c == 1 {
                        format!("")
                    } else {
                        format!("R({}..={}) = ", a, a + c - 2)
                    };
                    format!("CALL\t{}R({})({})", result, a, parameters)
                }
                TAILCALL => format!("TAILCALL\treturn R({})({}..={})", a, a + 1, a + b - 2),
                RETURN => {
                    if b == 0 {
                        format!("RETURN\treturn R({}..=top)", a)
                    } else if b == 1 {
                        format!("RETURN\treturn;")
                    } else {
                        format!("RETURN\treturn R({}..={})", a, a + b - 2)
                    }
                }
                FORLOOP => format!("FORLOOP"),  // TODO
                FORPREP => format!("FORPREP"),
                TFORCALL => format!("TFORCALL\tR({}..={}) = R({})(R({}), R({}))", a+3, a+2+c, a, a+1, a+2),
                TFORLOOP => format!("TFORLOOP\tif R({}) != nil then R({}) = R({}); jump to {} ({:+})", a+1, a, a+1, index as isize + sbx + 2, sbx), // See #JMP for why this is incremented by 2
                SETLIST => {
                    let first_index = if c == 0 {
                        self.code.get(index + 1).map(|t| *t).unwrap_or(0) as usize - 1    // unwrap_or(0) intentionally leads to a huge value here TODO: Replace with some kind of error
                    } else {
                        c - 1
                    };
                    format!("SETLIST\tR({})[{}..={}] = R({}..={})", a, first_index * LUA_FIELDS_PER_FLUSH + 1, first_index + b, a + 1, a+b)
                },
                CLOSURE => format!("CLOSURE"),
                VARARG => format!("VARARG"),
                EXTRAARG => format!("EXTRAARG"),
                _ => format!("[UNKNOWN OPCODE] {}", instruction)
            };

            write!(f, "{}\t{}\n", index + 1, instruction_string)?;
        }
        write!(f, "CONSTANTS: {}\n", self.constants.len())?;
        for (index, constant) in self.constants.iter().enumerate() {
            write!(f, "{}\t{}\n", index, constant)?;
        }

        write!(f, "UPVALUES: {}\n", self.upvalues.len())?;
        for (index, upvalue) in self.upvalues.iter().enumerate() {
            let unnamed = LuaString::UNICODE(Rc::from("Unnamed upvalue"));
            let name = &match match self.upvaluenames.get(index) {
                None => &None,
                Some(opt) => opt,
            } {
                Some(name) => name,
                None => &unnamed,
            };
            write!(f, "{}\t{}\t{}\n", name, index, upvalue)?;
        }

        write!(f, "FUNCTIONS: {}\n", self.functions.len())?;
        for (index, function) in self.functions.iter().enumerate() {
            write!(f, "Function-{}\n{}", index, function)?;
        }
        Ok(())
    }
}

impl Debug for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Prototype").field(&self.source_string).finish()
    }
}

pub struct ClosureImpl {
    // TODO: Ensure upvalues are a DAG!
    pub proto: Rc<Prototype>,
    pub upvalues: Vec<Upvalue>,
    pub parent: Option<Rc<RefCell<ClosureImpl>>>, // what?
}

impl Debug for ClosureImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("ClosureImpl")
            .field("prototype", &self.proto)
            .field("upvalues", &self.upvalues.len())
            .field("parent", &self.parent.as_ref().map(|rc| rc.as_ptr()))
            .finish()
    }
}

impl ClosureImpl {
    pub fn from_proto_with_env(proto: Prototype, env: LuaValue) -> ClosureImpl {
        ClosureImpl {
            proto: Rc::from(proto),
            upvalues: vec![Upvalue::new_closed(env)],
            parent: None,
        }
    }
}

pub type NativeFunction = fn(&mut ExecutionState, &[LuaValue]) -> Result<Varargs, TracedError>;

impl LuaType for NativeFunction {
    const CONTAINER_NAME: &'static str = "Host function";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for NativeFunction {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(LuaFunction::RUST_FUNCTION(func)) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}

pub type NativeClosure = Rc<RefCell<dyn FnMut(&mut ExecutionState, &[LuaValue]) -> Result<Varargs, TracedError>>>;

impl LuaType for NativeClosure {
    const CONTAINER_NAME: &'static str = "Host closure";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for NativeClosure {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(LuaFunction::RUST_CLOSURE(func)) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}

pub type LuaClosure = Rc<RefCell<ClosureImpl>>;

impl LuaType for LuaClosure {
    const CONTAINER_NAME: &'static str = "Lua closure";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaClosure {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(LuaFunction::LUA_CLOSURE(func)) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone)]
pub enum LuaFunction {
    LUA_CLOSURE(LuaClosure),
    RUST_FUNCTION(NativeFunction),
    RUST_CLOSURE(NativeClosure),
}

impl LuaFunction {
    pub fn from_proto_with_env(proto: Prototype, env: LuaValue) -> LuaFunction {
        LuaFunction::LUA_CLOSURE(
            Rc::from(
                RefCell::from(
                    ClosureImpl::from_proto_with_env(proto, env)
                )
            )
        )
    }
}

impl LuaType for LuaFunction {
    const CONTAINER_NAME: &'static str = "function";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaFunction {
    fn coerce(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(func) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}

impl Debug for LuaFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LuaFunction::LUA_CLOSURE(c) => f.debug_tuple("LuaFunction::LUA_CLOSURE").field(c).finish(),
            LuaFunction::RUST_FUNCTION(func) => f.debug_tuple("LuaFunction::RUST_FUNCTION").field(&(*func as *const ())).finish(),          // Deref function pointer into raw pointer
            LuaFunction::RUST_CLOSURE(c) => f.debug_tuple("LuaFunction::RUST_CLOSURE").field(&(c as *const NativeClosure)).finish()
        }
    }
}

impl Display for LuaFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}

impl AsLuaPointer for LuaFunction {
    fn as_lua_pointer(&self) -> usize {
        match self {
            LuaFunction::LUA_CLOSURE(c) => ref_to_pointer(c.as_ref()),  // Pointer to refcell, as refcell has exclusive ownership of the closure we don't need to enter and get a ref to it's contents it here
            LuaFunction::RUST_FUNCTION(f) => *f as *const () as usize,      // Deref function pointer into raw pointer
            LuaFunction::RUST_CLOSURE(c) => ref_to_pointer(c.as_ref()), // Ditto
        }
    }
}

impl Eq for LuaFunction {}

impl PartialEq for LuaFunction {
    fn eq(&self, other: &Self) -> bool {
        self.as_lua_pointer() == other.as_lua_pointer()
        // TODO: Remove old Eq if the above functions as desired
        // match (self, other) {
        //     (LuaFunction::LUA_CLOSURE(lhs), LuaFunction::LUA_CLOSURE(rhs)) => RefCell::as_ptr(lhs) as *const LuaClosure == RefCell::as_ptr(rhs) as *const LuaClosure,
        //     (LuaFunction::RUST_FUNCTION(lhs), LuaFunction::RUST_FUNCTION(rhs)) => lhs as *const NativeFunction == rhs as *const NativeFunction,
        //     (LuaFunction::RUST_CLOSURE(lhs), LuaFunction::RUST_CLOSURE(rhs)) => Rc::ptr_eq(lhs, rhs),
        //     _ => false
        // }
    }
}
