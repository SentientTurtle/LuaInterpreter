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

impl Display for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        for (index, instruction) in self.code.iter().enumerate() {
            let opcode: u8 = (instruction & 0b111111) as u8;
            let a = (instruction >> 6) as usize & 0b1111_1111;
            let b = (instruction >> 23) as usize & 0b1_1111_1111;
            let c = (instruction >> 14) as usize & 0b1_1111_1111;
            let bx = (instruction >> 14) as usize & 0b11_1111_1111_1111_1111;
            let sbx = ((instruction >> 14) as usize & 0b11_1111_1111_1111_1111) as isize - (18isize.pow(2) / 2);
            write!(f, "{}\t{}\ta:{} b:{} c:{} bx:{} sbx:{}\n", index, constants::opcodes::name(opcode), a, b, c, bx, sbx)?;
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
    pub fn from_proto_with_env(proto: Rc<Prototype>, env: LuaValue) -> ClosureImpl {
        ClosureImpl {
            proto,
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

impl LuaType for LuaFunction {
    const CONTAINER_NAME: &'static str = "function";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaFunction{
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
            LuaFunction::RUST_FUNCTION(func) => f.debug_tuple("LuaFunction::RUST_FUNCTION").field(&(func as *const NativeFunction)).finish(),
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
            LuaFunction::LUA_CLOSURE(c) => ref_to_pointer(c.as_ref()),  // Pointer to refcell, as refcell has exclusive ownership of the closure we don't need to enter it here
            LuaFunction::RUST_FUNCTION(f) => ref_to_pointer(f),
            LuaFunction::RUST_CLOSURE(c) => ref_to_pointer(c),
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
