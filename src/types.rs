use std::rc::Rc;
use std::collections::HashMap;
use std::any::Any;
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INSTRUCTION, HOST_INT};
use std::fmt::{Display, Formatter, Error};
use crate::constants;
use std::cell::RefCell;
use std::ops::{Add, Sub};

pub struct TypeCastError{ pub(crate) expected: &'static str, pub(crate) found: &'static str}
pub enum TypeCoerceError {
    Cast(TypeCastError),
    FromString(LuaString),
}

impl From<TypeCastError> for TypeCoerceError {
    fn from(e: TypeCastError) -> Self {
        TypeCoerceError::Cast(e)
    }
}

#[derive(Clone)]
pub enum LuaValue {
    NIL,
    BOOLEAN(bool),
    NUMBER(LuaNumber),
    STRING(LuaString),
    USERDATA(UserData),
    FUNCTION(LuaFunction),
    THREAD(LuaThread),
    TABLE(LuaTable)
}

impl LuaValue {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaValue::NIL => "nil",
            LuaValue::BOOLEAN(_) => "boolean",
            LuaValue::NUMBER(_) => "number",
            LuaValue::STRING(_) => "string",
            LuaValue::USERDATA(_) => "userdata",
            LuaValue::FUNCTION(_) => "function",
            LuaValue::THREAD(_) => "thread",
            LuaValue::TABLE(_) => "table",
        }
    }

    pub fn try_number(&self) -> Result<&LuaNumber, TypeCastError> {
        match self {
            LuaValue::NUMBER(number) => Ok(number),
            _ => Err(TypeCastError{expected: "number", found: self.type_name()})
        }
    }

    pub fn coerce_number(&self) -> Result<LuaNumber, TypeCoerceError> {
        match self {
            LuaValue::NUMBER(number) => Ok(number.clone()),
            LuaValue::STRING(string) => {
                let utf8 = string.try_utf8()?;
                match utf8.parse::<LUA_INT>() {
                    Ok(int) => Ok(LuaNumber::INT(int)),
                    Err(_) => {
                        match utf8.parse::<LUA_FLOAT>() {
                            Ok(float) => Ok(LuaNumber::FLOAT(float)),
                            Err(_) => Err(TypeCoerceError::FromString(string.clone())),
                        }
                    },
                }
            }
            _ => Err(TypeCoerceError::Cast(TypeCastError{expected: "number", found: self.type_name()}))
        }
    }

    pub fn try_table(&self) -> Result<&LuaTable, TypeCastError> {
        match self {
            LuaValue::TABLE(table) => Ok(table),
            _ => Err(TypeCastError{expected: "table", found: self.type_name()})
        }
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LuaValue::NIL => write!(f, "NIL"),
            LuaValue::BOOLEAN(b) => write!(f, "{}", b),
            LuaValue::NUMBER(n) => write!(f, "{}", n),
            LuaValue::STRING(s) => write!(f, "{}", s),
            LuaValue::USERDATA(u) => write!(f, "{}", u),
            LuaValue::FUNCTION(function) => write!(f, "{}", function),
            LuaValue::THREAD(thread) => write!(f, "{}", thread),
            LuaValue::TABLE(table) => write!(f, "{}", table),
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
pub enum LuaNumber {
    INT(LUA_INT),
    FLOAT(LUA_FLOAT),
}

impl LuaNumber {
    pub fn as_int(&self) -> Result<LUA_INT, LUA_FLOAT> {
        match self {
            LuaNumber::INT(int) => Ok(*int),
            LuaNumber::FLOAT(float) => {
                if float.is_finite() {
                    Ok(*float as LUA_INT)
                } else {
                    Err(*float)
                }
            },
        }
    }

    pub fn as_float(&self) -> LUA_FLOAT {
        match self {
            LuaNumber::INT(int) => *int as LUA_FLOAT,
            LuaNumber::FLOAT(float) => *float,
        }
    }
}

impl From<LUA_INT> for LuaNumber {
    fn from(int: LUA_INT) -> Self {
        LuaNumber::INT(int)
    }
}

impl From<LUA_FLOAT> for LuaNumber {
    fn from(float: LUA_FLOAT) -> Self {
        LuaNumber::FLOAT(float)
    }
}

impl Add for LuaNumber {
    type Output = LuaNumber;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LuaNumber::INT(lhs) => {
                match rhs {
                    LuaNumber::INT(rhs) => {
                        LuaNumber::INT(lhs + rhs)
                    },
                    LuaNumber::FLOAT(rhs) => {
                        if rhs == (rhs as LUA_INT) as LUA_FLOAT {
                            LuaNumber::INT(lhs + rhs as LUA_INT)
                        } else {
                            LuaNumber::FLOAT(lhs as LUA_FLOAT + rhs)
                        }
                    },
                }
            }
            LuaNumber::FLOAT(lhs) => {
                match rhs {
                    LuaNumber::INT(rhs) => {
                        if lhs == (lhs as LUA_INT) as LUA_FLOAT {
                            LuaNumber::INT(lhs as LUA_INT + rhs)
                        } else {
                            LuaNumber::FLOAT(lhs + rhs as LUA_FLOAT)
                        }
                    },
                    LuaNumber::FLOAT(rhs) => {
                        LuaNumber::FLOAT(lhs + rhs)
                    },
                }
            },
        }
    }
}

impl Sub for LuaNumber {
    type Output = LuaNumber;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LuaNumber::INT(lhs) => {
                match rhs {
                    LuaNumber::INT(rhs) => {
                        LuaNumber::INT(lhs - rhs)
                    },
                    LuaNumber::FLOAT(rhs) => {
                        if rhs == (rhs as LUA_INT) as LUA_FLOAT {
                            LuaNumber::INT(lhs - rhs as LUA_INT)
                        } else {
                            LuaNumber::FLOAT(lhs as LUA_FLOAT - rhs)
                        }
                    },
                }
            }
            LuaNumber::FLOAT(lhs) => {
                match rhs {
                    LuaNumber::INT(rhs) => {
                        if lhs == (lhs as LUA_INT) as LUA_FLOAT {
                            LuaNumber::INT(lhs as LUA_INT - rhs)
                        } else {
                            LuaNumber::FLOAT(lhs - rhs as LUA_FLOAT)
                        }
                    },
                    LuaNumber::FLOAT(rhs) => {
                        LuaNumber::FLOAT(lhs - rhs)
                    },
                }
            },
        }
    }
}

impl Display for LuaNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LuaNumber::INT(integer) => write!(f, "{}", integer),
            LuaNumber::FLOAT(float) => write!(f, "{}", float),
        }
    }
}

#[derive(Clone)]
pub enum LuaString {
    UNICODE(Rc<str>),
    BINARY(Rc<[u8]>)
}

impl From<Box<[u8]>> for LuaString {
    fn from(boxed: Box<[u8]>) -> Self {
        match std::str::from_utf8(&boxed) {
            Ok(string) => unsafe {
                LuaString::UNICODE(Rc::from(std::str::from_boxed_utf8_unchecked(boxed)))
            },
            Err(_) => {
                LuaString::BINARY(Rc::from(boxed))
            }
        }
    }
}

impl LuaString {
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            LuaString::UNICODE(string) => string.as_bytes(),
            LuaString::BINARY(bytes) => bytes,
        }
    }

    pub fn append(self, other: LuaString) -> LuaString {
        match self {
            LuaString::UNICODE(self_string) => {
                match other {
                    LuaString::UNICODE(other_string) => {
                        let mut new_string = String::from(&*self_string);
                        new_string.push_str(&*other_string);
                        LuaString::UNICODE(Rc::from(new_string))
                    },
                    LuaString::BINARY(other_bytes) => {
                        let mut vec = Vec::from(self_string.as_bytes());
                        vec.extend_from_slice(&*other_bytes);
                        LuaString::BINARY(Rc::from(vec))
                    },
                }
            },
            LuaString::BINARY(self_bytes) => {
                let mut vec = Vec::from(&*self_bytes);
                vec.extend_from_slice(other.as_bytes());
                LuaString::BINARY(Rc::from(vec))
            },
        }
    }

    pub fn try_utf8(&self) -> Result<&str, TypeCastError> {
        match self {
            LuaString::UNICODE(unicode) => Ok(unicode),
            LuaString::BINARY(bytes) => {
                debug_assert!(std::str::from_utf8(bytes).is_err());
                Err(TypeCastError{ expected: "string:utf8", found: "string:binary" })
            },
        }
    }
}

impl Display for LuaString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LuaString::UNICODE(string) => {
                write!(f, "{}", *string)
            },
            LuaString::BINARY(bytes) => {
                write!(f, "{:X?}", bytes)
            },
        }
    }
}

struct TableImpl {
    map: HashMap<LuaValue, LuaValue>,
    array: Vec<LuaValue>
}

impl Display for TableImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        unimplemented!()
    }
}

#[derive(Clone)]
pub struct LuaTable {
    inner: Rc<RefCell<TableImpl>>
}

impl LuaTable {
    pub fn with_capacity(array_capacity: usize, hash_capacity: usize) -> LuaTable {
        unimplemented!()
    }

    // Note to self: _NO NOT ENTER REFCELLS IN THIS METHOD_
    pub fn get(&self, key: &LuaValue) -> &LuaValue {
        unimplemented!()
    }

    // Note to self: _NO NOT ENTER REFCELLS IN THIS METHOD_
    pub fn get_mut(&self, key: &LuaValue) -> &mut LuaValue { // maybe pub this later
        unimplemented!()
    }
}

impl Display for LuaTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.inner.borrow())
    }
}

#[derive(Clone)]
pub struct UserData {
    pub value: Rc<dyn Any>
}

impl Display for UserData {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "USERDATA@{}", &self.value as *const dyn Any as *const () as usize)
    }
}

pub struct Prototype {  // TODO: Extract debug info to it's own type
    pub source_string: Option<LuaString>,
    pub first_line_defined: HOST_INT,
    pub last_line_defined: HOST_INT,
    pub param_count: u8,
    pub is_vararg: u8,
    pub max_stack_size: u8,
    pub code: Vec<LUA_INSTRUCTION>,
    pub constants: Vec<LuaValue>,
    pub upvalues: Vec<UpvalueDesc>,
    pub functions: Vec<Prototype>,
    pub lineinfo: Vec<HOST_INT>,
    pub locvars: Vec<LocVar>,
    pub upvaluenames: Vec<Option<LuaString>>
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
        functions: Vec<Prototype>,
        lineinfo: Vec<HOST_INT>,
        locvars: Vec<LocVar>,
        upvaluenames: Vec<Option<LuaString>>
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
            upvaluenames
        }
    }
}

impl Display for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
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

pub struct LuaClosure {
    pub proto: Rc<Prototype>,
    pub upvalues: HashMap<UpvalueDesc, LuaValue>,
    pub parent: Option<Rc<RefCell<LuaClosure>>>
}

#[derive(Clone)]
pub enum LuaFunction {
    CLOSURE(Rc<RefCell<LuaClosure>>)
}

impl Display for LuaFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LuaFunction::CLOSURE(closure) => write!(f, "{}", closure.borrow().proto),
        }
    }
}

#[derive(Clone)]
pub struct LuaThread {
    // TODO: Implement
}

impl Display for LuaThread {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        unimplemented!()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct UpvalueDesc {
    instack: u8,
    idx: u8
}

impl UpvalueDesc {
    pub fn new(instack: u8, idx: u8) -> Self {
        UpvalueDesc { instack, idx }
    }

    pub fn stack_index(&self) -> u8 {
        self.idx
    }

    pub fn in_stack(&self) -> bool {
        self.instack != 0
    }
}

impl Display for UpvalueDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "instack: {}\tidx:{}", self.instack, self.idx)
    }
}

pub struct LocVar {
    name: Option<LuaString>,
    startpc: HOST_INT,
    endpc: HOST_INT
}

impl LocVar {
    pub fn new(name: Option<LuaString>, startpc: HOST_INT, endpc: HOST_INT) -> Self {
        LocVar { name, startpc, endpc }
    }
}