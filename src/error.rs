use crate::constants::types::{LUA_INT, LUA_INSTRUCTION, LUA_FLOAT};
use std::rc::Rc;
use std::fmt::{Formatter, Debug, Display};
use std::{fmt, io};
use crate::types::value::string::LuaString;
use crate::types::value::LuaValue;
use crate::types::value::function::{Prototype, NativeFunction};
use std::io::Error;
use crate::constants;

pub enum TraceEntry {
    LUA(usize, Rc<Prototype>),
    RUST(NativeFunction),
}

impl Debug for TraceEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TraceEntry::LUA(program_counter, prototype) => {
                let mut debug = f.debug_struct("TraceElement::LUA");
                if let Some(line) = prototype.get_line(*program_counter) {
                    debug.field("line", &line);
                }
                debug.field("program_counter", program_counter);
                if let Some(instruction) = prototype.code.get(*program_counter) {
                    debug.field("opcode", &instruction.opcode_name());
                }
                debug.field("prototype", prototype)
                    .finish()
            }
            TraceEntry::RUST(func) => {
                f.debug_tuple("TraceElement::RUST").field(&func.name()).field(&(func as *const NativeFunction)).finish()
            }
        }
    }
}

#[derive(Debug)]
pub struct TracedError {
    cause: LuaError,
    stacktrace: Vec<TraceEntry>,
}

impl TracedError {
    pub fn from_rust<T: Into<LuaError>>(cause: T, func: NativeFunction) -> TracedError {
        TracedError {
            cause: cause.into(),
            stacktrace: vec![TraceEntry::RUST(func)],
        }
    }

    pub fn from_lua<T: Into<LuaError>>(cause: T, program_counter: usize, prototype: Rc<Prototype>) -> TracedError {
        TracedError {
            cause: cause.into(),
            stacktrace: vec![TraceEntry::LUA(program_counter, prototype)],
        }
    }

    pub fn push_rust(mut self, func: NativeFunction) -> TracedError {
        self.stacktrace.push(TraceEntry::RUST(func));
        self
    }

    pub fn push_lua(mut self, program_counter: usize, prototype: Rc<Prototype>) -> TracedError {
        self.stacktrace.push(TraceEntry::LUA(program_counter, prototype));
        self
    }

    pub fn message(self) -> LuaValue {
        match self.cause {
            LuaError::ArgumentError(err) => LuaValue::from(LuaString::UNICODE(Rc::from(format!("{}", err)))),
            LuaError::ByteCodeError(err) => LuaValue::from(LuaString::UNICODE(Rc::from(format!("{}", err)))),
            LuaError::CompileError(err) => LuaValue::from(LuaString::UNICODE(Rc::from(format!("{}", err)))),
            LuaError::DecodeError(err) => LuaValue::from(LuaString::UNICODE(Rc::from(format!("{}", err)))),
            LuaError::UserError { message, level: _ } => message.unwrap_or(LuaValue::NIL),
            LuaError::InterpreterBug { message } => LuaValue::from(message),
        }
    }
}

pub enum TraceableError {
    TRACED(TracedError),
    LUA(LuaError)
}

impl TraceableError {
    pub fn trace(self, caller: NativeFunction) -> TracedError {
        match self {
            TraceableError::TRACED(error) => error.push_rust(caller),
            TraceableError::LUA(error) => TracedError::from_rust(error, caller)
        }
    }
}

impl<T: Into<LuaError>> From<T> for TraceableError {
    fn from(e: T) -> Self {
        TraceableError::LUA(e.into())
    }
}

impl From<TracedError> for TraceableError {
    fn from(e: TracedError) -> Self {
        TraceableError::TRACED(e)
    }
}

#[derive(Debug)]
pub enum LuaError {
    ArgumentError(ArgumentError),
    ByteCodeError(ByteCodeError),
    CompileError(CompileError),
    DecodeError(DecodeError),
    UserError { message: Option<LuaValue>, level: LUA_INT },
    InterpreterBug { message: &'static str },
}

impl LuaError {
    pub fn user_str(message: &str) -> Self {
        LuaError::UserError { message: Some(LuaValue::from(message)), level: 0 }
    }

    pub fn user_string(message: String) -> Self {
        LuaError::UserError { message: Some(LuaValue::from(message)), level: 0 }
    }

    pub fn interpreter_bug(message: &'static str) -> Self {
        LuaError::InterpreterBug { message }
    }
}

impl From<ArgumentError> for LuaError {
    fn from(e: ArgumentError) -> Self { LuaError::ArgumentError(e) }
}

impl From<ByteCodeError> for LuaError {
    fn from(e: ByteCodeError) -> Self { LuaError::ByteCodeError(e) }
}

impl From<CompileError> for LuaError {
    fn from(e: CompileError) -> Self {
        LuaError::CompileError(e)
    }
}

impl From<DecodeError> for LuaError {
    fn from(e: DecodeError) -> Self {
        LuaError::DecodeError(e)
    }
}

#[derive(Debug)]
pub enum ArgumentError {
    InvalidArgument { expected: String, found: &'static str, index: usize }, // TODO: Replace `expected` with a &str once concatenation of str literals and str constants is possible.
    CannotCoerce { expected: &'static str, found: &'static str },
    InvalidType { expected: &'static str, found: &'static str },
    TableKeyIsNaN,
    TableKeyIsNil,
    AttemptToCallNonFunction(LuaValue),
    ConcatenationTooLarge,
    IncomparableTypes { lhs_type: &'static str, rhs_type: &'static str },
    InvalidPatternFeature { message: &'static str },
    InvalidPatternOrFormat { message: String },
    InvalidTableContent { expected: &'static str, found: LuaValue, key: LuaValue },
}

impl Display for ArgumentError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ArgumentError::InvalidArgument { expected, found, index } => write!(f, "Invalid argument [{}] expected: {}, found: {}", index + 1, expected, found),
            ArgumentError::InvalidType { expected, found } => write!(f, "Invalid type expected: {}, found: {}", expected, found),
            ArgumentError::CannotCoerce { expected, found } => write!(f, "Cannot coerce {} to {}", found, expected),
            ArgumentError::TableKeyIsNaN => write!(f, "Table key is NaN"),
            ArgumentError::TableKeyIsNil => write!(f, "Table key is nil"),
            ArgumentError::ConcatenationTooLarge => write!(f, "Concatenation too large"),
            ArgumentError::IncomparableTypes { lhs_type, rhs_type } => write!(f, "Cannot compare type {} against type {}", lhs_type, rhs_type),
            ArgumentError::InvalidPatternFeature { message } => write!(f, "Invalid pattern: {}", message),
            ArgumentError::InvalidPatternOrFormat { message } => write!(f, "Invalid pattern/format: {}", message),
            ArgumentError::InvalidTableContent { expected, found, key } => write!(f, "Invalid table contents at [{}], expected: {}, found: {}", key, expected, found),
            ArgumentError::AttemptToCallNonFunction(val) => write!(f, "Attempt to call {}", val)
        }
    }
}

#[derive(Debug)]
pub enum ByteCodeError {
    ProgramCounterOutOfBounds { counter: usize, code_length: usize },
    RegisterIndexOutOfBounds { index: usize, registers_length: usize },
    ConstantIndexOutOfBounds { index: usize, constants_length: usize },
    UpvalueIndexOutOfBounds { upvalue_index: usize, upvalues_length: usize },
    UpvalueRegisterIndexOutOfBounds { upvalue_index: usize, register_index: usize, registers_length: usize },
    UpvalueStackIndexOutOfBounds { upvalue_index: usize, stack_index: usize, stack_length: usize },
    PrototypeIndexOutOfBounds { prototype_index: usize, prototype_len: usize },
    UnknownOpcode { opcode: u8 },
    AttemptToExecuteExtraArg,
    ExpectedExtraArg { found: LUA_INSTRUCTION },
}

impl Display for ByteCodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ByteCodeError::ProgramCounterOutOfBounds { counter, code_length } => write!(f, "Program counter out of bounds at index {} with program length {}", counter, code_length),
            ByteCodeError::RegisterIndexOutOfBounds { index, registers_length } => write!(f, "Register index out of bounds at index {} with register count {}", index, registers_length),
            ByteCodeError::ConstantIndexOutOfBounds { index, constants_length } => write!(f, "Constant index out of bounds at index {} with constant count {}", index, constants_length),
            ByteCodeError::UpvalueIndexOutOfBounds { upvalue_index, upvalues_length } => write!(f, "Upvalue index out of bounds at index {} with upvalue count {}", upvalue_index, upvalues_length),
            ByteCodeError::UpvalueRegisterIndexOutOfBounds { upvalue_index, register_index, registers_length } => write!(f, "Upvalue ({}) register index out of bounds at index {} with register count {}", upvalue_index, register_index, registers_length),
            ByteCodeError::UpvalueStackIndexOutOfBounds { upvalue_index, stack_index, stack_length } => write!(f, "Upvalue ({}) stack index out of bounds at index {} with stack length {}", upvalue_index, stack_index, stack_length),
            ByteCodeError::PrototypeIndexOutOfBounds { prototype_index, prototype_len } => write!(f, "Prototype index out of bounds at index {} with protoype count {}", prototype_index, prototype_len),
            ByteCodeError::UnknownOpcode { opcode } => write!(f, "Unknown opcode: {:X}", opcode),
            ByteCodeError::AttemptToExecuteExtraArg => write!(f, "Attempt to execute ExtraArg opcode"),
            ByteCodeError::ExpectedExtraArg { found } => write!(f, "Expected ExtraArg, found opcode {:X}", found.as_bytes()),
        }
    }
}

#[derive(Debug)]
pub enum DecodeError {
    IO(io::Error),
    InvalidSignature { found: [u8; constants::LUA_SIGNATURE.len()], expected: &'static [u8; constants::LUA_SIGNATURE.len()] },
    ConversionDataCorrupt { found: [u8; constants::LUA_CONV_DATA.len()], expected: &'static [u8; constants::LUA_CONV_DATA.len()] },
    IncompatibleSystemParam { found: [u8; constants::LUA_SYSTEM_PARAMETER.len()], expected: &'static [u8; constants::LUA_SYSTEM_PARAMETER.len()] },
    CorruptCheckInt(LUA_INT),
    CorruptCheckFloat(LUA_FLOAT),
    VectorSizeOverflow(usize, usize),
    UnknownConstantTypeTag(u8),
    NullStringInConstant(),
    NonBinaryBooleanByte(u8),
    InvalidVersion(u8),
    InvalidFormat(u8),
    IntegerOverflow(usize, u8),
}

impl From<io::Error> for DecodeError {
    fn from(err: io::Error) -> Self {
        DecodeError::IO(err)
    }
}

impl Display for DecodeError {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum CompileError {
    ExternalCommandFailed(io::Error),
    CompileFailed(String),
    DecodeError(DecodeError),
}

impl Display for CompileError {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl From<io::Error> for CompileError {
    fn from(err: Error) -> Self {
        CompileError::ExternalCommandFailed(err)
    }
}

impl From<DecodeError> for CompileError {
    fn from(err: DecodeError) -> Self {
        CompileError::DecodeError(err)
    }
}