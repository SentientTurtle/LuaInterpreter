use std::fmt::{Formatter, Debug, Display};
use std::fmt;
use crate::types::value::string::LuaString;
use crate::constants::types::{HOST_INT, LUA_INSTRUCTION, LUA_FLOAT, UnpackedInstruction, HOST_SIGNED_BYTE};
use crate::types::value::LuaValue;
use crate::types::upvalue::{UpvalueDesc, Upvalue};
use std::rc::Rc;
use crate::types::locvar::LocVar;
use std::cell::RefCell;
use crate::vm::ExecutionState;
use crate::types::varargs::Varargs;
use crate::error::TraceableError;
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use crate::constants::opcodes;

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
    pub lineinfo: Vec<HOST_SIGNED_BYTE>,
    pub abslineinfo: Vec<(HOST_INT, HOST_INT)>,
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
        lineinfo: Vec<HOST_SIGNED_BYTE>,
        abslineinfo: Vec<(HOST_INT, HOST_INT)>,
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
            abslineinfo,
            locvars,
            upvaluenames,
        }
    }

    // TODO: Note this is a direct port of the Lua source
    // TODO: Replace with various stdlib things
    fn get_abs_lineinfo(&self, pc: usize) -> (i32, i32) {
        if self.abslineinfo.len() == 0 || pc < self.abslineinfo[0].0 as usize {
            (-1, self.first_line_defined)
        } else {
            let mut i = (pc / 128) as isize - 1;
            assert!(i < 0 || (i < self.abslineinfo.len() as isize && self.abslineinfo[i as usize].0 as usize <= pc));
            while i + 1 < self.abslineinfo.len() as isize && pc >= self.abslineinfo[(i + 1) as usize].0 as usize {
                i+=1
            }
            return self.abslineinfo[i as usize]
        }
    }

    // TODO: Note this is a direct port of the Lua source
    // TODO: Replace with various stdlib things
    pub fn get_line(&self, pc: usize) -> Option<usize> {
        if self.lineinfo.is_empty() {
            return None;
        } else {
            let (mut basepc, mut baseline) = self.get_abs_lineinfo(pc);
            while basepc < (pc as HOST_INT) {
                basepc += 1;
                baseline += self.lineinfo[basepc as usize] as i32;
            }
            Some(baseline as usize)
        }
    }
}

// Utility struct, packs all information needed to Display a LUA_INSTRUCTION
pub struct InstructionDisplay<'a> {
    pub(crate) proto: &'a Prototype,
    pub(crate) index: usize,
    pub(crate) instruction: LUA_INSTRUCTION,
}

impl<'a> Display for InstructionDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let InstructionDisplay { proto, index, instruction } = *self;

        let UnpackedInstruction {
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
            k
        } = instruction.unpack();

        match opcode {
            opcodes::MOVE => write!(f, "MOVE\tR[{}] = R[{}]", a, b),

            opcodes::LOADI => write!(f, "LOADI\tR[{}] = {}", a, sbx),
            opcodes::LOADF => write!(f, "LOADF\tR[{}] = {}", a, sbx as LUA_FLOAT),
            opcodes::LOADK => write!(f, "LOAK\tR[{}] = `{}`", a, proto.constants.get(bx).unwrap_or(&LuaValue::NIL)),
            opcodes::LOADKX => write!(f, "LOADKX\tR[{}] = K(EXTRA_ARG)", a),
            opcodes::LOADFALSE => write!(f, "LOADFALSE\tR[{}] = false", a),
            opcodes::LOADFALSESKIP => write!(f, "LOADFALSESKIP\tR[{}] = false; pc++", a),
            opcodes::LOADTRUE => write!(f, "LOADTRUE\tR[{}] = true", a),
            opcodes::LOADNIL => write!(f, "LOADNIL\tR({}..={}) = NIL", a + 1, a + b),

            opcodes::GETUPVAL => write!(f, "GETUPVAL\tR[{}] = Upval[{}]", a, b),
            opcodes::SETUPVAL => write!(f, "SETUPVAL\tUpval[{}] = R[{}]", b, a),

            opcodes::GETTABUP => write!(f, "GETTABUP\tR[{}] = Upval[{}][`{}`]", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::GETTABLE => write!(f, "GETTABLE\tR[{}] = R[{}][R[{}]]", a, b, c),
            opcodes::GETI => write!(f, "GETI\tR[{}] = R[{}][{}]", a, b, c),
            opcodes::GETFIELD => write!(f, "GETFIELD\tR[{}] = R[{}][`{}`]", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),

            opcodes::SETTABUP => {
                if k == 1 {
                    write!(f, "SETTABUP\tUpval[{}][`{}`] = `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETTABUP\tUpval[{}][`{}`] = R[{}]", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), c)
                }
            }
            opcodes::SETTABLE => {
                if k == 1 {
                    write!(f, "SETTABLE\tR[{}][R[{}]] = `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETTABLE\tR[{}][R[{}]] = R[{}]", a, b, c)
                }
            }
            opcodes::SETI => {
                if k == 1 {
                    write!(f, "SETI\tR[{}][{}] = `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETI\tR[{}][{}] = R[{}]", a, b, c)
                }
            }
            opcodes::SETFIELD => {
                if k == 1 {
                    write!(f, "SETFIELD\tR[{}][`{}`] = `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETFIELD\tR[{}][`{}`] = R[{}]", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), c)
                }
            }

            opcodes::NEWTABLE => write!(f, "NEWTABLE\tR[{}] = {{}}", a),

            opcodes::SELF => write!(f, "SELF\tR[{}] = R[{}]; R[{}] = R[{}][`{}`]", a + 1, b, a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),

            opcodes::ADDI => write!(f, "ADDI\tR[{}] = R[{}] + {}", a, b, sc),
            opcodes::ADDK => write!(f, "ADDK\tR[{}] = R[{}] + `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::SUBK => write!(f, "SUBK\tR[{}] = R[{}] - `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::MULK => write!(f, "MULK\tR[{}] = R[{}] * `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::MODK => write!(f, "MODK\tR[{}] = R[{}] % `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::POWK => write!(f, "POWK\tR[{}] = R[{}] ^ `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::DIVK => write!(f, "DIVK\tR[{}] = R[{}] / `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::IDIVK => write!(f, "IDIVK\tR[{}] = R[{}] // `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::BANDK => write!(f, "BAND\tR[{}] = R[{}] & `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::BORK => write!(f, "BORK\tR[{}] = R[{}] | `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::BXORK => write!(f, "BXORK\tR[{}] = R[{}] ~ `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::SHLI => write!(f, "SHLI\tR[{}] = R[{}] >> {}", a, b, sc),
            opcodes::SHRI => write!(f, "SHRI\tR[{}] = {} << R[{}]", a, sc, b),

            opcodes::ADD => write!(f, "ADD\tR[{}] = R[{}] + R[{}]", a, b, c),
            opcodes::SUB => write!(f, "SUB\tR[{}] = R[{}] - R[{}]", a, b, c),
            opcodes::MUL => write!(f, "MUL\tR[{}] = R[{}] * R[{}]", a, b, c),
            opcodes::MOD => write!(f, "MOD\tR[{}] = R[{}] % R[{}]", a, b, c),
            opcodes::POW => write!(f, "POW\tR[{}] = R[{}] ^ R[{}]", a, b, c),
            opcodes::DIV => write!(f, "DIV\tR[{}] = R[{}] / R[{}]", a, b, c),
            opcodes::IDIV => write!(f, "IDIV\tR[{}] = R[{}] // R[{}]", a, b, c),
            opcodes::BAND => write!(f, "BAND\tR[{}] = R[{}] & R[{}]", a, b, c),
            opcodes::BOR => write!(f, "BOR\tR[{}] = R[{}] | R[{}]", a, b, c),
            opcodes::BXOR => write!(f, "BXOR\tR[{}] = R[{}] ~ R[{}]", a, b, c),
            opcodes::SHL => write!(f, "SHL\tR[{}] = R[{}] << R[{}]", a, b, c),
            opcodes::SHR => write!(f, "SHR\tR[{}] = R[{}] >> R[{}]", a, b, c),

            opcodes::MMBIN => write!(f, "MMBIN\t{}(R[{}], R[{}])", c, a, b),
            opcodes::MMBINI => write!(f, "MMBINI\t{}(R[{}], {})", c, a, sb),
            opcodes::MMBINK => write!(f, "MMBINK\t{}(R[{}], `{}`)", c, a, proto.constants.get(b).unwrap_or(&LuaValue::NIL)),

            opcodes::UNM => write!(f, "UNM\tR[{}] = -R[{}]", a, b),
            opcodes::BNOT => write!(f, "BNOT\tR[{}] = ~R[{}]", a, b),
            opcodes::NOT => write!(f, "NOT\tR[{}] = not R[{}]", a, b),
            opcodes::LEN => write!(f, "LEN\tR[{}] = #R[{}]", a, b),

            opcodes::CONCAT => write!(f, "CONCAT\tR({}) = R({}) .. ... .. R({})", a, a, a + b + 1),

            opcodes::CLOSE => write!(f, "CLOSE\tR[{}..]", a),
            opcodes::TBC => write!(f, "TBC\tMark for close R[{}]", a),

            // TODO: Displayed offsets are wrong, or implementation of JMP is
            opcodes::JMP => write!(f, "JMP\tto {} ({:+})", (index as isize) + sj + 2, sj), // Offset the target index by 2; One as the display indices start at 1 whereas the opcode-array starts at 0, and one to accommodate the fact that lua-jumps occur after the program counter has been incremented.
            opcodes::EQ => {
                if k == 0 {
                    write!(f, "EQ\tR[{}] == R[{}]", a, b)
                } else {
                    write!(f, "EQ\tR[{}] != R[{}]", a, b)
                }
            }
            opcodes::LT => {
                if k == 0 {
                    write!(f, "LT\tR[{}] < R[{}]", a, b)
                } else {
                    write!(f, "LT\tR[{}] >= R[{}]", a, b)
                }
            }
            opcodes::LE => {
                if k == 0 {
                    write!(f, "LE\tR[{}] <= R[{}]", a, b)
                } else {
                    write!(f, "LE\tR[{}] > R[{}]", a, b)
                }
            }

            opcodes::EQK => {
                if k == 0 {
                    write!(f, "EQK\tR[{}] == `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "EQK\tR[{}] != `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL))
                }
            }
            opcodes::EQI => {
                if k == 0 {
                    write!(f, "EQI\tR[{}] == {}", a, sb)
                } else {
                    write!(f, "EQI\tR[{}] != {}", a, sb)
                }
            }
            opcodes::LTI => {
                if k == 0 {
                    write!(f, "LTI\tR[{}] < {}", a, sb)
                } else {
                    write!(f, "LTI\tR[{}] >= {}", a, sb)
                }
            }
            opcodes::LEI => {
                if k == 0 {
                    write!(f, "LEI\tR[{}] <= {}", a, sb)
                } else {
                    write!(f, "LEI\tR[{}] > {}", a, sb)
                }
            }
            opcodes::GTI => {
                if k == 0 {
                    write!(f, "GTI\tR[{}] > {}", a, sb)
                } else {
                    write!(f, "GTI\tR[{}] <= {}", a, sb)
                }
            }
            opcodes::GEI => {
                if k == 0 {
                    write!(f, "GEI\tR[{}] >= {}", a, sb)
                } else {
                    write!(f, "GEI\tR[{}] < {}", a, sb)
                }
            }

            opcodes::TEST => {
                if k == 0 {
                    write!(f, "TEST\tif R[{}]", a)
                } else {
                    write!(f, "TEST\tif not R[{}]", a)
                }
            }
            opcodes::TESTSET => {
                if k == 0 {
                    write!(f, "TEST\tif R[{}] else R[{}] = R[{}]", b, a, b)
                } else {
                    write!(f, "TEST\tif not R[{}] else R[{}] = R[{}]", b, a, b)
                }
            }

            opcodes::CALL => {
                if b == 0 {
                    write!(f, "CALL\tR[{}..={}] = R[{}](R[{}..=top])", a, (a + c).wrapping_sub(2), a, a + 1)
                } else {
                    write!(f, "CALL\tR[{}..={}] = R[{}](R[{}..={}])", a, (a + c).wrapping_sub(2), a, a + 1, (a + b).wrapping_sub(1))
                }
            }
            opcodes::TAILCALL => {
                if b == 0 {
                    write!(f, "TAILCALL\treturn R[{}](R[{}..=top]", a, a + 1)
                } else {
                    write!(f, "TAILCALL\treturn R[{}](R[{}..={}]", a, a + 1, a + b - 1)
                }
            }

            opcodes::RETURN => {
                if b == 0 {
                    write!(f, "RETURN\treturn R[{}..=top]", a)
                } else {
                    write!(f, "RETURN\treturn R[{}..={}]", a, ((a + b) as isize) - 2)
                }
            }
            opcodes::RETURN0 => write!(f, "RETURN\treturn"),
            opcodes::RETURN1 => write!(f, "RETURN\treturn R[{}]", a),

            opcodes::FORLOOP => write!(f, "FORLOOP\tloop -{}", bx),
            opcodes::FORPREP => write!(f, "FORPREP\tprep +{}", bx + 1),

            opcodes::TFORPREP => write!(f, "TFORPREP\tprep +{}", bx),
            opcodes::TFORCALL => write!(f, "TFORCALL\tR[{}..={}] = R[{}](R[{}], R[{}])", a + 4, a + 3 + c, a, a + 1, a + 2),
            opcodes::TFORLOOP => write!(f, "TFORLOOP\tif R[{}] != nil then R[{}] = R[{}]; jump to {} (-{})", a + 2, a, a + 2, (index as isize) - (bx as isize) + 2, bx), // See #JMP for why this is incremented by 2

            opcodes::SETLIST => {
                let start = if k == 1 {
                    (
                        proto.code.get(index + 1)
                            .copied()
                            .map(LUA_INSTRUCTION::unpack)
                            .map(|i| i.ax << 8)
                            .unwrap_or(0)
                    ) | c
                } else {
                    c
                };
                if b == 0 {
                    write!(f, "SETLIST\tR[{}][{}..={}] = R[{}..=top]", a, start + 1, c + b, a + 1)
                } else {
                    write!(f, "SETLIST\tR[{}][{}..={}] = R[{}..={}]", a, start + 1, c + b, a + 1, a + b)
                }
            }

            opcodes::CLOSURE => write!(f, "CLOSURE\tR[{}] = closure({})", a, bx),

            opcodes::VARARG => write!(f, "VARARG"), // TODO: Specify what range of varargs

            opcodes::VARARGPREP => write!(f, "VARARGPREP"),

            opcodes::EXTRAARG => write!(f, "EXTRAARG\t{}", ax),
            _ => write!(f, "[UNKNOWN OPCODE] {}", opcode)
        }
    }
}

//noinspection ALL
impl Display for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (index, instruction) in self.code.iter().enumerate() {
            match self.get_line(index) {
                Some(line) => {
                    write!(f, "{}\t[{}]\t{}\n", index + 1, line, InstructionDisplay {
                        proto: self,
                        index,
                        instruction: *instruction,
                    })?;
                }
                None => {
                    write!(f, "{}\t{}\n", index + 1, InstructionDisplay {
                        proto: self,
                        index,
                        instruction: *instruction,
                    })?;
                }
            }
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

#[derive(Copy, Clone)]
pub struct NativeFunction {
    name: &'static str,
    inner: fn(&mut ExecutionState, &[LuaValue]) -> Result<Varargs, TraceableError>,
}

impl NativeFunction {
    pub fn from_parts(name: &'static str, inner: fn(&mut ExecutionState, &[LuaValue]) -> Result<Varargs, TraceableError>) -> NativeFunction {
        NativeFunction {
            name,
            inner,
        }
    }

    pub fn ptr(&self) -> fn(&mut ExecutionState, &[LuaValue]) -> Result<Varargs, TraceableError> {
        self.inner
    }

    pub fn name(&self) -> &'static str {
        self.name
    }
}

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

#[derive(Clone)]
pub struct NativeClosure {
    pub name: &'static str,
    pub(crate) closure: Rc<RefCell<dyn FnMut(&mut ExecutionState, &[LuaValue]) -> Result<Varargs, TraceableError>>>
}

impl NativeClosure {
    pub fn new(name: &'static str, closure: Rc<RefCell<dyn FnMut(&mut ExecutionState, &[LuaValue]) -> Result<Varargs, TraceableError>>>) -> NativeClosure {
        NativeClosure {
            name,
            closure
        }
    }
}

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
            LuaFunction::RUST_FUNCTION(func) => f.debug_tuple("LuaFunction::RUST_FUNCTION").field(&(func.inner as *const ())).finish(),          // Deref function pointer into raw pointer
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
            LuaFunction::RUST_FUNCTION(f) => f.inner as *const () as usize,      // Deref function pointer into raw pointer
            LuaFunction::RUST_CLOSURE(c) => ref_to_pointer(c.closure.as_ref()), // Ditto
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
