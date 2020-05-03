use crate::types::{Prototype, LuaValue, LuaClosure, TypeCastError, LuaTable, TypeCoerceError, LuaString, Varargs, MathError, LuaFunction, Upvalue, LuaNumber, KeyError};
use crate::constants::{opcodes, LUA_FIELDS_PER_FLUSH};
use std::mem;
use crate::constants::types::LUA_INSTRUCTION;
use std::rc::Rc;
use self::fetch::*;
use self::helper::*;
use std::cmp::Ordering;
use std::cell::RefCell;

#[derive(Debug)]
pub enum ExecuteError {
    ProgramCounterOutOfBounds { counter: usize, code_len: usize },
    UnknownOpcode(u8),
    AttemptToExecuteExtraArg,
    MissingExtraArg(LUA_INSTRUCTION),
    RegisterOutOfBounds(usize),
    ConstantOutOfBounds(usize),
    UpvalueOutOfBounds(usize),
    UpvalueRegisterOutOfBounds(usize),
    UpvalueStackOutOfBounds(usize),
    IncompatibleType { expected: &'static str, found: &'static str },
    EmptyStack,
    NonNumericString(LuaString),
    MathError(MathError),
    PrototypeOutOfBounds(usize),
    KeyError(KeyError)
}

impl From<TypeCastError> for ExecuteError {
    fn from(e: TypeCastError) -> Self {
        ExecuteError::IncompatibleType { expected: e.expected, found: e.found }
    }
}

impl From<TypeCoerceError> for ExecuteError {
    fn from(e: TypeCoerceError) -> Self {
        match e {
            TypeCoerceError::Cast(cast) => ExecuteError::from(cast),
            TypeCoerceError::StringNotNumerical(string) => ExecuteError::NonNumericString(string),
        }
    }
}

impl From<MathError> for ExecuteError {
    fn from(m: MathError) -> Self {
        ExecuteError::MathError(m)
    }
}

impl From<KeyError> for ExecuteError {
    fn from(k: KeyError) -> Self {
        ExecuteError::KeyError(k)
    }
}

pub struct TypeMetatables {
    boolean: Option<LuaTable>,
    number: Option<LuaTable>,
    string: Option<LuaTable>,
    function: Option<LuaTable>,
    thread: Option<LuaTable>,
}

pub struct ExecutionState {
    stack: Vec<StackFrame>,
    metatables: TypeMetatables,
    #[allow(dead_code)] time_to_live: u64,   // TODO: Implement TTL
}

impl ExecutionState {
    pub fn blank() -> ExecutionState {
        ExecutionState {
            stack: vec![],
            metatables: TypeMetatables {
                boolean: None,
                number: None,
                string: None,
                function: None,
                thread: None
            },
            time_to_live: 0
        }
    }
}

struct StackFrame {
    registers: Vec<LuaValue>,
    pc: usize,
    proto: Rc<Prototype>,
    upvalues: Vec<Option<Upvalue>>,
}

impl StackFrame {
    pub fn get_upvalue_from_reg(&mut self, register: usize, frame_index: usize) -> Result<Upvalue, ExecuteError> {      // TODO: Refactor this in a way that doesn't rely on the frame's index being passed in
        let opt = self.upvalues.get_mut(register).ok_or(ExecuteError::RegisterOutOfBounds(register))?;
        match opt {
            None => {
                opt.replace(Upvalue::new_open(register, frame_index));
                Ok(opt.as_ref().unwrap().clone())
            }
            Some(upval) => Ok(upval.clone()),
        }
    }
}

impl Drop for StackFrame {
    fn drop(&mut self) {
        debug_assert_eq!(self.upvalues.len(), self.registers.len());
        for i in 0..self.upvalues.len() {
            if let Some(upval) = &self.upvalues[i] {
                upval.close(self.registers.get_mut(i).unwrap().clone())
            }
        }
    }
}

// Mem-replace with the arguments swapped for borrow checker reasons
#[inline(always)]
fn replace<T>(src: T, dest: &mut T) {
    mem::replace(dest, src);
}

mod fetch {
    // Module to isolate "get" functions
    use crate::vm::*;
    use crate::types::*;
    use crate::constants::types::LUA_INSTRUCTION;
    use std::mem;

    pub(super) fn next_op(proto: &Prototype, pc: &mut usize) -> Result<LUA_INSTRUCTION, ExecuteError> {
        let op = proto.code.get(*pc).ok_or(ExecuteError::ProgramCounterOutOfBounds { counter: *pc, code_len: proto.code.len() })?;
        mem::replace(pc, *pc + 1);
        Ok(*op)
    }

    pub(super) fn get_reg(registers: &Vec<LuaValue>, index: usize) -> Result<&LuaValue, ExecuteError> {
        registers.get(index).ok_or(ExecuteError::RegisterOutOfBounds(index))
    }

    pub(super) fn get_reg_mut(registers: &mut Vec<LuaValue>, index: usize) -> Result<&mut LuaValue, ExecuteError> {
        registers.get_mut(index).ok_or(ExecuteError::RegisterOutOfBounds(index))
    }

    pub(super) fn get_const(proto: &Prototype, index: usize) -> Result<&LuaValue, ExecuteError> {
        proto.constants.get(index).ok_or(ExecuteError::ConstantOutOfBounds(index))
    }

    pub(super) fn get_rk<'a, 'b: 'a>(proto: &'a Prototype, registers: &'b Vec<LuaValue>, rk: (bool, usize)) -> Result<&'a LuaValue, ExecuteError> {
        if rk.0 {
            get_const(proto, rk.1)
        } else {
            get_reg(registers, rk.1)
        }
    }

    pub(super) fn get_upvalue(closure: &LuaClosure, index: usize, stack: &mut [StackFrame]) -> Result<LuaValue, ExecuteError> {
        if let Some(upvalue) = closure.upvalues.get(index) {
            match upvalue.get() {
                UpvalueImpl::Open { frame, register } => {
                    if let Some(frame) = stack.get(frame) {
                        match frame.registers.get(register) {
                            None => Err(ExecuteError::UpvalueRegisterOutOfBounds(register)),
                            Some(value) => Ok(value.clone()),
                        }
                    } else {
                        Err(ExecuteError::UpvalueStackOutOfBounds(frame))
                    }
                }
                UpvalueImpl::Closed(value) => Ok(value)
            }
        } else {
            Err(ExecuteError::UpvalueOutOfBounds(index))
        }
    }

    pub(super) fn set_upvalue<'a: 'c, 'b, 'c>(closure: &'a mut LuaClosure, index: usize, stack: &'c mut [StackFrame], value: LuaValue) -> Result<LuaValue, ExecuteError> {
        if let Some(upvalue) = closure.upvalues.get_mut(index) {
            match upvalue.get() {
                UpvalueImpl::Open { frame, register } => {
                    if let Some(frame) = stack.get_mut(frame) {
                        match frame.registers.get_mut(register) {
                            None => Err(ExecuteError::UpvalueRegisterOutOfBounds(register)),
                            Some(dest) => {
                                Ok(mem::replace(dest, value))
                            }
                        }
                    } else {
                        Err(ExecuteError::UpvalueStackOutOfBounds(frame))
                    }
                }
                UpvalueImpl::Closed(old) => {
                    upvalue.close(value);
                    Ok(old)
                }
            }
        } else {
            Err(ExecuteError::UpvalueOutOfBounds(index))
        }
    }

    pub fn get_metatable<'a, 'b: 'a>(value: &'a LuaValue, metatables: &'b TypeMetatables) -> Option<&'a LuaTable> {
        match value {
            LuaValue::NIL => None,
            LuaValue::BOOLEAN(_) => metatables.boolean.as_ref(),
            LuaValue::NUMBER(_) => metatables.number.as_ref(),
            LuaValue::STRING(_) => metatables.string.as_ref(),
            LuaValue::USERDATA(usedata) => usedata.metatable().as_ref(),
            LuaValue::FUNCTION(_) => metatables.function.as_ref(),
            LuaValue::TABLE(table) => table.metatable().as_ref(),
            LuaValue::THREAD(_) => metatables.thread.as_ref(),
        }
    }
}

mod helper {
    use crate::types::{LuaFunction, Varargs, LuaClosure, LuaValue};
    use crate::vm::{ExecutionState, execute_closure, ExecuteError};

    pub fn do_call(parent: &mut LuaClosure, function: &LuaFunction, execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, ExecuteError> {
        drop(parent);   // Obtain a lock on the parent closure, to ensure that it's refcell is unborrowed before the match block below may potentially re-borrow it.      TODO: Validate this with recursive functions
        match function {
            LuaFunction::CLOSURE(closure) => Ok(execute_closure(&mut closure.borrow_mut(), execstate, params)?),
            LuaFunction::RUST(func) => Ok(func(params)?)
        }
    }
}

macro_rules! init_frame_vars {
    ($state:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {
        $frame = $state.stack[..].last_mut().ok_or(ExecuteError::EmptyStack)?;
        $registers = &mut $frame.$registers;
        #[allow(unused)]
        $pc = &mut $frame.$pc;
        $proto = &mut $frame.$proto;
        debug_assert_eq!($proto.max_stack_size as usize, $registers.len());
    };
}

macro_rules! math_binary_op {
    ($op:tt, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_reg($registers, $b)?.clone();
            let rhs = get_reg($registers, $c)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {       // TODO: RHS metatable
                let func = func?.try_function()?.clone();
                let result = do_call($closure, &func, $execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                replace(
                    result.into_first(),
                    get_reg_mut($registers, $a)?,
                );
            } else {
                replace(
                    LuaValue::from((&lhs $op &rhs)?),
                    get_reg_mut($registers, $a)?,
                );
            }
    }};
    (func: $op:ident, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_reg($registers, $b)?.clone();
            let rhs = get_reg($registers, $c)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {       // TODO: RHS metatable
                let func = func?.try_function()?.clone();
                let result = do_call($closure, &func, $execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                replace(
                    result.into_first(),
                    get_reg_mut($registers, $a)?,
                );
            } else {
                replace(
                    LuaValue::from(lhs.$op(&rhs)?),
                    get_reg_mut($registers, $a)?,
                );
            }
    }};
}

macro_rules! math_unary_op {
    ($op:tt, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_reg($registers, $b)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {
                let func = func?.try_function()?.clone();
                let result = do_call($closure, &func, $execstate, &[lhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                replace(
                    result.into_first(),
                    get_reg_mut($registers, $a)?,
                );
            } else {
                replace(
                    LuaValue::from(($op &lhs)?),
                    get_reg_mut($registers, $a)?,
                );
            }
    }};
    (func: $op:ident, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_reg($registers, $b)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {
                let func = func?.try_function()?.clone();
                let result = do_call($closure, &func, $execstate, &[lhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                replace(
                    result.into_first(),
                    get_reg_mut($registers, $a)?,
                );
            } else {
                replace(
                    LuaValue::from(lhs.$op()?),
                    get_reg_mut($registers, $a)?,
                );
            }
    }};
}

#[allow(unused)]
pub(crate) fn execute_closure(closure: &mut LuaClosure, execstate: &mut ExecutionState, parameters: &[LuaValue]) -> Result<Varargs, ExecuteError> {
    {
        let mut new_frame = StackFrame {
            registers: Vec::with_capacity(closure.proto.max_stack_size as usize),
            pc: 0,
            proto: closure.proto.clone(),
            upvalues: Vec::with_capacity(closure.proto.max_stack_size as usize),
        };
        for x in parameters {
            new_frame.registers.push(x.clone())
        }
        for _ in new_frame.registers.len()..new_frame.registers.capacity() {
            new_frame.registers.push(LuaValue::NIL)
        }
        for i in 0..new_frame.upvalues.capacity() {
            new_frame.upvalues.push(None);
        }
        execstate.stack.push(new_frame);
    }
    let results = closure_loop(closure, execstate, parameters);
    execstate.stack.truncate(execstate.stack.len() - 1);
    return results;
}

// Inner function to allow Try usage; Outer function is execute_closure and handles stack push/pop
fn closure_loop(closure: &mut LuaClosure, execstate: &mut ExecutionState, parameters: &[LuaValue]) -> Result<Varargs, ExecuteError> {
    // init_frame_vars!(execstate, frame, registers, pc, proto);        // Commented out and replaced with expanded version for IDE suggestions
    let (frame_index, mut frame) = {
        if execstate.stack.len() == 0 {
            return Err(ExecuteError::EmptyStack);
        } else {
            let frame_index = execstate.stack.len() - 1;
            (frame_index, &mut execstate.stack[frame_index])
        }
    };

    let mut registers = &mut frame.registers;
    #[allow(unused)]    // Not strictly required, but IDE doesn't seem to recognize this
    let mut pc = &mut frame.pc;
    let mut proto = &frame.proto;
    debug_assert_eq!(proto.max_stack_size as usize, registers.len());

    loop {
        init_frame_vars!(execstate, frame, registers, pc, proto);
        let instruction = next_op(proto, pc)?;

        let opcode: u8 = (instruction & 0b111111) as u8;
        let a = (instruction >> 6) as usize & 0b1111_1111;
        let b = (instruction >> 23) as usize & 0b1_1111_1111;
        let c = (instruction >> 14) as usize & 0b1_1111_1111;
        let bx = (instruction >> 14) as usize & 0b11_1111_1111_1111_1111;
        let sbx = ((instruction >> 14) as usize & 0b11_1111_1111_1111_1111) as isize - (18isize.pow(2) / 2);

        #[allow(unused)] let a_rk = (a >> 7 == 1, a & 0b0111_1111);  // Technically never used
        let b_rk = (b >> 8 == 1, b & 0b0_1111_1111);
        let c_rk = (c >> 8 == 1, c & 0b0_1111_1111);

        match opcode {
            opcodes::MOVE => replace(get_reg_mut(registers, b)?.clone(), get_reg_mut(registers, a)?),
            opcodes::LOADK => replace(
                get_const(proto, bx)?.clone(),
                get_reg_mut(registers, a)?,
            ),
            opcodes::LOADKX => {
                let extra_arg_op = next_op(proto, pc)?;
                let extra_arg_opcode = (extra_arg_op & 0b111111) as u8;
                if extra_arg_opcode == opcodes::EXTRAARG {
                    let ax = (extra_arg_op >> 6) as usize;
                    replace(
                        get_const(proto, ax)?.clone(),
                        get_reg_mut(registers, a)?,
                    )
                } else {
                    return Err(ExecuteError::MissingExtraArg(extra_arg_op));
                }
            }
            opcodes::LOADBOOL => {
                replace(LuaValue::BOOLEAN(b != 0), get_reg_mut(registers, a)?);
                if c != 0 {
                    frame.pc += 1;
                }
            }
            opcodes::LOADNIL => {
                for index in a..=b {
                    replace(LuaValue::NIL, get_reg_mut(registers, index)?);
                }
            }
            opcodes::GETUPVAL => {
                let upval = get_upvalue(closure, b, &mut execstate.stack[..])?.clone();

                mem::replace(
                    get_reg_mut(&mut execstate.stack.last_mut().unwrap().registers, a)?,
                    upval,
                );
            }
            opcodes::GETTABUP => {
                let upvalue = get_upvalue(closure, b, &mut execstate.stack[..])?;
                let table = upvalue.try_table()?;

                init_frame_vars!(execstate, frame, registers, pc, proto);
                replace(
                    table.get(get_rk(proto, registers, c_rk)?)?.clone(),
                    get_reg_mut(registers, a)?,
                )
            }
            opcodes::GETTABLE => {
                replace(
                    get_reg(registers, b)?.try_table()?.get(get_rk(proto, registers, c_rk)?)?.clone(),
                    get_reg_mut(registers, a)?,
                )
            }
            opcodes::SETTABUP => {
                let upvalue = get_upvalue(closure, a, &mut execstate.stack[..])?;
                init_frame_vars!(execstate, frame, registers, pc, proto);
                upvalue.try_table()?.set(get_rk(proto, registers, b_rk)?.clone(), get_rk(proto, registers, c_rk)?.clone())?;
            }
            opcodes::SETUPVAL => {
                let value = get_reg(registers, a)?.clone();
                set_upvalue(closure, b, &mut execstate.stack[..], value)?;
            }
            opcodes::SETTABLE => {
                let value = get_rk(proto, registers, c_rk)?.clone();
                let key = get_rk(proto, registers, b_rk)?.clone();
                get_reg_mut(registers, a)?
                    .try_table()?
                    .set(key, value)?;
            }
            opcodes::NEWTABLE => {
                fn floating_point_byte(floating: usize) -> usize {
                    let mantissa = 0b111 & floating;
                    let exponent = (0b1111_1000 & floating) >> 3;
                    if exponent == 0 {      // This has slightly different semantics for B and C, the reference implementation does not ignore the 9th bit and just checks for <8 and immediately returns input
                        mantissa
                    } else {
                        (mantissa | 0b1000) * (2usize.pow(exponent as u32 - 1))
                    }
                }

                replace(
                    LuaValue::TABLE(LuaTable::with_capacity(floating_point_byte(b), floating_point_byte(c))),
                    get_reg_mut(registers, a)?,
                );
            }
            opcodes::SELF => {
                let table = get_reg(registers, b)?.clone();
                replace(
                    table.try_table()?.get(get_rk(proto, registers, c_rk)?)?.clone(),
                    get_reg_mut(registers, a)?,
                );
                replace(
                    table,
                    get_reg_mut(registers, a + 1)?,
                );
            }
            opcodes::ADD => math_binary_op!(+, "__add", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::SUB => math_binary_op!(-, "__sub", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::MUL => math_binary_op!(*, "__mul", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::MOD => math_binary_op!(%, "__mod", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::POW => math_binary_op!(func: pow, "__pow", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::DIV => math_binary_op!(/, "__div", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::IDIV => math_binary_op!(func: idiv, "__idiv", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::BAND => math_binary_op!(&, "__band", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::BOR => math_binary_op!(|, "__bor", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::BXOR => math_binary_op!(^, "__bxor", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::SHL => math_binary_op!(<<, "__shl", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::SHR => math_binary_op!(>>, "__shr", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
            opcodes::UNM => math_unary_op!(-, "__unm", registers, a, b, closure, execstate, frame, registers, pc, proto),
            opcodes::BNOT => math_unary_op!(func: bnot, "__bnot", registers, a, b, closure, execstate, frame, registers, pc, proto),
            opcodes::NOT => {   // No metamethod
                replace(
                    LuaValue::from((!get_reg(registers, b)?)?),
                    get_reg_mut(registers, a)?,
                );
            }
            opcodes::LEN => math_unary_op!(func: len, "__len", registers, a, b, closure, execstate, frame, registers, pc, proto),
            opcodes::CONCAT => {
                let mut buffer = Vec::with_capacity((c + 1).saturating_sub(b));
                for index in b..=c {
                    buffer.push(get_reg(registers, index)?);
                }
                replace(
                    LuaValue::concat(&buffer[..])?,
                    get_reg_mut(registers, a)?,
                );
            }
            opcodes::JMP => {
                replace(
                    (*pc as isize + sbx) as usize,
                    pc,
                )
                // TODO: if (A) close all upvalues >= R(A - 1), maybe redundant if upvalue-closing-on-drop is implemented?
            }
            opcodes::EQ => {
                let lhs = get_reg(registers, b)?.clone();
                let rhs = get_reg(registers, c)?.clone();
                match (
                    get_metatable(&lhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__eq"))),
                    get_metatable(&rhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__eq")))
                ) {
                    (Some(Ok(LuaValue::FUNCTION(lhs_metamethod))), Some(Ok(LuaValue::FUNCTION(rhs_metamethod)))) if lhs_metamethod == rhs_metamethod => {   // If both have the same metamethod, call that
                        let result = do_call(closure, &lhs_metamethod.clone(), execstate, &[lhs, rhs])?;
                        init_frame_vars!(execstate, frame, registers, pc, proto);
                        if *result.first() != (a != 0) {
                            replace(*pc + 1, pc)
                        }
                    },
                    (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                    (Some(Err(_)), _) => unreachable!(),
                    (_, Some(Err(_))) => unreachable!(),
                    _ => {  // else, compare values for equality based on cmp::Eq implementation
                        if (lhs == rhs) != (a != 0) {
                            replace(*pc + 1, pc)
                        }
                    }
                }
            }
            opcodes::LT => {
                let lhs = get_reg(registers, b)?.clone();
                let rhs = get_reg(registers, c)?.clone();
                match (
                    get_metatable(&lhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__lt"))),
                    get_metatable(&rhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__lt")))
                ) {
                    (Some(Ok(LuaValue::FUNCTION(lhs_metamethod))), Some(Ok(LuaValue::FUNCTION(rhs_metamethod)))) if lhs_metamethod == rhs_metamethod => {
                        let result = do_call(closure, &lhs_metamethod.clone(), execstate, &[lhs, rhs])?;
                        init_frame_vars!(execstate, frame, registers, pc, proto);
                        if *result.first() != (a != 0) {
                            replace(*pc + 1, pc)
                        }
                    },
                    (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                    (Some(Err(_)), _) => unreachable!(),
                    (_, Some(Err(_))) => unreachable!(),
                    _ => {
                        if (lhs.partial_cmp(&rhs).ok_or(MathError::IncomparableTypes(lhs.type_name(), rhs.type_name()))? == Ordering::Less) != (a != 0) {
                            replace(*pc + 1, pc)
                        }
                    }
                }
            }
            opcodes::LE => {
                let lhs = get_reg(registers, b)?.clone();
                let rhs = get_reg(registers, c)?.clone();
                match (
                    get_metatable(&lhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__le"))),
                    get_metatable(&rhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__le")))
                ) {
                    (Some(Ok(LuaValue::FUNCTION(lhs_metamethod))), Some(Ok(LuaValue::FUNCTION(rhs_metamethod)))) if lhs_metamethod == rhs_metamethod => {
                        let result = do_call(closure, &lhs_metamethod.clone(), execstate, &[lhs, rhs])?;
                        init_frame_vars!(execstate, frame, registers, pc, proto);
                        if *result.first() != (a != 0) {
                            replace(*pc + 1, pc)
                        }
                    },
                    (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                    (Some(Err(_)), _) => unreachable!(),
                    (_, Some(Err(_))) => unreachable!(),
                    _ => {
                        if (lhs.partial_cmp(&rhs).ok_or(MathError::IncomparableTypes(lhs.type_name(), rhs.type_name()))? != Ordering::Greater) != (a != 0) {
                            replace(*pc + 1, pc)
                        }
                    }
                }
            }
            opcodes::TEST => {
                if get_reg(registers, a)?.is_truthy() != (c != 0) {
                    replace(*pc + 1, pc)
                }
            }
            opcodes::TESTSET => {
                if get_reg(registers, b)?.is_truthy() == (c != 0) {
                    replace(*pc + 1, pc)
                } else {
                    replace(
                        get_reg(registers, b)?.clone(),
                        get_reg_mut(registers, a)?,
                    )
                }
            }
            opcodes::CALL => {
                let function = get_reg(registers, a)?.try_function()?.clone();
                let param_range = if b == 0 {
                    a+1..(registers.len())
                } else {
                    a+1..a + b
                };
                let param_count = if param_range.start + 1 >= param_range.end {
                    0
                } else {
                    param_range.end - param_range.start - 1
                };

                let mut params = Vec::with_capacity(param_count);    // TODO: Verify that this doesn't lead to absurd allocations
                for i in param_range {
                    params.push(get_reg(registers, i)?.clone());
                }
                let result = do_call(closure, &function, execstate, &params[..])?;
                init_frame_vars!(execstate, frame, registers, pc, proto);

                let result_count = if c == 0 {
                    result.count()
                } else {
                    c - 1
                };

                for i in a..a + result_count {
                    replace(
                        result.n(i - a).clone(),
                        get_reg_mut(registers, i)?,
                    )
                }
            }
            opcodes::TAILCALL => {
                // TODO implement
            }
            opcodes::RETURN => {
                let result_range = match b {
                    0 => a..registers.len(),
                    1 => a..a,
                    b => a..a + b - 1
                };
                let result_count = if result_range.start + 1 >= result_range.end {
                    0
                } else {
                    result_range.end - result_range.start - 1
                };

                let mut results = Vec::with_capacity(result_count);
                for i in result_range {
                    results.push(get_reg(registers, i)?.clone())
                }
                return Ok(results.into());
            }
            opcodes::FORLOOP => {
                replace(
                    (get_reg(registers, a)? + get_reg(registers, a + 2)?)?,
                    get_reg_mut(registers, a)?,
                );

                let increment_is_positive = get_reg(registers, a + 2)? > &LuaValue::from(0);
                let index = get_reg(registers, a)?;
                let limit = get_reg(registers, a + 1)?;

                if (index >= limit && increment_is_positive) || (index <= limit && !increment_is_positive) {
                    replace(
                        (*pc as isize + sbx) as usize,
                        pc,
                    );
                    replace(
                        index.clone(),
                        get_reg_mut(registers, a + 3)?,
                    );
                }
            }
            opcodes::FORPREP => {
                replace(
                    (get_reg(registers, a)? - get_reg(registers, a + 2)?)?,
                    get_reg_mut(registers, a)?,
                );
                replace(
                    (*pc as isize + sbx) as usize,
                    pc,
                );
            }
            opcodes::TFORCALL => {
                let function = get_reg(registers, a)?.try_function()?.clone();

                let params = [get_reg(registers, a + 1)?.clone(), get_reg(registers, a + 2)?.clone()];
                let result = do_call(closure, &function, execstate, &params[..])?;
                init_frame_vars!(execstate, frame, registers, pc, proto);

                for i in a + 3..=a + 2 + c {
                    replace(
                        result.n(i - a + 3).clone(),
                        get_reg_mut(registers, i)?,
                    )
                }
            }
            opcodes::TFORLOOP => {
                if get_reg(registers, a + 1)? != &LuaValue::NIL {
                    replace(
                        get_reg(registers, a + 1)?.clone(),
                        get_reg_mut(registers, a)?,
                    );
                    replace(
                        (*pc as isize + sbx) as usize,
                        pc,
                    );
                }
            }
            opcodes::SETLIST => {
                let table = get_reg(registers, a)?.try_table()?.clone();
                for i in 1..=b {
                    let value = get_reg(registers, a + 1)?;
                    table.set(LuaValue::NUMBER(LuaNumber::from((c - 1) * LUA_FIELDS_PER_FLUSH + i)), value.clone())?;
                }
            }
            opcodes::CLOSURE => {
                let new_proto = proto.functions.get(bx).ok_or(ExecuteError::PrototypeOutOfBounds(bx))?.clone();

                let mut upvalues = Vec::with_capacity(new_proto.upvalues.len());    // TODO: set _ENV
                for desc in &new_proto.upvalues {
                    let upvalue;
                    if desc.in_stack() {
                        upvalue = frame.get_upvalue_from_reg(desc.index(), frame_index)?.clone()
                    } else {
                        upvalue = closure.upvalues.get(desc.index()).ok_or(ExecuteError::UpvalueOutOfBounds(desc.index()))?.clone();
                    }
                    upvalues.push(upvalue);
                }
                init_frame_vars!(execstate, frame, registers, pc, proto);

                let new_closure = LuaClosure {
                    proto: new_proto,
                    upvalues,
                    parent: None,
                };
                replace(
                    LuaValue::FUNCTION(LuaFunction::CLOSURE(Rc::from(RefCell::from(new_closure)))),
                    get_reg_mut(registers, a)?,
                );
            }
            opcodes::VARARG => {
                let vararg_len = match b {
                    0 => parameters.len(),
                    b => b - 1
                };
                for i in 0..vararg_len {
                    replace(
                        parameters.get(i).map(LuaValue::clone).unwrap_or(LuaValue::NIL),
                        get_reg_mut(registers, a + i)?,
                    );
                }
            }
            opcodes::EXTRAARG => {
                return Err(ExecuteError::AttemptToExecuteExtraArg);
            }
            _ => {
                return Err(ExecuteError::UnknownOpcode(opcode));
            }
        }
    }
}
