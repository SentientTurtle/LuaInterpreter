use crate::types::{Prototype, LuaValue, LuaClosure, TypeCastError, LuaTable, TypeCoerceError, LuaString};
use crate::constants::opcodes;
use std::mem;
use crate::constants::types::LUA_INSTRUCTION;
use std::rc::Rc;
use self::fetch::*;

pub enum ExecuteError {
    ProgramCounterOutOfBounds { counter: usize, code_len: usize },
    UnknownOpcode(u8),
    AttemptToExecuteExtraArg,
    MissingExtraArg(LUA_INSTRUCTION),
    RegisterOutOfBounds(usize),
    ConstantOutOfBounds(usize),
    UpvalueOutOfBounds(usize),
    UpvalueRegisterOutOfBounds(usize),
    UpvalueUpvalueOutOfBounds(usize),
    IncompatibleType { expected: &'static str, found: &'static str },
    EmptyStack,
    UpvalueUnwoundStack,
    NonNumericString(LuaString)
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
            TypeCoerceError::FromString(string) => ExecuteError::NonNumericString(string),
        }
    }
}

pub struct ExecutionState {
    stack: Vec<StackFrame>
}

struct StackFrame {
    registers: Vec<LuaValue>,
    pc: usize,
    proto: Rc<Prototype>,
}

// Mem-replace with the arguments swapped for borrow checker reasons
#[inline(always)]
fn ireplace<T>(src: T, dest: &mut T) {
    mem::replace(dest, src);
}

mod fetch {
    // Module to isolate "get" functions
    use crate::executor::*;
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

    pub fn get_upvalue_desc(proto: &Prototype, index: usize) -> Result<&UpvalueDesc, ExecuteError> {
        proto.upvalues.get(index).ok_or(ExecuteError::UpvalueOutOfBounds(index))
    }

    fn fetch_upvalue<'a, 'b>(desc: &'a UpvalueDesc, stack: &'b mut [StackFrame]) -> Result<&'b LuaValue, ExecuteError> {
        let (frame, substack) = stack.split_last_mut().ok_or(ExecuteError::UpvalueUnwoundStack)?;
        if desc.in_stack() {
            get_reg(&frame.registers, desc.stack_index() as usize).map_err(|_| ExecuteError::UpvalueRegisterOutOfBounds(desc.stack_index() as usize))
        } else {
            let new_desc = frame.proto.upvalues.get(desc.stack_index() as usize).ok_or(ExecuteError::UpvalueUpvalueOutOfBounds(desc.stack_index() as usize))?;
            fetch_upvalue(new_desc, substack)   // Substack is always length N-1 of stack, thus preventing an infinite loop
        }
    }

    pub(super) fn get_upvalue<'a: 'c, 'b, 'c>(closure: &'a LuaClosure, proto: &'b Prototype, index: usize, stack: &'c mut [StackFrame]) -> Result<&'c LuaValue, ExecuteError> {
        let desc = get_upvalue_desc(proto, index)?;
        match closure.upvalues.get(desc) {
            None => fetch::fetch_upvalue(desc, stack),
            Some(val) => Ok(val),
        }
    }

    fn fetch_upvalue_mut<'a, 'b>(desc: &'a UpvalueDesc, stack: &'b mut [StackFrame]) -> Result<&'b mut LuaValue, ExecuteError> {
        let (frame, substack) = stack.split_last_mut().ok_or(ExecuteError::UpvalueUnwoundStack)?;
        if desc.in_stack() {
            get_reg_mut(&mut frame.registers, desc.stack_index() as usize).map_err(|_| ExecuteError::UpvalueRegisterOutOfBounds(desc.stack_index() as usize))
        } else {
            let new_desc = frame.proto.upvalues.get(desc.stack_index() as usize).ok_or(ExecuteError::UpvalueUpvalueOutOfBounds(desc.stack_index() as usize))?;
            fetch_upvalue_mut(new_desc, substack)   // Substack is always length N-1 of stack, thus preventing an infinite loop
        }
    }

    pub(super) fn get_upvalue_mut<'a: 'c, 'b, 'c>(closure: &'a mut LuaClosure, proto: &'b Prototype, index: usize, stack: &'c mut [StackFrame]) -> Result<&'c mut LuaValue, ExecuteError> {
        let desc = get_upvalue_desc(proto, index)?;
        match closure.upvalues.get_mut(desc) {
            None => fetch::fetch_upvalue_mut(desc, stack),
            Some(val) => Ok(val),
        }
    }
}

#[allow(unused)]
fn execute_closure(closure: &mut LuaClosure, mut state: ExecutionState) -> Result<(), ExecuteError> {
    {
        let new_frame = StackFrame {
            registers: Vec::with_capacity(closure.proto.max_stack_size as usize),
            pc: 0,
            proto: closure.proto.clone(),
        };
        state.stack.push(new_frame);
    }

    let (frame, substack) = state.stack[..].split_last_mut().ok_or(ExecuteError::EmptyStack)?;
    let StackFrame { registers, pc, proto } = frame;
    debug_assert_eq!(proto.max_stack_size as usize, registers.len());

    let instruction = next_op(proto, pc)?;

    let opcode: u8 = (instruction & 0b111111) as u8;
    let a = (instruction >> 6) as usize & 0b1111_1111;
    let b = (instruction >> 23) as usize & 0b1_1111_1111;
    let c = (instruction >> 14) as usize & 0b1_1111_1111;
    let bx = (instruction >> 14) as usize & 0b11_1111_1111_1111_1111;
    let sbx = ((instruction >> 14) as usize & 0b11_1111_1111_1111_1111) as isize - (18isize.pow(2) / 2);

    let a_rk = (a >> 7 == 1, a & 0b0111_1111);  // Technically never used
    let b_rk = (b >> 8 == 1, b & 0b0_1111_1111);
    let c_rk = (c >> 8 == 1, c & 0b0_1111_1111);

    match opcode {
        opcodes::MOVE => ireplace(get_reg_mut(registers, b)?.clone(), get_reg_mut(registers, a)?),
        opcodes::LOADK => ireplace(
            get_const(proto, bx)?.clone(),
            get_reg_mut(registers, a)?,
        ),
        opcodes::LOADKX => {
            let extra_arg_op = next_op(proto, pc)?;
            let extra_arg_opcode = (extra_arg_op & 0b111111) as u8;
            if extra_arg_opcode == opcodes::EXTRAARG {
                let ax = (extra_arg_op >> 6) as usize;
                ireplace(
                    get_const(proto, ax)?.clone(),
                    get_reg_mut(registers, a)?,
                )
            } else {
                return Err(ExecuteError::MissingExtraArg(extra_arg_op));
            }
        }
        opcodes::LOADBOOL => {
            ireplace(LuaValue::BOOLEAN(b != 0), get_reg_mut(registers, a)?);
            if c != 0 {
                frame.pc += 1;
            }
        }
        opcodes::LOADNIL => {
            for index in a..a + b {
                ireplace(LuaValue::NIL, get_reg_mut(registers, index)?);
            }
        }
        opcodes::GETUPVAL => {
            ireplace(
                get_upvalue(closure, proto, b, substack)?.clone(),
                get_reg_mut(registers, a)?,
            )
        }
        opcodes::GETTABUP => {
            let upvalue = get_upvalue(closure, proto, b, substack)?;
            let table = upvalue.try_table()?;
            ireplace(
                table.get(get_rk(proto, registers, c_rk)?).clone(),
                get_reg_mut(registers, a)?,
            )
        }
        opcodes::GETTABLE => {
            ireplace(
                get_reg(registers, b)?.try_table()?.get(get_rk(proto, registers, c_rk)?).clone(),
                get_reg_mut(registers, a)?,
            )
        }
        opcodes::SETTABUP => {
            ireplace(
                get_rk(proto, registers, c_rk)?.clone(),
                get_upvalue_mut(closure, proto, a, substack)?
                    .try_table()?
                    .get_mut(get_rk(proto, registers, b_rk)?),
            );
        }
        opcodes::SETUPVAL => {
            ireplace(
                get_reg(registers, a)?.clone(),
                get_upvalue_mut(closure, proto, b, substack)?
            )
        }
        opcodes::SETTABLE => {
            let value = get_rk(proto, registers, c_rk)?.clone();
            let key = get_rk(proto, registers, b_rk)?.clone();
            ireplace(
                value,
                get_reg_mut(registers, a)?
                    .try_table()?
                    .get_mut(&key),
            );
        }
        opcodes::NEWTABLE => {
            fn floating_point_byte(floating: usize) -> usize {
                let mantissa = 0b111 & floating;
                let exponent = ((0b1111_1000 & floating) >> 3);
                if exponent == 0 {      // This has slightly different semantics for B and C, the reference implementation does not ignore the 9th bit and just checks for <8 and immediately returns input
                    mantissa
                } else {
                    (mantissa | 0b1000)*(2usize.pow(exponent as u32 - 1))
                }
            }

            ireplace(
                LuaValue::TABLE(LuaTable::with_capacity(floating_point_byte(b), floating_point_byte(c))),
                get_reg_mut(registers, a)?
            );
        }
        opcodes::SELF => {
            let table = get_reg(registers, b)?.clone();
            ireplace(
                table.try_table()?.get_mut(get_rk(proto, registers, c_rk)?).clone(),
                get_reg_mut(registers, a)?
            );
            ireplace(
                table,
                get_reg_mut(registers, a + 1)?
            );
        }
        opcodes::ADD => {
            ireplace(
                get_reg(registers, b)? + get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::SUB => {
            ireplace(
                get_reg(registers, b)? - get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::MUL => {
            ireplace(
                get_reg(registers, b)? * get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::MOD => {
            ireplace(
                get_reg(registers, b)? % get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::POW => {
            ireplace(
                get_reg(registers, b)?.pow(get_reg(registers, c)?),
                get_reg_mut(registers, a)?
            );
        }
        opcodes::DIV => {
            ireplace(
                get_reg(registers, b)? / get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::IDIV => {
            ireplace(
                get_reg(registers, b)?.idiv(get_reg(registers, c)?),
                get_reg_mut(registers, a)?
            );
        }
        opcodes::BAND => {
            ireplace(
                get_reg(registers, b)? & get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::BOR => {
            ireplace(
                get_reg(registers, b)? | get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::BXOR => {
            ireplace(
                get_reg(registers, b)? ^ get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::SHL => {
            ireplace(
                get_reg(registers, b)? << get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::SHR => {
            ireplace(
                get_reg(registers, b)? >> get_reg(registers, c)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::UNM => {
            ireplace(
                -get_reg(registers, b)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::BNOT => {
            ireplace(
                !get_reg(registers, b)?,
                get_reg_mut(registers, a)?
            );
        }
        opcodes::NOT => {
            ireplace(
                get_reg(registers, b)?.as_boolean().not(),
                get_reg_mut(registers, a)?
            );
        }
        opcodes::LEN => {
            ireplace(
                get_reg(registers, b)?.len(),
                get_reg_mut(registers, a)?
            );
        }
        opcodes::CONCAT => {
            ireplace(
                get_reg(registers, b)?.concat(get_reg(registers, c)?),
                get_reg_mut(registers, a)?
            );
        }
        opcodes::JMP => {}
        opcodes::EQ => {}
        opcodes::LT => {}
        opcodes::LE => {}
        opcodes::TEST => {}
        opcodes::TESTSET => {}
        opcodes::CALL => {}
        opcodes::TAILCALL => {}
        opcodes::RETURN => {}
        opcodes::FORLOOP => {}
        opcodes::FORPREP => {}
        opcodes::TFORCALL => {}
        opcodes::TFORLOOP => {}
        opcodes::SETLIST => {}
        opcodes::CLOSURE => {}
        opcodes::VARARG => {}
        opcodes::EXTRAARG => {
            return Err(ExecuteError::AttemptToExecuteExtraArg);
        }
        _ => {
            return Err(ExecuteError::UnknownOpcode(opcode));
        }
    }
    state.stack.truncate(state.stack.len() - 1);
    Ok(())
}