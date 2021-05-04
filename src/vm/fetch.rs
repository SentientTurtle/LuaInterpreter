// Module to isolate "get" functions
use crate::vm::*;
use crate::constants::types::LUA_INSTRUCTION;
use std::mem;
use crate::error::{ByteCodeError};
use crate::types::upvalue::UpvalueImpl;
use crate::types::value::function::{ClosureImpl, Prototype};
use crate::types::value::LuaValue;

pub(super) fn next_op(proto: &Prototype, pc: &mut usize) -> Result<LUA_INSTRUCTION, ByteCodeError> {
    let op = proto.code.get(*pc).ok_or(ByteCodeError::ProgramCounterOutOfBounds { counter: *pc, code_length: proto.code.len() })?;
    *pc = *pc + 1;
    Ok(*op)
}

pub(super) fn get_reg(registers: &Vec<LuaValue>, index: usize) -> Result<&LuaValue, ByteCodeError> {
    let registers_length = registers.len();
    registers.get(index).ok_or(ByteCodeError::RegisterIndexOutOfBounds { index, registers_length })
}

pub(super) fn get_reg_mut(registers: &mut Vec<LuaValue>, index: usize) -> Result<&mut LuaValue, ByteCodeError> {
    let registers_length = registers.len();
    registers.get_mut(index).ok_or(ByteCodeError::RegisterIndexOutOfBounds { index, registers_length})
}

// TODO: Maybe forcibly inline
pub(super) fn set_reg(registers: &mut Vec<LuaValue>, index: usize, value: LuaValue) -> Result<(), ByteCodeError> {
    Ok(*get_reg_mut(registers, index)? = value)
}

pub(super) fn get_const(proto: &Prototype, index: usize) -> Result<&LuaValue, ByteCodeError> {
    proto.constants.get(index).ok_or(ByteCodeError::ConstantIndexOutOfBounds { index, constants_length: proto.constants.len() })
}

pub(super) fn get_rk<'a, 'b: 'a>(proto: &'a Prototype, registers: &'b Vec<LuaValue>, k: u8, index: usize) -> Result<&'a LuaValue, ByteCodeError> {
    if k == 1 {
        get_const(proto, index)
    } else {
        get_reg(registers, index)
    }
}

pub(super) fn get_upvalue(closure: &ClosureImpl, index: usize, stack: &mut [StackFrame]) -> Result<LuaValue, ByteCodeError> {
    if let Some(upvalue) = closure.upvalues.get(index) {
        match upvalue.get() {
            UpvalueImpl::Open { frame, register } => {
                if let Some(frame) = stack.get(frame) {
                    match frame.registers.get(register) {
                        None => Err(ByteCodeError::UpvalueRegisterIndexOutOfBounds { upvalue_index: index, register_index: register, registers_length: frame.registers.len() }),
                        Some(value) => Ok(value.clone()),
                    }
                } else {
                    Err(ByteCodeError::UpvalueStackIndexOutOfBounds {
                        upvalue_index: index,
                        stack_index: frame,
                        stack_length: stack.len(),
                    })
                }
            }
            UpvalueImpl::Closed(value) => Ok(value)
        }
    } else {
        Err(ByteCodeError::UpvalueIndexOutOfBounds { upvalue_index: index, upvalues_length: closure.upvalues.len() })
    }
}

pub(super) fn set_upvalue<'a: 'c, 'b, 'c>(closure: &'a mut ClosureImpl, index: usize, stack: &'c mut [StackFrame], value: LuaValue) -> Result<LuaValue, ByteCodeError> {
    if let Some(upvalue) = closure.upvalues.get_mut(index) {
        match upvalue.get() {
            UpvalueImpl::Open { frame, register } => {
                if let Some(frame) = stack.get_mut(frame) {
                    match frame.registers.get_mut(register) {
                        None => Err(ByteCodeError::UpvalueRegisterIndexOutOfBounds { upvalue_index: index, register_index: register, registers_length: frame.registers.len() }),
                        Some(dest) => {
                            Ok(mem::replace(dest, value))
                        }
                    }
                } else {
                    Err(ByteCodeError::UpvalueStackIndexOutOfBounds {
                        upvalue_index: index,
                        stack_index: frame,
                        stack_length: stack.len(),
                    })
                }
            }
            UpvalueImpl::Closed(old) => {
                upvalue.close(value);
                Ok(old)
            }
        }
    } else {
        Err(ByteCodeError::UpvalueIndexOutOfBounds { upvalue_index: index, upvalues_length: closure.upvalues.len() })
    }
}