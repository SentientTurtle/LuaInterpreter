pub(crate) mod fetch;
pub(crate) mod helper;

use crate::constants::{opcodes, LUA_FIELDS_PER_FLUSH};
use std::mem;
use std::rc::Rc;
use self::fetch::*;
use self::helper::*;
use std::cmp::Ordering;
use std::cell::{RefCell};
use crate::error::{TracedError, LuaError, ByteCodeError, ArgumentError};
use crate::constants::types::LUA_INT;
use std::collections::HashMap;
use crate::types::value::table::LuaTable;
use crate::types::value::LuaValue;
use crate::types::value::function::{Prototype, ClosureImpl, LuaFunction, LuaClosure};
use crate::types::value::number::LuaNumber;
use crate::types::upvalue::Upvalue;
use crate::types::varargs::Varargs;
use crate::types::{LuaType, CoerceFrom};


pub struct TypeMetatables {
    boolean: Option<LuaTable>,
    number: Option<LuaTable>,
    string: Option<LuaTable>,
    function: Option<LuaTable>,
    thread: Option<LuaTable>,
}

pub struct ExecutionState {
    stack: Vec<StackFrame>,
    pub metatables: TypeMetatables,
    #[allow(dead_code)] time_to_live: u64,
    // TODO: Implement TTL
    pub global_env: HashMap<&'static str, LuaValue>,
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
                thread: None,
            },
            time_to_live: 0,
            global_env: HashMap::new(),
        }
    }

    pub fn create_env_table(&self) -> LuaTable {
        let table = LuaTable::with_capacity(0, self.global_env.len());
        table.set(LuaValue::from("_G"), LuaValue::from(table.clone())).unwrap();
        for (name, value) in &self.global_env {
            table.set(LuaValue::from(*name), value.clone()).unwrap();
        }
        table
    }
}

struct StackFrame {
    registers: Vec<LuaValue>,
    pc: usize,
    proto: Rc<Prototype>,
    upvalues: Vec<Option<Upvalue>>,
}

impl StackFrame {
    pub fn get_upvalue_from_reg(&mut self, register: usize, frame_index: usize) -> Result<Upvalue, ByteCodeError> {      // TODO: Refactor this in a way that doesn't rely on the frame's index being passed in
        debug_assert_eq!(self.upvalues.len(), self.registers.len());
        let opt = self.upvalues.get_mut(register).ok_or(ByteCodeError::RegisterIndexOutOfBounds { index: register, registers_length: self.registers.len() })?;
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

macro_rules! init_frame_vars {
    ($state:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {
        $frame = $state.stack[..].last_mut().unwrap();      // macro is only used in execute_closure, which guarantees at least 1 frame
        $registers = &mut $frame.$registers;
        #[allow(unused)]
        $pc = &mut $frame.$pc;
        $proto = &mut $frame.$proto;
        debug_assert_eq!($proto.max_stack_size as usize, $registers.len());
    };
}

macro_rules! math_binary_op {
    ($op:tt, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_rk($proto, $registers, $b)?.clone();
            let rhs = get_rk($proto, $registers, $c)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {       // TODO: RHS metatable
                let func = func?;
                let result = match do_call_from_lua($closure, *$pc, func, $execstate, &[lhs, rhs]) {
                    Ok(result) => result,
                    Err(err) => return CallResult::Err(err)
                };
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
            Ok(())
    }};
    (func: $op:ident, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_rk($proto, $registers, $b)?.clone();
            let rhs = get_rk($proto, $registers, $c)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {       // TODO: RHS metatable
                let func = func?;
                let result = match do_call_from_lua($closure, *$pc, func, $execstate, &[lhs, rhs]) {
                    Ok(result) => result,
                    Err(err) => return CallResult::Err(err)
                };
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
            Ok(())
    }};
}

macro_rules! math_unary_op {
    ($op:tt, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_reg($registers, $b)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {
                let func = func?;
                let result = match do_call_from_lua($closure, *$pc, func, $execstate, &[lhs]) {
                    Ok(result) => result,
                    Err(err) => return CallResult::Err(err)
                };
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
            Ok(())
    }};
    (func: $op:ident, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
            let lhs = get_reg($registers, $b)?.clone();
            if let Some(func) = get_metatable(&lhs, &$execstate.metatables).map(|table| table.get(&LuaValue::from($metamethod))) {
                let func = func?;
                let result = match do_call_from_lua($closure, *$pc, func, $execstate, &[lhs]) {
                    Ok(result) => result,
                    Err(err) => return CallResult::Err(err)
                };
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
            Ok(())
    }};
}

#[allow(unused)]
pub(crate) fn execute_closure(closure: &mut ClosureImpl, execstate: &mut ExecutionState, parameters: &[LuaValue]) -> Result<Varargs, TracedError> {
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

    let mut result = closure_loop(closure, execstate, parameters);
    execstate.stack.truncate(execstate.stack.len() - 1);
    loop {
        match result {
            CallResult::Ok(values) => return Ok(values),
            CallResult::TailCall { closure: tc_closure, parameters: tc_params } => {
                // TODO: See if this code-duplication can be cleaned up without borrowing nightmares
                let closure = &mut *tc_closure.borrow_mut();
                let parameters = &tc_params[..];
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
                result = closure_loop(closure, execstate, parameters);
                execstate.stack.truncate(execstate.stack.len() - 1);
            }
            CallResult::Err(err) => return Err(err),
        }
    }
}

enum CallResult {
    Ok(Varargs),
    TailCall { closure: LuaClosure, parameters: Vec<LuaValue> },
    Err(TracedError),
}

// Inner function to allow Try usage; Outer function is execute_closure and handles stack push/pop
fn closure_loop(closure: &mut ClosureImpl, execstate: &mut ExecutionState, parameters: &[LuaValue]) -> CallResult {
    // init_frame_vars!(execstate, frame, registers, pc, proto);        // Commented out and replaced with expanded version for IDE suggestions
    let (frame_index, mut frame) = {
        if execstate.stack.len() == 0 {
            unreachable!()  // TODO: Merge this function back with execute_closure to guarantee this.
        } else {
            let frame_index = execstate.stack.len() - 1;
            (frame_index, &mut execstate.stack[frame_index])
        }
    };

    let mut registers = &mut frame.registers;
    #[allow(unused)] let mut pc = &mut frame.pc;
    let mut proto = &frame.proto;
    debug_assert_eq!(proto.max_stack_size as usize, registers.len());

    // Variable used to extend the lifetime of tailcall parameters to that of the entire closure_loop call

    loop {
        init_frame_vars!(execstate, frame, registers, pc, proto);
        // Variables for stack trace
        let current_pc = *pc;
        let current_proto = proto.clone();

        let result: Result<(), LuaError> = try {
            let instruction = next_op(proto, pc)?;

            let opcode: u8 = (instruction & 0b111111) as u8;
            let a = (instruction >> 6) as usize & 0b1111_1111;
            let b = (instruction >> 23) as usize & 0b1_1111_1111;
            let c = (instruction >> 14) as usize & 0b1_1111_1111;
            let bx = (instruction >> 14) as usize & 0b11_1111_1111_1111_1111;
            let sbx = ((instruction >> 14) as usize & 0b11_1111_1111_1111_1111) as isize - 0x1FFFF;

            let b_rk = (b >> 8 == 1, b & 0b0_1111_1111);
            let c_rk = (c >> 8 == 1, c & 0b0_1111_1111);

            // TODO: remove
            #[cfg(test)] {
                println!("{} {} A:{} B:{} C:{} BX:{} SBX:{}", pc, crate::constants::opcodes::name(opcode), a, b, c, bx, sbx);
                println!("{:?}", registers)
            }


            match opcode {
                opcodes::MOVE => {
                    replace(
                        get_reg_mut(registers, b)?.clone(),
                        get_reg_mut(registers, a)?,
                    );
                    Ok(())
                }
                opcodes::LOADK => {
                    replace(
                        get_const(proto, bx)?.clone(),
                        get_reg_mut(registers, a)?,
                    );
                    Ok(())
                }
                opcodes::LOADKX => {
                    let extra_arg_op = next_op(proto, pc)?;
                    let extra_arg_opcode = (extra_arg_op & 0b111111) as u8;
                    if extra_arg_opcode == opcodes::EXTRAARG {
                        let ax = (extra_arg_op >> 6) as usize;
                        replace(
                            get_const(proto, ax)?.clone(),
                            get_reg_mut(registers, a)?,
                        );
                        Ok(())
                    } else {
                        Err(ByteCodeError::ExpectedExtraArg { found: extra_arg_op })
                    }
                }
                opcodes::LOADBOOL => {
                    replace(LuaValue::BOOLEAN(b != 0), get_reg_mut(registers, a)?);
                    if c != 0 {
                        frame.pc += 1;
                    }
                    Ok(())
                }
                opcodes::LOADNIL => {
                    for index in a..=b {
                        replace(LuaValue::NIL, get_reg_mut(registers, index)?);
                    }
                    Ok(())
                }
                opcodes::GETUPVAL => {
                    let upval = get_upvalue(closure, b, &mut execstate.stack[..])?.clone();
                    mem::replace(
                        get_reg_mut(&mut execstate.stack.last_mut().unwrap().registers, a)?,
                        upval,
                    );
                    Ok(())
                }
                opcodes::GETTABUP => {
                    let upvalue = get_upvalue(closure, b, &mut execstate.stack[..])?;
                    let table = LuaTable::coerce_from(&upvalue)?;

                    init_frame_vars!(execstate, frame, registers, pc, proto);
                    replace(
                        table.get(get_rk(proto, registers, c_rk)?)?.clone(),
                        get_reg_mut(registers, a)?,
                    );
                    Ok(())
                }
                opcodes::GETTABLE => {
                    replace(
                        LuaTable::coerce_from(get_reg(registers, b)?)?.get(get_rk(proto, registers, c_rk)?)?.clone(),
                        get_reg_mut(registers, a)?,
                    );
                    Ok(())
                }
                opcodes::SETTABUP => {
                    let upvalue = get_upvalue(closure, a, &mut execstate.stack[..])?;
                    init_frame_vars!(execstate, frame, registers, pc, proto);
                    let table = LuaTable::coerce_from(&upvalue)?;
                    let key = get_rk(proto, registers, b_rk)?.clone();
                    let value = get_rk(proto, registers, c_rk)?.clone();
                    if let Some(Ok(func)) = table.metatable().as_ref().map(|t| t.get(&LuaValue::from("__newindex"))) {
                        if let Err(err) = do_call_from_lua(closure, current_pc, func, execstate, &[table.clone().into(), key, value]) {
                            return CallResult::Err(err);
                        }
                    } else {
                        table.set(key, value)?;
                    }
                    Ok(())
                }
                opcodes::SETUPVAL => {
                    let value = get_reg(registers, a)?.clone();
                    set_upvalue(closure, b, &mut execstate.stack[..], value)?;
                    Ok(())
                }
                opcodes::SETTABLE => {
                    let value = get_rk(proto, registers, c_rk)?.clone();
                    let key = get_rk(proto, registers, b_rk)?.clone();
                    let table = LuaTable::coerce_from(get_reg_mut(registers, a)?)?;
                    if let Some(Ok(func)) = table.metatable().as_ref().map(|t| t.get(&LuaValue::from("__newindex"))) {
                        let table = table.clone().into();
                        if let Err(err) = do_call_from_lua(closure, current_pc, func, execstate, &[table, key, value]) {
                            return CallResult::Err(err);
                        }
                    } else {
                        table.set(key, value)?;
                    }
                    Ok(())
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
                    Ok(())
                }
                opcodes::SELF => {
                    let table = get_reg(registers, b)?.clone();
                    replace(
                        LuaTable::coerce_from(&table)?.get(get_rk(proto, registers, c_rk)?)?.clone(),
                        get_reg_mut(registers, a)?,
                    );
                    replace(
                        table,
                        get_reg_mut(registers, a + 1)?,
                    );
                    Ok(())
                }
                opcodes::ADD => math_binary_op!(+, "__add", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::SUB => math_binary_op!(-, "__sub", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::MUL => math_binary_op!(*, "__mul", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::MOD => math_binary_op!(%, "__mod", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::POW => math_binary_op!(func: pow, "__pow", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::DIV => math_binary_op!(/, "__div", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::IDIV => math_binary_op!(func: idiv, "__idiv", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::BAND => math_binary_op!(&, "__band", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::BOR => math_binary_op!(|, "__bor", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::BXOR => math_binary_op!(^, "__bxor", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::SHL => math_binary_op!(<<, "__shl", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::SHR => math_binary_op!(>>, "__shr", registers, a, b_rk, c_rk, closure, execstate, frame, registers, pc, proto),
                opcodes::UNM => math_unary_op!(-, "__unm", registers, a, b, closure, execstate, frame, registers, pc, proto),
                opcodes::BNOT => math_unary_op!(func: bnot, "__bnot", registers, a, b, closure, execstate, frame, registers, pc, proto),
                opcodes::NOT => {   // No metamethod
                    replace(
                        LuaValue::from((!get_reg(registers, b)?)?),
                        get_reg_mut(registers, a)?,
                    );
                    Ok(())
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
                    Ok(())
                }
                opcodes::JMP => {
                    replace(
                        (*pc as isize + sbx) as usize,
                        pc,
                    );
                    // TODO: if (A) close all upvalues >= R(A - 1), maybe redundant if upvalue-closing-on-drop is implemented?
                    Ok(())
                }
                opcodes::EQ => {
                    let lhs = get_rk(proto, registers, b_rk)?.clone();
                    let rhs = get_rk(proto, registers, c_rk)?.clone();
                    match (
                        get_metatable(&lhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__eq"))),
                        get_metatable(&rhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__eq")))
                    ) {
                        (Some(Ok(lhs_metamethod)), Some(Ok(rhs_metamethod))) if lhs_metamethod == rhs_metamethod => {   // If both have the same metamethod, call that
                            let result = match do_call_from_lua(closure, current_pc, lhs_metamethod, execstate, &[lhs, rhs]) {
                                Ok(result) => result,
                                Err(err) => return CallResult::Err(err)
                            };
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (a != 0) {
                                replace(*pc + 1, pc)
                            }
                        }
                        (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                        (Some(Err(_)), _) => unreachable!(),
                        (_, Some(Err(_))) => unreachable!(),
                        _ => {  // else, compare values for equality based on cmp::Eq implementation
                            if (lhs == rhs) != (a != 0) {
                                replace(*pc + 1, pc)
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LT => {
                    let lhs = get_reg(registers, b)?.clone();
                    let rhs = get_reg(registers, c)?.clone();
                    match (
                        get_metatable(&lhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__lt"))),
                        get_metatable(&rhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__lt")))
                    ) {
                        (Some(Ok(lhs_metamethod)), Some(Ok(rhs_metamethod))) if lhs_metamethod == rhs_metamethod => {
                            let result = match do_call_from_lua(closure, current_pc, lhs_metamethod, execstate, &[lhs, rhs]) {
                                Ok(result) => result,
                                Err(err) => return CallResult::Err(err)
                            };
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (a != 0) {
                                replace(*pc + 1, pc)
                            }
                        }
                        (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                        (Some(Err(_)), _) => unreachable!(),
                        (_, Some(Err(_))) => unreachable!(),
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? == Ordering::Less) != (a != 0) {
                                replace(*pc + 1, pc)
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LE => {
                    let lhs = get_reg(registers, b)?.clone();
                    let rhs = get_reg(registers, c)?.clone();
                    match (
                        get_metatable(&lhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__le"))),
                        get_metatable(&rhs, &execstate.metatables).map(|table| table.get(&LuaValue::from("__le")))
                    ) {
                        (Some(Ok(lhs_metamethod)), Some(Ok(rhs_metamethod))) if lhs_metamethod == rhs_metamethod => {
                            let result = match do_call_from_lua(closure, current_pc, lhs_metamethod, execstate, &[lhs, rhs]) {
                                Ok(result) => result,
                                Err(err) => return CallResult::Err(err)
                            };
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (a != 0) {
                                replace(*pc + 1, pc)
                            }
                        }
                        (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                        (Some(Err(_)), _) => unreachable!(),
                        (_, Some(Err(_))) => unreachable!(),
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? != Ordering::Greater) != (a != 0) {
                                replace(*pc + 1, pc)
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::TEST => {
                    if bool::coerce_from(get_reg(registers, a)?)? != (c != 0) {
                        replace(*pc + 1, pc)
                    }
                    Ok(())
                }
                opcodes::TESTSET => {
                    if bool::coerce_from(get_reg(registers, b)?)? == (c != 0) {
                        replace(*pc + 1, pc)
                    } else {
                        replace(
                            get_reg(registers, b)?.clone(),
                            get_reg_mut(registers, a)?,
                        )
                    }
                    Ok(())
                }
                opcodes::TAILCALL => {
                    let function_value = get_reg(registers, a)?;
                    let function = fetch::get_function_from_value_call(function_value.clone(), &execstate.metatables)
                        .ok_or(ArgumentError::AttemptToCallNonFunction(function_value.clone()))?;

                    let param_range = if b == 0 {
                        a + 1..(registers.len())
                    } else {
                        a + 1..a + b
                    };
                    let param_count = if param_range.start + 1 >= param_range.end {
                        0
                    } else {
                        param_range.end - param_range.start - 1
                    };

                    let mut param_vec = Vec::with_capacity(param_count);    // TODO: Verify that this doesn't lead to absurd allocations
                    for i in param_range {
                        param_vec.push(get_reg(registers, i)?.clone());
                    }

                    return if let LuaFunction::LUA_CLOSURE(new_closure) = function {
                        CallResult::TailCall { closure: new_closure.clone(), parameters: param_vec }
                    } else {
                        match do_call_from_lua(closure, current_pc, function_value.clone(), execstate, &param_vec[..]) {
                            Ok(result) => CallResult::Ok(result),
                            Err(err) => CallResult::Err(err)
                        }
                    };
                }
                opcodes::CALL => {  // TODO: _CALL METAMETHOD
                    let function = get_reg(registers, a)?.clone();
                    let param_range = if b == 0 {
                        a + 1..(registers.len())
                    } else {
                        a + 1..a + b
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
                    let result = match do_call_from_lua(closure, current_pc, function, execstate, &params[..]) {
                        Ok(result) => result,
                        Err(err) => return CallResult::Err(err)
                    };
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
                    Ok(())
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
                    return CallResult::Ok(results.into());
                }
                opcodes::FORLOOP => {
                    replace(
                        (get_reg(registers, a)? + get_reg(registers, a + 2)?)?,
                        get_reg_mut(registers, a)?,
                    );

                    let increment_is_positive = get_reg(registers, a + 2)? > &LuaValue::from(0 as LUA_INT);
                    let index = get_reg(registers, a)?;
                    let limit = get_reg(registers, a + 1)?;

                    if (index <= limit && increment_is_positive) || (index >= limit && !increment_is_positive) {
                        replace(
                            (*pc as isize + sbx) as usize,
                            pc,
                        );
                        replace(
                            index.clone(),
                            get_reg_mut(registers, a + 3)?,
                        );
                    }
                    Ok(())
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
                    Ok(())
                }
                opcodes::TFORCALL => {
                    let function = get_reg(registers, a)?.clone();

                    let params = [get_reg(registers, a + 1)?.clone(), get_reg(registers, a + 2)?.clone()];
                    let result = match do_call_from_lua(closure, current_pc, function, execstate, &params[..]) {
                        Ok(result) => result,
                        Err(err) => return CallResult::Err(err)
                    };
                    init_frame_vars!(execstate, frame, registers, pc, proto);

                    for i in a + 3..=a + 2 + c {
                        replace(
                            result.n(i - a + 3).clone(),
                            get_reg_mut(registers, i)?,
                        )
                    }
                    Ok(())
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
                    Ok(())
                }
                opcodes::SETLIST => {       // TODO: Does this invoke metatables?
                    let table = LuaTable::coerce_from(get_reg(registers, a)?)?;
                    for i in 1..=b {
                        let value = get_reg(registers, a + 1)?;
                        table.set(LuaValue::NUMBER(LuaNumber::from((c - 1) * LUA_FIELDS_PER_FLUSH + i)), value.clone())?;
                    }
                    Ok(())
                }
                opcodes::CLOSURE => {
                    let new_proto = proto.functions.get(bx).ok_or(ByteCodeError::PrototypeIndexOutOfBounds { prototype_index: bx, prototype_len: proto.functions.len() })?.clone();

                    let mut upvalues = Vec::with_capacity(new_proto.upvalues.len());    // TODO: set _ENV
                    for desc in &new_proto.upvalues {
                        let upvalue;
                        if desc.in_stack() {
                            upvalue = frame.get_upvalue_from_reg(desc.index(), frame_index)?.clone()
                        } else {
                            upvalue = closure.upvalues.get(desc.index()).ok_or(ByteCodeError::UpvalueIndexOutOfBounds { upvalue_index: desc.index(), upvalues_length: closure.upvalues.len() })?.clone();
                        }
                        upvalues.push(upvalue);
                    }
                    init_frame_vars!(execstate, frame, registers, pc, proto);

                    let new_closure = ClosureImpl {
                        proto: new_proto,
                        upvalues,
                        parent: None,
                    };
                    replace(
                        LuaValue::FUNCTION(LuaFunction::LUA_CLOSURE(Rc::from(RefCell::from(new_closure)))),
                        get_reg_mut(registers, a)?,
                    );
                    Ok(())
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
                    Ok(())
                }
                opcodes::EXTRAARG => Err(ByteCodeError::AttemptToExecuteExtraArg),
                _ => Err(ByteCodeError::UnknownOpcode { opcode })
            }?
        };
        if let Err(err) = result {
            return CallResult::Err(TracedError::from_lua(err, current_pc + 1, current_proto));  // Lua indexes instructions from 1
        }
    }
}
