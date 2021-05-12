mod fetch;
// pub(crate) mod fetch;

use crate::constants::opcodes;
use std::rc::Rc;
use self::fetch::*;
use std::cmp::Ordering;
use std::cell::{RefCell};
use crate::error::{TracedError, LuaError, ByteCodeError, ArgumentError, TraceableError};
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INT_UNSIGNED, UnpackedInstruction};
use std::collections::HashMap;
use crate::types::value::table::LuaTable;
use crate::types::value::{LuaValue, TypeMetatables};
use crate::types::value::function::{Prototype, ClosureImpl, LuaFunction, LuaClosure};
use crate::types::value::number::LuaNumber;
use crate::types::upvalue::Upvalue;
use crate::types::varargs::Varargs;
use crate::types::{LuaType, CoerceFrom};


pub struct ExecutionState {
    stack: Vec<StackFrame>,
    pub metatables: TypeMetatables,
    #[allow(dead_code)] time_to_live: u64,
    // TODO: Implement TTL
    pub global_env: LuaTable,
    pub modules: HashMap<&'static str, LuaTable>,
}

impl ExecutionState {
    pub fn blank() -> ExecutionState {
        let global_env = LuaTable::empty();
        global_env.raw_set("_G", global_env.clone()).expect("Raw set with string key should not error!");
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
            global_env,
            modules: HashMap::new(),
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
        for i in 0..self.upvalues.len() {
            if let Some(upval) = &self.upvalues[i] {
                upval.close(self.registers.get_mut(i).unwrap().clone())
            }
        }
    }
}

macro_rules! init_frame_vars {
    ($state:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {
        $frame = $state.stack[..].last_mut().unwrap();      // macro is only used in execute_closure, which guarantees at least 1 frame
        $registers = &mut $frame.$registers;
        #[allow(unused)]
        $pc = &mut $frame.$pc;
        #[allow(unused)]    // TODO: Diagnose this one
        $proto = &mut $frame.$proto;
        debug_assert_eq!($frame.upvalues.len(), $registers.len());
    };
}

macro_rules! math_binary_op {
    (REG $op:tt, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_reg($registers, $c)?.clone();
        match (
            lhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod)),
            rhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod))
        ) {
            (Some(Ok(LuaValue::NIL)), Some(Ok(LuaValue::NIL)))
                            | (Some(Ok(LuaValue::NIL)), None)
                            | (None, Some(Ok(LuaValue::NIL)))
                            | (None, None) => {
                set_reg($registers, $a, LuaValue::from((&lhs $op &rhs)?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let func = func?;
                let result = func.call($execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
    (REG func: $op:ident, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_reg($registers, $c)?.clone();

        match (
            lhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod)),
            rhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod))
        ) {
            (Some(Ok(LuaValue::NIL)), Some(Ok(LuaValue::NIL)))
                            | (Some(Ok(LuaValue::NIL)), None)
                            | (None, Some(Ok(LuaValue::NIL)))
                            | (None, None) => {
                set_reg($registers, $a, LuaValue::from(lhs.$op(&rhs)?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let func = func?;
                let result = func.call($execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
    (CONST $op:tt, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_const($proto, $c)?.clone();
        match (
            lhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod)),
            rhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod))
        ) {
            (Some(Ok(LuaValue::NIL)), Some(Ok(LuaValue::NIL)))
                            | (Some(Ok(LuaValue::NIL)), None)
                            | (None, Some(Ok(LuaValue::NIL)))
                            | (None, None) => {
                set_reg($registers, $a, LuaValue::from((&lhs $op &rhs)?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let func = func?;
                let result = func.call($execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
    (CONST func: $op:ident, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_const($proto, $c)?.clone();

        match (
            lhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod)),
            rhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod))
        ) {
            (Some(Ok(LuaValue::NIL)), Some(Ok(LuaValue::NIL)))
                            | (Some(Ok(LuaValue::NIL)), None)
                            | (None, Some(Ok(LuaValue::NIL)))
                            | (None, None) => {
                set_reg($registers, $a, LuaValue::from(lhs.$op(&rhs)?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let func = func?;
                let result = func.call($execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
    (SHRI $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = LuaValue::from(($c as isize - ((2isize.pow(8) / 2) - 1)) as LUA_INT);
        match lhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod)) {
            Some(Ok(LuaValue::NIL)) | None => {
                set_reg($registers, $a, LuaValue::from((&lhs >> &rhs)?))?;
            }
            Some(func) => {
                let func = func?;
                let result = func.call($execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
    (SHLI $metamethod:expr, $registers:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = LuaValue::from(($c as isize - ((2isize.pow(8) / 2) - 1)) as LUA_INT);
        let rhs = get_reg($registers, $b)?.clone();
        match (rhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod))) {
            Some(Ok(LuaValue::NIL)) | None => {
                set_reg($registers, $a, LuaValue::from((&lhs << &rhs)?))?;
            }
            Some(func) => {
                let func = func?;
                let result = func.call($execstate, &[lhs, rhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
}


macro_rules! math_unary_op {
    ($op:tt, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = get_reg($registers, $b)?.clone();

        match lhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod)) {
            Some(Ok(LuaValue::NIL)) | None => {
                set_reg($registers, $a, LuaValue::from(($op &lhs)?))?;
            }
            Some(func) => {
                let func = func?;
                let result = func.call($execstate, &[lhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
    (func: $op:ident, $metamethod:expr, $registers:expr, $a:expr, $b:expr, $closure:expr, $execstate:expr, $frame:ident, $register_ident:ident, $pc:ident, $proto:ident) => {{
        let lhs = get_reg($registers, $b)?.clone();

        match lhs.get_metatable(&$execstate.metatables).map(|table| table.raw_get_into($metamethod)) {
            Some(Ok(LuaValue::NIL)) | None => {
                set_reg($registers, $a, LuaValue::from(lhs.$op()?))?;
            }
            Some(func) => {
                let func = func?;
                let result = func.call($execstate, &[lhs])?;
                init_frame_vars!($execstate, $frame, $register_ident, $pc, $proto);
                set_reg($registers, $a, result.into_first())?;
            }
        }
        Ok(())
    }};
}

#[allow(unused)]
pub(crate) fn execute_closure(closure: &mut ClosureImpl, execstate: &mut ExecutionState, parameters: &[LuaValue]) -> Result<Varargs, TracedError> {
    let register_capacity = usize::max(closure.proto.max_stack_size as usize, parameters.len());
    let mut new_frame = StackFrame {
        registers: Vec::with_capacity(register_capacity),
        pc: 0,
        proto: closure.proto.clone(),
        upvalues: Vec::with_capacity(register_capacity),
    };

    new_frame.registers.extend_from_slice(parameters);
    new_frame.registers.resize_with(register_capacity, Default::default);
    new_frame.upvalues.resize_with(register_capacity, Default::default);

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

                let register_capacity = usize::max(closure.proto.max_stack_size as usize, parameters.len());
                let mut new_frame = StackFrame {
                    registers: Vec::with_capacity(register_capacity),
                    pc: 0,
                    proto: closure.proto.clone(),
                    upvalues: Vec::with_capacity(register_capacity),
                };

                new_frame.registers.extend_from_slice(parameters);
                new_frame.registers.resize_with(register_capacity, Default::default);
                new_frame.upvalues.resize_with(register_capacity, Default::default);

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

//noinspection DuplicatedCode
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

    #[allow(unused)]    // IDE type hinting; Overwritten in loop
    let mut registers = &mut frame.registers;

    #[allow(unused)]
    let mut pc = &mut frame.pc;

    #[allow(unused)]    // IDE type hinting; Overwritten in loop
    let mut proto = &frame.proto;

    let mut top = registers.len();

    // Variable used to extend the lifetime of tailcall parameters to that of the entire closure_loop call
    loop {
        // init_frame_vars!(execstate, frame, registers, pc, proto);

        frame = execstate.stack[..].last_mut().unwrap();
        registers = &mut frame.registers;
        #[allow(unused)]
            pc = &mut frame.pc;
        #[allow(unused)]
            proto = &mut frame.proto;
        debug_assert_eq!(frame.upvalues.len(), registers.len());

        // Variables for stack trace
        let current_pc = *pc;
        let current_proto = proto.clone();

        let result: Result<(), TraceableError> = try {
            let instruction = next_op(proto, pc)?;

            // println!("{}\t{}", current_pc, InstructionDisplay {
            //     proto,
            //     index: current_pc,
            //     instruction
            // });

            let UnpackedInstruction {
                opcode,
                a,
                b,
                sb,
                c,
                sc,
                bx,
                sbx,
                ax: _ax,
                sj,
                k
            } = instruction.unpack();

            match opcode {
                opcodes::MOVE => {
                    let value = get_reg_mut(registers, b)?.clone();
                    set_reg(registers, a, value)
                }
                opcodes::LOADI => set_reg(registers, a, LuaValue::from(sbx as LUA_INT)),
                opcodes::LOADF => set_reg(registers, a, LuaValue::from(sbx as LUA_FLOAT)),
                opcodes::LOADK => {
                    let val = get_const(proto, bx)?.clone();
                    set_reg(registers, a, val)
                }
                opcodes::LOADKX => {
                    let extra_arg_op = next_op(proto, pc)?;
                    if extra_arg_op.opcode() == opcodes::EXTRAARG {
                        set_reg(registers, a, get_const(proto, extra_arg_op.unpack().ax)?.clone())
                    } else {
                        Err(ByteCodeError::ExpectedExtraArg { found: extra_arg_op })
                    }
                }
                opcodes::LOADFALSE => set_reg(registers, a, LuaValue::from(false)),
                opcodes::LOADFALSESKIP => {
                    set_reg(registers, a, LuaValue::from(false))?;
                    frame.pc += 1;
                    Ok(())
                }
                opcodes::LOADTRUE => set_reg(registers, a, LuaValue::from(true)),
                opcodes::LOADNIL => {
                    for index in a..=a + b {
                        set_reg(registers, index, LuaValue::NIL)?;
                    }
                    Ok(())
                }
                opcodes::GETUPVAL => {
                    let upval = get_upvalue(closure, b, &mut execstate.stack[..])?.clone();
                    set_reg(&mut execstate.stack.last_mut().unwrap().registers, a, upval)
                }
                opcodes::SETUPVAL => {
                    let value = get_reg(registers, a)?.clone();
                    set_upvalue(closure, b, &mut execstate.stack[..], value).map(|old| drop(old))
                }
                opcodes::GETTABUP => {
                    // Todo: check for :string meaning
                    let upvalue = get_upvalue(closure, b, &mut execstate.stack[..])?;
                    init_frame_vars!(execstate, frame, registers, pc, proto);
                    set_reg(registers, a, upvalue.index_with_metatable(get_const(proto, c)?, &execstate.metatables)?.clone())
                }
                opcodes::GETTABLE => set_reg(registers, a, get_reg(registers, b)?.index_with_metatable(get_reg(registers, c)?, &execstate.metatables)?.clone()),
                opcodes::GETI => {
                    // TODO: Check for signed C
                    set_reg(registers, a, get_reg(registers, b)?.index_with_metatable(&LuaValue::from(c as LUA_INT), &execstate.metatables)?.clone())
                }
                opcodes::GETFIELD => {
                    set_reg(registers, a, get_reg(registers, b)?.index_with_metatable(get_const(proto, c)?, &execstate.metatables)?.clone())
                }
                opcodes::SETTABUP => {
                    let upvalue = get_upvalue(closure, a, &mut execstate.stack[..])?;
                    init_frame_vars!(execstate, frame, registers, pc, proto);
                    let table = LuaTable::coerce_from(&upvalue)?;
                    let key = get_const(proto, b)?.clone();
                    let value = get_rk(proto, registers, k, c)?.clone();

                    if let Some(Ok(function)) = table.metatable().as_ref().map(|t| t.raw_get_into("__newindex")) {
                        function.call(execstate, &[table.clone().into(), value])?;
                    } else {
                        table.set(key, value)?;
                    }
                    Ok(())
                }
                opcodes::SETTABLE => {
                    let key = get_reg(registers, b)?.clone();
                    let value = get_rk(proto, registers, k, c)?.clone();
                    let table = LuaTable::coerce_from(get_reg_mut(registers, a)?)?;
                    if let Some(Ok(function)) = table.metatable().as_ref().map(|t| t.raw_get_into("__newindex")) {
                        let table = table.clone().into();
                        function.call(execstate, &[table, key, value])?;
                    } else {
                        table.set(key, value)?;
                    }
                    Ok(())
                }
                opcodes::SETI => {
                    // TODO: Check if B needs to be signed
                    let key = LuaValue::from(b as LUA_INT);
                    let value = get_rk(proto, registers, k, c)?.clone();
                    let table = LuaTable::coerce_from(get_reg_mut(registers, a)?)?;
                    if let Some(Ok(function)) = table.metatable().as_ref().map(|t| t.raw_get_into("__newindex")) {
                        function.call(execstate, &[table.into(), key, value])?;
                    } else {
                        table.set(key, value)?;
                    }
                    Ok(())
                }
                opcodes::SETFIELD => {
                    let key = get_const(proto, b)?.clone();
                    let value = get_rk(proto, registers, k, c)?.clone();
                    let table = LuaTable::coerce_from(get_reg_mut(registers, a)?)?;
                    if let Some(Ok(function)) = table.metatable().as_ref().map(|t| t.raw_get_into("__newindex")) {
                        function.call(execstate, &[table.into(), key, value])?;
                    } else {
                        table.set(key, value)?;
                    }
                    Ok(())
                }
                opcodes::NEWTABLE => {
                    let extra_arg_op = next_op(proto, pc)?;
                    let array_capacity = if extra_arg_op.opcode() == opcodes::EXTRAARG {
                        if k != 0 {
                            (extra_arg_op.unpack().ax << 8) | c
                        } else {
                            c
                        }
                    } else {
                        Err(ByteCodeError::ExpectedExtraArg { found: extra_arg_op })?
                    };
                    let hash_capacity = if b != 0 { 2usize.pow(b as u32) + 1 } else { 0 };

                    set_reg(registers, a, LuaValue::TABLE(LuaTable::with_capacity(array_capacity, hash_capacity)))?;
                    Ok(())
                }
                opcodes::SELF => {
                    let table = get_reg(registers, b)?.clone();
                    set_reg(registers, a, table.index_with_metatable(get_rk(proto, registers, k, c)?, &execstate.metatables)?)?;
                    set_reg(registers, a + 1, table)
                }
                opcodes::ADDI => {
                    let addition = get_reg(registers, b)? + &LuaValue::from(sc as LUA_INT);
                    set_reg(registers, a, addition?)
                }
                opcodes::ADDK => math_binary_op!(CONST +, "__add", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::SUBK => math_binary_op!(CONST -, "__sub", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::MULK => math_binary_op!(CONST *, "__mul", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::MODK => math_binary_op!(CONST %, "__mod", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::POWK => math_binary_op!(CONST func: pow, "__pow", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::DIVK => math_binary_op!(CONST /, "__div", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::IDIVK => math_binary_op!(CONST func: idiv, "__idiv", registers, a, b, c, closure, execstate, frame, registers, pc, proto),

                opcodes::BANDK => math_binary_op!(CONST &, "__band", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::BORK => math_binary_op!(CONST |, "__bor", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::BXORK => math_binary_op!(CONST ^, "__bxor", registers, a, b, c, closure, execstate, frame, registers, pc, proto),

                opcodes::SHLI => math_binary_op!(SHLI "__shl", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::SHRI => math_binary_op!(SHRI "__shr", registers, a, b, c, closure, execstate, frame, registers, pc, proto),

                opcodes::ADD => math_binary_op!(REG +, "__add", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::SUB => math_binary_op!(REG -, "__sub", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::MUL => math_binary_op!(REG *, "__mul", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::MOD => math_binary_op!(REG %, "__mod", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::POW => math_binary_op!(REG func: pow, "__pow", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::DIV => math_binary_op!(REG /, "__div", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::IDIV => math_binary_op!(REG func: idiv, "__idiv", registers, a, b, c, closure, execstate, frame, registers, pc, proto),

                opcodes::BAND => math_binary_op!(REG &, "__band", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::BOR => math_binary_op!(REG |, "__bor", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::BXOR => math_binary_op!(REG ^, "__bxor", registers, a, b, c, closure, execstate, frame, registers, pc, proto),

                opcodes::SHL => math_binary_op!(REG <<, "__shl", registers, a, b, c, closure, execstate, frame, registers, pc, proto),
                opcodes::SHR => math_binary_op!(REG >>, "__shr", registers, a, b, c, closure, execstate, frame, registers, pc, proto),

                // TODO
                opcodes::MMBIN => {
                    Ok(())
                }
                opcodes::MMBINI => {
                    Ok(())
                }
                opcodes::MMBINK => {
                    Ok(())
                }


                opcodes::UNM => math_unary_op!(-, "__unm", registers, a, b, closure, execstate, frame, registers, pc, proto),
                opcodes::BNOT => math_unary_op!(func: bnot, "__bnot", registers, a, b, closure, execstate, frame, registers, pc, proto),
                opcodes::NOT => set_reg(registers, a, LuaValue::from((!get_reg(registers, b)?)?)),
                opcodes::LEN => math_unary_op!(func: len, "__len", registers, a, b, closure, execstate, frame, registers, pc, proto),


                opcodes::CONCAT => {
                    let mut buffer = Vec::with_capacity((c + 1).saturating_sub(b));
                    for index in b..=c {
                        buffer.push(get_reg(registers, index)?);
                    }
                    let value = LuaValue::concat(&buffer[..])?;
                    set_reg(registers, a, value)
                }


                opcodes::CLOSE => {
                    unimplemented!()
                }
                opcodes::TBC => {
                    unimplemented!()
                }

                opcodes::JMP => {
                    *pc = (*pc as isize + sj) as usize;
                    // TODO: if (A) close all upvalues >= R(A - 1), maybe redundant if upvalue-closing-on-drop is implemented?
                    Ok(())
                }
                opcodes::EQ => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_reg(registers, b)?.clone();
                    match (
                        lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__eq")),
                        rhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__eq"))
                    ) {
                        (Some(Ok(lhs_metamethod)), Some(Ok(rhs_metamethod))) if lhs_metamethod == rhs_metamethod && lhs_metamethod != LuaValue::NIL => {   // If both have the same metamethod, call that
                            let result = lhs_metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (a != 0) {
                                *pc = *pc + 1
                            }
                        }
                        (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                        (Some(Err(_)), _) => unreachable!(),
                        (_, Some(Err(_))) => unreachable!(),
                        _ => {  // else, compare values for equality based on cmp::Eq implementation
                            if (lhs == rhs) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LT => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_reg(registers, b)?.clone();
                    match (
                        lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__lt")),
                        rhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__lt"))
                    ) {
                        (Some(Ok(lhs_metamethod)), Some(Ok(rhs_metamethod))) if lhs_metamethod == rhs_metamethod => {
                            let result = lhs_metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                        (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                        (Some(Err(_)), _) => unreachable!(),
                        (_, Some(Err(_))) => unreachable!(),
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? == Ordering::Less) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LE => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_reg(registers, b)?.clone();
                    match (
                        lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__le")),
                        rhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__le"))
                    ) {
                        (Some(Ok(lhs_metamethod)), Some(Ok(rhs_metamethod))) if lhs_metamethod == rhs_metamethod => {
                            let result = lhs_metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                        (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                        (Some(Err(_)), _) => unreachable!(),
                        (_, Some(Err(_))) => unreachable!(),
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? != Ordering::Greater) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::EQK => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_const(proto, b)?.clone();
                    match (
                        lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__eq")),
                        rhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__eq"))
                    ) {
                        (Some(Ok(lhs_metamethod)), Some(Ok(rhs_metamethod))) if lhs_metamethod == rhs_metamethod && lhs_metamethod != LuaValue::NIL => {   // If both have the same metamethod, call that
                            let result = lhs_metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (a != 0) {
                                *pc = *pc + 1
                            }
                        }
                        (Some(Err(_)), Some(Err(_))) => unreachable!(), // String can't keyerror
                        (Some(Err(_)), _) => unreachable!(),
                        (_, Some(Err(_))) => unreachable!(),
                        _ => {  // else, compare values for equality based on cmp::Eq implementation
                            if (lhs == rhs) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::EQI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__eq")) {
                        Some(Ok(metamethod)) if metamethod != LuaValue::NIL => {   // If metamethod, call that
                            let result = metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (a != 0) {
                                *pc = *pc + 1
                            }
                        }
                        Some(Err(_)) => unreachable!(), // String can't keyerror
                        _ => {  // else, compare values for equality based on cmp::Eq implementation
                            if (lhs == rhs) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LTI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__lt")) {
                        Some(Ok(metamethod)) if metamethod != LuaValue::NIL => {
                            let result = metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                        Some(Err(_)) => unreachable!(), // String can't keyerror
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? == Ordering::Less) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LEI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__le")) {
                        Some(Ok(metamethod)) if metamethod != LuaValue::NIL => {
                            let result = metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                        Some(Err(_)) => unreachable!(), // String can't keyerror
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? != Ordering::Greater) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::GTI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__le")) {
                        Some(Ok(metamethod)) if metamethod != LuaValue::NIL => {
                            let result = metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() == (k != 0) {    // invert result of metatable
                                *pc = *pc + 1
                            }
                        }
                        Some(Err(_)) => unreachable!(), // String can't keyerror
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? == Ordering::Greater) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::GEI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.get_metatable(&execstate.metatables).map(|table| table.raw_get_into("__lt")) {
                        Some(Ok(metamethod)) if metamethod != LuaValue::NIL => {
                            let result = metamethod.call(execstate, &[lhs, rhs])?;
                            init_frame_vars!(execstate, frame, registers, pc, proto);
                            if *result.first() == (k != 0) {    // invert result of metatable
                                *pc = *pc + 1
                            }
                        }
                        Some(Err(_)) => unreachable!(), // String can't keyerror
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(ArgumentError::IncomparableTypes { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? != Ordering::Less) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::TEST => {
                    let condition = bool::coerce_from(get_reg(registers, a)?)?;
                    if (if condition { 1 } else { 0 }) != k {
                        *pc = *pc + 1
                    }
                    Ok(())
                }
                opcodes::TESTSET => {
                    let condition = bool::coerce_from(get_reg(registers, b)?)?;
                    if if condition { 1 } else { 0 } != k {
                        *pc = *pc + 1;
                    } else {
                        set_reg(registers, a, get_reg(registers, b)?.clone())?;
                    }
                    Ok(())
                }
                opcodes::CALL => {  // TODO: _CALL METAMETHOD
                    let function = get_reg(registers, a)?.clone();
                    let param_range = if b == 0 {
                        a + 1..top    // TODO: Top
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
                    let result = function.call(execstate, &params[..])?;
                    init_frame_vars!(execstate, frame, registers, pc, proto);

                    let result_count = if c == 0 {
                        result.count()
                    } else {
                        c - 1
                    };

                    for i in a..a + result_count {  // TODO: Resize registers if need be
                        set_reg(registers, i, result.n(i - a).clone())?;
                    }
                    top = a+result_count;

                    Ok(())
                }
                opcodes::TAILCALL => {
                    let function_value = get_reg(registers, a)?;
                    let function = function_value.clone().prep_call_with_metatable(&execstate.metatables)?;

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

                    // TODO: Check result values
                    return if let LuaFunction::LUA_CLOSURE(new_closure) = function {
                        CallResult::TailCall { closure: new_closure.clone(), parameters: param_vec }
                    } else {
                        match function_value.clone().call(execstate, &param_vec[..]) {
                            Ok(result) => CallResult::Ok(result),
                            Err(err) => CallResult::Err(err.trace_tail_call())
                        }
                    };
                }
                opcodes::RETURN => {
                    let result_range = match b {
                        0 => a..registers.len(),    // TODO: Check if registers.len == top
                        1 => a..a,
                        b => a..a + b - 1
                    };
                    let result_count = if result_range.start + 1 >= result_range.end {
                        0
                    } else {
                        result_range.end - result_range.start - 1
                    };

                    let mut results = Vec::with_capacity(result_count);
                    for i in result_range.clone() { // TODO: Remove .clone
                        results.push(get_reg(registers, i)?.clone())
                    }
                    return CallResult::Ok(results.into());
                }
                opcodes::RETURN0 => {
                    return CallResult::Ok(Varargs::empty());
                }
                opcodes::RETURN1 => {
                    return CallResult::Ok(Varargs::from(get_reg(registers, a)?.clone()));
                }
                opcodes::FORLOOP => {
                    set_reg(registers, a, (get_reg(registers, a)? + get_reg(registers, a + 2)?)?)?;

                    let increment_is_positive = get_reg(registers, a + 2)? > &LuaValue::from(0 as LUA_INT);
                    let index = get_reg(registers, a)?.clone();
                    let limit = get_reg(registers, a + 1)?;

                    if (&index <= limit && increment_is_positive) || (&index >= limit && !increment_is_positive) {
                        *pc = *pc - bx;
                        set_reg(registers, a + 3, index)?;
                    }
                    Ok(())
                }
                opcodes::FORPREP => {
                    let init = get_reg(registers, a)?.clone();  // Early clone to release borrow on 'registers' before the cloned value is moved.
                    let limit = get_reg(registers, a + 1)?;
                    let step = get_reg(registers, a + 2)?;

                    if let (LuaValue::NUMBER(LuaNumber::INT(init_int)), LuaValue::NUMBER(LuaNumber::INT(step_int))) = (&init, step) {
                        if *step_int == 0 {
                            Err(LuaError::user_str("Loop with step of 0!"))?;  // TODO: Replace with a more appropriate error?
                        }

                        // Convert limit to integer
                        let mut skip = false;
                        let limit_int = if let Some(limit_int) = LUA_INT::coerce(limit) {
                            limit_int
                        } else {
                            let limit_float = LUA_FLOAT::coerce_from(limit)?;
                            if *step_int > 0 && limit_float > (LUA_INT::MAX as LUA_FLOAT) {
                                LUA_INT::MAX
                            } else if *step_int > 0 && limit_float < (LUA_INT::MAX as LUA_FLOAT) {
                                skip = true; // Don't loop at all, the init value cannot be smaller than limit
                                0
                            } else if *step_int < 0 && limit_float < (LUA_INT::MAX as LUA_FLOAT) {
                                LUA_INT::MIN
                            } else if *step_int < 0 && limit_float >(LUA_INT::MAX as LUA_FLOAT) {
                                skip = true; // Don't loop at all, the init value cannot be larger than limit (negative step)
                                0
                            } else {
                                limit_float as LUA_INT
                            }
                        };

                        if (*step_int > 0 && *init_int > limit_int) || (*step_int < 0 && *init_int < limit_int)  {
                            skip = true;
                        }

                        if skip {
                            *pc += bx + 1;
                        } else {
                            let counter = if *step_int > 0 {    // TODO: Lua checks if step!=1 before division as an optimization
                                (limit_int as LUA_INT_UNSIGNED - *init_int as LUA_INT_UNSIGNED) / (*step_int as LUA_INT_UNSIGNED)
                            } else {
                                (*init_int as LUA_INT_UNSIGNED - limit_int as LUA_INT_UNSIGNED) / (((-(*step_int + 1)) as LUA_INT_UNSIGNED) + 1)
                            };
                            set_reg(registers, a + 1, LuaValue::from(counter as LUA_INT))?;
                            set_reg(registers, a + 3, init)?;
                        }
                    } else {
                        let limit_float = LUA_FLOAT::coerce_from(limit)?;
                        let step_float = LUA_FLOAT::coerce_from(step)?;
                        let init_float = LUA_FLOAT::coerce_from(&init)?;

                        if step_float == 0 as LUA_FLOAT {
                            Err(LuaError::user_str("Loop with step of 0!"))?;  // TODO: Replace with a more appropriate error?
                        } else if (step_float < 0 as LUA_FLOAT && init_float > limit_float) || (step_float >= 0 as LUA_FLOAT && init_float < limit_float) {
                            *pc += bx + 1;
                        } else {
                            set_reg(registers, a, LuaValue::from(init_float))?;
                            set_reg(registers, a + 1, LuaValue::from(limit_float))?;
                            set_reg(registers, a + 2, LuaValue::from(step_float))?;
                            set_reg(registers, a + 3, LuaValue::from(init_float))?;
                        }
                    }

                    set_reg(registers, a, (get_reg(registers, a)? - get_reg(registers, a + 2)?)?)?;
                    Ok(())
                }
                opcodes::TFORPREP => {
                    // TODO: Create upvalue for R[A+3]
                    *pc += bx;
                    Ok(())
                }
                opcodes::TFORCALL => {
                    let function = get_reg(registers, a)?.clone();
                    let params = [get_reg(registers, a + 1)?.clone(), get_reg(registers, a + 2)?.clone()];  // This momentarily borrows execstate
                    let result = function.call(execstate, &params)?;
                    init_frame_vars!(execstate, frame, registers, pc, proto);
                    for i in a + 4..=a + 3 + c {
                        set_reg(registers, i, result.n(i - (a + 4)).clone())?;
                    }
                    Ok(())
                }
                opcodes::TFORLOOP => {
                    if get_reg(registers, a + 2)? != &LuaValue::NIL {
                        set_reg(registers, a, get_reg(registers, a + 2)?.clone())?;
                        *pc -= bx;
                    }
                    Ok(())
                }
                opcodes::SETLIST => {       // TODO: Does this invoke metatables?
                    let table = LuaTable::coerce_from(get_reg(registers, a)?)?;
                    let start_index = if k == 1 {
                        let extra_arg_op = next_op(proto, pc)?;
                        if extra_arg_op.opcode() == opcodes::EXTRAARG {
                            (extra_arg_op.unpack().ax << 8) | c
                        } else {
                            Err(ByteCodeError::ExpectedExtraArg { found: extra_arg_op })?
                        }
                    } else {
                        c
                    };
                    let element_count = if b == 0 {
                        match top.checked_sub(a) {
                            None => {
                                println!("SETLIST UNDERFLOW A:{} TOP:{}", a, top);  // TODO: Replace with error
                                0
                            }
                            Some(count) => count
                        }
                    } else {
                        b
                    };
                    for i in 1..=element_count {    // TODO: Replace with register slice and bulk-set
                        let value = get_reg(registers, a + i)?;
                        table.set(LuaNumber::from(start_index + i), value.clone())?;
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
                    set_reg(registers, a, LuaValue::FUNCTION(LuaFunction::LUA_CLOSURE(Rc::from(RefCell::from(new_closure)))))?;
                    Ok(())
                }
                opcodes::VARARG => {
                    let mut vararg_len = match c {
                        0 => parameters.len(),
                        c => c - 1
                    };
                    // Variadic functions have registers' length set to the minimum value, so we need to upsize when copying parameters
                    for _ in registers.len()..(a + vararg_len) {
                        registers.push(LuaValue::NIL);
                    }

                    let varargs_parameter_count = parameters.len().saturating_sub(proto.param_count as usize);
                    if vararg_len > varargs_parameter_count {
                        vararg_len = varargs_parameter_count;
                    }

                    for i in 0..vararg_len {
                        set_reg(registers, a + i, parameters.get(proto.param_count as usize + i).map(LuaValue::clone).unwrap_or(LuaValue::NIL))?
                    }
                    top = a + vararg_len;

                    let upvalues = &mut frame.upvalues;
                    for _ in upvalues.len()..(a + vararg_len) {
                        upvalues.push(None);
                    }

                    Ok(())
                }
                opcodes::VARARGPREP => {
                    // TODO: Implement
                    Ok(())
                }
                opcodes::EXTRAARG => Err(ByteCodeError::AttemptToExecuteExtraArg),
                _ => Err(ByteCodeError::UnknownOpcode { opcode })
            }?
        };
        if let Err(err) = result {
            return CallResult::Err(err.trace_lua(current_pc, current_proto));
        }
    }
}
