// TODO: Maybe merge into fetch module?
use crate::vm::{ExecutionState, execute_closure, fetch};
use crate::error::{TracedError, ArgumentError};
use std::ops::DerefMut;
use crate::types::value::function::{LuaFunction, ClosureImpl, NativeFunction};
use crate::types::varargs::Varargs;
use crate::types::value::LuaValue;

pub fn do_call_from_lua(parent: &mut ClosureImpl, program_counter: usize, function_value: LuaValue, execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let proto = parent.proto.clone();
    let function = fetch::get_function_from_value_call(function_value.clone(), &execstate.metatables)
        .ok_or(TracedError::from_lua(ArgumentError::AttemptToCallNonFunction(function_value), program_counter, proto.clone()))?; //TODO replace 0 index

    // TODO: where is the refmut for this? .-.
    drop(parent);   // Obtain a lock on the parent closure, to ensure that it's refcell is unborrowed before the match block below may potentially re-borrow it.      TODO: Validate this with recursive functions
    let result: Result<Varargs, TracedError> = try {
        match function {
            LuaFunction::LUA_CLOSURE(closure) => execute_closure(&mut closure.borrow_mut(), execstate, params)?,
            LuaFunction::RUST_FUNCTION(func) => func(execstate, params).map_err(|e| e.push_rust(func))?,
            LuaFunction::RUST_CLOSURE(closure) => (&mut closure.borrow_mut().deref_mut())(execstate, params)? //.map_err(|e| e.push_rust(*func))?   TODO: Do we need to push a stacktrace frame here?
        }
    };
    result.map_err(|e| e.push_lua(program_counter, proto))
}

pub fn do_call_from_rust(parent: NativeFunction, function_value: LuaValue, execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let function = fetch::get_function_from_value_call(function_value.clone(), &execstate.metatables)
        .ok_or(TracedError::from_rust(ArgumentError::AttemptToCallNonFunction(function_value), parent))?; //TODO replace 0 index
    drop(parent);   // Obtain a lock on the parent closure, to ensure that it's refcell is unborrowed before the match block below may potentially re-borrow it.      TODO: Validate this with recursive functions
    let result: Result<Varargs, TracedError> = try {
        match function {
            LuaFunction::LUA_CLOSURE(closure) => execute_closure(&mut closure.borrow_mut(), execstate, params)?,
            LuaFunction::RUST_FUNCTION(func) => func(execstate, params).map_err(|e| e.push_rust(func))?,
            LuaFunction::RUST_CLOSURE(closure) => (&mut closure.borrow_mut().deref_mut())(execstate, params)? //.map_err(|e| e.push_rust(*func))?   TODO: Do we need to push a stacktrace frame here?
        }
    };
    result.map_err(|e| e.push_rust(parent))
}
