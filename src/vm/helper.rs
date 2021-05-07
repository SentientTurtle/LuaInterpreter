// TODO: Maybe merge into fetch module?
use crate::vm::{ExecutionState, execute_closure};
use crate::error::{TracedError, TraceableError};
use std::ops::DerefMut;
use crate::types::value::function::{LuaFunction, ClosureImpl, NativeFunction};
use crate::types::varargs::Varargs;
use crate::types::value::LuaValue;

// TODO: Rework this to use the CallFrom enum
// pub enum CallFrom<'a> {
//     Lua{parent: &'a mut ClosureImpl, program_counter: usize}
// }

pub fn do_call(function: LuaValue, execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TraceableError> {
    match function.prep_call_with_metatable(&execstate.metatables)? {
        LuaFunction::LUA_CLOSURE(closure) => {
            execute_closure(&mut *closure.borrow_mut(), execstate, params)
                .map_err(TraceableError::TRACED)
        }
        LuaFunction::RUST_FUNCTION(function) => {
            function.ptr()(execstate, params)
                .map_err(|e| e.trace_native_wrap(function))
        }
        LuaFunction::RUST_CLOSURE(closure) => {
            closure.closure.borrow_mut().deref_mut()(execstate, params)
                .map_err(|e| e.trace_closure_wrap(closure.name))
        }
    }
}

pub fn do_call_from_lua(parent: &mut ClosureImpl, program_counter: usize, function_value: LuaValue, execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TraceableError> {
    let proto = parent.proto.clone();
    let function = function_value.prep_call_with_metatable(&execstate.metatables)
        .map_err(|error| TracedError::from_lua(error, program_counter, proto.clone()))?;

    // TODO: where is the refmut struct for this? .-.
    drop(parent);   // Obtain a lock on the parent closure, to ensure that it's refcell is unborrowed before the match block below may potentially re-borrow it.      TODO: Validate this with recursive functions
    let result: Result<Varargs, TraceableError> = try {
        match function {
            LuaFunction::LUA_CLOSURE(closure) => {
                let closure = &mut closure.borrow_mut();
                execute_closure(closure, execstate, params)?
            }
            LuaFunction::RUST_FUNCTION(func) => {
                func.ptr()(execstate, params).map_err(|e| e.trace_native(func))?
            }
            LuaFunction::RUST_CLOSURE(closure) => {
                (&mut closure.closure.borrow_mut().deref_mut())(execstate, params)?
            } //.map_err(|e| e.push_rust(*func))?   TODO: Do we need to push a stacktrace frame here?
        }
    };
    result
}


// TODO: Why does this trace for the callee?
pub fn do_call_from_rust(parent: NativeFunction, function_value: LuaValue, execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let function = function_value.prep_call_with_metatable(&execstate.metatables)
        .map_err(|error| TracedError::from_rust(error, parent))?;

    // TODO: Why is this here for rust functions (⊙_⊙)？
    drop(parent);   // Obtain a lock on the parent closure, to ensure that it's refcell is unborrowed before the match block below may potentially re-borrow it.      TODO: Validate this with recursive functions
    let result: Result<Varargs, TraceableError> = try {
        match function {
            LuaFunction::LUA_CLOSURE(closure) => execute_closure(&mut closure.borrow_mut(), execstate, params)?,
            LuaFunction::RUST_FUNCTION(func) => func.ptr()(execstate, params).map_err(|e| e.trace_native(func))?,
            LuaFunction::RUST_CLOSURE(closure) => (&mut closure.closure.borrow_mut().deref_mut())(execstate, params)? //.map_err(|e| e.push_rust(*func))?   TODO: Do we need to push a stacktrace frame here?
        }
    };
    result.map_err(|e| e.trace_native(parent))
}
