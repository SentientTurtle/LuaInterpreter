use crate::vm::ExecutionState;
use crate::error::{TracedError, ArgumentError, Traceable};
use crate::types::value::LuaValue;
use crate::types::value::string::LuaString;
use crate::types::varargs::Varargs;
use crate::types::value::table::LuaTable;
use crate::types::value::function::LuaFunction;
use crate::types::parameters::LuaParameters;
use crate::constants::types::LUA_INT;
use crate::types::CoerceFrom;


pub fn concat(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let table = params.try_coerce::<LuaTable>(0)?;
        let separator_option = params.try_coerce::<LuaString>(1).ok();
        let separator = separator_option.as_ref().map(LuaString::as_bytes).unwrap_or(b"");
        let start = params.try_coerce::<LUA_INT>(2).unwrap_or(2);
        let end = params.try_coerce::<LUA_INT>(3).ok();

        let mut buffer = Vec::new();
        let mut index = start;
        loop {
            if let Some(end) = end { if index > end { break; } }
            let key = LuaValue::from(index);
            match table.raw_get(&key) { // TODO: Check for __index
                Err(..) | Ok(LuaValue::NIL) => {
                    break;
                }
                Ok(val) => {
                    let string = LuaString::coerce_from(&val)?;
                    if index != start {
                        buffer.extend_from_slice(separator)
                    }
                    buffer.extend_from_slice(string.as_bytes());
                }
            }
            index += 1;
        }

        Varargs::from(LuaValue::from(buffer.into_boxed_slice()))
    };
    result.trace(concat)
}

pub fn unpack(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let table = params.try_coerce::<LuaTable>(0)?;
        let start = params.try_coerce::<LUA_INT>(1).unwrap_or(1);
        let end = params.try_coerce::<LUA_INT>(2).ok();

        let mut buffer = Vec::new();
        let mut index = start;
        loop {
            if let Some(end) = end { if index > end { break; } }
            let key = LuaValue::from(index);
            match table.raw_get(&key) {
                Err(..) | Ok(LuaValue::NIL) => {
                    break;
                }
                Ok(val) => {
                    buffer.push(val);
                }
            }
            index += 1;
        }

        Varargs::from(buffer)
    };
    result.trace(concat)
}

pub fn insert_table_lib(execstate: &mut ExecutionState) {
    let table = LuaTable::empty();

    set_table!(table, concat);
    set_table!(table, unpack);

    execstate.global_env.insert("table", LuaValue::from(table));
}