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
        let mut is_utf8 = true;

        let table = params.try_coerce::<LuaTable>(0)?;
        let separator_option = params.try_coerce::<LuaString>(1).ok();
        let separator = match separator_option.as_ref() {
            None => b"",
            Some(sep) => {
                is_utf8 &= sep.is_utf8();
                sep.as_bytes()
            },
        };
        let start = params.try_coerce::<LUA_INT>(2).unwrap_or(1);
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
                    is_utf8 &= string.is_utf8();
                    if index != start {
                        buffer.extend_from_slice(separator)
                    }
                    buffer.extend_from_slice(string.as_bytes());
                }
            }
            index += 1;
        }

        if is_utf8 {    // If concatenated string is UTF-8, try to preserve this; Should String::from_utf8 fail, a binary LuaString is created
            match String::from_utf8(buffer) {   // TODO: Maybe replace with an unchecked conversion if 'is_utf8' is reliable enough. For now the performance impact is minimal.
                Ok(string) => Varargs::from(LuaValue::from(string)),
                Err(err) => {
                    debug_assert!(false, "Byte-vec to String conversion failed; This should not happen and is a bug as is_utf8 variable should track if the string is valid utf-8!");
                    Varargs::from(LuaValue::from(err.into_bytes().into_boxed_slice()))
                }
            }
        } else {
            Varargs::from(LuaValue::from(buffer.into_boxed_slice()))
        }
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

    execstate.global_env.raw_set("table",table.clone()).expect("Raw set with string key should not error!");
    execstate.modules.insert("table", table);
}