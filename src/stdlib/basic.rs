use crate::error::{TracedError, LuaError, ArgumentError, Traceable};
use crate::vm::ExecutionState;
use crate::vm;
use std::rc::Rc;
use std::cell::RefCell;
use crate::constants::types::{LUA_INT, LUA_FLOAT};
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::types::value::string::LuaString;
use crate::types::value::number::LuaNumber;
use crate::types::value::function::{LuaFunction, NativeClosure};
use crate::types::parameters::LuaParameters;
use crate::types::value::table::LuaTable;
use crate::types::{LuaType, CoerceFrom};

pub fn assert(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    match params.first() {
        Some(val) if bool::coerce_from(val).expect("Coerce to bool never fails!") => Ok(Varargs::from(params)),
        _ => error(execstate, &[params.get(1).map(LuaValue::clone).unwrap_or(LuaValue::from("assertion failed!"))])
    }
}

pub fn collectgarbage(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let option = params.try_coerce::<LuaString>(0).unwrap_or(LuaString::from("collect"));
    match &option {
        s if s == "collect" => Ok(Varargs::nil()),
        s if s == "stop" => Ok(Varargs::nil()),
        s if s == "restart" => Ok(Varargs::nil()),
        s if s == "count" => unimplemented!(),
        s if s == "step" => Ok(Varargs::nil()),
        s if s == "setpause" => Ok(Varargs::from(LuaValue::from(0 as LUA_INT))),
        s if s == "setstepmul" => Ok(Varargs::from(LuaValue::from(0 as LUA_INT))),
        s if s == "isrunning" => Ok(Varargs::from(LuaValue::from(true))),
        s => Err(TracedError::from_rust(LuaError::UserError { message: Some(LuaValue::from(LuaString::from("invalid option: ").append(s))), level: 0 }, collectgarbage))
    }
}

pub fn dofile(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn error(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let level;
    if let Some(LuaValue::NUMBER(LuaNumber::INT(n))) = params.get(1) {
        level = *n;
    } else {
        level = 0;
    }
    Err(TracedError::from_rust(LuaError::UserError { message: params.first().map(LuaValue::clone), level }, error))
}

pub fn getmetatable(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    if let Some(val) = params.first() {
        let metatable = vm::fetch::get_metatable(val, &execstate.metatables);
        if let Some(table) = metatable {
            if let Ok(guard) = table.get(&LuaValue::from("__metatable")) {
                Ok(Varargs::from(guard))
            } else {
                Ok(Varargs::from(table.clone()))
            }
        } else {
            Ok(Varargs::nil())
        }
    } else {
        Err(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "value", found: "no value", index: 0 }, getmetatable))
    }
}

pub fn ipairs(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let table = params.try_coerce::<LuaTable>(0)?;
        let closure_table = table.clone();
        let mut index: LUA_INT = 1;
        let closure: NativeClosure = Rc::new(RefCell::new(move |_: &mut ExecutionState, _: &[LuaValue]| {
            let result = closure_table.get(&LuaValue::from(index));
            match &result {
                Ok(LuaValue::NIL) => {} // End of iteration
                Ok(_) => index += 1,
                Err(_) => {} // End of iteration
            }
            result.map(|val| Varargs::from((LuaValue::from(index), val))).map_err(|e| TracedError::from_rust(e, ipairs))
        }));
        Varargs::from((LuaValue::from(LuaFunction::RUST_CLOSURE(closure)), LuaValue::from(table.clone()), LuaValue::from(0 as LUA_INT)))
    };
    result.trace(ipairs)
}

pub fn load(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn loadfile(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn next(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let table = params.try_coerce::<LuaTable>(0)?;
        let index = params.get(1).unwrap_or(&LuaValue::NIL);
        let result = table.next(index);
        Varargs::from(result.unwrap_or((LuaValue::NIL, LuaValue::NIL)))
    };
    result.trace(next)
}

pub fn pairs(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    if let Some(val) = params.first() {
        if let Some(metatable) = vm::fetch::get_metatable(val, &execstate.metatables) {
            if let Ok(function_value) = metatable.get(&LuaValue::from("__pairs")) {
                let result = vm::helper::do_call_from_rust(pairs, function_value, execstate, &[val.clone()])?;
                return Ok(result.select_range(0..=3));
            }
        }

        Ok(Varargs::from((LuaValue::from(LuaFunction::RUST_FUNCTION(next)), val.clone(), LuaValue::NIL)))
    } else {
        Err(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "value", found: "no value", index: 0 }, pairs))
    }
}

pub fn pcall(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let (func, params) = params.split_first()
        .ok_or(TracedError::from_rust(
            ArgumentError::InvalidArgument {
                expected: "function",
                found: params.first().unwrap_or(&LuaValue::NIL).type_name(),
                index: 0,
            },
            pcall,
        ))?;
    let result = match vm::helper::do_call_from_rust(pcall, func.clone(), execstate, params) {
        Ok(result) => Varargs::prepend(LuaValue::from(true), &result),
        Err(err) => Varargs::from((LuaValue::from(false), err.message())),
    };
    Ok(result)
}

pub fn print(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    match params.len() {
        0 => println!("{}", "\n"),
        _ => {
            let (first, others) = params.split_first().unwrap();
            print!("{}", first);
            for val in others {
                print!("\t{}", val)
            }
            println!();
        }
    }
    Ok(Varargs::empty())
}

pub fn rawequal(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let lhs = params.try_coerce::<LuaValue>(0)?;
        let rhs = params.try_coerce::<LuaValue>(1)?;
        Varargs::from(LuaValue::from(lhs == rhs))
    };
    result.trace(rawequal)
}

pub fn rawget(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let table = params.try_coerce::<LuaTable>(0)?;
        let index = params.try_coerce::<LuaValue>(1)?;
        Varargs::from(table.get(&index)?)
    };
    result.trace(rawget)
}

pub fn rawlen(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    match params.first() {
        Some(LuaValue::TABLE(t)) => Ok(Varargs::from(LuaValue::from(t.len()))),
        Some(LuaValue::STRING(s)) => Ok(Varargs::from(LuaValue::from(s.len()))),
        Some(_) | None => Err(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "table or string", found: params.get_value_or_nil(0).type_name(), index: 0 }, rawlen))
    }
}

pub fn rawset(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let table = params.try_coerce::<LuaTable>(0)?;
        let index = params.try_coerce::<LuaValue>(1)?;
        let value = params.try_coerce::<LuaValue>(2)?;

        table.set(index.clone(), value.clone())?;
        Varargs::from(table.clone())
    };
    result.trace(rawset)
}

pub fn select(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let index = params.try_coerce::<LUA_INT>(0)?;
        if index > 0 {
            Ok(Varargs::from(params.get(index as usize).map(LuaValue::clone).unwrap_or(LuaValue::NIL)))
        } else {
            Err(ArgumentError::InvalidArgument { expected: ">0 integer index", found: params.get_value_or_nil(0).type_name(), index: 0 })
        }?
    };
    result.trace(select)
}

pub fn setmetatable(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, LuaError> = try {
        let table = params.try_coerce::<LuaTable>(0)?;
        let new_metatable = params.try_coerce::<LuaTable>(1)?;
        if let Some(current_metatable) = table.metatable() {
            if let Ok(_) = current_metatable.get(&LuaValue::from("__metatable")) {
                Err(LuaError::user_str("cannot change protected metatable"))?;
            }
        }
        table.set_metatable(new_metatable.clone());
        Varargs::from(table.clone())
    };
    result.trace(setmetatable)
}

pub fn tonumber(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        match params.try_coerce::<LuaValue>(0)? {
            value @ LuaValue::NUMBER(_) => Varargs::from(value.clone()),
            LuaValue::STRING(LuaString::UNICODE(s)) => {
                let radix = match params.get(1) {
                    Some(LuaValue::NUMBER(LuaNumber::INT(i))) => (*i).clamp(2, 36) as u32,
                    _ => 10
                };
                let s = s.trim();
                match LUA_INT::from_str_radix(s, radix) {
                    Ok(number) => Varargs::from(LuaValue::from(number)),
                    Err(_) if radix == 10 => match s.parse::<LUA_FLOAT>() {
                        Ok(number) => Varargs::from(LuaValue::from(number)),
                        Err(_) => Varargs::nil()
                    },
                    Err(_) => Varargs::nil()
                }
            }
            _ => Varargs::nil()
        }
    };
    result.trace(tonumber)
}

pub fn tostring(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    if let Some(val) = params.first() {
        if let Some(metatable) = vm::fetch::get_metatable(val, &execstate.metatables) {
            if let Ok(function_value) = metatable.get(&LuaValue::from("__tostring")) {
                return vm::helper::do_call_from_rust(pairs, function_value, execstate, &[val.clone()]);
            }
        }
        Ok(Varargs::from(LuaValue::from(format!("{}", val))))
    } else {
        Err(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "value", found: "no value", index: 0 }, pairs))
    }
}

pub fn lua_type(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    if let Some(val) = params.first() {
        Ok(Varargs::from(LuaValue::from(val.type_name())))
    } else {
        Err(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "value", found: "no value", index: 0 }, pairs))
    }
}

pub const VERSION: &str = "Lua 5.3";

pub fn xpcall(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let (func, remainder) = params.split_first().ok_or(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "function", found: params.get_value_or_nil(0).type_name(), index: 0}, pcall))?;
    let (handler, params) = remainder.split_first().ok_or(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "message handler", found: remainder.get_value_or_nil(0).type_name(), index: 1}, pcall))?;

    let (call_result, is_err) = match vm::helper::do_call_from_rust(pcall, func.clone(), execstate, params) {
        Ok(result) => (result, false),
        Err(err) => (Varargs::from(err.message()), true),
    };

    match vm::helper::do_call_from_rust(pcall, handler.clone(), execstate, call_result.as_slice()) {
        Ok(result) => Ok(Varargs::prepend(LuaValue::from(is_err), &result)),
        Err(err) => Err(err),
    }
}

pub fn insert_basic_lib(execstate: &mut ExecutionState) {
    macro_rules! set_global {
        ($execstate:ident, $func:ident) => {
            set_global!($execstate, stringify!($func), $func)
        };
        ($execstate:ident, $name:expr, $func:ident) => {
            $execstate.global_env.insert($name, LuaValue::FUNCTION(LuaFunction::RUST_FUNCTION($func)));
        };
    }

    set_global!(execstate, assert);
    set_global!(execstate, collectgarbage);
    set_global!(execstate, dofile);
    set_global!(execstate, error);
    set_global!(execstate, getmetatable);
    set_global!(execstate, ipairs);
    set_global!(execstate, load);
    set_global!(execstate, loadfile);
    set_global!(execstate, next);
    set_global!(execstate, pairs);
    set_global!(execstate, pcall);
    set_global!(execstate, print);
    set_global!(execstate, rawequal);
    set_global!(execstate, rawget);
    set_global!(execstate, rawlen);
    set_global!(execstate, rawset);
    set_global!(execstate, select);
    set_global!(execstate, setmetatable);
    set_global!(execstate, tonumber);
    set_global!(execstate, tostring);
    set_global!(execstate, "type", lua_type);
    execstate.global_env.insert("_VERSION", LuaValue::from(VERSION));
    set_global!(execstate, "xpcall", xpcall);
}
