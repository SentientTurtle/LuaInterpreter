use crate::vm::ExecutionState;
use crate::types::value::table::LuaTable;
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::error::{TracedError, Traceable, ArgumentError, LuaError};
use crate::types::value::function::LuaFunction;
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INT_UNSIGNED};
use crate::types::parameters::LuaParameters;
use crate::types::value::number::LuaNumber;
use nom::lib::std::cmp::Ordering;
use crate::types::LuaType;

pub fn abs(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LuaNumber>(0)?;
        Varargs::from(if number > 0 { number } else { (-number)? })
    };
    result.trace(abs)
}

pub fn acos(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.acos())
    };
    result.trace(acos)
}

pub fn asin(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.asin())
    };
    result.trace(asin)
}

pub fn atan(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let y = params.try_coerce::<LUA_FLOAT>(0)?;
        let x = params.try_coerce::<LUA_FLOAT>(1).unwrap_or(1.0);
        Varargs::from(y.atan2(x)) // TODO: Test that atan behavior matches that of Lua
    };
    result.trace(atan)
}

pub fn ceil(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LuaNumber>(0)?;
        match number {
            LuaNumber::INT(_) => Varargs::from(number),
            LuaNumber::FLOAT(f) => Varargs::from(f.ceil()),
        }
    };
    result.trace(ceil)
}

pub fn cos(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.cos())
    };
    result.trace(cos)
}

pub fn deg(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.to_degrees())
    };
    result.trace(deg)
}

pub fn exp(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.exp())
    };
    result.trace(exp)
}

pub fn floor(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LuaNumber>(0)?;
        match number {
            LuaNumber::INT(_) => Varargs::from(number),
            LuaNumber::FLOAT(f) => Varargs::from(f.floor()),
        }
    };
    result.trace(floor)
}

// TODO: Verify this function works as expected
pub fn fmod(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let x = params.try_coerce::<LuaNumber>(0)?;
        let y = params.try_coerce::<LuaNumber>(1)?;
        match x {
            LuaNumber::INT(lhs) => {
                match y {
                    LuaNumber::INT(rhs) => {
                        Varargs::from(lhs % rhs)
                    }
                    LuaNumber::FLOAT(rhs) => {
                        let lhs = lhs as LUA_FLOAT;
                        Varargs::from(lhs % rhs)
                    }
                }
            }
            LuaNumber::FLOAT(lhs) => {
                let rhs = match y {
                    LuaNumber::INT(rhs) => rhs as f64,
                    LuaNumber::FLOAT(rhs) => rhs,
                };
                Varargs::from(lhs % rhs)
            }
        }
    };
    result.trace(fmod)
}

pub const HUGE: LUA_FLOAT = LUA_FLOAT::INFINITY;

pub fn log(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let x = params.try_coerce::<LUA_FLOAT>(0)?;
        let base = params.try_coerce::<LUA_FLOAT>(1).ok();
        Varargs::from(match base {
            None => x.ln(),
            Some(base) => x.log(base),
        })
    };
    result.trace(log)
}

// TODO: LT/LE metamethods
pub fn max(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, LuaError> = try {
        let first = params.try_coerce::<LuaValue>(0)?;
        let mut maximum = &first;
        for value in &params[1..] {
            match value.partial_cmp(maximum) {
                None => Err(LuaError::user_string(format!("cannot compare {} to {}", value.type_name(), maximum.type_name())))?,
                Some(Ordering::Less) | Some(Ordering::Equal) => {}
                Some(Ordering::Greater) => {
                    maximum = value;
                }
            }
        }
        Varargs::from(maximum.clone())
    };
    result.trace(max)
}

pub const MAX_INTEGER: LUA_INT = LUA_INT::MAX;

// TODO: LT/LE metamethods
pub fn min(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, LuaError> = try {
        let first = params.try_coerce::<LuaValue>(0)?;
        let mut minimum = &first;
        for value in &params[1..] {
            match value.partial_cmp(minimum) {
                None => Err(LuaError::user_string(format!("cannot compare {} to {}", value.type_name(), minimum.type_name())))?,
                Some(Ordering::Greater) | Some(Ordering::Equal) => {}
                Some(Ordering::Less) => {
                    minimum = value;
                }
            }
        }
        Varargs::from(minimum.clone())
    };
    result.trace(min)
}

pub const MIN_INTEGER: LUA_INT = LUA_INT::MIN;

pub fn modf(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LuaNumber>(0)?;
        match number {
            LuaNumber::INT(_) => Varargs::from((number, LuaNumber::from(0.0))),
            LuaNumber::FLOAT(f) => {
                let truncated = f.trunc();
                if (truncated as LUA_INT) as LUA_FLOAT == truncated {
                    Varargs::from((truncated as LUA_INT, f.fract()))
                } else {
                    Varargs::from((truncated, f.fract()))
                }
            }
        }
    };
    result.trace(modf)
}

pub const PI: LUA_FLOAT = std::f64::consts::PI as LUA_FLOAT;

pub fn rad(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.to_radians())
    };
    result.trace(rad)
}

pub fn random(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, LuaError> = try {
        let m = params.opt_coerce::<LUA_FLOAT>(0);  // Maybe replace this messy construct with a Ok/Err/None triplet return from try_coerce
        let n = params.opt_coerce::<LUA_FLOAT>(1);
        let (lower, upper) = match (m, n) {
            (None, None) => (0 as LUA_FLOAT, 1 as LUA_FLOAT),
            (Some(upper), None) => (1 as LUA_FLOAT, upper?),
            (Some(lower), Some(upper)) => (lower?, upper?),
            (None, Some(_)) => unreachable!("None parameter cannot be followed by existent parameter"),
        };

        let diff = upper - lower;

        if diff >= (0 as LUA_FLOAT) {
            let value = rand::random::<LUA_FLOAT>();
            Ok(Varargs::from(lower + (value*diff)))
        } else {
            Err(LuaError::user_str("range is empty!"))
        }?
    };
    result.trace(random)
}

#[allow(unused)]
pub fn randomseed(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, LuaError> = try {
        let m = params.try_coerce::<LUA_INT>(0);
        let n = params.try_coerce::<LUA_INT>(1);
        unimplemented!()
    };
    result.trace(randomseed)
}

pub fn sin(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.sin())
    };
    result.trace(sin)
}

pub fn sqrt(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.sqrt())
    };
    result.trace(sqrt)
}

pub fn tan(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_FLOAT>(0)?;
        Varargs::from(number.tan())
    };
    result.trace(tan)
}

pub fn tointeger(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LUA_INT>(0);
        match number {
            Ok(int) => Varargs::from(int),
            _ => Varargs::fail()
        }
    };
    result.trace(tan)
}

pub fn numtype(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let number = params.try_coerce::<LuaNumber>(0);
        match number {
            Ok(LuaNumber::INT(_)) => Varargs::from("int"),
            Ok(LuaNumber::FLOAT(_)) => Varargs::from("float"),
            _ => Varargs::fail()
        }
    };
    result.trace(numtype)
}

pub fn ult(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let m = params.try_coerce::<LUA_INT>(0)?;
        let n = params.try_coerce::<LUA_INT>(0)?;
        Varargs::from((m as LUA_INT_UNSIGNED) < (n as LUA_INT_UNSIGNED))
    };
    result.trace(ult)
}



pub fn insert_math_lib(execstate: &mut ExecutionState) {
    let table = LuaTable::empty();

    set_table!(table, abs);
    set_table!(table, acos);
    set_table!(table, asin);
    set_table!(table, atan);
    set_table!(table, ceil);
    set_table!(table, cos);
    set_table!(table, deg);
    set_table!(table, exp);
    set_table!(table, floor);
    set_table!(table, fmod);
    table.raw_set("huge", HUGE).unwrap();
    set_table!(table, log);
    set_table!(table, max);
    table.raw_set("maxinteger", MAX_INTEGER).unwrap();
    set_table!(table, min);
    table.raw_set("mininteger", MIN_INTEGER).unwrap();
    set_table!(table, modf);
    table.raw_set("pi", PI).unwrap();
    set_table!(table, rad);
    set_table!(table, random);
    set_table!(table, randomseed);
    set_table!(table, sin);
    set_table!(table, sqrt);
    set_table!(table, tan);
    set_table!(table, tointeger);
    set_table!(table, "type", numtype);
    set_table!(table, ult);

    execstate.global_env.insert("math", LuaValue::from(table));
}