#![feature(type_ascription)]
#![feature(nll)]
#![feature(option_result_contains)]
#![feature(result_map_or)]
#![feature(stmt_expr_attributes)]

use std::path::Path;
use crate::types::{LuaClosure, Upvalue, LuaTable, LuaValue, Varargs, LuaFunction};
use crate::vm::{ExecutionState, ExecuteError};

#[allow(unused_variables, dead_code)]
mod types;
#[allow(unused_variables, dead_code)]
mod decode;
mod vm;
mod constants;
mod macros;
mod util;

fn print(params: &[LuaValue]) -> Result<Varargs, ExecuteError> {
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
        // fn(&'r [types::LuaValue]) -> std::result::Result<types::Varargs, vm::ExecuteError> {print}
        // fn(&'r [types::LuaValue]) -> std::result::Result<types::Varargs, vm::ExecuteError>
    }
    Ok(Varargs::empty())
}

fn main() -> Result<(), ExecuteError> {
    match decode::decode_file(Path::new("./luac.out")) {
        Ok(proto) => {
            println!("{}", proto);
            let env = LuaTable::empty();
            env.set(LuaValue::from("print"), LuaValue::from(LuaFunction::RUST(print)));
            let mut closure = LuaClosure {
                proto,
                upvalues: vec![Upvalue::new_closed(LuaValue::from(env))],
                parent: None
            };
            let mut execstate = ExecutionState::blank();
            vm::execute_closure(&mut closure, &mut execstate, &[])?;
        },
        Err(err) => {
            println!("{:?}", err);
        },
    }
    Ok(())
}
