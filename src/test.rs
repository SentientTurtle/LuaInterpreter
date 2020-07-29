use std::process::Command;
use crate::bytecode;
use crate::vm;
use crate::vm::{ExecutionState};
use crate::types::value::function::ClosureImpl;
use crate::types::value::LuaValue;

fn do_test(file_name: &str) {
    eprintln!("Loading script: {:?}", file_name);
    let command = Command::new("luac").arg("-o").arg("-").arg(String::from("lua-tests/") + file_name + ".lua").output().expect("Luac error!");
    if command.stderr.len() > 0 {
        panic!("Script compile error: {}", String::from_utf8(command.stderr).unwrap());
    }

    let mut script_buffer: Vec<u8>;
    if cfg!(target_os = "windows") {    // Stupid hack to remove the LF -> CRLF upgrading windows does for SOME REASON
        let mut iter = command.stdout.into_iter();
        let mut prev = iter.next().unwrap();
        script_buffer = iter.filter_map(|b| {
            if b == 0xA && prev == 0xD {
                prev = b;
                None
            } else {
                let ret = Some(prev);
                prev = b;
                ret
            }
        }).collect();
        script_buffer.push(prev);
    } else {
        script_buffer = command.stdout;
    }
    let mut execstate = ExecutionState::blank();
    crate::stdlib::insert_all_lib(&mut execstate);

    eprintln!("Decoding script: {:?}", file_name);
    let proto = bytecode::read::decode_bytes(&mut &script_buffer).unwrap();
    let env = execstate.create_env_table();
    let mut closure = ClosureImpl::from_proto_with_env(proto, LuaValue::from(env));
    eprintln!("Running script: {:?}", file_name);
    match vm::execute_closure(&mut closure, &mut execstate, &[]) {
        Ok(_) => eprintln!("Completed script: {:?}", file_name),
        Err(err) => {
            eprintln!("Failed script: {:?}", file_name);
            panic!("{:?}", err);
        },
    };
}

macro_rules! test {
    ($($name:ident),+) => {
        $(
            #[test]
            fn $name() {
                do_test(stringify!($name));
            }
        )+
    };
}

#[test]
fn aaa_reproduce() {
    do_test(stringify!( aaa_reproduce ));
}

test!(api, attrib, /*big,*/ bitwise, calls, closure, code, constructs, coroutine, db, errors, events, files, goto, literals, locals, math, nextvar, pm, sort, strings, tpack, utf8, vararg, verybig);