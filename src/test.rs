use std::process::Command;
use crate::bytecode;
use crate::vm;
use crate::vm::{ExecutionState};
use crate::types::value::function::ClosureImpl;
use crate::types::value::LuaValue;
use crate::compiler::{LuaCompiler, DefaultCompiler};
use std::io::BufReader;
use std::fs::File;

fn do_test(file_name: &str) {
    eprintln!("Loading script: {:?}", file_name);
    let command = Command::new("luac").arg("-o").arg("-").arg(String::from("lua-tests/") + file_name + ".lua").output().expect("Luac error!");
    if command.stderr.len() > 0 {
        panic!("Script compile error: {}", String::from_utf8(command.stderr).unwrap());
    }

    let mut script_buffer: Vec<u8>;
    if cfg!(target_os = "windows") {    // Stupid hack to remove the LF -> CRLF upgrading windows does for SOME REASON (一︿一)
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
    let proto = bytecode::loader::load_chunk(&mut &script_buffer[..]).unwrap();
    // println!("{}", proto);
    let mut closure = ClosureImpl::from_proto_with_env(proto, LuaValue::from(execstate.global_env.clone()));
    eprintln!("Running script: {:?}", file_name);
    match vm::execute_closure(&mut closure, &mut execstate, &[]) {
        Ok(_) => eprintln!("Completed script: {:?}", file_name),
        Err(err) => {
            eprintln!("Failed script: {:?}", file_name);
            panic!("{:?}", err);
        }
    };
}

// TODO: Add compile-from-file to compiler module; Current implementation removes filename information
#[allow(unused)]
fn do_test_with_compiler_module(file_name: &str) {
    eprintln!("Loading script: {:?}", file_name);
    let file = match File::open(String::from("./lua-tests/") + file_name + ".lua") {
        Ok(file) => file,
        Err(err) => panic!("{}", err)
    };

    let proto = match DefaultCompiler::compile(
        &mut BufReader::new(file)
    ) {
        Ok(proto) => proto,
        Err(err) => panic!("{:?}", err),
    };
    let mut execstate = ExecutionState::blank();
    crate::stdlib::insert_all_lib(&mut execstate);
    let mut closure = ClosureImpl::from_proto_with_env(proto, LuaValue::from(execstate.global_env.clone()));
    eprintln!("Running script: {:?}", file_name);
    match vm::execute_closure(&mut closure, &mut execstate, &[]) {
        Ok(_) => eprintln!("Completed script: {:?}", file_name),
        Err(err) => {
            eprintln!("Failed script: {:?}", file_name);
            panic!("{:?}", err);
        }
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

test!(
    all,
    api,
    attrib,
    big,
    bitwise,
    bwcoercion,
    calls,
    closure,
    code,
    constructs,
    coroutine,
    cstack,
    db,
    errors,
    events,
    files,
    gc,
    gengc,
    goto,
    heavy,
    literals,
    locals,
    main,
    math,
    nextvar,
    pm,
    sort,
    strings,
    tpack,
    utf8,
    vararg,
    verybig
);