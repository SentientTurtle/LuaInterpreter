use crate::vm::ExecutionState;

macro_rules! set_table {
    ($table:ident, $func:ident) => {
        set_table!($table, stringify!($func), $func)
    };

    ($table:ident, $name:expr, $func:ident) => {
        $table.raw_set($name, LuaFunction::RUST_FUNCTION($func)).unwrap();
    };
}

#[allow(dead_code)]
pub mod basic;
#[allow(dead_code)]
pub mod coroutine;
#[allow(dead_code)]
pub mod package;
#[allow(dead_code)]
pub mod string;
#[allow(dead_code)]
pub mod utf8;
#[allow(dead_code)]
pub mod table;
#[allow(dead_code)]
pub mod math;
#[allow(dead_code)]
pub mod io;
#[allow(dead_code)]
pub mod os;
#[allow(dead_code)]
pub mod debug;

#[allow(dead_code)]
pub fn insert_all_lib(execstate: &mut ExecutionState) {
    basic::insert_basic_lib(execstate);
    coroutine::insert_coroutine_lib(execstate);
    package::insert_package_lib(execstate);
    string::insert_string_lib(execstate);
    utf8::insert_utf8_lib(execstate);
    table::insert_table_lib(execstate);
    math::insert_math_lib(execstate);
    io::insert_io_lib(execstate);
    os::insert_os_lib(execstate);
    debug::insert_debug_lib(execstate);
}