use crate::vm::ExecutionState;
use crate::types::value::table::LuaTable;

pub fn insert_io_lib(execstate: &mut ExecutionState) {
    let table = LuaTable::empty();

    // set_table!(table, byte);

    execstate.global_env.raw_set("io", table.clone()).expect("Raw set with string key should not error!");
    execstate.modules.insert("io", table);
}