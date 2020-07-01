use crate::vm::ExecutionState;
use crate::types::value::table::LuaTable;
use crate::types::value::LuaValue;

pub fn insert_utf8_lib(execstate: &mut ExecutionState) {
    let table = LuaTable::empty();

    // set_table!(table, byte);

    execstate.global_env.insert("utf8", LuaValue::from(table));
}