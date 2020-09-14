use crate::constants::types::{HOST_INT, LUA_INT, LUA_FLOAT, LUA_INSTRUCTION, HOST_OBJECT_SIZE};
use std::mem;
use std::rc::Rc;
use crate::constants;
use crate::types::value::string::LuaString;
use crate::types::value::LuaValue;
use crate::types::value::number::LuaNumber;
use crate::types::upvalue::UpvalueDesc;
use crate::types::locvar::LocVar;
use crate::types::value::function::Prototype;

fn write_byte(byte: u8, dest: &mut Vec<u8>) {
    dest.push(byte)
}

fn write_int(int: &HOST_INT, dest: &mut Vec<u8>) {
    dest.extend_from_slice(&HOST_INT::to_le_bytes(*int))
}

fn write_vec<T>(values: &Vec<T>, write_fn: fn(&T, &mut Vec<u8>), dest: &mut Vec<u8>) {
    for value in values {
        write_fn(value, dest)
    }
}

fn write_boolean(val: bool, dest: &mut Vec<u8>) {
    if val {
        dest.push(1)
    } else {
        dest.push(0)
    }
}

fn write_integer(val: LUA_INT, dest: &mut Vec<u8>) {
    dest.extend_from_slice(&LUA_INT::to_le_bytes(val))
}

fn write_floating(val: LUA_FLOAT, dest: &mut Vec<u8>) {
    let int_rep = unsafe { mem::transmute::<LUA_FLOAT, LUA_INT>(val) };
    dest.extend_from_slice(&LUA_INT::to_le_bytes(int_rep))
}

fn write_instruction(instruction: &LUA_INSTRUCTION, dest: &mut Vec<u8>) {
    dest.extend_from_slice(&LUA_INSTRUCTION::to_le_bytes(*instruction))
}

fn write_string(string: &Option<LuaString>, dest: &mut Vec<u8>) {
    if let Some(string) = string {
        match string.len() {
            0 => dest.push(0),
            0..=0xFF => dest.push((string.len() - 1) as u8),
            _ => {
                dest.push(0xFF);
                dest.extend_from_slice(&HOST_OBJECT_SIZE::to_le_bytes(string.len() - 1));
            }
        }
        dest.extend_from_slice(string.as_bytes())
    } else {
        dest.push(0)
    }
}

fn write_constant(value: &LuaValue, dest: &mut Vec<u8>) {
    write_byte(
        match value {
            LuaValue::NIL => constants::typetag::TNIL,
            LuaValue::BOOLEAN(_) => constants::typetag::TBOOLEAN,
            LuaValue::NUMBER(LuaNumber::INT(_)) => constants::typetag::TINTEGER,
            LuaValue::NUMBER(LuaNumber::FLOAT(_)) => constants::typetag::TNUMBER,
            LuaValue::STRING(s) if s.len() < 255 => constants::typetag::TSHORTSTRING,
            LuaValue::STRING(_) => constants::typetag::TLONGSTRING,
            LuaValue::USERDATA(_) => constants::typetag::TUSERDATA,
            LuaValue::FUNCTION(_) => constants::typetag::TFUNCTION,
            LuaValue::THREAD(_) => constants::typetag::TTHREAD,
            LuaValue::TABLE(_) => constants::typetag::TTABLE,
        },
        dest
    );
    match value {
        LuaValue::NIL => {}
        LuaValue::BOOLEAN(b) => write_boolean(*b, dest),
        LuaValue::NUMBER(LuaNumber::INT(i)) => write_integer(*i, dest),
        LuaValue::NUMBER(LuaNumber::FLOAT(f)) => write_floating(*f, dest),
        LuaValue::STRING(s) => write_string(&Some(s.clone()), dest),
        LuaValue::USERDATA(_) => debug_assert!(false, "unreachable!"),
        LuaValue::FUNCTION(_) => debug_assert!(false, "unreachable!"), // use write_function
        LuaValue::THREAD(_) => debug_assert!(false, "unreachable!"),
        LuaValue::TABLE(_) => debug_assert!(false, "unreachable!"),
    }
}

fn write_upvalue(upvalue: &UpvalueDesc, dest: &mut Vec<u8>) {
    dest.extend_from_slice(&upvalue.bytes())
}

fn write_locvar(locvar: &LocVar, dest: &mut Vec<u8>) {
    write_string(&locvar.name, dest);
    write_int(&locvar.startpc, dest);
    write_int(&locvar.endpc, dest);
}

// TODO: Rename internal functions
fn write_function(proto: &Rc<Prototype>, dest: &mut Vec<u8>) {
    let Prototype {
        source_string,
        first_line_defined,
        last_line_defined,
        param_count,
        is_vararg,
        max_stack_size,
        code,
        constants,
        upvalues,
        functions,
        lineinfo,
        locvars,
        upvaluenames
    } = &**proto;
    write_string(source_string, dest);
    write_int(first_line_defined, dest);
    write_int(last_line_defined, dest);
    write_byte(*param_count, dest);
    write_byte(*is_vararg, dest);
    write_byte(*max_stack_size, dest);
    write_int(&(code.len() as i32), dest);
    write_vec(code, write_instruction, dest);
    write_int(&(constants.len() as i32), dest);
    write_vec(constants, write_constant, dest);
    write_int(&(upvalues.len() as i32), dest);
    write_vec(upvalues, write_upvalue, dest);
    write_int(&(functions.len() as i32), dest);
    write_vec(functions, write_function, dest);
    write_int(&(lineinfo.len() as i32), dest);
    write_vec(lineinfo, write_int, dest);
    write_int(&(locvars.len() as i32), dest);
    write_vec(locvars, write_locvar, dest);
    write_int(&(upvaluenames.len() as i32), dest);
    write_vec(upvaluenames, write_string, dest);
}

pub fn dump_chunk(proto: &Rc<Prototype>, dest: &mut Vec<u8>) {
    dest.extend_from_slice(constants::LUA_SIGNATURE);
    dest.push(constants::LUA_VERSION);
    dest.push(constants::LUA_FORMAT);
    dest.extend_from_slice(constants::LUA_CONV_DATA);
    dest.extend_from_slice(&constants::LUA_SYSTEM_PARAMETER);
    write_integer(constants::LUA_CHECK_INTEGER.as_int().unwrap(), dest);
    write_floating(constants::LUA_CHECK_FLOATING.as_float(), dest);
    write_byte(0, dest);
    write_function(proto, dest);
}
