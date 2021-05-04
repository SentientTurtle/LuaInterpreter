use std::io::Read;
use core::mem;
use crate::constants;
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INSTRUCTION, HOST_INT, HOST_SIGNED_BYTE};
use std::rc::Rc;
use crate::types::value::number::LuaNumber;
use crate::types::value::string::LuaString;
use crate::types::value::LuaValue;
use crate::types::upvalue::UpvalueDesc;
use crate::types::locvar::LocVar;
use crate::types::value::function::Prototype;
use crate::error::DecodeError;

fn read_byte<R: Read>(reader: &mut R) -> Result<u8, DecodeError> {
    let mut byte_buf = [0u8];
    reader.read_exact(&mut byte_buf)?;
    Ok(byte_buf[0])
}

fn read_size<R: Read>(reader: &mut R) -> Result<usize, DecodeError> {
    let mut value: usize = 0;
    loop {
        let byte = read_byte(reader)?;
        value = value.checked_shl(7).ok_or(DecodeError::IntegerOverflow(value, byte))?;
        value |= byte as usize & 0x7f;
        if byte & 0x80 != 0 { break };
    }
    Ok(value)
}

fn read_int<R: Read>(reader: &mut R) -> Result<HOST_INT, DecodeError> {
    // let mut int_buf = [0u8; mem::size_of::<HOST_INT>()];
    // reader.read_exact(&mut int_buf)?;
    // println!("int:{:X?}", int_buf);
    // Ok(HOST_INT::from_le_bytes(int_buf))
    return Ok(read_size(reader)? as HOST_INT)
}

fn read_vec<T, R: Read, F: Fn(&mut R) -> Result<T, DecodeError>>(reader: &mut R, count: usize, read_fn: F) -> Result<Vec<T>, DecodeError> {
    let obj_size = mem::size_of::<T>();
    let vec_size = (usize::checked_mul(count, obj_size)).ok_or(DecodeError::VectorSizeOverflow(count, obj_size))?;
    let mut buf = Vec::with_capacity(vec_size);
    for _ in 0..count {
        buf.push(read_fn(reader)?)
    }
    Ok(buf)
}

fn read_boolean<R: Read>(reader: &mut R) -> Result<bool, DecodeError> {
    match read_byte(reader)? {
        0 => Ok(false),
        1 => Ok(true),
        byte => Err(DecodeError::NonBinaryBooleanByte(byte))
    }
}

fn read_integer<R: Read>(reader: &mut R) -> Result<LuaNumber, DecodeError> {
    let mut int_buf = [0u8; mem::size_of::<LUA_INT>()];
    reader.read_exact(&mut int_buf)?;
    let integer = LUA_INT::from_le_bytes(int_buf);
    Ok(LuaNumber::INT(integer))
}

fn read_floating<R: Read>(reader: &mut R) -> Result<LuaNumber, DecodeError> {
    let mut float_buf = [0u8; mem::size_of::<LUA_FLOAT>()];
    reader.read_exact(&mut float_buf)?;
    let floating = unsafe { mem::transmute::<LUA_INT, LUA_FLOAT>(LUA_INT::from_le_bytes(float_buf)) };
    Ok(LuaNumber::FLOAT(floating))
}

fn read_instruction<R: Read>(reader: &mut R) -> Result<LUA_INSTRUCTION, DecodeError> {
    let mut integer_buf = [0u8; mem::size_of::<LUA_INSTRUCTION>()];
    reader.read_exact(&mut integer_buf)?;
    let instruction = LUA_INSTRUCTION::from(u32::from_le_bytes(integer_buf));
    Ok(instruction)
}

fn read_string<R: Read>(reader: &mut R) -> Result<Option<LuaString>, DecodeError> {
    let size = read_size(reader)?;
    if size == 0 {
        Ok(None)
    } else {
        let mut str_buffer: Vec<u8> = vec![0u8; size - 1];
        reader.read_exact(&mut str_buffer[..])?;
        Ok(Some(LuaString::from(str_buffer.into_boxed_slice())))
    }

    // let size_byte = read_byte(reader)?;
    // if size_byte == 0 {
    //     Ok(None)
    // } else {
    //     let str_size = if size_byte < 0xFF {
    //         size_byte as HOST_OBJECT_SIZE
    //     } else {
    //         let mut size_buf = [0u8; mem::size_of::<HOST_OBJECT_SIZE>()];
    //         reader.read_exact(&mut size_buf)?;
    //         HOST_OBJECT_SIZE::from_le_bytes(size_buf)
    //     };
    //     let mut str_buffer: Vec<u8> = vec![0u8; str_size - 1];
    //     reader.read_exact(&mut str_buffer[..])?;
    //     Ok(Some(LuaString::from(str_buffer.into_boxed_slice())))
    // }
}

fn read_constant<R: Read>(reader: &mut R) -> Result<LuaValue, DecodeError> {
    let constant_type = read_byte(reader)?;
    match constant_type {
        constants::typetag::TNIL => Ok(LuaValue::NIL),
        constants::typetag::VFALSE => Ok(LuaValue::BOOLEAN(false)),
        constants::typetag::VTRUE => Ok(LuaValue::BOOLEAN(true)),
        constants::typetag::VFLOAT => Ok(LuaValue::NUMBER(read_floating(reader)?)),
        constants::typetag::VINTEGER => Ok(LuaValue::NUMBER(read_integer(reader)?)),
        constants::typetag::TSTRING | constants::typetag::VLONGSTRING => Ok(LuaValue::STRING(read_string(reader)?.ok_or(DecodeError::NullStringInConstant())?)),

        _ => Err(DecodeError::UnknownConstantTypeTag(constant_type))
    }
}

fn read_upvalue<R: Read>(reader: &mut R) -> Result<UpvalueDesc, DecodeError> {
    Ok(UpvalueDesc::new(read_byte(reader)?, read_byte(reader)?, read_byte(reader)?))
}

fn read_locvar<R: Read>(reader: &mut R) -> Result<LocVar, DecodeError> {
    Ok(LocVar::new(read_string(reader)?, read_int(reader)?, read_int(reader)?))
}

// TODO: Rename internal functions
fn read_function<R: Read>(reader: &mut R, parent_name: Option<LuaString>) -> Result<Prototype, DecodeError> {
    let source_string = read_string(reader)?;
    let first_line_defined = read_int(reader)?;
    let last_line_defined = read_int(reader)?;
    let param_count = read_byte(reader)?;
    let is_vararg = read_byte(reader)?;
    let max_stack_size = read_byte(reader)?;

    let code_count = read_int(reader)?;
    let code = read_vec(reader, code_count as usize, read_instruction)?;

    let constant_count = read_int(reader)?;
    let constants = read_vec(reader, constant_count as usize, read_constant)?;

    let upvalue_count = read_int(reader)?;
    let upvalues = read_vec(reader, upvalue_count as usize, read_upvalue)?;

    let function_count = read_int(reader)?;
    let functions = read_vec(reader, function_count as usize, |reader: &mut R| read_function(reader, source_string.clone()).map(Rc::from))?;

    // Debug info
    let lineinfo_count = read_int(reader)?;
    let lineinfo = read_vec(reader, lineinfo_count as usize, |reader: &mut R| read_byte(reader).map(|byte| byte as HOST_SIGNED_BYTE))?;

    let abslineinfo_count = read_int(reader)?;
    let abslineinfo = read_vec(reader, abslineinfo_count as usize, |reader| Ok((read_int(reader)?, read_int(reader)?)))?;

    let locvars_count = read_int(reader)?;
    let locvars = read_vec(reader, locvars_count as usize, read_locvar)?;

    let debug_upvalue_count = read_int(reader)?;
    debug_assert_eq!(debug_upvalue_count, upvalue_count);       // TODO: Replace with runtime checks
    let upvaluenames = read_vec(reader, debug_upvalue_count as usize, read_string)?;
    debug_assert_eq!(upvalues.len(), upvaluenames.len());       // TODO: Replace with runtime checks

    Ok(Prototype::from_parts(
        source_string.or(parent_name),
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
        abslineinfo,
        locvars,
        upvaluenames,
    ))
}

fn load_header<R: Read>(reader: &mut R) -> Result<(), DecodeError> {
    let mut sig_buf = [0u8; constants::LUA_SIGNATURE.len()];
    reader.read_exact(&mut sig_buf)?;
    if &sig_buf != constants::LUA_SIGNATURE {
        return Err(DecodeError::InvalidSignature { found: sig_buf, expected: constants::LUA_SIGNATURE });
    }

    let version = read_byte(reader)?;
    let format = read_byte(reader)?;
    if version != constants::LUA_VERSION {
        return Err(DecodeError::InvalidVersion(version));
    }
    if format != constants::LUA_FORMAT {
        return Err(DecodeError::InvalidFormat(format));
    }

    let mut convdata_buf = [0u8; constants::LUA_CONV_DATA.len()];
    reader.read_exact(&mut convdata_buf)?;
    if &convdata_buf != constants::LUA_CONV_DATA {
        return Err(DecodeError::ConversionDataCorrupt { found: convdata_buf, expected: constants::LUA_CONV_DATA });
    }

    let mut syspar_buf = [0u8; constants::LUA_SYSTEM_PARAMETER.len()];  // TODO: Perhaps inline instead of having a constant value to compare against
    reader.read_exact(&mut syspar_buf)?;
    if &syspar_buf != &constants::LUA_SYSTEM_PARAMETER {
        return Err(DecodeError::IncompatibleSystemParam{ found: syspar_buf, expected: &constants::LUA_SYSTEM_PARAMETER });
    }

    let integer = read_integer(reader)?;
    if integer != constants::LUA_CHECK_INTEGER {
        return Err(DecodeError::CorruptCheckInt(integer.as_int().unwrap()));
    }

    let floating = read_floating(reader)?;
    if floating != constants::LUA_CHECK_FLOATING {
        return Err(DecodeError::CorruptCheckFloat(floating.as_float()));
    }

    Ok(())
}

pub fn load_chunk<R: Read>(reader: &mut R) -> Result<Prototype, DecodeError> {
    load_header(reader)?;

    let upvaluesize = read_byte(reader)?; // TODO: Figure out what on earth this is for

    let function = read_function(reader, None)?;
    Ok(function)
}
