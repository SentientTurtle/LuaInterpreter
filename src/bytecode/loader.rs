use std::io::Read;
use core::mem;
use crate::constants;
use crate::constants::types::{HOST_OBJECT_SIZE, LUA_INT, LUA_FLOAT, LUA_INSTRUCTION, HOST_INT};
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

fn read_int<R: Read>(reader: &mut R) -> Result<HOST_INT, DecodeError> {
    let mut int_buf = [0u8; mem::size_of::<HOST_INT>()];
    reader.read_exact(&mut int_buf)?;
    //xdbg!(int_buf);
    Ok(HOST_INT::from_le_bytes(int_buf))
}

fn read_vec<T, R: Read>(reader: &mut R, count: usize, read_fn: fn(&mut R) -> Result<T, DecodeError>) -> Result<Vec<T>, DecodeError> {
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
    let instruction = LUA_INSTRUCTION::from_le_bytes(integer_buf);
    Ok(instruction)
}

fn read_string<R: Read>(reader: &mut R) -> Result<Option<LuaString>, DecodeError> {
    let size_byte = read_byte(reader)?;
    if size_byte == 0 {
        Ok(None)
    } else {
        let str_size = if size_byte < 0xFF {
            size_byte as HOST_OBJECT_SIZE
        } else {
            let mut size_buf = [0u8; mem::size_of::<HOST_OBJECT_SIZE>()];
            reader.read_exact(&mut size_buf)?;
            HOST_OBJECT_SIZE::from_le_bytes(size_buf)
        };
        let mut str_buffer: Vec<u8> = vec![0u8; str_size - 1];
        reader.read_exact(&mut str_buffer[..])?;
        Ok(Some(LuaString::from(str_buffer.into_boxed_slice())))
    }
}

fn read_constant<R: Read>(reader: &mut R) -> Result<LuaValue, DecodeError> {
    let constant_type = read_byte(reader)?;
    match constant_type {
        constants::typetag::TNIL => Ok(LuaValue::NIL),
        constants::typetag::TBOOLEAN => Ok(LuaValue::BOOLEAN(read_boolean(reader)?)),
        constants::typetag::TNUMBER => Ok(LuaValue::NUMBER(read_floating(reader)?)),
        constants::typetag::TINTEGER => Ok(LuaValue::NUMBER(read_integer(reader)?)),
        constants::typetag::TSTRING | constants::typetag::TLONGSTRING => Ok(LuaValue::STRING(read_string(reader)?.ok_or(DecodeError::NullStringInConstant())?)),

        _ => Err(DecodeError::UnknownConstantTypeTag(constant_type))
    }
}

fn read_upvalue<R: Read>(reader: &mut R) -> Result<UpvalueDesc, DecodeError> {
    Ok(UpvalueDesc::new(read_byte(reader)?, read_byte(reader)?))
}

fn read_locvar<R: Read>(reader: &mut R) -> Result<LocVar, DecodeError> {
    Ok(LocVar::new(read_string(reader)?, read_int(reader)?, read_int(reader)?))
}

// TODO: Rename internal functions
fn read_function<R: Read>(reader: &mut R) -> Result<Prototype, DecodeError> {
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
    let functions = read_vec(reader, function_count as usize, |reader: &mut R| read_function(reader).map(Rc::from))?;

    let lineinfo_count = read_int(reader)?;
    let lineinfo = read_vec(reader, lineinfo_count as usize, read_int)?;

    let locvars_count = read_int(reader)?;
    let locvars = read_vec(reader, locvars_count as usize, read_locvar)?;

    let debug_upvalue_count = read_int(reader)?;
    debug_assert_eq!(debug_upvalue_count, upvalue_count);       // TODO: Replace with runtime checks
    let upvaluenames = read_vec(reader, debug_upvalue_count as usize, read_string)?;
    debug_assert_eq!(upvalues.len(), upvaluenames.len());       // TODO: Replace with runtime checks

    Ok(Prototype::from_parts(
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
        upvaluenames,
    ))
}

pub fn load_chunk<R: Read>(reader: &mut R) -> Result<Prototype, DecodeError> {
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

    let mut syspar_buf: [u8; 5] = [0u8; constants::LUA_SYSTEM_PARAMETER.len()];
    reader.read_exact(&mut syspar_buf)?;
    if &syspar_buf != &constants::LUA_SYSTEM_PARAMETER {
        return Err(DecodeError::IncompatibleSystemParam(syspar_buf));
    }

    let integer = read_integer(reader)?;
    if integer != constants::LUA_CHECK_INTEGER {
        return Err(DecodeError::CorruptCheckInt(integer.as_int().unwrap()));
    }

    let floating = read_floating(reader)?;
    if floating != constants::LUA_CHECK_FLOATING {
        return Err(DecodeError::CorruptCheckFloat(floating.as_float()));
    }

    let mut upvaluesize_buf = [0u8; 1];     // TODO: Figure out what on earth this is for
    reader.read_exact(&mut upvaluesize_buf)?;
    let upvaluesize = upvaluesize_buf[0];

    let function = read_function(reader)?;
    Ok(function)
}
