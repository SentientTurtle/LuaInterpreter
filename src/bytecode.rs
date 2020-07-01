pub mod read {
    use std::path::Path;
    use std::fs::{File};
    use std::io;
    use std::io::{BufRead, BufReader};
    use core::mem;
    use crate::constants;
    use crate::constants::types::{HOST_OBJECT_SIZE, LUA_INT, LUA_FLOAT, LUA_INSTRUCTION, HOST_INT};
    //use crate::xdbg;
    use std::rc::Rc;
    use crate::types::value::number::LuaNumber;
    use crate::types::value::string::LuaString;
    use crate::types::value::LuaValue;
    use crate::types::upvalue::UpvalueDesc;
    use crate::types::locvar::LocVar;
    use crate::types::value::function::Prototype;

    #[derive(Debug)]
    pub enum DecodeError {
        IO(io::Error),
        InvalidSignature { found: [u8; constants::LUA_SIGNATURE.len()], expected: &'static [u8; constants::LUA_SIGNATURE.len()] },
        ConversionDataCorrupt { found: [u8; constants::LUA_CONV_DATA.len()], expected: &'static [u8; constants::LUA_CONV_DATA.len()] },
        IncompatibleSystemParam([u8; constants::LUA_SYSTEM_PARAMETER.len()]),
        CorruptCheckInt(LUA_INT),
        CorruptCheckFloat(LUA_FLOAT),
        VectorSizeOverflow(usize, usize),
        UnknownConstantTypeTag(u8),
        NullStringInConstant(),
        NonBinaryBooleanByte(u8),
        InvalidVersion(u8),
        InvalidFormat(u8),
    }

    impl From<io::Error> for DecodeError {
        fn from(err: io::Error) -> Self {
            DecodeError::IO(err)
        }
    }

    fn read_byte<R: BufRead>(reader: &mut R) -> Result<u8, DecodeError> {
        let mut byte_buf = [0u8];
        reader.read_exact(&mut byte_buf)?;
        //xdbg!(byte_buf);
        Ok(byte_buf[0])
    }

    fn read_int<R: BufRead>(reader: &mut R) -> Result<HOST_INT, DecodeError> {
        let mut int_buf = [0u8; mem::size_of::<HOST_INT>()];
        reader.read_exact(&mut int_buf)?;
        //xdbg!(int_buf);
        Ok(HOST_INT::from_le_bytes(int_buf))
    }

    fn read_vec<T, R: BufRead>(reader: &mut R, count: usize, read_fn: fn(&mut R) -> Result<T, DecodeError>) -> Result<Vec<T>, DecodeError> {
        let obj_size = mem::size_of::<T>();
        let vec_size = (usize::checked_mul(count, obj_size)).ok_or(DecodeError::VectorSizeOverflow(count, obj_size))?;
        let mut buf = Vec::with_capacity(vec_size);
        for i in 0..count {
            buf.push(read_fn(reader)?)
        }
        Ok(buf)
    }

    fn read_boolean<R: BufRead>(reader: &mut R) -> Result<bool, DecodeError> {
        match read_byte(reader)? {
            0 => Ok(false),
            1 => Ok(true),
            byte => Err(DecodeError::NonBinaryBooleanByte(byte))
        }
    }

    fn read_integer<R: BufRead>(reader: &mut R) -> Result<LuaNumber, DecodeError> {
        let mut int_buf = [0u8; mem::size_of::<LUA_INT>()];
        reader.read_exact(&mut int_buf)?;
        //xdbg!(int_buf);
        let integer = LUA_INT::from_le_bytes(int_buf);
        Ok(LuaNumber::INT(integer))
    }

    fn read_floating<R: BufRead>(reader: &mut R) -> Result<LuaNumber, DecodeError> {
        let mut float_buf = [0u8; mem::size_of::<LUA_FLOAT>()];
        reader.read_exact(&mut float_buf)?;
        //xdbg!(float_buf);
        let floating = unsafe { mem::transmute::<LUA_INT, LUA_FLOAT>(LUA_INT::from_le_bytes(float_buf)) };
        Ok(LuaNumber::FLOAT(floating))
    }

    fn read_instruction<R: BufRead>(reader: &mut R) -> Result<LUA_INSTRUCTION, DecodeError> {
        let mut integer_buf = [0u8; mem::size_of::<LUA_INSTRUCTION>()];
        reader.read_exact(&mut integer_buf)?;
        //xdbg!(integer_buf);
        let instruction = LUA_INSTRUCTION::from_le_bytes(integer_buf);
        Ok(instruction)
    }

    fn read_string<R: BufRead>(reader: &mut R) -> Result<Option<LuaString>, DecodeError> {
        let size_byte = read_byte(reader)?;
        if size_byte == 0 {
            Ok(None)
        } else {
            let str_size = if size_byte < 0xFF {
                size_byte as HOST_OBJECT_SIZE
            } else {
                let mut size_buf = [0u8; mem::size_of::<HOST_OBJECT_SIZE>()];
                reader.read_exact(&mut size_buf)?;
                //xdbg!(size_buf);
                HOST_OBJECT_SIZE::from_le_bytes(size_buf)
            };
            let mut str_buffer: Vec<u8> = vec![0u8; str_size - 1];
            reader.read_exact(&mut str_buffer[..])?;
            //xdbg!(&str_buffer[..]);
            Ok(Some(LuaString::from(str_buffer.into_boxed_slice())))
        }
    }

    fn read_constant<R: BufRead>(reader: &mut R) -> Result<LuaValue, DecodeError> {
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

    fn read_upvalue<R: BufRead>(reader: &mut R) -> Result<UpvalueDesc, DecodeError> {
        Ok(UpvalueDesc::new(read_byte(reader)?, read_byte(reader)?))
    }

    fn read_locvar<R: BufRead>(reader: &mut R) -> Result<LocVar, DecodeError> {
        Ok(LocVar::new(read_string(reader)?, read_int(reader)?, read_int(reader)?))
    }

    fn read_function<R: BufRead>(reader: &mut R) -> Result<Rc<Prototype>, DecodeError> {
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
        let functions = read_vec(reader, function_count as usize, read_function)?;

        let lineinfo_count = read_int(reader)?;
        let lineinfo = read_vec(reader, lineinfo_count as usize, read_int)?;

        let locvars_count = read_int(reader)?;
        let locvars = read_vec(reader, locvars_count as usize, read_locvar)?;

        let debug_upvalue_count = read_int(reader)?;
        debug_assert_eq!(debug_upvalue_count, upvalue_count);
        let upvaluenames = read_vec(reader, debug_upvalue_count as usize, read_string)?;
        debug_assert_eq!(upvalues.len(), upvaluenames.len());

        Ok(Rc::from(Prototype::from_parts(
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
        )))
    }

    pub fn decode_file(path: &Path) -> Result<Rc<Prototype>, DecodeError> {
        decode_chunk(
            &mut BufReader::new(
                File::open(path)?
            )
        )
    }

    pub fn decode_bytes(mut bytes: &[u8]) -> Result<Rc<Prototype>, DecodeError> {
        decode_chunk(&mut bytes)
    }

    fn decode_chunk<R: BufRead>(reader: &mut R) -> Result<Rc<Prototype>, DecodeError> {
        let mut sig_buf = [0u8; constants::LUA_SIGNATURE.len()];
        reader.read_exact(&mut sig_buf)?;
        if &sig_buf != constants::LUA_SIGNATURE {
            return Err(DecodeError::InvalidSignature { found: sig_buf, expected: constants::LUA_SIGNATURE });
        }
        //xdbg!(sig_buf);

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
        //xdbg!(convdata_buf);
        if &convdata_buf != constants::LUA_CONV_DATA {
            return Err(DecodeError::ConversionDataCorrupt { found: convdata_buf, expected: constants::LUA_CONV_DATA });
        }

        let mut syspar_buf: [u8; 5] = [0u8; constants::LUA_SYSTEM_PARAMETER.len()];
        reader.read_exact(&mut syspar_buf)?;
        //xdbg!(syspar_buf);
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
        //xdbg!(upvaluesize_buf);
        let upvaluesize = upvaluesize_buf[0];

        let function = read_function(reader)?;
        Ok(function)
    }
}

pub mod write {
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

    pub fn write_chunk(proto: &Rc<Prototype>, dest: &mut Vec<u8>) {
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
}