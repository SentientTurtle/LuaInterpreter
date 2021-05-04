use std::io::Read;
use std::process::{Command, Stdio};
use std::io;
use crate::error::CompileError;
use crate::types::value::function::Prototype;
use crate::bytecode;

pub type DefaultCompiler = LuaC;

pub trait LuaCompiler: Sized {
    fn compile<R: Read>(reader: &mut R) -> Result<Prototype, CompileError>;
}

pub struct LuaC {}

impl LuaCompiler for LuaC {
    fn compile<R: Read>(reader: &mut R) -> Result<Prototype, CompileError> {
        let mut process = Command::new("luac")  // TODO: Reuse command if possible/desirable
            .arg("-o").arg("-").arg("-").arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        if let Some(stdin) = &mut process.stdin {
            match io::copy(reader, stdin) {
                Ok(_) => {}
                Err(_err) => {} // Maybe log this somewhere? In practice the process output should provide more detail
            };
        }

        let output = process.wait_with_output()?;
        if !output.status.success() {
            let code = output.status.code();
            let message = String::from_utf8_lossy(&output.stderr);
            return if let Some(code) = code {
                Err(CompileError::CompileFailed(format!("LuaC compilation failed with error code {} and message:\n{}", code, message)))
            } else {
                Err(CompileError::CompileFailed(format!("LuaC compilation failed with error message:\n{}", message)))
            };
        }

        let mut script_buffer: Vec<u8>;
        if cfg!(target_os = "windows") {    // Stupid hack to remove the LF -> CRLF upgrading windows does for SOME REASON (一︿一)
            let mut iter = output.stdout.into_iter();
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
            script_buffer = output.stdout;
        }

        bytecode::loader::load_chunk(&mut &script_buffer[..]).map_err(CompileError::from)
    }
}