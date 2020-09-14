use crate::vm::ExecutionState;
use crate::error::{TracedError, ArgumentError, Traceable, LuaError};
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::types::value::table::LuaTable;
use crate::types::value::function::LuaFunction;
use crate::types::parameters::LuaParameters;
use crate::types::value::string::LuaString;
use std::ffi::OsString;
use std::fs::File;
use crate::types::CoerceFrom;
use crate::vm;

pub fn require(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, LuaError> = try {
        let module_name = params.try_coerce::<LuaString>(0)?;
        // If a builtin library, try loading those from the execstate environment
        if let Some(name) = module_name.try_utf8().ok() {
            match name {
                "coroutine" | "package" | "string" | "utf8" | "table" | "math" | "io" | "os" | "debug" => {
                    if let Some(value) = execstate.modules.get(name) {
                        return Ok(Varargs::from(value.clone()));
                    }
                }
                _ => {}
            }
        }

        if let Some(package_module) = execstate.modules.get("package") {
            let loaded_table = package_module.raw_get_into("loaded")
                .unwrap_or(LuaValue::NIL);

            let module = loaded_table.index_with_metatable(&LuaValue::from(module_name.clone()), &execstate.metatables)?;
            if module != LuaValue::NIL {
                return Ok(Varargs::from(module));
            }

            let preload_table = package_module.raw_get_into("preload")
                .unwrap_or(LuaValue::NIL)
                .not_nil();

            if let Some(preload) = preload_table {    // If preload table is nil, no preloading attempt is made, otherwise, coerce to a table.
                let loader = preload.index_with_metatable(&LuaValue::from(module_name.clone()), &execstate.metatables)?;
                let load_result = match vm::helper::do_call_from_rust(require, loader, execstate, &[LuaValue::from(module_name.clone())]) {
                    Ok(r) => r,
                    Err(err) => return Err(err),
                };
                match load_result.into_first() {
                    LuaValue::NIL => {
                        loaded_table.write_index_with_metatable(LuaValue::from(module_name), LuaValue::from(true), &execstate.metatables)?;
                        Varargs::from(true)
                    },
                    module @ _ => {
                        loaded_table.write_index_with_metatable(LuaValue::from(module_name), module.clone(), &execstate.metatables)?;
                        Varargs::from(module)
                    }
                }
            } else {
                // TODO: Implement other loaders
                Err(LuaError::user_string(format!("Unknown module: {}", module_name)))?
            }
        } else {
            Err(LuaError::user_str("Package module was unloaded!"))?
        }
    };
    result.trace(require)
}

pub const CONFIG: &str = {
    if cfg!(windows) {
        "\\;?!-"
    } else {
        "//;?!-"
    }
};

pub const CPATH: &str = ""; // TODO: More clearly mark as non-applicable

pub const LOADED: fn() -> LuaTable = LuaTable::empty;

pub fn loadlib(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub const PATH: &str = {
    if cfg!(windows) {
        "!\\?.lua"
    } else {
        "!/?.lua"
    }
};

pub const PRELOAD: fn() -> LuaTable = LuaTable::empty;      // TODO: Implement

pub const SEARCHERS: fn() -> LuaTable = LuaTable::empty;    // TODO: Implement

fn replace_bytes(search_text: &[u8], pattern: &[u8], replacement: &[u8]) -> Vec<u8> {
    if pattern.len() > 0 {
        let mut text_buffer = Vec::with_capacity(search_text.len());   // We assume sep.len() == rep.len()
        let mut find_buffer = Vec::with_capacity(pattern.len());

        for byte in search_text {
            if *byte == pattern[find_buffer.len()] {
                find_buffer.push(*byte);
                if find_buffer.len() == pattern.len() {
                    find_buffer.truncate(0);
                    text_buffer.extend_from_slice(replacement);
                }
            } else {
                text_buffer.extend_from_slice(&find_buffer[..]);    // No-op when find_buffer is empty
                find_buffer.truncate(0);
                text_buffer.push(*byte);
            }
        };

        text_buffer
    } else {
        Vec::from(search_text)
    }
}

// TODO: Implement additional file path flag to encode UTF-8 cleanly
// TODO: Exact file path handling magic to a different function
// TODO: This might look for empty-string paths? Needs a bugfix
// This function is quite ugly and could use some cleanup; A lot of vectors are created and likely not needed
pub fn searchpath(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let name = params.try_coerce::<LuaString>(0)?;
        let paths = params.try_coerce::<LuaString>(1)?;

        let sep = params.try_coerce::<LuaString>(2);    // Split for borrowing reasons
        let sep = sep.as_ref().map(LuaString::as_bytes).unwrap_or(b".");

        let rep = params.try_coerce::<LuaString>(3);    // Ditto
        let rep = rep.as_ref().map(LuaString::as_bytes).unwrap_or(b".");

        let name = replace_bytes(name.as_bytes(), sep, rep);
        let mut files = <[u8]>::split(paths.as_bytes(), |b| *b == b';')
            .map(|path| replace_bytes(path, b"?", &name[..]))
            .collect::<Vec<Vec<u8>>>();

        for (index, file) in files.iter().enumerate() {
            let os_string = {
                #[cfg(windows)] {
                    use std::os::windows::ffi::OsStringExt;
                    let file_wide = file.iter().map(|byte| *byte as u16).collect::<Vec<u16>>();

                    <OsString as OsStringExt>::from_wide(&file_wide[..])
                }

                #[cfg(unix)] {
                    use std::os::unix::ffi::OsStrExt;
                    <OsString as OsStringExt>::from_vec(file)
                }
            };
            if let Ok(open_file) = File::open(os_string) {
                drop(open_file);
                return Ok(Varargs::from(LuaString::from(files.swap_remove(index).into_boxed_slice())));
            }
        }

        let mut error = Vec::new();
        for file in files {
            error.extend_from_slice(b"\n\t\tno file: '");
            error.extend_from_slice(&file[..]);
            error.push(b'\'');
        }

        Varargs::from((LuaValue::NIL, LuaString::from(error.into_boxed_slice())))
    };
    result.trace(searchpath)
}

pub fn insert_package_lib(execstate: &mut ExecutionState) {
    let table = LuaTable::empty();

    table.set("config", CONFIG).unwrap();
    table.set("cpath", CPATH).unwrap();
    table.set("loaded", LOADED()).unwrap();
    set_table!(table, loadlib);
    table.set("path", PATH).unwrap();
    table.set("preload", PRELOAD()).unwrap();
    table.set("searchers", SEARCHERS()).unwrap();
    set_table!(table, searchpath);

    execstate.global_env.raw_set("require", LuaFunction::RUST_FUNCTION(require)).expect("Raw set with string key should not error!");
    execstate.global_env.raw_set("package", table.clone()).expect("Raw set with string key should not error!");
    execstate.modules.insert("package", table);
}
