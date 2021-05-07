use crate::vm::ExecutionState;
use crate::error::{LuaError, TraceableError};
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::types::value::table::LuaTable;
use crate::types::value::function::LuaFunction;
use crate::types::value::function::NativeFunction;
use crate::types::parameters::LuaParameters;
use crate::types::value::string::LuaString;
use std::ffi::OsString;
use std::fs::File;
use crate::lua_func;
use crate::types::CoerceFrom;
use crate::util::ResultFrom;

pub fn require(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TraceableError> {
    let module_name = params.try_coerce::<LuaString>(0)?;
    // If a builtin library, try loading those from the execstate environment
    // TODO: Maybe replace with a more generic execstate.modules.get() call, rather than matching the name?
    if let Some(name) = module_name.try_utf8().ok() {
        match name {
            "coroutine" | "package" | "string" | "utf8" | "table" | "math" | "io" | "os" | "debug" => {
                if let Some(value) = execstate.modules.get(name) {
                    return Ok(Varargs::from(value.clone()));    // TODO: Maybe set package.loaded? And move this below that check?
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

        let searchers = package_module.raw_get_into("searchers")
            .unwrap_or(LuaValue::NIL);


        for (_, search_function) in LuaTable::coerce_from(&searchers)?.iter() {
            debug_assert!(params.len() >= 1);   // Coercion to module_name string should fail if there are no arguments provided
            let result = crate::vm::helper::do_call_from_rust(lua_func!(require), search_function, execstate, &params[0..=0])?;

            let [loader, data] = result.into_array::<2>();

            if let LuaValue::FUNCTION(_) = &loader {
                crate::vm::helper::do_call_from_rust(lua_func!(require), loader, execstate, &[data]);
            } else {

            }
        }

        Err(LuaError::user_str("No valid searcher for module!"))?
    } else {
        Err(LuaError::user_str("Package module was unloaded!"))?
    }
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

pub fn loadlib(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TraceableError> {
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

pub const SEARCHERS: fn() -> LuaTable = || {
    let table = LuaTable::empty();

    table
};

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
pub fn searchpath(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TraceableError> {
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

    Varargs::ok_from((LuaValue::NIL, LuaString::from(error.into_boxed_slice())))
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

    execstate.global_env.raw_set("require", LuaFunction::RUST_FUNCTION(lua_func!(require))).expect("Raw set with string key should not error!");
    execstate.global_env.raw_set("package", table.clone()).expect("Raw set with string key should not error!");
    execstate.modules.insert("package", table);
}
