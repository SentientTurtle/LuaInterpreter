#[macro_export]
macro_rules! lua_func {
    ($func:ident) => {
        NativeFunction::from_parts(stringify!($func), $func)
    };
}

#[macro_export]
macro_rules! trace_error {
    ($error: ident, $func:ident) => {
        $error.map_err(|e| e.trace(lua_func!($func)))
    };
}