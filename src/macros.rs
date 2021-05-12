#[macro_export]
macro_rules! lua_func {
    ($func:ident) => {
        NativeFunction::from_parts(stringify!($func), $func)
    };
}