#[macro_export]
macro_rules! xdbg {
    ($val:expr) => {
        eprintln!("[{}:{}] {} = {:X?}", file!(), line!(), stringify!($val), &$val);
    };
}