#![feature(option_result_contains)]
#![feature(stmt_expr_attributes)]
#![feature(try_blocks)]
#![feature(clamp)]
#![feature(const_panic)]

#[macro_use]
extern crate nom;

#[allow(unused_variables, dead_code)]
mod types;
#[allow(unused_variables, dead_code)]
mod bytecode;
mod compiler;
mod vm;
mod constants;
mod util;
mod stdlib;
mod error;

#[cfg(test)]
mod test;

fn test() {}

pub fn main() {
    let fn_ptr = test;
    assert_eq!(fn_ptr as *const fn(), (fn_ptr.clone()) as *const fn())
}