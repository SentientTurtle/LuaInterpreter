#![feature(option_result_contains)]
#![feature(stmt_expr_attributes)]
#![feature(try_blocks)]
#![feature(const_panic)]

#[macro_use]
extern crate nom;
#[macro_use]
mod macros;

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
