#![feature(option_result_contains)]
#![feature(result_map_or)]
#![feature(stmt_expr_attributes)]
#![feature(try_blocks)]
#![feature(clamp)]
#![feature(const_if_match)]

#[macro_use]
extern crate nom;

#[allow(unused_variables, dead_code)]
mod types;
#[allow(unused_variables, dead_code)]
mod bytecode;
mod vm;
mod constants;
mod util;
mod stdlib;
mod error;

#[cfg(test)]
mod test;

pub fn main() {
}