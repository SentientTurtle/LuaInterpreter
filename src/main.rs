#![feature(type_ascription)]
#![feature(nll)]
use std::path::Path;

#[allow(unused_variables, dead_code)]
mod types;
#[allow(unused_variables, dead_code)]
mod decode;
mod executor;
mod constants;
mod macros;

fn main() {
    match decode::decode_file(Path::new("./luac.out")) {
        Ok(proto) => {
            println!("{}", proto)
        },
        Err(err) => {
            println!("{:?}", err);
        },
    }
}
