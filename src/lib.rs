#![feature(globs)]
#![experimental]
extern crate serialize;
extern crate encoding;

pub use header::{Header, HeaderMap, FromHeader};
pub use address::{Address};

pub mod utils;
pub mod rfc5322;
pub mod rfc2047;
mod header;
mod address;
