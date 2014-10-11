#![feature(globs)]
#![experimental]

pub use header::{Header, HeaderMap, FromHeader};

pub mod utils;
mod header;
