#![feature(macro_rules)]
#![feature(globs)]
#![unstable]
extern crate serialize;
extern crate encoding;

pub use header::{
    Header,
    HeaderMap,
    HeaderItems,
    FromHeader,
    ToHeader,
    ToFoldedHeader,
};
pub use address::{Address};
pub use message::{
    MimeMessage,
    MimeMultipartType,
};

pub mod rfc5322;
pub mod rfc2047;
pub mod rfc2045;
pub mod mimeheaders;
mod header;
mod address;
mod message;
