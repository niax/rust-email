#![unstable]
extern crate "rustc-serialize" as rustc_serialize;
extern crate encoding;
extern crate time;
extern crate chrono;

#[macro_use]
extern crate lazy_static;

pub use header::{
    Header,
    HeaderMap,
    HeaderIter,
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
pub mod rfc822;
pub mod mimeheaders;
mod header;
mod address;
mod message;
