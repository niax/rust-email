#![cfg_attr(test, feature(test))]

extern crate rustc_serialize;
extern crate encoding;
extern crate time;
extern crate chrono;
extern crate rand;

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
pub use address::{Address, Mailbox};
pub use message::{
    MimeMessage,
    MimeMultipartType,
};

pub mod rfc5322;
pub mod rfc2047;
pub mod rfc2045;
pub mod rfc822;
pub mod mimeheaders;
pub mod results;
mod header;
mod address;
mod message;
