#![cfg_attr(all(feature = "nightly", test), feature(test))]

extern crate base64;
extern crate encoding;
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
