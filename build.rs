extern crate version_check;

use version_check::Channel;

fn main() {
    if Channel::read().as_ref().map(Channel::is_nightly).unwrap_or(false) {
        println!("cargo:rustc-cfg=feature=\"nightly\"");
    }
}
