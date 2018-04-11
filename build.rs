extern crate version_check;

fn main() {
    if version_check::is_nightly().unwrap_or(false) {
        println!("cargo:rustc-cfg=feature=\"nightly\"");
    }
}
