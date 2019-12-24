extern crate email;

use email::MimeMessage;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn main() {
    let args: Vec<_> = env::args().collect();
    assert!(args.len() > 1);
    let msg_path = Path::new(&args[1]);

    let mut file = File::open(&msg_path).expect("can't open file");
    let raw_msg_bytes = {
        let mut rv: Vec<u8> = vec![];
        file.read_to_end(&mut rv).expect("can't read from file");
        rv
    };
    let raw_msg = String::from_utf8_lossy(&raw_msg_bytes);

    println!("INPUT:");
    println!("{}", &raw_msg);

    let msg = MimeMessage::parse(&raw_msg[..]).unwrap();

    println!("PARSED:");
    println!("{}", msg.as_string());
}
