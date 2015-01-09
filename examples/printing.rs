#![allow(unstable)]
extern crate email;

use email::{MimeMessage, Header, Address};

fn main() {
    let parts = vec![
        MimeMessage::new("First part".to_string()),
        MimeMessage::new("Second part".to_string()),
    ];

    let mut message = MimeMessage::new("Parent".to_string());

    for part in parts.into_iter() {
        message.children.push(part);
    }

    message.headers.insert(
        Header::new_with_value("To".to_string(), vec![
            Address::new_mailbox_with_name("John Doe".to_string(), "john@example.org".to_string()),
            Address::new_mailbox_with_name("Joe Blogs".to_string(), "joe@example.org".to_string()),
            Address::new_mailbox_with_name("Mr Black".to_string(), "mafia_black@example.org".to_string()),
        ]).unwrap()
    );

    message.update_headers();

    println!("{}", message.as_string());
}
