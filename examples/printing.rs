extern crate mime;

use mime::{MimeMessage, Header};

fn main() {
    let parts = vec![
        MimeMessage::new("First part".to_string()),
        MimeMessage::new("Second part".to_string()),
    ];

    let mut message = MimeMessage::new("Parent".to_string());

    for part in parts.into_iter() {
        message.children.push(part);
    }

    message.headers.insert(Header::new("Foo".to_string(), "bar".to_string()));

    println!("{}", message.to_string());
}
