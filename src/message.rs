use super::header::{FromHeader,HeaderMap};
use super::rfc5322::Rfc5322Parser;
use super::rfc2045::Rfc2045Parser;

use std::collections::HashMap;

/// Content-Type string, major/minor as the first and second elements
/// respectively.
pub type MimeContentType = (String, String);

/// Special header type for the Content-Type header.
pub struct MimeContentTypeHeader {
    /// The content type presented by this header
    pub content_type: MimeContentType,
    /// Parameters of this header
    pub params: HashMap<String, String>,
}

impl FromHeader for MimeContentTypeHeader {
    fn from_header(value: String) -> Option<MimeContentTypeHeader> {
        let mut parser = Rfc2045Parser::new(value.as_slice());
        let (value, params) = parser.consume_all();

        let mime_parts: Vec<&str> = value.as_slice().splitn(2, '/').collect();

        if mime_parts.len() == 2 {
            Some(MimeContentTypeHeader {
                content_type: (mime_parts[0].to_string(), mime_parts[1].to_string()),
                params: params
            })
        } else {
            None
        }
    }
}

/// Represents the common data of a MIME message
#[deriving(Show)]
pub struct MimeMessageData {
    /// The headers for this message
    pub headers: HeaderMap,
    /// The content of this message
    pub body: String
}

/// Marks the type of a multipart message
#[deriving(Eq,PartialEq,Show)]
pub enum MimeMultipartType {
    /// Entries which are independent.
    ///
    /// This value is the default.
    ///
    /// As defined by Section 5.1.3 of RFC 2046
    MimeMultipartMixed,
    /// Entries which are interchangeable, such that the system can choose
    /// whichever is "best" for its use.
    ///
    /// As defined by Section 5.1.4 of RFC 2046
    MimeMultipartAlternate,
    /// Entries are (typically) a collection of messages.
    ///
    /// As defined by Section 5.1.5 of RFC 2046
    MimeMultipartDigest,
    /// Entry order does not matter, and could be displayed simultaneously.
    ///
    /// As defined by Section 5.1.6 of RFC 2046
    MimeMultipartParallel,
}

impl MimeMultipartType {
    /// Returns the appropriate `MimeMultipartType` for the given MimeContentType
    pub fn from_content_type(ct: MimeContentType) -> MimeMultipartType {
        let (major, minor) = ct;
        match (major.as_slice(), minor.as_slice()) {
            ("multipart", "alternate") => MimeMultipartAlternate,
            ("multipart", "digest") => MimeMultipartDigest,
            ("multipart", "parallel") => MimeMultipartParallel,
            ("multipart", "mixed") | ("multipart", _) => MimeMultipartMixed,
            _ => fail!("ContentType is not multipart"),
        }
    }

    /// Returns a MimeContentType that represents this multipart type.
    pub fn to_content_type(&self) -> MimeContentType {
        let multipart = "multipart".to_string();
        match *self {
            MimeMultipartMixed => (multipart, "mixed".to_string()),
            MimeMultipartAlternate => (multipart, "alternate".to_string()),
            MimeMultipartDigest => (multipart, "digest".to_string()),
            MimeMultipartParallel => (multipart, "parallel".to_string()),
        }
    }
}

/// Enum type over the different types of multipart message.
#[deriving(Show)]
pub enum MimeMessage {
    /// This message is made of multiple sub parts.
    ///
    /// The `body` of MimeMessageData is the content of the message between
    /// the final header and the first boundary.
    MimeMultipart(MimeMessageData, MimeMultipartType, Vec<MimeMessage>),
    /// A simple non-multipart message.
    MimeNonMultipart(MimeMessageData),
}

impl MimeMessage {
    /// Get a reference to the headers for this message.
    pub fn headers(&self) -> &HeaderMap {
        match *self {
            MimeMultipart(ref data, _, _) => &data.headers,
            MimeNonMultipart(ref data) => &data.headers,
        }
    }

    // Make a message from a header map and body, parsing out any multi-part
    // messages that are discovered by looking at the Content-Type header.
    fn from_headers(headers: HeaderMap, body: String) -> Option<MimeMessage> {
        let content_type = {
            let header =  headers.get("Content-Type".to_string());
            match header {
                Some(h) => h.get_value(),
                None => Some(MimeContentTypeHeader{
                    content_type: ("text".to_string(), "plain".to_string()),
                    params: HashMap::new(),
                })
            }
        };

        if content_type.is_none() {
            // If we failed to parse the Content-Type header, something went wrong, so bail.
            None
        } else {
            let content_type = content_type.unwrap();
            // Pull out the major mime type and the boundary (if it exists)
            let (mime_type, sub_mime_type) = content_type.content_type;
            let boundary = content_type.params.find(&"boundary".to_string());

            let message = match mime_type.as_slice() {
                // Only consider a multipart message if we have a boundary, otherwise don't
                // bother and just assume it's a single message.
                "multipart" if boundary.is_some() => {
                    let boundary = boundary.unwrap();
                    // Pull apart the message on the boundary.
                    let mut parts = MimeMessage::split_boundary(body, boundary.clone());
                    // Pop off the first message, as it's part of the parent.
                    let body = parts.remove(0).unwrap_or("".to_string());
                    // Parse out each of the child parts, recursively downwards.
                    // Filtering out and unwrapping None as we go.
                    let message_parts: Vec<MimeMessage> = parts.iter()
                                                               .map(|part| { MimeMessage::parse(part.as_slice()) })
                                                               .filter(|part| { part.is_some() })
                                                               .map(|part| { part.unwrap() })
                                                               .collect();
                    let data = MimeMessageData {
                        headers: headers,
                        body: body,
                    };
                    let multipart_type = MimeMultipartType::from_content_type((mime_type, sub_mime_type));
                    MimeMultipart(data, multipart_type, message_parts)
                },
                _ => {
                    // Boring message, bung the headers & body together and return.
                    let data = MimeMessageData {
                        headers: headers,
                        body: body,
                    };
                    MimeNonMultipart(data)
                },
            };

            Some(message)
        }
    }

    // Split `body` up on the `boundary` string.
    fn split_boundary(body: String, boundary: String) -> Vec<String> {
        let mut lines = body.as_slice().split('\n');

        let mut parts = Vec::new();
        let mut current_part = String::new();

        for line in lines {
            let mut is_boundary = false;
            if line.starts_with("--") {
                let boundary_value = line.slice_from(2).trim_right();
                is_boundary = boundary_value.to_string() == boundary;
            }
            if is_boundary {
                // Clip off the final \r\n
                parts.push(current_part);
                current_part = String::new()
            } else {
                current_part.push_str(line);
                current_part.push_str("\n");
            }
        }

        if current_part.len() > 0 {
            // Push what remains as the last message part
            current_part.pop(); // Clear the final \n that we put in
            parts.push(current_part);
        }

        parts
    }

    /// Parse `s` into a MimeMessage.
    ///
    /// Recurses down into each message, supporting an unlimited depth of messages.
    ///
    /// Be warned that each sub-message that fails to be parsed will be thrown away.
    pub fn parse(s: &str) -> Option<MimeMessage> {
        let mut parser = Rfc5322Parser::new(s);
        match parser.consume_message() {
            Some((headers, body)) => MimeMessage::from_headers(headers, body),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate test;

    use super::*;
    use super::super::header::{Header,HeaderMap};
    use self::test::Bencher;

    #[deriving(Show)]
    struct MessageTestResult<'s> {
        headers: Vec<(&'s str, &'s str)>,
        body: &'s str,
        children: Option<Vec<MessageTestResult<'s>>>,
    }

    impl<'s> Equiv<MimeMessage> for MessageTestResult<'s> {
        fn equiv(&self, other: &MimeMessage) -> bool {
            let mut headers = HeaderMap::new();
            for &(name, value) in self.headers.iter() {
                let header = Header::new(name.to_string(), value.to_string());
                headers.insert(header);
            }

            match (&self.children, other) {
                (&Some(ref our_messages), &MimeMultipart(ref data, _, ref other_messages)) => {
                    let mut children_match = true;
                    for (index, child) in our_messages.iter().enumerate() {
                        if !child.equiv(&other_messages[index]) {
                            children_match = false;
                            break;
                        }
                    }
                    let header_match = headers == data.headers;
                    let body_match = self.body.to_string() == data.body;

                    if !children_match {
                        println!("Children do not match!");
                    }
                    if !header_match {
                        println!("Headers do not match!");
                    }
                    if !body_match {
                        println!("Body does not match! ({} != {})", self.body, data.body);
                    }

                    header_match && body_match && children_match
                },
                (&None, &MimeNonMultipart(ref data)) => {
                    let header_match = headers == data.headers;
                    let body_match = self.body.to_string() == data.body;

                    if !header_match {
                        println!("Headers do not match!");
                    }
                    if !body_match {
                        println!("Body does not match! ({} != {})", self.body, data.body);
                    }

                    header_match && body_match
                },
                (_, _) => {
                    println!("Expected different message type than what was given");
                    false
                },
            }
        }
    }

    struct ParseTest<'s> {
        input: &'s str,
        output: Option<MessageTestResult<'s>>,
        name: &'s str,
    }

    #[test]
    fn test_message_parse() {
        let tests = vec![
            ParseTest {
                input: "From: joe@example.org\r\nTo: john@example.org\r\n\r\nHello!",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                    ],
                    body: "Hello!",
                    children: None,
                }),
                name: "Simple single part message parse",
            },

            ParseTest {
                input: "From: joe@example.org\r\n\
                        To: john@example.org\r\n\
                        Content-Type: multipart/alternate; boundary=foo\r\n\
                        \r\n\
                        Parent\r\n\
                        --foo\r\n\
                        Hello!\r\n\
                        --foo\r\n\
                        Other\r\n",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                        ("Content-Type", "multipart/alternate; boundary=foo"),
                    ],
                    body: "Parent\r\n",
                    children: Some(vec![
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Hello!\r\n",
                            children: None,
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Other\r\n",
                            children: None,
                        },
                    ]),
                }),
                name: "Simple multipart message parse",
            },

            ParseTest {
                input: "From: joe@example.org\r\n\
                        To: john@example.org\r\n\
                        Content-Type: multipart/mixed; boundary=foo\r\n\
                        \r\n\
                        Parent\r\n\
                        --foo\r\n\
                        Content-Type: multipart/alternate; boundary=bar\r\n\

                        --bar\r\n\
                        Hello!\r\n\
                        --bar\r\n\
                        Other\r\n\
                        --foo\r\n\
                        Outside\r\n\
                        --foo\r\n
                        ",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                        ("Content-Type", "multipart/mixed; boundary=foo"),
                    ],
                    body: "Parent\r\n",
                    children: Some(vec![
                        MessageTestResult {
                            headers: vec![
                                ("Content-Type", "multipart/alternate; boundary=bar"),
                            ],
                            body: "",
                            children: Some(vec![
                                MessageTestResult {
                                    headers: vec![ ],
                                    body: "Hello!\r\n",
                                    children: None,
                                },
                                MessageTestResult {
                                    headers: vec![ ],
                                    body: "Other\r\n",
                                    children: None,
                                },
                            ]),
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Outside\r\n",
                            children: None,
                        },
                    ]),
                }),
                name: "Deeply nested multipart test",
            },
        ];

        for test in tests.into_iter() {
            println!("--- Next test: {}", test.name);
            let message = MimeMessage::parse(test.input);
            let result = match (test.output, message) {
                (Some(ref expected), Some(ref given)) => expected.equiv(given),
                (None, None) => true,
                (_, _) => false,
            };
            assert!(result, test.name);
        }
    }

    fn bench_parse(s: &str, b: &mut Bencher) {
        b.iter(|| {
            MimeMessage::parse(s);
        });
    }

    macro_rules! bench_parser {
        ($name:ident, $test:expr) => (
            #[bench]
            fn $name(b: &mut Bencher) {
                let s = $test;
                b.iter(|| {
                    MimeMessage::parse(s);
                });
            }
        );
    }

    bench_parser!(bench_simple, "From: joe@example.org\r\nTo: john@example.org\r\n\r\nHello!")
    bench_parser!(bench_simple_multipart,
        "From: joe@example.org\r\n\
         To: john@example.org\r\n\
         Content-Type: multipart/alternate; boundary=foo\r\n\
         \r\n\
         Parent\r\n\
         --foo\r\n\
         Hello!\r\n\
         --foo\r\n\
         Other\r\n\
         --foo"
    )
    bench_parser!(bench_deep_multipart,
        "From: joe@example.org\r\n\
         To: john@example.org\r\n\
         Content-Type: multipart/mixed; boundary=foo\r\n\
         \r\n\
         Parent\r\n\
         --foo\r\n\
         Content-Type: multipart/alternate; boundary=bar\r\n\
         \r\n\
         --bar\r\n\
         Hello!\r\n\
         --bar\r\n\
         Other\r\n\
         --foo\r\n\
         Outside\r\n\
         --foo\r\n"
    )


    #[test]
    fn test_multipart_type_type_parsing() {
        let multipart = "multipart".to_string();
        assert_eq!(MimeMultipartType::from_content_type((multipart.clone(), "mixed".to_string())), MimeMultipartMixed);
        assert_eq!(MimeMultipartType::from_content_type((multipart.clone(), "alternate".to_string())), MimeMultipartAlternate);
        assert_eq!(MimeMultipartType::from_content_type((multipart.clone(), "digest".to_string())), MimeMultipartDigest);
        assert_eq!(MimeMultipartType::from_content_type((multipart.clone(), "parallel".to_string())), MimeMultipartParallel);

        // Test failback onto multipart/mixed
        assert_eq!(MimeMultipartType::from_content_type((multipart.clone(), "potato".to_string())), MimeMultipartMixed);
    }

    #[test]
    fn test_multipart_type_to_content_type() {
        let multipart = "multipart".to_string();

        assert_eq!(MimeMultipartMixed.to_content_type(),     (multipart.clone(), "mixed".to_string()));
        assert_eq!(MimeMultipartAlternate.to_content_type(), (multipart.clone(), "alternate".to_string()));
        assert_eq!(MimeMultipartDigest.to_content_type(),    (multipart.clone(), "digest".to_string()));
        assert_eq!(MimeMultipartParallel.to_content_type(),  (multipart.clone(), "parallel".to_string()));
    }
}
