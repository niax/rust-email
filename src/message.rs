use super::header::HeaderMap;
use super::rfc5322::{Rfc5322Parser, Rfc5322Builder};
use super::mimeheaders::{
    MimeContentType,
    MimeContentTypeHeader,
    MimeContentTransferEncoding
};

use std::collections::HashMap;
use std::rand::{task_rng, Rng};

use encoding::label::encoding_from_whatwg_label;
use encoding::DecoderTrap;

const BOUNDARY_LENGTH: uint = 30;

/// Marks the type of a multipart message
#[deriving(Eq,PartialEq,Show,Copy)]
#[stable]
pub enum MimeMultipartType {
    /// Entries which are independent.
    ///
    /// This value is the default.
    ///
    /// As defined by Section 5.1.3 of RFC 2046
    Mixed,
    /// Entries which are interchangeable, such that the system can choose
    /// whichever is "best" for its use.
    ///
    /// As defined by Section 5.1.4 of RFC 2046
    Alternate,
    /// Entries are (typically) a collection of messages.
    ///
    /// As defined by Section 5.1.5 of RFC 2046
    Digest,
    /// Entry order does not matter, and could be displayed simultaneously.
    ///
    /// As defined by Section 5.1.6 of RFC 2046
    Parallel,
}

impl MimeMultipartType {
    /// Returns the appropriate `MimeMultipartType` for the given MimeContentType
    #[stable]
    pub fn from_content_type(ct: MimeContentType) -> Option<MimeMultipartType> {
        let (major, minor) = ct;
        match (major.as_slice(), minor.as_slice()) {
            ("multipart", "alternate") => Some(MimeMultipartType::Alternate),
            ("multipart", "digest") => Some(MimeMultipartType::Digest),
            ("multipart", "parallel") => Some(MimeMultipartType::Parallel),
            ("multipart", "mixed") | ("multipart", _) => Some(MimeMultipartType::Mixed),
            _ => None,
        }
    }

    /// Returns a MimeContentType that represents this multipart type.
    #[stable]
    pub fn to_content_type(&self) -> MimeContentType {
        let multipart = "multipart".to_string();
        match *self {
            MimeMultipartType::Mixed => (multipart, "mixed".to_string()),
            MimeMultipartType::Alternate => (multipart, "alternate".to_string()),
            MimeMultipartType::Digest => (multipart, "digest".to_string()),
            MimeMultipartType::Parallel => (multipart, "parallel".to_string()),
        }
    }
}

/// Represents a MIME message
#[unstable]
pub struct MimeMessage {
    /// The headers for this message
    pub headers: HeaderMap,

    /// The content of this message
    ///
    /// Keep in mind that this is the undecoded form, so may be quoted-printable
    /// or base64 encoded.
    pub body: String,

    /// The MIME multipart message type of this message, or `None` if the message
    /// is not a multipart message.
    pub message_type: Option<MimeMultipartType>,

    /// The sub-messages of this message
    pub children: Vec<MimeMessage>,

    /// The boundary used for MIME multipart messages
    ///
    /// This will always be set, even if the message only has a single part
    pub boundary: String,
}

impl MimeMessage {
    fn random_boundary() -> String {
        task_rng().gen_ascii_chars().take(BOUNDARY_LENGTH).collect()
    }

    #[unstable]
    pub fn new(body: String) -> MimeMessage {
        MimeMessage {
            headers: HeaderMap::new(),
            body: body,
            message_type: None,
            children: Vec::new(),

            boundary: MimeMessage::random_boundary(),
        }
    }

    #[experimental]
    pub fn new_with_children(body: String, message_type: MimeMultipartType, children: Vec<MimeMessage>) -> MimeMessage {
        MimeMessage {
            headers: HeaderMap::new(),
            body: body,
            message_type: Some(message_type),
            children: children,

            boundary: MimeMessage::random_boundary(),
        }
    }

    #[experimental]
    pub fn new_with_boundary(body: String,
                             message_type: MimeMultipartType,
                             children: Vec<MimeMessage>,
                             boundary: String) -> MimeMessage {
        MimeMessage {
            headers: HeaderMap::new(),
            body: body,
            message_type: Some(message_type),
            children: children,

            boundary: boundary,
        }
    }

    /// Parse `s` into a MimeMessage.
    ///
    /// Recurses down into each message, supporting an unlimited depth of messages.
    ///
    /// Be warned that each sub-message that fails to be parsed will be thrown away.
    #[unstable]
    pub fn parse(s: &str) -> Option<MimeMessage> {
        let mut parser = Rfc5322Parser::new(s);
        match parser.consume_message() {
            Some((headers, body)) => MimeMessage::from_headers(headers, body),
            None => None,
        }
    }

    #[experimental]
    pub fn as_string(&self) -> String {
        let mut builder = Rfc5322Builder::new();

        for header in self.headers.iter() {
            builder.emit_folded(header.to_string().as_slice());
            builder.emit_raw("\r\n");
        }

        builder.emit_raw(format!("\r\n{}\r\n", self.body).as_slice());

        if self.children.len() > 0 {

            for part in self.children.iter() {
                builder.emit_raw(
                    format!("--{}\r\n{}\r\n",
                            self.boundary,
                            part.as_string()).as_slice()
                );
            }

            builder.emit_raw(format!("--{}\r\n", self.boundary).as_slice());
        }

        builder.result().clone()
    }

    /// Decode the body of this message, as a series of bytes
    #[experimental]
    pub fn decoded_body_bytes(&self) -> Option<Vec<u8>> {
        let transfer_encoding: MimeContentTransferEncoding =
            self.headers.get_value("Content-Transfer-Encoding".to_string())
                        .unwrap_or(MimeContentTransferEncoding::Identity);
        transfer_encoding.decode(&self.body)
    }

    /// Decode the body of this message, as a string.
    ///
    /// This takes into account any charset as set on the `Content-Type` header,
    /// decoding the bytes with this character set.
    #[experimental]
    pub fn decoded_body_string(&self) -> Option<String> {
        let content_type: Option<MimeContentTypeHeader> =
            self.headers.get_value("Content-Type".to_string());

        match self.decoded_body_bytes() {
            Some(bytes) => {
                let charset = match content_type {
                    Some(ct) => {
                        ct.params.get(&"charset".to_string()).cloned()
                    }
                    _ => None,
                }.unwrap_or("us-ascii".to_string());

                let decoder = encoding_from_whatwg_label(charset.as_slice());

                match decoder {
                    Some(d) => d.decode(bytes.as_slice(), DecoderTrap::Replace).ok(),
                    _ => None,
                }
            },
            None => None,
        }
    }

    // Make a message from a header map and body, parsing out any multi-part
    // messages that are discovered by looking at the Content-Type header.
    fn from_headers(headers: HeaderMap, body: String) -> Option<MimeMessage> {
        let content_type = {
            let header =  headers.get("Content-Type".to_string());
            match header {
                Some(h) => h.get_value(),
                None => Some(MimeContentTypeHeader {
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
            let boundary = content_type.params.get(&"boundary".to_string());

            let mut message = match mime_type.as_slice() {
                // Only consider a multipart message if we have a boundary, otherwise don't
                // bother and just assume it's a single message.
                "multipart" if boundary.is_some() => {
                    let boundary = boundary.unwrap();
                    // Pull apart the message on the boundary.
                    let mut parts = MimeMessage::split_boundary(&body, boundary);
                    // Pop off the first message, as it's part of the parent.
                    let pre_body = parts.remove(0).unwrap_or("".to_string());
                    // Parse out each of the child parts, recursively downwards.
                    // Filtering out and unwrapping None as we go.
                    let message_parts: Vec<MimeMessage> = parts.iter()
                                                               .map(|part| { MimeMessage::parse(part.as_slice()) })
                                                               .filter(|part| { part.is_some() })
                                                               .map(|part| { part.unwrap() })
                                                               .collect();
                    // It should be safe to unwrap the multipart type here because we know the main
                    // mimetype is "multipart"
                    let multipart_type = MimeMultipartType::from_content_type((mime_type, sub_mime_type)).unwrap();

                    MimeMessage::new_with_boundary(pre_body, multipart_type, message_parts, boundary.clone())
                },
                _ => MimeMessage::new(body),
            };

            message.headers = headers;

            Some(message)
        }
    }


    // Split `body` up on the `boundary` string.
    fn split_boundary(body: &String, boundary: &String) -> Vec<String> {
        #[deriving(Show)]
        enum ParseState {
            Normal,
            SeenCr,
            SeenLf,
            SeenDash,
            ReadBoundary,
            BoundaryEnd,
        }

        // Start in a state where we're at the beginning of a line.
        let mut state = ParseState::SeenLf;

        // Initialize starting positions
        let mut pos = 0u;
        let mut boundary_start = 0u;
        let mut boundary_end = 0u;

        let mut parts = Vec::new();

        let body_slice = body.as_slice();

        while pos < body.len() {
            let ch_range = body_slice.char_range_at(pos);
            let c = ch_range.ch;

            state = match (state, c) {
                (ParseState::BoundaryEnd, _) => {
                    // We're now out of a boundary, so remember where the end is,
                    // so we can slice from the end of this boundary to the start of the next.
                    boundary_end = pos;
                    if c == '\n' {
                        ParseState::BoundaryEnd
                    } else {
                        ParseState::Normal
                    }
                },
                (ParseState::ReadBoundary, '\r') => {
                    let read_boundary = body_slice.slice(boundary_start + 1, pos).trim();
                    if &read_boundary.to_string() == boundary {
                        // Boundary matches, push the part
                        // The part is from the last boundary's end to this boundary's beginning
                        let part = body_slice.slice(boundary_end, boundary_start - 1);
                        parts.push(part.to_string());
                        // This is our boundary, so consume boundary end
                        ParseState::BoundaryEnd
                    } else {
                        // This isn't our boundary, so leave it.
                        ParseState::Normal
                    }
                },
                (ParseState::ReadBoundary, _) => ParseState::ReadBoundary,
                (ParseState::SeenDash, '-') => {
                    boundary_start = pos;
                    ParseState::ReadBoundary
                },
                (ParseState::SeenLf, '-') => ParseState::SeenDash,
                (ParseState::SeenCr, '\n') => ParseState::SeenLf,
                (ParseState::Normal, '\r') => ParseState::SeenCr,
                (ParseState::Normal, _) => ParseState::Normal,
                (_, _) => ParseState::Normal,
            };

            pos = ch_range.next;
        }

        // Push in the final part of the message (what remains)
        let final_part = body_slice.slice_from(boundary_end);
        if final_part.trim().len() != 0 {
            parts.push(final_part.to_string());
        }

        parts
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
        children: Vec<MessageTestResult<'s>>,
    }

    impl<'s> MessageTestResult<'s> {
        fn matches(&self, other: &MimeMessage) -> bool {
            let mut headers = HeaderMap::new();
            for &(name, value) in self.headers.iter() {
                let header = Header::new(name.to_string(), value.to_string());
                headers.insert(header);
            }


            let header_match = headers == other.headers;
            let body_match = self.body.to_string() == other.body;

            let mut children_match = self.children.len() == other.children.len();
            if children_match {
                for (index, child) in self.children.iter().enumerate() {
                    if !child.matches(&other.children[index]) {
                        children_match = false;
                        break;
                    }
                }
            }

            if !children_match {
                println!("Children do not match!");
            }
            if !header_match {
                println!("Headers do not match!");
            }
            if !body_match {
                println!("Body does not match! ({} != {})", self.body, other.body);
            }

            header_match && body_match && children_match
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
                    children: vec![],
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
                    children: vec![
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Hello!\r\n",
                            children: vec![],
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Other\r\n",
                            children: vec![],
                        },
                    ],
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
                        \r\n\
                        --bar\r\n\
                        Hello!\r\n\
                        --bar\r\n\
                        Other\r\n\
                        --foo\r\n\
                        Outside\r\n\
                        --foo\r\n",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                        ("Content-Type", "multipart/mixed; boundary=foo"),
                    ],
                    body: "Parent\r\n",
                    children: vec![
                        MessageTestResult {
                            headers: vec![
                                ("Content-Type", "multipart/alternate; boundary=bar"),
                            ],
                            body: "",
                            children: vec![
                                MessageTestResult {
                                    headers: vec![ ],
                                    body: "Hello!\r\n",
                                    children: vec![],
                                },
                                MessageTestResult {
                                    headers: vec![ ],
                                    body: "Other\r\n",
                                    children: vec![],
                                },
                            ],
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Outside\r\n",
                            children: vec![],
                        },
                    ],
                }),
                name: "Deeply nested multipart test",
            },
        ];

        for test in tests.into_iter() {
            println!("--- Next test: {}", test.name);
            let message = MimeMessage::parse(test.input);
            let result = match (test.output, message) {
                (Some(ref expected), Some(ref given)) => expected.matches(given),
                (None, None) => true,
                (_, _) => false,
            };
            assert!(result, test.name);
        }
    }

    struct BodyDecodingTestResult<'s> {
        body: &'s str,
        headers: Vec<(&'s str, &'s str)>,
        result: Option<&'s str>,
    }

    #[test]
    fn test_body_string_decoding() {
        let tests = vec![
            BodyDecodingTestResult {
                body: "foo=\r\nbar\r\nbaz",
                headers: vec![
                    ("Content-Type", "text/plain"),
                    ("Content-Transfer-Encoding", "quoted-printable"),
                ],
                result: Some("foobar\r\nbaz"),
            },
            BodyDecodingTestResult {
                body: "foo=\r\nbar\r\nbaz",
                headers: vec![
                    ("Content-Type", "text/plain"),
                    ("Content-Transfer-Encoding", "7bit"),
                ],
                result: Some("foo=\r\nbar\r\nbaz"),
            },
        ];

        for test in tests.into_iter() {
            let mut headers = HeaderMap::new();
            for (name, value) in test.headers.into_iter() {
                headers.insert(Header::new(name.to_string(), value.to_string()));
            }
            let mut message = MimeMessage::new(test.body.to_string());
            message.headers = headers;
            let expected = test.result.map(|s| { s.to_string() });
            assert_eq!(message.decoded_body_string(), expected);
        }
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

    struct MultipartParseTest<'s> {
        mime_type: (&'s str, &'s str),
        result: Option<MimeMultipartType>,
    }

    #[test]
    fn test_multipart_type_type_parsing() {
        let tests = vec![
            MultipartParseTest {
                mime_type: ("multipart", "mixed"),
                result: Some(MimeMultipartType::Mixed),
            },
            MultipartParseTest {
                mime_type: ("multipart", "alternate"),
                result: Some(MimeMultipartType::Alternate),
            },
            MultipartParseTest {
                mime_type: ("multipart", "digest"),
                result: Some(MimeMultipartType::Digest),
            },
            MultipartParseTest {
                mime_type: ("multipart", "parallel"),
                result: Some(MimeMultipartType::Parallel),
            },
            // Test fallback on multipart/mixed
            MultipartParseTest {
                mime_type: ("multipart", "potato"),
                result: Some(MimeMultipartType::Mixed),
            },
            // Test failure state
            MultipartParseTest {
                mime_type: ("text", "plain"),
                result: None,
            },
        ];

        for test in tests.into_iter() {
            let (major_type, minor_type) = test.mime_type;
            assert_eq!(
                MimeMultipartType::from_content_type((major_type.to_string(), minor_type.to_string())),
                test.result
            );
        }
    }

    #[test]
    fn test_multipart_type_to_content_type() {
        let multipart = "multipart".to_string();

        assert_eq!(MimeMultipartType::Mixed.to_content_type(),     (multipart.clone(), "mixed".to_string()));
        assert_eq!(MimeMultipartType::Alternate.to_content_type(), (multipart.clone(), "alternate".to_string()));
        assert_eq!(MimeMultipartType::Digest.to_content_type(),    (multipart.clone(), "digest".to_string()));
        assert_eq!(MimeMultipartType::Parallel.to_content_type(),  (multipart.clone(), "parallel".to_string()));
    }

    #[test]
    fn test_boundary_generation() {
        let message = MimeMessage::new("Body".to_string());
        // This is random, so we can only really check that it's the expected length
        assert_eq!(message.boundary.len(), super::BOUNDARY_LENGTH);
    }
}
