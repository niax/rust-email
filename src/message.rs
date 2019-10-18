
use super::header::{HeaderMap, Header};
use super::rfc5322::{Rfc5322Parser, Rfc5322Builder};
use super::results::{ParsingResult,ParsingError};
use super::mimeheaders::{
    MimeContentType,
    MimeContentTypeHeader,
    MimeContentTransferEncoding
};

use std::collections::HashMap;
use std::fmt;

use encoding::label::encoding_from_whatwg_label;
use encoding::DecoderTrap;

use rand::{thread_rng, Rng};
use rand::distributions::Alphanumeric;

const BOUNDARY_LENGTH: usize = 30;

/// Marks the type of a multipart message
#[derive(Eq,PartialEq,Debug,Clone,Copy)]
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
    Alternative,
    /// Entries are (typically) a collection of messages.
    ///
    /// As defined by Section 5.1.5 of RFC 2046
    Digest,
    /// An encyrpted multipart composed by two independent entries.
    ///
    /// As defined by Section 2.2 of RFC 1847
    Encrypted,
    /// Entry order does not matter, and could be displayed simultaneously.
    ///
    /// As defined by Section 5.1.6 of RFC 2046
    Parallel,
    /// An signed multipart composed by two independent entries.
    ///
    /// As defined by Section 2.1 of RFC 1847
    Signed,
}

impl MimeMultipartType {
    /// Returns the appropriate `MimeMultipartType` for the given MimeContentType
    pub fn from_content_type(ct: MimeContentType) -> Option<MimeMultipartType> {
        let (major, minor) = ct;
        match (&major[..], &minor[..]) {
            ("multipart", "alternative") => Some(MimeMultipartType::Alternative),
            ("multipart", "digest") => Some(MimeMultipartType::Digest),
            ("multipart", "parallel") => Some(MimeMultipartType::Parallel),
            ("multipart", "encrypted") => Some(MimeMultipartType::Encrypted),
            ("multipart", "signed") => Some(MimeMultipartType::Signed),
            ("multipart", "mixed") | ("multipart", _) => Some(MimeMultipartType::Mixed),
            _ => None,
        }
    }

    /// Returns a MimeContentType that represents this multipart type.
    pub fn to_content_type(&self) -> MimeContentType {
        let multipart = "multipart".to_string();
        match *self {
            MimeMultipartType::Mixed => (multipart, "mixed".to_string()),
            MimeMultipartType::Alternative => (multipart, "alternative".to_string()),
            MimeMultipartType::Digest => (multipart, "digest".to_string()),
            MimeMultipartType::Encrypted => (multipart, "encrypted".to_string()),
            MimeMultipartType::Parallel => (multipart, "parallel".to_string()),
            MimeMultipartType::Signed => (multipart, "signed".to_string()),
        }
    }
}

impl fmt::Display for MimeMultipartType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ct = self.to_content_type();
        write!(f, "{}/{}", ct.0, ct.1)
    }
}

/// Represents a MIME message
/// [unstable]
#[derive(Eq,PartialEq,Debug,Clone)]
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
        let mut rng = thread_rng();
        std::iter::repeat(())
            .map(|()| rng.sample(Alphanumeric))
            .take(BOUNDARY_LENGTH).collect()
    }

    /// [unstable]
    pub fn new(body: String) -> MimeMessage {
        let mut message = MimeMessage::new_blank_message();
        message.body = body;
        message.update_headers();
        message
    }

    pub fn new_with_children(body: String, message_type: MimeMultipartType, children: Vec<MimeMessage>) -> MimeMessage {
        let mut message = MimeMessage::new_blank_message();
        message.body = body;
        message.message_type = Some(message_type);
        message.children = children;
        message.update_headers();
        message
    }

    pub fn new_with_boundary(body: String,
                             message_type: MimeMultipartType,
                             children: Vec<MimeMessage>,
                             boundary: String) -> MimeMessage {
        let mut message = MimeMessage::new_blank_message();
        message.body = body;
        message.message_type = Some(message_type);
        message.children = children;
        message.boundary = boundary;
        message.update_headers();
        message
    }

    pub fn new_blank_message() -> MimeMessage {
        MimeMessage {
            headers: HeaderMap::new(),
            body: "".to_string(),
            message_type: None,
            children: Vec::new(),

            boundary: MimeMessage::random_boundary(),
        }
    }

    /// Update the headers on this message based on the internal state.
    ///
    /// When certain properties of the message are modified, the headers
    /// used to represent them are not automatically updated.
    /// Call this if these are changed.
    ///
    /// If the message has children:
    /// if `message_type` is set, it adds a new `Content-Type` header of type
    /// `message_type`. Note that `message_type` does not allow parameters.
    /// otherwise, if it has a `Content-Type` header, use it as it is,
    /// otherwise, assume the `Content-Type` is 'multipart/mixed'.
    /// Finally, update the `Content-Type` header with the `boundary`.
    /// It does not update the `message_type`
    pub fn update_headers(&mut self) {

        if self.children.len() > 0 {
            // Since a single header can not be replace, clone all the headers
            // except th `Content-Type`, which needs will be added later.
            let mut headers = self.headers_without_content_type();
            // Instead of returning the value of the `Content-Type` as a tuple,
            // return it as String since it could have params.
            let ct_value: String;
            if let Some(message_type) = self.message_type {
                // Use `message_type` as `Content-Type` if present
                ct_value = message_type.to_string();
            } else {
                ct_value = match self.headers.get("Content-Type".to_string()) {
                    // Use `Content-Type` header if present
                    Some(ct_header) => ct_header.get_value().unwrap(),
                    // Or assume it is `multipart/mixed`.
                    None => MimeMultipartType::Mixed.to_string()
                };
            }
            // Insert the boundary param in the `Content-Type` header.
            // A MimeContentTypeHeader could be created, but it is
            // converted back to string when inserted into the headers
            let ct_value_boundary = format!("{}; {}=\"{}\"",
                                            ct_value, "boundary".to_string(),
                                            self.boundary.clone());
            let header = Header::new("Content-Type".to_string(),
                                     ct_value_boundary);
            // Add the `Content-Type` with the boundary.
            headers.insert(header);
            // And replace all the headers.
            self.headers = headers;
        }
    }

    /// Returns a copy of the headers without the `Content-Type` header.
    ///
    /// It used to add a parameter to the `Content-Type` header, by cloning
    /// all the headers except that one and then adding it.
    fn headers_without_content_type(&self) -> HeaderMap {
        let mut header_map = HeaderMap::new();
        let headers: Vec<&Header> = self.headers.iter()
            .filter(|header| !header.to_string().starts_with("Content-Type"))
            .collect();
        for header in headers {
            header_map.insert(header.clone());
        }
        header_map
    }

    /// Parse `s` into a MimeMessage.
    ///
    /// Recurses down into each message, supporting an unlimited depth of messages.
    ///
    /// Be warned that each sub-message that fails to be parsed will be thrown away.
    /// [unstable]
    pub fn parse(s: &str) -> ParsingResult<MimeMessage> {
        let mut parser = Rfc5322Parser::new(s);
        match parser.consume_message() {
            Some((headers, body)) => MimeMessage::from_headers(headers, body),
            None => Err(ParsingError::new("Couldn't parse MIME message.".to_string()))
        }
    }

    pub fn as_string(&self) -> String {
        let mut builder = Rfc5322Builder::new();

        for header in self.headers.iter() {
            builder.emit_folded(&header.to_string()[..]);
            builder.emit_raw("\r\n");
        }
        builder.emit_raw("\r\n");

        self.as_string_without_headers_internal(builder)
    }

    pub fn as_string_without_headers(&self) -> String {
        let builder = Rfc5322Builder::new();

        self.as_string_without_headers_internal(builder)
    }

    fn as_string_without_headers_internal(&self, mut builder: Rfc5322Builder) -> String {
        builder.emit_raw(&format!("{}\r\n", self.body)[..]);

        if self.children.len() > 0 {

            for part in self.children.iter() {
                builder.emit_raw(
                    &format!("--{}\r\n{}\r\n",
                            self.boundary,
                            part.as_string())[..]
                );
            }

            builder.emit_raw(&format!("--{}--\r\n", self.boundary)[..]);
        }

        builder.result().clone()
    }

    /// Decode the body of this message, as a series of bytes
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
    pub fn decoded_body_string(&self) -> ParsingResult<String> {
        let bytes = match self.decoded_body_bytes() {  // FIXME
            Some(x) => x,
            None => return Err(ParsingError::new("Unable to get decoded body bytes.".to_string()))
        };

        let content_type: Result<MimeContentTypeHeader, _> =
            self.headers.get_value("Content-Type".to_string());
        let charset = match content_type {
            Ok(ct) => ct.params.get(&"charset".to_string()).cloned(),
            Err(_) => None,
        }.unwrap_or("us-ascii".to_string());

        match encoding_from_whatwg_label(&charset[..]) {
            Some(decoder) => match decoder.decode(&bytes, DecoderTrap::Replace) {
                Ok(x) => Ok(x),
                Err(e) => Err(ParsingError::new(format!("Unable to decode body: {}", e)))
            },
            None => Err(ParsingError::new(format!("Invalid encoding: {}", charset)))
        }
    }

    // Make a message from a header map and body, parsing out any multi-part
    // messages that are discovered by looking at the Content-Type header.
    fn from_headers(headers: HeaderMap, body: String) -> ParsingResult<MimeMessage> {
        let content_type = try!({
            let header = headers.get("Content-Type".to_string());
            match header {
                Some(h) => h.get_value(),
                None => Ok(MimeContentTypeHeader {
                    content_type: ("text".to_string(), "plain".to_string()),
                    params: HashMap::new(),
                })
            }
        });

        // Pull out the major mime type and the boundary (if it exists)
        let (mime_type, sub_mime_type) = content_type.content_type;
        let boundary = content_type.params.get(&"boundary".to_string());

        let mut message = match (&mime_type[..], boundary) {
            // Only consider a multipart message if we have a boundary, otherwise don't
            // bother and just assume it's a single message.
            ("multipart", Some(boundary)) => {
                // Pull apart the message on the boundary.
                let mut parts = MimeMessage::split_boundary(&body, boundary);
                // Pop off the first message, as it's part of the parent.
                let pre_body = if parts.is_empty() { "".to_string() } else { parts.remove(0) };
                // Parse out each of the child parts, recursively downwards.
                // Filtering out and unwrapping None as we go.
                let message_parts: Vec<MimeMessage> = parts
                    .iter()
                    .filter_map(|part| match MimeMessage::parse(&part[..]) {
                        Ok(x) => Some(x),
                        Err(_) => None
                    })
                    .collect();
                // It should be safe to unwrap the multipart type here because we know the main
                // mimetype is "multipart"
                let multipart_type = MimeMultipartType::from_content_type((mime_type, sub_mime_type)).unwrap();

                MimeMessage::new_with_boundary(pre_body, multipart_type, message_parts, boundary.clone())
            },
            _ => MimeMessage::new(body),
        };

        message.headers = headers;
        Ok(message)
    }


    // Split `body` up on the `boundary` string.
    fn split_boundary(body: &String, boundary: &String) -> Vec<String> {
        #[derive(Debug)]
        enum ParseState {
            Distinguished,
            DistinguishedEnd,
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
        let mut boundary_start = 0;
        let mut boundary_end = 0;

        let mut parts = Vec::new();

        let body_slice = &body[..];

        let mut done = false;

        for (pos, c) in body.char_indices() {
            state = match (state, c) {
                (ParseState::ReadBoundary, '-') => {
                    ParseState::Distinguished
                },
                (ParseState::Distinguished, '-') => {
                    ParseState::DistinguishedEnd
                },
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
                (ParseState::DistinguishedEnd, '\r') | (ParseState::DistinguishedEnd, '\n') => {
                    let read_boundary = body_slice[(boundary_start + 1)..(pos - 2)].trim();
                    if &read_boundary.to_string() == boundary {
                        // Boundary matches, push the part
                        // The part is from the last boundary's end to this boundary's beginning
                        let part = &body_slice[boundary_end..(boundary_start - 1)];
                        parts.push(part.to_string());
                        done = true;
                        break
                    } else {
                        // This isn't our boundary, so leave it.
                        ParseState::Normal
                    }
                },
                (ParseState::ReadBoundary, '\r') | (ParseState::ReadBoundary, '\n') => {
                    let read_boundary = body_slice[(boundary_start + 1)..pos].trim();
                    if &read_boundary.to_string() == boundary {
                        // Boundary matches, push the part
                        // The part is from the last boundary's end to this boundary's beginning
                        let part = &body_slice[boundary_end..(boundary_start - 1)];
                        parts.push(part.to_string());
                        // This is our boundary, so consume boundary end
                        ParseState::BoundaryEnd
                    } else {
                        // This isn't our boundary, so leave it.
                        ParseState::Normal
                    }
                },
                (ParseState::ReadBoundary, _) => ParseState::ReadBoundary,
                (_, '\n') => ParseState::SeenLf,
                (_, '\r') => ParseState::SeenCr,
                (ParseState::SeenDash, '-') => {
                    boundary_start = pos;
                    ParseState::ReadBoundary
                },
                (ParseState::SeenLf, '-') => ParseState::SeenDash,
                (ParseState::Normal, _) => ParseState::Normal,
                (_, _) => ParseState::Normal,
            };
        }

        if !done {
            // Push in the final part of the message (what remains)
            let final_part = &body_slice[boundary_end..];
            if final_part.trim().len() != 0 {
                parts.push(final_part.to_string());
            }
        }

        parts
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::header::{Header,HeaderMap};

    #[derive(Debug)]
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
                        Content-Type: multipart/alternative; boundary=foo\r\n\
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
                        ("Content-Type", "multipart/alternative; boundary=foo"),
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
                input: "From: joe@example.org\n\
                        To: john@example.org\n\
                        Content-Type: multipart/alternative; boundary=\"foo\"\n\
                        \n\
                        \n\
                        Parent\n\
                        --foo\n\
                        Hello!\n\
                        --foo\n\
                        Other\n",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                        ("Content-Type", "multipart/alternative; boundary=\"foo\""),
                    ],
                    body: "\nParent\n",
                    children: vec![
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Hello!\n",
                            children: vec![],
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Other\n",
                            children: vec![],
                        },
                    ],
                }),
                name: "Unix line ending multipart message parse",
            },

            ParseTest {
                input: "From: joe@example.org\r\n\
                        To: john@example.org\r\n\
                        Content-Type: multipart/mixed; boundary=foo\r\n\
                        \r\n\
                        Parent\r\n\
                        --foo\r\n\
                        Content-Type: multipart/alternative; boundary=bar\r\n\
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
                                ("Content-Type", "multipart/alternative; boundary=bar"),
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
            ParseTest {
                input: "From: joe@example.org\n\
                        To: john@example.org\n\
                        Content-Type: multipart/alternative; boundary=\"foo\"\n\
                        \n\
                        \n\
                        Parent\n\
                        --foo\n\
                        Hello!\n\
                        --foo\n\
                        Other\n\
                        --foo--\n\
                        Outside\n",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                        ("Content-Type", "multipart/alternative; boundary=\"foo\""),
                    ],
                    body: "\nParent\n",
                    children: vec![
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Hello!\n",
                            children: vec![],
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Other\n",
                            children: vec![],
                        },
                    ],
                }),
                name: "Distinguished boundary",
            },
            ParseTest {
                input: "From: joe@example.org\n\
                        To: john@example.org\n\
                        Content-Type: multipart/encrypted; protocol=\"application/pgp-encrypted\"; boundary=\"foo\"\n\
                        \n\
                        \n\
                        Parent\n\
                        --foo\n\
                        Hello!\n\
                        --foo\n\
                        Other\n",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                        ("Content-Type", "multipart/encrypted; protocol=\"application/pgp-encrypted\"; boundary=\"foo\""),
                    ],
                    body: "\nParent\n",
                    children: vec![
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Hello!\n",
                            children: vec![],
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Other\n",
                            children: vec![],
                        },
                    ],
                }),
                name: "Distinguished boundary",
            },
            ParseTest {
                input: "From: joe@example.org\n\
                        To: john@example.org\n\
                        Content-Type: multipart/signed; protocol=\"application/pgp-signature\"; boundary=\"foo\"\n\
                        \n\
                        \n\
                        Parent\n\
                        --foo\n\
                        Hello!\n\
                        --foo\n\
                        Other\n",
                output: Some(MessageTestResult {
                    headers: vec![
                        ("From", "joe@example.org"),
                        ("To", "john@example.org"),
                        ("Content-Type", "multipart/signed; protocol=\"application/pgp-signature\"; boundary=\"foo\""),
                    ],
                    body: "\nParent\n",
                    children: vec![
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Hello!\n",
                            children: vec![],
                        },
                        MessageTestResult {
                            headers: vec![ ],
                            body: "Other\n",
                            children: vec![],
                        },
                    ],
                }),
                name: "Signed",
            },

        ];

        for test in tests.into_iter() {
            println!("--- Next test: {}", test.name);
            let message = MimeMessage::parse(test.input);
            let result = match (test.output, message) {
                (Some(ref expected), Ok(ref given)) => expected.matches(given),
                (None, Err(_)) => true,
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
            assert_eq!(message.decoded_body_string().ok(), expected);
        }
    }

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
                mime_type: ("multipart", "alternative"),
                result: Some(MimeMultipartType::Alternative),
            },
            MultipartParseTest {
                mime_type: ("multipart", "digest"),
                result: Some(MimeMultipartType::Digest),
            },
            MultipartParseTest {
                mime_type: ("multipart", "encrypted"),
                result: Some(MimeMultipartType::Encrypted),
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
            MultipartParseTest {
                mime_type: ("multipart", "signed"),
                result: Some(MimeMultipartType::Signed),
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
        assert_eq!(MimeMultipartType::Alternative.to_content_type(), (multipart.clone(), "alternative".to_string()));
        assert_eq!(MimeMultipartType::Digest.to_content_type(),    (multipart.clone(), "digest".to_string()));
        assert_eq!(MimeMultipartType::Encrypted.to_content_type(),    (multipart.clone(), "encrypted".to_string()));
        assert_eq!(MimeMultipartType::Parallel.to_content_type(),  (multipart.clone(), "parallel".to_string()));
        assert_eq!(MimeMultipartType::Signed.to_content_type(),    (multipart.clone(), "signed".to_string()));
    }

    #[test]
    fn test_boundary_generation() {
        let message = MimeMessage::new("Body".to_string());
        // This is random, so we can only really check that it's the expected length
        assert_eq!(message.boundary.len(), super::BOUNDARY_LENGTH);
    }

    #[test]
    fn test_headers_without_content_type() {
        let mime = MimeMessage::parse(
            "Content-Type: multipart/signed; protocol=pgp-encrypted\nBody"
        ).unwrap();
        assert_eq!(mime.headers.len(), 1);
        let ct_value: String = mime.headers.get("Content-Type".to_string())
            .unwrap().get_value().unwrap();
        assert_eq!(ct_value, "multipart/signed; protocol=pgp-encrypted");
        let headers = mime.headers_without_content_type();
        assert_eq!(headers.len(), 0);
    }

    #[test]
    fn test_multipart_encrypted() {
        let text = "From: Alice Lovelace <alice@openpgp.example>
To: Bob Babbage <bob@openpgp.example>
Subject: Encrypted message
MIME-Version: 1.0
Content-Type: multipart/encrypted; protocol=\"application/pgp-encrypted\";
 boundary=\"boundary_encrypted\"

This is an OpenPGP/MIME encrypted message (RFC 4880 and 3156)
--boundary_encrypted
Content-Type: application/pgp-encrypted
Content-Description: PGP/MIME version identification

Version: 1

--boundary_encrypted
Content-Type: application/octet-stream; name=\"encrypted.asc\"
Content-Description: OpenPGP encrypted message
Content-Disposition: inline; filename=\"encrypted.asc\"

-----BEGIN PGP MESSAGE-----
-----END PGP MESSAGE-----

--boundary_encrypted--";
        let mime = MimeMessage::parse(text).unwrap();
        assert_eq!(mime.message_type.unwrap(), MimeMultipartType::Encrypted);
        assert_eq!(mime.headers.len(), 5);
        assert_eq!(mime.body,
                   "This is an OpenPGP/MIME encrypted message \
                    (RFC 4880 and 3156)\n");
        assert_eq!(mime.children.len(), 2);
        // XXX: The original boundary should have been removed, lettre does it.
        assert_eq!(mime.children[1].body,
                   "-----BEGIN PGP MESSAGE-----\n\
                    -----END PGP MESSAGE-----\n\n--boundary_encrypted--");

        // XXX: parse add extra new lines.
        assert_eq!(mime.children[0].body, "Version: 1\n\n");
        assert_eq!(mime.boundary, "boundary_encrypted");

        let mime2 = MimeMessage::parse(&mime.as_string()).unwrap();
        assert_eq!(format!("{}\r\n", mime.body), mime2.body);
        assert_eq!(mime.boundary, mime2.boundary);
        assert_eq!(mime.message_type, mime2.message_type);

        let ct: String = mime.headers.get("Content-Type".to_string())
            .unwrap().get_value().unwrap();
        assert_eq!(ct,
                   "multipart/encrypted; \
                    protocol=\"application/pgp-encrypted\";\
                    boundary=\"boundary_encrypted\"");

        // XXX: the space after `multipart` gets removed.
        let ct2: String = mime2.headers.get("Content-Type".to_string())
            .unwrap().get_value().unwrap();
        assert_eq!(ct2,
                   "multipart/encrypted;\
                    protocol=\"application/pgp-encrypted\";\
                    boundary=\"boundary_encrypted\"");

        assert_eq!(format!("{}\r\n\r\n", mime.children[0].body),
            mime2.children[0].body);
        assert_eq!(mime.children[1].body,
            format!("{}--boundary_encrypted--", mime2.children[1].body));
    }
}

#[cfg(all(feature = "nightly", test))]
mod bench {
    extern crate test;
    use self::test::Bencher;

    use super::*;

    macro_rules! bench_parser {
        ($name:ident, $test:expr) => (
            #[bench]
            fn $name(b: &mut Bencher) {
                let s = $test;
                b.iter(|| {
                    let _ = MimeMessage::parse(s);
                });
            }
        );
    }

    bench_parser!(bench_simple, "From: joe@example.org\r\nTo: john@example.org\r\n\r\nHello!");
    bench_parser!(bench_simple_multipart,
        "From: joe@example.org\r\n\
         To: john@example.org\r\n\
         Content-Type: multipart/alternative; boundary=foo\r\n\
         \r\n\
         Parent\r\n\
         --foo\r\n\
         Hello!\r\n\
         --foo\r\n\
         Other\r\n\
         --foo"
    );
    bench_parser!(bench_deep_multipart,
        "From: joe@example.org\r\n\
         To: john@example.org\r\n\
         Content-Type: multipart/mixed; boundary=foo\r\n\
         \r\n\
         Parent\r\n\
         --foo\r\n\
         Content-Type: multipart/alternative; boundary=bar\r\n\
         \r\n\
         --bar\r\n\
         Hello!\r\n\
         --bar\r\n\
         Other\r\n\
         --foo\r\n\
         Outside\r\n\
         --foo\r\n"
    );
}
