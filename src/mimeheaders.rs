use super::header::{
    FromHeader,
    ToHeader,
};
use super::rfc2045::Rfc2045Parser;
use super::rfc2047::decode_q_encoding;
use super::results::{ParsingResult,ParsingError};

use std::ascii::OwnedAsciiExt;
use std::collections::HashMap;
use rustc_serialize::base64::FromBase64;

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
    fn from_header(value: String) -> ParsingResult<MimeContentTypeHeader> {
        let mut parser = Rfc2045Parser::new(&value[..]);
        let (value, params) = parser.consume_all();

        let mime_parts: Vec<&str> = value[..].splitn(2, '/').collect();

        if mime_parts.len() == 2 {
            Ok(MimeContentTypeHeader {
                content_type: (mime_parts[0].to_string(), mime_parts[1].to_string()),
                params: params
            })
        } else {
            Err(ParsingError::new(format!("Invalid mimetype: {}", value)))
        }
    }
}

impl ToHeader for MimeContentTypeHeader {
    fn to_header(value: MimeContentTypeHeader) -> ParsingResult<String> {
        let (mime_major, mime_minor) = value.content_type;
        let mut result = format!("{}/{}", mime_major, mime_minor);
        for (key, val) in value.params.iter() {
            result = format!("{} {}={};", result, key, val);
        }
        Ok(result)
    }
}

/// Special header type for the Content-Transfer-Encoding header.
#[derive(Debug,PartialEq,Eq,Clone,Copy)]
pub enum MimeContentTransferEncoding {
    /// Message content is not encoded in any way.
    Identity,
    /// Content transfered using the quoted-printable encoding.
    ///
    /// This encoding is defined in RFC 2045 Section 6.7
    QuotedPrintable,
    /// Content transfered as BASE64
    ///
    /// This encoding is defined in RFC 2045 Section 6.8
    Base64,
}

impl MimeContentTransferEncoding {
    /// Decode the input string with this transfer encoding.
    ///
    /// Note that this will return a clone of the input's bytes if the
    /// transfer encoding is the Identity encoding.
    /// [unstable]
    pub fn decode(&self, input: &String) -> Option<Vec<u8>> {
        match *self {
            MimeContentTransferEncoding::Identity => Some(input.clone().into_bytes()),
            MimeContentTransferEncoding::QuotedPrintable => decode_q_encoding(&input[..]).ok(),
            MimeContentTransferEncoding::Base64 => input[..].from_base64().ok(),
        }
    }
}

impl FromHeader for MimeContentTransferEncoding {
    fn from_header(value: String) -> ParsingResult<MimeContentTransferEncoding> {
        let lower = value.into_ascii_lowercase();
        match &lower[..] {
            "7bit" | "8bit" | "binary" => Ok(MimeContentTransferEncoding::Identity),
            "quoted-printable" => Ok(MimeContentTransferEncoding::QuotedPrintable),
            "base64" => Ok(MimeContentTransferEncoding::Base64),
            x => Err(ParsingError::new(format!("Invalid encoding: {}", x)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::header::Header;

    use std::collections::HashMap;

    struct ContentTypeParseTestResult<'a> {
        major_type: &'a str,
        minor_type: &'a str,
        params: Vec<(&'a str, &'a str)>,
    }

    struct ContentTypeParseTest<'a> {
        input: &'a str,
        result: Option<ContentTypeParseTestResult<'a>>,
    }

    #[test]
    fn test_content_type_parse() {
        let tests = vec![
            ContentTypeParseTest {
                input: "text/plain",
                result: Some(ContentTypeParseTestResult {
                    major_type: "text",
                    minor_type: "plain",
                    params: vec![],
                }),
            },
            ContentTypeParseTest {
                input: "text/plain; charset=us-ascii",
                result: Some(ContentTypeParseTestResult {
                    major_type: "text",
                    minor_type: "plain",
                    params: vec![
                        ("charset", "us-ascii"),
                    ],
                }),
            },
            ContentTypeParseTest {
                input: "application/octet-stream; charset=us-ascii; param=value",
                result: Some(ContentTypeParseTestResult {
                    major_type: "application",
                    minor_type: "octet-stream",
                    params: vec![
                        ("charset", "us-ascii"),
                        ("param", "value"),
                    ],
                }),
            },
        ];

        for test in tests.into_iter() {
            let header = Header::new("Content-Type".to_string(), test.input.to_string());
            let parsed_header: Option<MimeContentTypeHeader> = header.get_value().ok();

            let result = match (parsed_header, test.result) {
                (Some(given_result), Some(expected_result)) => {
                    let (given_major, given_minor) = given_result.content_type;
                    let mut expected_params = HashMap::new();
                    for &(param_name, param_value) in expected_result.params.iter() {
                        expected_params.insert(param_name.to_string(), param_value.to_string());
                    }
                    given_major == expected_result.major_type.to_string() &&
                        given_minor == expected_result.minor_type.to_string() &&
                        given_result.params == expected_params
                },
                (None, None) => true,
                (_, _) => false,
            };
            assert!(result, format!("Content-Type parse: '{}'", test.input));
        }
    }

    #[test]
    fn test_content_transfer_parse() {
        let tests = vec![
            ("base64", Some(MimeContentTransferEncoding::Base64)),
            ("quoted-printable", Some(MimeContentTransferEncoding::QuotedPrintable)),
            ("7bit", Some(MimeContentTransferEncoding::Identity)),
            ("8bit", Some(MimeContentTransferEncoding::Identity)),
            ("binary", Some(MimeContentTransferEncoding::Identity)),
            // Check for case insensitivity
            ("BASE64", Some(MimeContentTransferEncoding::Base64)),
            // Check for fail case
            ("lkasjdl", None),
        ];

        for (test, expected) in tests.into_iter() {
            let header = Header::new("Content-Transfer-Encoding".to_string(), test.to_string());
            let parsed: Option<MimeContentTransferEncoding> = header.get_value().ok();
            assert_eq!(parsed, expected);
        }
    }

    struct ContentTransferDecodeTest<'s> {
        encoding: MimeContentTransferEncoding,
        input: &'s str,
        output: Option<Vec<u8>>,
    }

    #[test]
    fn test_content_transfer_decode() {
        let tests = vec![
            ContentTransferDecodeTest {
                encoding: MimeContentTransferEncoding::Identity,
                input: "foo",
                output: Some(vec![102, 111, 111]),
            },
            ContentTransferDecodeTest {
                encoding: MimeContentTransferEncoding::QuotedPrintable,
                input: "foo=\r\nbar\r\nbaz",
                output: Some(vec![
                    102, 111, 111, 98, 97, 114, 13, 10,   // foobar
                    98, 97, 122,                          // baz
                ]),
            },
            ContentTransferDecodeTest {
                encoding: MimeContentTransferEncoding::Base64,
                input: "Zm9vCmJhcgpi\r\nYXoKcXV4Cg==",
                output: Some(vec![
                    102, 111, 111, 10, // foo
                    98, 97, 114, 10,   // bar
                    98, 97, 122, 10,   // baz
                    113, 117, 120, 10, // qux
                ]),
            },
            // Bad base64 content
            ContentTransferDecodeTest {
                encoding: MimeContentTransferEncoding::Base64,
                input: "/?#",
                output: None,
            },
        ];

        for test in tests.into_iter() {
            let result = test.encoding.decode(&test.input.to_string());
            assert_eq!(result, test.output);
        }
    }
}
