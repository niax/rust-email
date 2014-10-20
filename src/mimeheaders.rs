use super::header::FromHeader;
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
            let parsed_header: Option<MimeContentTypeHeader> = header.get_value();

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
}
