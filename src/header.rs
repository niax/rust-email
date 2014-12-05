use std::fmt;
use std::slice::Items;

use super::rfc2047::decode_rfc2047;

/// Trait for converting from RFC822 Header values into
/// Rust types.
pub trait FromHeader {
    /// Parse the `value` of the header.
    ///
    /// Returns None if the value failed to be parsed
    fn from_header(value: String) -> Option<Self>;
}

/// Trait for converting from a Rust type into a Header value.
pub trait ToHeader {
    /// Turn the `value` into a String suitable for being used in
    /// a message header.
    ///
    /// Returns None if the value cannot be stringified.
    fn to_header(value: Self) -> Option<String>;
}

/// Trait for converting from a Rust time into a Header value
/// that handles its own folding.
///
/// Be mindful that this trait does not mean that the value will
/// not be folded later, rather that the type returns a value that
/// should not be folded, given that the header value starts so far
/// in to a line.
pub trait ToFoldedHeader {
    fn to_folded_header(start_pos: uint, value: Self) -> Option<String>;
}

impl<T: ToHeader> ToFoldedHeader for T {
    fn to_folded_header(_: uint, value: T) -> Option<String> {
        // We ignore the start_position because the thing will fold anyway.
        ToHeader::to_header(value)
    }
}

impl FromHeader for String {
    fn from_header(value: String) -> Option<String> {
        #[deriving(Show)]
        enum ParseState {
            Normal(uint),
            SeenEquals(uint),
            SeenQuestion(uint, uint),
        }

        let mut state = ParseState::Normal(0u);
        let mut decoded = String::new();
        let mut pos = 0u;

        let value_slice = value.as_slice();

        while pos < value.len() {
            let ch_range = value_slice.char_range_at(pos);
            let c = ch_range.ch;

            state = match (state, c) {
                (ParseState::SeenQuestion(start_pos, 4), '=') => {
                    // Go to decode if we've seen enough ?
                    let part_decoded = decode_rfc2047(value_slice.slice(start_pos, ch_range.next));
                    let to_push = match part_decoded {
                        Some(ref s) => s.as_slice(),
                        // Decoding failed, push the undecoded string in.
                        None => value_slice.slice(start_pos, pos),
                    };
                    decoded.push_str(to_push);
                    // Revert us to normal state, but starting at the next character.
                    ParseState::Normal(ch_range.next)
                },
                (ParseState::SeenQuestion(start_pos, count), '?') => {
                    ParseState::SeenQuestion(start_pos, count + 1)
                },
                (ParseState::SeenQuestion(start_pos, count), _) => {
                    if count > 4 {
                        // This isn't a RFC2047 sequence, so go back to a normal.
                        ParseState::Normal(start_pos)
                    } else {
                        state
                    }
                }
                (ParseState::SeenEquals(start_pos), '?') => {
                    ParseState::SeenQuestion(start_pos, 1)
                },
                (ParseState::SeenEquals(start_pos), _) => {
                    // This isn't a RFC2047 sequence, so go back to a normal.
                    ParseState::Normal(start_pos)
                }
                (ParseState::Normal(start_pos), '=') => {
                    if start_pos != pos {
                        // Push all up to the =, if there is stuff to push.
                        decoded.push_str(value_slice.slice(start_pos, pos));
                    }
                    ParseState::SeenEquals(pos)
                },
                (ParseState::Normal(_), _) => state,
            };

            pos = ch_range.next;
        }

        // Don't forget to push on whatever we have left
        let last_start = match state {
            ParseState::Normal(start_pos) => start_pos,
            ParseState::SeenEquals(start_pos) => start_pos,
            ParseState::SeenQuestion(start_pos, _) => start_pos,
        };
        decoded.push_str(value_slice.slice_from(last_start));


        Some(decoded)
    }
}

impl ToHeader for String {
    fn to_header(value: String) -> Option<String> {
        Some(value)
    }
}

impl<'a> ToHeader for &'a str {
    fn to_header(value: &'a str) -> Option<String> {
        Some(value.to_string())
    }
}

/// Represents an RFC 822 Header
#[deriving(PartialEq, Eq, Clone, Hash)]
pub struct Header {
    /// The name of this header
    pub name: String,
    value: String,
}

impl Header {
    /// Creates a new Header for the given `name` and `value`
    pub fn new(name: String, value: String) -> Header {
        Header {
            name: name,
            value: value,
        }
    }

    /// Creates a new Header for the given `name` and `value`,
    /// as converted through the `ToHeader` or `ToFoldedHeader` trait.
    ///
    /// Returns None if the value failed to be converted.
    pub fn new_with_value<T: ToFoldedHeader>(name: String, value: T) -> Option<Header> {
        let header_len = name.len() + 2;
        ToFoldedHeader::to_folded_header(header_len, value).map(|val| { Header::new(name.clone(), val) })
    }

    /// Get the value represented by this header, as parsed
    /// into whichever type `T`
    pub fn get_value<T: FromHeader>(&self) -> Option<T> {
        FromHeader::from_header(self.value.clone())
    }
}

impl fmt::Show for Header {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}: {}", self.name, self.value)
    }
}

/// A collection of Headers
#[deriving(Eq,PartialEq)]
pub struct HeaderMap {
    headers: Vec<Header>,
}

impl HeaderMap {
    pub fn new() -> HeaderMap{
        HeaderMap {
            headers: Vec::new(),
        }
    }

    /// Adds a header to the collection
    pub fn insert(&mut self, header: Header) {
        self.headers.push(header);
    }

    /// Get an Iterator over the collection of headers.
    pub fn iter(&self) -> Items<Header> {
        self.headers.iter()
    }

    /// Get the last value of the header
    pub fn get(&self, name: String) -> Option<&Header> {
        self.iter().filter(|h| { h.name == name }).last()
    }

    /// Get the last value of the header, as a decoded type.
    pub fn get_value<T: FromHeader>(&self, name: String) -> Option<T> {
        match self.get(name) {
            Some(ref header) => header.get_value(),
            None => None,
        }
    }

    pub fn len(&self) -> uint {
        self.headers.len()
    }

    pub fn find(&self, key: &String) -> Option<Vec<&Header>> {
        let headers: Vec<&Header> = self.iter().filter(|h| { &h.name == key }).collect();

        if headers.len() > 0u {
            Some(headers)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    static SAMPLE_HEADERS: [(&'static str, &'static str), ..4] = [
        ("Test", "Value"),
        ("Test", "Value 2"),
        ("Test-2", "Value 3"),
        ("Test-Multiline", "Foo\nBar"),
    ];

    fn make_sample_headers() -> Vec<Header> {
        SAMPLE_HEADERS.iter().map(|&(name, value)| {
            Header::new(name.to_string(), value.to_string())
        }).collect()
    }


    #[test]
    fn test_header_to_string() {
        let header = Header::new("Test".to_string(), "Value".to_string());
        assert_eq!(header.to_string(), "Test: Value".to_string());
    }

    #[test]
    fn test_string_get_value() {
        struct HeaderTest<'s> {
            input: &'s str,
            result: Option<&'s str>,
        }

        let tests = vec![
            HeaderTest {
                input: "Value",
                result: Some("Value"),
            },
            HeaderTest {
                input: "=?ISO-8859-1?Q?Test=20text?=",
                result: Some("Test text"),
            },
            HeaderTest {
                input: "=?ISO-8859-1?Q?Multiple?= =?utf-8?b?ZW5jb2Rpbmdz?=",
                result: Some("Multiple encodings"),
            },
            HeaderTest {
                input: "Some things with =?utf-8?b?ZW5jb2Rpbmdz?=, other things without.",
                result: Some("Some things with encodings, other things without."),
            },
            HeaderTest {
                input: "Encoding =?utf-8?q?fail",
                result: Some("Encoding =?utf-8?q?fail"),
            },
        ];

        for test in tests.into_iter() {
            let header = Header::new("Test".to_string(), test.input.to_string());
            let string_value = header.get_value::<String>();
            assert_eq!(string_value, test.result.map(|s| { s.to_string() }));
        }
    }

    #[test]
    fn test_to_header_string() {
        let header = Header::new_with_value("Test".to_string(), "Value".to_string()).unwrap();
        let header_value = header.get_value::<String>().unwrap();
        assert_eq!(header_value, "Value".to_string());
    }

    #[test]
    fn test_to_header_str() {
        let header = Header::new_with_value("Test".to_string(), "Value").unwrap();
        let header_value = header.get_value::<String>().unwrap();
        assert_eq!(header_value, "Value".to_string());
    }

    #[test]
    fn test_header_map_len() {
        let mut headers = HeaderMap::new();
        for (i, header) in make_sample_headers().into_iter().enumerate() {
            headers.insert(header);
            assert_eq!(headers.len(), i + 1);
        }
    }
    #[test]
    fn test_header_map_iter() {
        let mut headers = HeaderMap::new();
        let mut expected_headers = HashSet::new();
        for header in make_sample_headers().into_iter() {
            headers.insert(header.clone());
            expected_headers.insert(header);
        }

        let mut count = 0u;
        // Ensure all the headers returned are expected
        for header in headers.iter() {
            assert!(expected_headers.contains(header));
            count += 1;
        }
        // And that there is the right number of them
        assert_eq!(count, expected_headers.len());
    }
}
