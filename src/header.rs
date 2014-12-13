use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt;
use std::iter::Map;
use std::slice::Items;
use std::rc::Rc;

use super::rfc2047::decode_rfc2047;

/// Trait for converting from RFC822 Header values into
/// Rust types.
#[stable]
pub trait FromHeader {
    /// Parse the `value` of the header.
    ///
    /// Returns None if the value failed to be parsed
    fn from_header(value: String) -> Option<Self>;
}

/// Trait for converting from a Rust type into a Header value.
#[stable]
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
#[unstable]
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
        #[deriving(Show,Copy)]
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
#[unstable]
pub struct Header {
    /// The name of this header
    pub name: String,
    value: String,
}

impl Header {
    /// Creates a new Header for the given `name` and `value`
    #[unstable]
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
    #[unstable]
    pub fn new_with_value<T: ToFoldedHeader>(name: String, value: T) -> Option<Header> {
        let header_len = name.len() + 2;
        ToFoldedHeader::to_folded_header(header_len, value).map(|val| { Header::new(name.clone(), val) })
    }

    /// Get the value represented by this header, as parsed
    /// into whichever type `T`
    #[unstable]
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
#[unstable]
pub struct HeaderMap {
    // We store headers "twice" inside the HeaderMap.
    //
    // The first is as an ordered list of headers,
    // which is used to iterate over.
    ordered_headers: Vec<Rc<Header>>,
    // The second is as a mapping between header names
    // and all of the headers with that name.
    //
    // This allows quick retrival of a header by name.
    headers: HashMap<String, Vec<Rc<Header>>>,
}

impl HeaderMap {
    #[unstable]
    pub fn new() -> HeaderMap {
        HeaderMap {
            ordered_headers: Vec::new(),
            headers: HashMap::new(),
        }
    }

    /// Adds a header to the collection
    #[unstable]
    pub fn insert(&mut self, header: Header) {
        let header_name = header.name.clone();
        let rc = Rc::new(header);
        // Add to the ordered list of headers
        self.ordered_headers.push(rc.clone());
        
        // and to the mapping between header names and values.
        match self.headers.entry(header_name) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().push(rc.clone());
            },
            Entry::Vacant(entry) => {
                // There haven't been any headers with this name
                // as of yet, so make a new list and push it in.
                let mut header_list = Vec::new();
                header_list.push(rc.clone());
                entry.set(header_list);
            },
        };
    }

    /// Get an Iterator over the collection of headers.
    #[unstable]
    pub fn iter(&self) -> Map<&Rc<Header>, &Header, Items<Rc<Header>>> {
        self.ordered_headers.iter()
                            .map(|rc| { rc.deref() })
    }

    /// Get the last value of the header with `name`
    #[unstable]
    pub fn get(&self, name: String) -> Option<&Header> {
        self.headers.get(&name).map(|headers| { headers.last().unwrap() })
                               .map(|rc| { rc.deref() })
    }

    /// Get the last value of the header with `name`, as a decoded type.
    #[unstable]
    pub fn get_value<T: FromHeader>(&self, name: String) -> Option<T> {
        match self.get(name) {
            Some(ref header) => header.get_value(),
            None => None,
        }
    }

    #[unstable]
    /// Get the number of headers within this map.
    pub fn len(&self) -> uint {
        self.ordered_headers.len()
    }

    #[unstable]
    /// Find a list of headers of `name`, `None` if there
    /// are no headers with that name.
    pub fn find(&self, name: &String) -> Option<Vec<&Header>> {
        let headers_rcs = self.headers.get(name);
        if headers_rcs.is_some() {
            Some(headers_rcs.unwrap().iter().map(|rc| { rc.deref() }).collect())
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
