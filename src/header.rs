use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt;
use std::ops::Deref;
use std::slice::Iter as SliceIter;
use std::sync::Arc;

use chrono::{
    DateTime,
    FixedOffset,
    Utc,
};

use super::rfc2047::decode_rfc2047;
use super::rfc822::Rfc822DateParser;
use super::results::{ParsingResult,ParsingError};

/// Trait for converting from RFC822 Header values into
/// Rust types.
pub trait FromHeader: Sized {
    /// Parse the `value` of the header.
    ///
    /// Returns None if the value failed to be parsed
    fn from_header(value: String) -> ParsingResult<Self>;
}

/// Trait for converting from a Rust type into a Header value.
pub trait ToHeader {
    /// Turn the `value` into a String suitable for being used in
    /// a message header.
    ///
    /// Returns None if the value cannot be stringified.
    fn to_header(value: Self) -> ParsingResult<String>;
}

/// Trait for converting from a Rust time into a Header value
/// that handles its own folding.
///
/// Be mindful that this trait does not mean that the value will
/// not be folded later, rather that the type returns a value that
/// should not be folded, given that the header value starts so far
/// in to a line.
/// [unstable]
pub trait ToFoldedHeader {
    fn to_folded_header(start_pos: usize, value: Self) -> ParsingResult<String>;
}

impl<T: ToHeader> ToFoldedHeader for T {
    fn to_folded_header(_: usize, value: T) -> ParsingResult<String> {
        // We ignore the start_position because the thing will fold anyway.
        ToHeader::to_header(value)
    }
}

impl FromHeader for String {
    fn from_header(value: String) -> ParsingResult<String> {
        #[derive(Debug,Clone,Copy)]
        enum ParseState {
            Normal(usize),
            SeenEquals(usize),
            SeenQuestion(usize, usize),
        }

        let mut state = ParseState::Normal(0);
        let mut decoded = String::new();

        let value_slice = &value[..];

        for (pos, c) in value.char_indices() {
            state = match (state, c) {
                (ParseState::SeenQuestion(start_pos, 4), '=') => {
                    let next_pos = pos + c.len_utf8();
                    // Go to decode if we've seen enough ?
                    let part_decoded = decode_rfc2047(&value_slice[start_pos..next_pos]);
                    let to_push = match part_decoded {
                        Some(ref s) => &s[..],
                        // Decoding failed, push the undecoded string in.
                        None => &value_slice[start_pos..pos],
                    };
                    decoded.push_str(to_push);
                    // Revert us to normal state, but starting at the next character.
                    ParseState::Normal(next_pos)
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
                        decoded.push_str(&value_slice[start_pos..pos]);
                    }
                    ParseState::SeenEquals(pos)
                },
                (ParseState::Normal(_), _) => state,
            };
        }

        // Don't forget to push on whatever we have left
        let last_start = match state {
            ParseState::Normal(start_pos) => start_pos,
            ParseState::SeenEquals(start_pos) => start_pos,
            ParseState::SeenQuestion(start_pos, _) => start_pos,
        };
        decoded.push_str(&value_slice[last_start..]);


        Ok(decoded)
    }
}

impl FromHeader for DateTime<FixedOffset> {
    fn from_header(value: String) -> ParsingResult<DateTime<FixedOffset>> {
        let mut parser = Rfc822DateParser::new(&value[..]);
        parser.consume_datetime()
    }
}

impl FromHeader for DateTime<Utc> {
    fn from_header(value: String) -> ParsingResult<DateTime<Utc>> {
        let dt: ParsingResult<DateTime<FixedOffset>> = FromHeader::from_header(value);
        dt.map(|i| i.with_timezone(&Utc))
    }
}

impl ToHeader for String {
    fn to_header(value: String) -> ParsingResult<String> {
        Ok(value)
    }
}

impl<'a> ToHeader for &'a str {
    fn to_header(value: &'a str) -> ParsingResult<String> {
        Ok(value.to_string())
    }
}

/// Represents an RFC 822 Header
/// [unstable]
#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Header {
    /// The name of this header
    pub name: String,
    value: String,
}

impl Header {
    /// Creates a new Header for the given `name` and `value`
    /// [unstable]
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
    /// [unstable]
    pub fn new_with_value<T: ToFoldedHeader>(name: String, value: T) -> ParsingResult<Header> {
        let header_len = name.len() + 2;
        ToFoldedHeader::to_folded_header(header_len, value).map(|val| { Header::new(name.clone(), val) })
    }

    /// Get the value represented by this header, as parsed
    /// into whichever type `T`
    /// [unstable]
    pub fn get_value<T: FromHeader>(&self) -> ParsingResult<T> {
        FromHeader::from_header(self.value.clone())
    }
}

impl fmt::Display for Header {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}: {}", self.name, self.value)
    }
}

/// [unstable]
pub struct HeaderIter<'s> {
    iter: SliceIter<'s, Arc<Header>>
}

impl<'s> HeaderIter<'s> {
    /// [unstable]
    fn new(iter: SliceIter<'s, Arc<Header>>) -> HeaderIter<'s> {
        HeaderIter {
            iter: iter
        }
    }
}

impl<'s> Iterator for HeaderIter<'s> {
    type Item = &'s Header;

    fn next(&mut self) -> Option<&'s Header> {
        match self.iter.next() {
            Some(s) => Some(s.deref()),
            None => None,
        }
    }
}

/// A collection of Headers
/// [unstable]
#[derive(Eq,PartialEq,Debug,Clone)]
pub struct HeaderMap {
    // We store headers "twice" inside the HeaderMap.
    //
    // The first is as an ordered list of headers,
    // which is used to iterate over.
    ordered_headers: Vec<Arc<Header>>,
    // The second is as a mapping between header names
    // and all of the headers with that name.
    //
    // This allows quick retrival of a header by name.
    headers: HashMap<String, Vec<Arc<Header>>>,
}

impl HeaderMap {
    /// [unstable]
    pub fn new() -> HeaderMap {
        HeaderMap {
            ordered_headers: Vec::new(),
            headers: HashMap::new(),
        }
    }

    /// Adds a header to the collection
    /// [unstable]
    pub fn insert(&mut self, header: Header) {
        let header_name = header.name.clone();
        let rc = Arc::new(header);
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
                entry.insert(header_list);
            },
        };
    }

    /// Get an Iterator over the collection of headers.
    /// [unstable]
    pub fn iter(&self) -> HeaderIter {
        HeaderIter::new(self.ordered_headers.iter())
    }

    /// Get the last value of the header with `name`
    /// [unstable]
    pub fn get(&self, name: String) -> Option<&Header> {
        self.headers.get(&name).map(|headers| { headers.last().unwrap() })
                               .map(|rc| { rc.deref() })
    }

    /// Get the last value of the header with `name`, as a decoded type.
    /// [unstable]
    pub fn get_value<T: FromHeader>(&self, name: String) -> ParsingResult<T> {
        match self.get(name) {
            Some(ref header) => header.get_value(),
            None => Err(ParsingError::new("Couldn't find header value.".to_string())),
        }
    }

    /// [unstable]
    /// Get the number of headers within this map.
    pub fn len(&self) -> usize {
        self.ordered_headers.len()
    }

    /// [unstable]
    /// Find a list of headers of `name`, `None` if there
    /// are no headers with that name.
    pub fn find(&self, name: &String) -> Option<Vec<&Header>> {
        self.headers.get(name)
            .map(|rcs| rcs.iter().map(|rc| rc.deref()).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    use chrono::{
        DateTime,
        FixedOffset,
        Utc,
    };
    use chrono::offset::TimeZone;

    static SAMPLE_HEADERS: [(&'static str, &'static str); 4] = [
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
            let string_value = header.get_value::<String>().ok();
            assert_eq!(string_value, test.result.map(|s| { s.to_string() }));
        }
    }

    #[test]
    fn test_datetime_get_value() {
        let header = Header::new("Date".to_string(), "Wed, 17 Dec 2014 09:35:07 +0100".to_string());
        let dt_value = header.get_value::<DateTime<FixedOffset>>().unwrap();
        assert_eq!(dt_value, FixedOffset::east(3600).ymd(2014, 12, 17).and_hms(9, 35, 7));
    }

    #[test]
    fn test_datetime_utc_get_value() {
        let header = Header::new("Date".to_string(), "Wed, 17 Dec 2014 09:35:07 +0100".to_string());
        let dt_value = header.get_value::<DateTime<Utc>>().unwrap();
        assert_eq!(dt_value, Utc.ymd(2014, 12, 17).and_hms(8, 35, 7));
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

        let mut count = 0;
        // Ensure all the headers returned are expected
        for header in headers.iter() {
            assert!(expected_headers.contains(header));
            count += 1;
        }
        // And that there is the right number of them
        assert_eq!(count, expected_headers.len());
    }
}
