use std::fmt;
use std::from_str::FromStr;
use std::collections::HashMap;
use std::collections::hashmap::{Occupied, Vacant};
use std::slice::Items;

use super::utils::MultiIter;

/// Trait for converting from RFC822 Header values into
/// Rust types.
pub trait FromHeader {
    /// Parse the `value` of the header.
    ///
    /// Returns None if the value failed to be parsed
    fn from_header(value: String) -> Option<Self>;
}

impl FromHeader for String {
    fn from_header(value: String) -> Option<String> {
        Some(value)
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

    /// Get the value represented by this header, as parsed
    /// into whichever type `T`
    pub fn get_value<T: FromHeader>(&self) -> Option<T> {
        FromHeader::from_header(self.value.clone())
    }
}

impl fmt::Show for Header {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut lines = self.value.as_slice().split('\n');
        // Take the first line as the start of the formatted value
        let mut formatted_value = lines.next().unwrap().to_string();
        for line in lines {
            // Add the separator between lines
            // RFC 822 uses linear whitespace (tabs or spaces) to indicate
            // a continuation of a header value over a newline
            formatted_value.push_str("\r\n\t");
            formatted_value.push_str(line);
        }

        write!(fmt, "{}: {}", self.name, formatted_value)
    }
}

impl FromStr for Header {
    fn from_str(s: &str) -> Option<Header> {
        // Split the header on the first semicolon
        let mut parts = s.splitn(2, ':');
        let before = parts.next();
        let after = parts.next();

        match (before, after) {
            // If either value is None, then this isn't a header
            (None, None) | (_, None) | (None, _) => None,
            (Some(ref name), Some(ref value)) => {
                Some(Header::new(
                    name.trim().to_string(),
                    value.trim().to_string()
                ))
            }
        }
    }
}

/// A collection of Headers
pub struct HeaderMap {
    headers: HashMap<String, Vec<Header>>,
}

impl HeaderMap {
    pub fn new() -> HeaderMap {
        HeaderMap {
            headers: HashMap::new()
        }
    }

    /// Adds a header to the collection
    pub fn insert(&mut self, header: Header) {
        // If the header hashmap already has this header, use that list.
        // Otherwise, make a new one.
        let header_list = match self.headers.entry(header.name.clone()) {
            Vacant(entry) => entry.set(Vec::new()),
            Occupied(entry) => entry.into_mut(),
        };
        // ... and add the new header to it
        header_list.push(header);
    }

    /// Get an Iterator over the collection of headers.
    pub fn iter(&self) -> MultiIter<&Header, Items<Header>> {
        let mut iters = Vec::new();
        for header_list in self.headers.values() {
            iters.push(header_list.iter());
        }
        MultiIter::new(iters)
    }
}

impl fmt::Show for HeaderMap {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for header in self.iter() {
            match write!(fmt, "{}\r\n", header) {
                Err(e) => return Err(e),
                _ => {}
            }
        }
        Ok(())
    }
}

impl FromStr for HeaderMap {
    fn from_str(s: &str) -> Option<HeaderMap> {
        #[deriving(PartialEq, Eq)]
        enum State {
            SeenCr,
            SeenLf,
            Awaiting,
        }

        let mut start = 0u;
        let mut pos = 0u;
        let mut state = Awaiting;
        let mut header_map = HeaderMap::new();

        while pos < s.len() {
            let c = s.char_range_at(pos);
            state = match c.ch {
                '\r' => SeenCr,
                '\n' if state == SeenCr => SeenLf,
                '\t' | ' ' if state == SeenLf => Awaiting,
                _ if state == SeenLf => {
                    let header = from_str(s.slice_chars(start, pos));
                    if header.is_none() {
                        return None
                    }
                    header_map.insert(header.unwrap());
                    // Move the next slice on
                    start = pos;
                    Awaiting
                },
                _ => Awaiting
            };
            pos = c.next;
        }

        // Handle the final header
        let final_header = from_str(s.slice_from(start));
        if final_header.is_none() {
            None
        } else {
            header_map.insert(final_header.unwrap());
            Some(header_map)
        }
    }
}

impl Collection for HeaderMap {
    fn len(&self) -> uint {
        self.iter().count()
    }
}

impl Map<String, Vec<Header>> for HeaderMap {
    fn find(&self, key: &String) -> Option<&Vec<Header>> {
        self.headers.find(key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    static SAMPLE_HEADERS: [&'static str, ..4] = [
        "Test: Value",
        "Test : Value 2",
        "Test-2: Value 3",
        "Test-Multiline: Foo\nBar",
    ];

    #[test]
    fn test_header_to_string() {
        let header = Header::new("Test".to_string(), "Value".to_string());
        assert_eq!(header.to_string(), "Test: Value".to_string());
    }

    #[test]
    fn test_multiline_header_to_string() {
        let header = Header::new("Test".to_string(), "Value\nOver lines".to_string());
        assert_eq!(header.to_string(), "Test: Value\r\n\tOver lines".to_string());
    }

    #[test]
    fn test_string_get_value() {
        let header = Header::new("Test".to_string(), "Value".to_string());
        let string_value: String = header.get_value().unwrap();
        assert_eq!(string_value, "Value".to_string());
    }

    #[test]
    fn test_parse_header() {
        let vals = ["Test: Value", "Test : Value"];
        for &val in vals.iter() {
            let header: Header = from_str(val).unwrap();
            assert_eq!(header.name, "Test".to_string());
            assert_eq!(header.value, "Value".to_string());
        }
    }

    #[test]
    fn test_parse_header_roundtrip() {
        let header: Header = from_str("Test: Value").unwrap();
        let header_string = header.to_string();
        assert_eq!(header_string, "Test: Value".to_string());
    }

    #[test]
    fn test_header_map_len() {
        let mut headers = HeaderMap::new();
        for (i, &val) in SAMPLE_HEADERS.iter().enumerate() {
            let header: Header = from_str(val).unwrap();
            headers.insert(header);
            assert_eq!(headers.len(), i + 1);
        }
    }

    #[test]
    fn test_header_map_iter() {
        let mut headers = HeaderMap::new();
        let mut expected_headers = HashSet::new();
        for &val in SAMPLE_HEADERS.iter() {
            let header: Header = from_str(val).unwrap();
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

    #[test]
    fn test_header_map_string() {
        let mut headers = HeaderMap::new();
        for &val in SAMPLE_HEADERS.iter() {
            let header: Header = from_str(val).unwrap();
            headers.insert(header.clone());
        }
        let result = headers.to_string();
        let slice = result.as_slice();
        assert!(slice.contains("Test: Value\r\n"));
        assert!(slice.contains("Test: Value 2\r\n"));
        assert!(slice.contains("Test-2: Value 3\r\n"));
        assert!(slice.contains("Test-Multiline: Foo\r\n\tBar"));
    }

    #[test]
    fn test_header_map_parse() {
        let headers = from_str::<HeaderMap>("Test: Value\r\nTest: Value 2\r\nTest-2: Value 3\r\nTest-Multiline: Foo\r\n\tBar\r\n").unwrap();
        println!("{}", headers);
        assert_eq!(headers.len(), 4);
        assert_eq!(
            headers.find(&"Test".to_string()).unwrap(), &vec![
                Header::new("Test".to_string(), "Value".to_string()),
                Header::new("Test".to_string(), "Value 2".to_string()),
        ]);
        assert_eq!(
            headers.find(&"Test-2".to_string()).unwrap(), &vec![
                Header::new("Test-2".to_string(), "Value 3".to_string()),
        ]);
        assert_eq!(
            headers.find(&"Test-Multiline".to_string()).unwrap(), &vec![
                Header::new("Test-Multiline".to_string(), "Foo\r\n\tBar".to_string()),
        ]);
    }
}
