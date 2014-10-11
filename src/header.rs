use std::fmt;
use std::from_str::FromStr;
use std::collections::HashMap;
use std::collections::hashmap::{Occupied, Vacant};
use std::slice::Items;

pub trait FromHeader {
    fn from_header(value: String) -> Self;
}

impl FromHeader for String {
    fn from_header(value: String) -> String {
        value
    }
}

#[deriving(PartialEq, Eq, Clone, Hash)]
pub struct Header {
    name: String,
    value: String,
}

impl Header {
    pub fn new(name: String, value: String) -> Header {
        Header {
            name: name,
            value: value,
        }
    }

    pub fn get_value<T: FromHeader>(&self) -> T {
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

pub struct MultiIter<'a, V, T: Iterator<V>> {
    iters: Vec<T>,
    current_iter: uint,
}

impl<'a, V, T: Iterator<V>> MultiIter<'a, V, T> {
    pub fn new(iters: Vec<T>) -> MultiIter<'a, V, T> {
        MultiIter {
            iters: iters,
            current_iter: 0u,
        }
    }
}

impl<'a, V, T: Iterator<V>> Iterator<V> for MultiIter<'a, V, T> {
    fn next(&mut self) -> Option<V> {
        loop {
            let iter_len = self.iters.len();
            let cur_iter = self.iters.get_mut(self.current_iter);
            let next = cur_iter.next();
            if next.is_some() {
                return next
            } else {
                // Go to the next iter and continue
                self.current_iter += 1;
                if self.current_iter >= iter_len {
                    // Make sure that we're not going out of bounds...
                    return None;
                }
            }
        }
    }
}

pub struct HeaderMap {
    headers: HashMap<String, Vec<Header>>,
}

impl HeaderMap {
    pub fn new() -> HeaderMap {
        HeaderMap {
            headers: HashMap::new()
        }
    }

    pub fn append_header(&mut self, header: Header) {
        // If the header hashmap already has this header, use that list.
        // Otherwise, make a new one.
        let header_list = match self.headers.entry(header.name.clone()) {
            Vacant(entry) => entry.set(Vec::new()),
            Occupied(entry) => entry.into_mut(),
        };
        // ... and add the new header to it
        header_list.push(header);
    }

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
        write!(fmt, "{}", self.headers)
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
        let string_value: String = header.get_value();
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
        let vals = ["Test: Value", "Test : Value 2", "Test-2: Value 3"];
        for (i, &val) in vals.iter().enumerate() {
            let header: Header = from_str(val).unwrap();
            headers.append_header(header);
            println!("{}", headers);
            assert_eq!(headers.len(), i + 1);
        }
    }

    #[test]
    fn test_header_map_iter() {
        let mut headers = HeaderMap::new();
        let mut expected_headers = HashSet::new();
        let vals = ["Test: Value", "Test : Value 2", "Test-2: Value 3"];
        for &val in vals.iter() {
            let header: Header = from_str(val).unwrap();
            headers.append_header(header.clone());
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
