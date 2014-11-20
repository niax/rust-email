use std::fmt;
use std::slice::Items;

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

impl FromHeader for String {
    fn from_header(value: String) -> Option<String> {
        Some(value)
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
    /// as converted through the `ToHeader` trait.
    ///
    /// Returns None if the value failed to be converted.
    pub fn new_with_value<T: ToHeader>(name: String, value: T) -> Option<Header> {
        ToHeader::to_header(value).map(|val| { Header::new(name.clone(), val) })
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
        let header = Header::new("Test".to_string(), "Value".to_string());
        let string_value: String = header.get_value().unwrap();
        assert_eq!(string_value, "Value".to_string());
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
