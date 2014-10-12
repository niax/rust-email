use std::fmt;
use std::from_str::FromStr;

use super::rfc5322::Rfc5322Parser;

/// Represents an RFC 822 address
pub struct Address {
    /// The given name for this address
    pub name: Option<String>,
    /// The mailbox address
    pub address: String,
}

impl Address {
    /// Create a new Address without a name
    pub fn new(address: String) -> Address {
        Address {
            name: None,
            address: address
        }
    }

    /// Create a new Address with a name
    pub fn new_with_name(name: String, address: String) -> Address {
        Address {
            name: Some(name),
            address: address
        }
    }
}

impl fmt::Show for Address {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.name {
            Some(ref name) => write!(fmt, "\"{}\" <{}>", name, self.address),
            None => write!(fmt, "<{}>", self.address),
        }
    }
}

impl FromStr for Address {
    fn from_str(s: &str) -> Option<Address> {
        AddressParser::new(s).parse_mailbox()
    }
}

pub struct AddressParser<'s> {
    p: Rfc5322Parser<'s>,
}

impl<'s> AddressParser<'s> {
    pub fn new(s: &str) -> AddressParser {
        AddressParser {
            p: Rfc5322Parser::new(s)
        }
    }

    pub fn parse_mailbox(&mut self) -> Option<Address> {
        // Push the current position of the parser so we can back out later
        self.p.push_position();
        let mut result = self.parse_name_addr();
        if result.is_none() {
            // Revert back to our original position to try to parse an addr-spec
            self.p.pop_position();

            let addr_spec = self.parse_addr_spec();
            if addr_spec.is_some() {
                result = Some(Address::new(addr_spec.unwrap()));
            }
        }

        result
    }

    fn parse_name_addr(&mut self) -> Option<Address> {
        // Find display-name
        let display_name = self.p.consume_phrase(false);
        self.p.consume_linear_whitespace();
        // Find angle-addr
        if !self.p.eof() && self.p.peek() == '<' {
            self.p.consume_char();
            let addr = self.parse_addr_spec();
            if self.p.consume_char() != '>' {
                // Fail because we should have a closing RANGLE here (to match the opening one)
                None
            } else {
                match (display_name, addr) {
                    (Some(name), Some(addr)) => Some(Address::new_with_name(name, addr)),
                    (None, Some(addr)) => Some(Address::new(addr)),
                    (_, _) => None,
                }
            }
        } else {
            None
        }
    }

    fn parse_addr_spec(&mut self) -> Option<String> {
        // local-part is a phrase, but allows dots in atoms
        let local_part = self.p.consume_phrase(true);
        if self.p.eof() || self.p.consume_char() != '@' {
            None
        } else {
            let domain = self.parse_domain();
            if local_part.is_some() && domain.is_some() {
                Some(format!("{}@{}", local_part.unwrap(), domain.unwrap()))
            } else {
                None
            }
        }
    }

    fn parse_domain(&mut self) -> Option<String> {
        // TODO: support domain-literal
        self.p.consume_atom(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_address_to_string() {
        let addr = Address::new("foo@example.org".to_string());
        assert_eq!(addr.to_string(), "<foo@example.org>".to_string());

        let name_addr = Address::new_with_name("Joe Blogs".to_string(), "foo@example.org".to_string());
        assert_eq!(name_addr.to_string(), "\"Joe Blogs\" <foo@example.org>".to_string());
    }

    #[test]
    fn test_address_from_string() {
        let addr = from_str::<Address>("\"Joe Blogs\" <joe@example.org>").unwrap();
        assert_eq!(addr.name.unwrap(), "Joe Blogs".to_string());
        assert_eq!(addr.address, "joe@example.org".to_string());

        assert!(from_str::<Address>("Not an address").is_none());
    }

    #[test]
    fn test_address_parsing() {
        let mut parser = AddressParser::new("\"Joe Blogs\" <joe@example.org>");
        let mut addr = parser.parse_mailbox().unwrap();
        assert_eq!(addr.name.unwrap(), "Joe Blogs".to_string());
        assert_eq!(addr.address, "joe@example.org".to_string());

        parser = AddressParser::new("joe@example.org");
        addr = parser.parse_mailbox().unwrap();
        assert_eq!(addr.name, None);
        assert_eq!(addr.address, "joe@example.org".to_string());
    }
}
