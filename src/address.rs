use std::fmt;
use std::str::FromStr;

use super::header::{FromHeader, ToFoldedHeader};
use super::results::{ParsingError, ParsingResult};
use super::rfc5322::{Rfc5322Parser, MIME_LINE_LENGTH};

/// Represents an RFC 5322 Address
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Address {
    /// A "regular" email address
    Mailbox(Mailbox),
    /// A named group of mailboxes
    Group(String, Vec<Mailbox>),
}

impl Address {
    /// Shortcut function to make a new Mailbox with the given address
    /// [unstable]
    pub fn new_mailbox(address: String) -> Address {
        Address::Mailbox(Mailbox::new(address))
    }

    /// Shortcut function to make a new Mailbox with the address and given-name
    /// [unstable]
    pub fn new_mailbox_with_name(name: String, address: String) -> Address {
        Address::Mailbox(Mailbox::new_with_name(name, address))
    }

    /// Shortcut function to make a new Group with a collection of mailboxes
    /// [unstable]
    pub fn new_group(name: String, mailboxes: Vec<Mailbox>) -> Address {
        Address::Group(name, mailboxes)
    }
}

impl fmt::Display for Address {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Address::Mailbox(ref mbox) => mbox.fmt(fmt),
            Address::Group(ref name, ref mboxes) => {
                let mut mailbox_list = String::new();
                for mbox in mboxes.iter() {
                    if !mailbox_list.is_empty() {
                        // Insert the separator if there's already things in this list
                        mailbox_list.push_str(", ");
                    }
                    mailbox_list.push_str(&mbox.to_string()[..]);
                }
                write!(fmt, "{}: {};", name, mailbox_list)
            }
        }
    }
}

/// Represents an RFC 5322 mailbox
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Mailbox {
    /// The given name for this address
    pub name: Option<String>,
    /// The mailbox address
    pub address: String,
}

impl Mailbox {
    /// Create a new Mailbox without a display name
    pub fn new(address: String) -> Mailbox {
        Mailbox {
            name: None,
            address,
        }
    }

    /// Create a new Mailbox with a display name
    pub fn new_with_name(name: String, address: String) -> Mailbox {
        Mailbox {
            name: Some(name),
            address,
        }
    }
}

impl fmt::Display for Mailbox {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.name {
            Some(ref name) => write!(fmt, "\"{}\" <{}>", name, self.address),
            None => write!(fmt, "<{}>", self.address),
        }
    }
}

impl<'a> From<&'a str> for Mailbox {
    fn from(mailbox: &'a str) -> Mailbox {
        Mailbox::new(mailbox.into())
    }
}

impl From<String> for Mailbox {
    fn from(mailbox: String) -> Mailbox {
        Mailbox::new(mailbox)
    }
}

impl<S: Into<String>, T: Into<String>> From<(S, T)> for Mailbox {
    fn from(header: (S, T)) -> Mailbox {
        let (address, alias) = header;
        Mailbox::new_with_name(alias.into(), address.into())
    }
}

impl FromStr for Mailbox {
    type Err = ParsingError;

    fn from_str(s: &str) -> ParsingResult<Mailbox> {
        AddressParser::new(s).parse_mailbox()
    }
}

impl FromHeader for Vec<Address> {
    fn from_header(value: String) -> ParsingResult<Vec<Address>> {
        AddressParser::new(&value[..]).parse_address_list()
    }
}

impl ToFoldedHeader for Vec<Address> {
    fn to_folded_header(start_pos: usize, value: Vec<Address>) -> ParsingResult<String> {
        let mut header = String::new();

        let mut line_len = start_pos;

        for addr in value.iter() {
            let addr_str = format!("{}, ", addr);

            if line_len + addr_str.len() > MIME_LINE_LENGTH {
                // Adding this would cause a wrap, so wrap before!
                header.push_str("\r\n\t");
                line_len = 0;
            }
            line_len += addr_str.len();
            header.push_str(&addr_str[..]);
        }

        // Clear up the final ", "
        let real_len = header.len() - 2;
        header.truncate(real_len);

        Ok(header)
    }
}

pub struct AddressParser<'s> {
    p: Rfc5322Parser<'s>,
}

impl<'s> AddressParser<'s> {
    pub fn new(s: &str) -> AddressParser {
        AddressParser {
            p: Rfc5322Parser::new(s),
        }
    }

    pub fn parse_address_list(&mut self) -> ParsingResult<Vec<Address>> {
        let mut result = Vec::new();
        let mut expected_separator: char;

        while !self.p.eof() {
            self.p.push_position();

            match self.parse_group() {
                Ok(x) => {
                    // Is a group
                    result.push(x);
                    expected_separator = ';';
                }
                Err(e) => {
                    // If we failed to parse as group, try again as mailbox
                    self.p.pop_position();
                    result.push(Address::Mailbox(match self.parse_mailbox() {
                        Ok(x) => x,
                        Err(e2) => {
                            return Err(ParsingError::new(format!(
                                "Failed to parse as group: {}\n\
                                 Failed to parse as mailbox: {}",
                                e, e2
                            )))
                        }
                    }));
                    expected_separator = ',';
                }
            };

            self.p.consume_linear_whitespace();
            if !self.p.eof() && self.p.peek() == expected_separator {
                // Clear the separator
                self.p.consume_char();
            }
        }

        Ok(result)
    }

    pub fn parse_group(&mut self) -> ParsingResult<Address> {
        let name = match self.p.consume_phrase(false) {
            Some(x) => x,
            None => {
                return Err(ParsingError::new(format!(
                    "Couldn't find group name: {}",
                    self.p.peek_to_end()
                )))
            }
        };

        self.p.assert_char(':')?;
        self.p.consume_char();

        let mut mailboxes = Vec::new();

        while !self.p.eof() && self.p.peek() != ';' {
            mailboxes.push(self.parse_mailbox()?);

            if !self.p.eof() && self.p.peek() == ',' {
                self.p.consume_char();
            }
        }

        Ok(Address::Group(name, mailboxes))
    }

    pub fn parse_mailbox(&mut self) -> ParsingResult<Mailbox> {
        // Push the current position of the parser so we can back out later
        self.p.push_position();
        match self.parse_name_addr() {
            Ok(result) => Ok(result),
            Err(_) => {
                // Revert back to our original position to try to parse an addr-spec
                self.p.pop_position();
                Ok(Mailbox::new(self.parse_addr_spec()?))
            }
        }
    }

    fn parse_name_addr(&mut self) -> ParsingResult<Mailbox> {
        // Find display-name
        let display_name = self.p.consume_phrase(false);
        self.p.consume_linear_whitespace();

        self.p.assert_char('<')?;
        self.p.consume_char();

        let addr = self.parse_addr_spec()?;
        if self.p.consume_char() != Some('>') {
            // Fail because we should have a closing RANGLE here (to match the opening one)
            Err(ParsingError::new(
                "Missing '>' at end while parsing address header.".to_string(),
            ))
        } else {
            Ok(match display_name {
                Some(name) => Mailbox::new_with_name(name, addr),
                None => Mailbox::new(addr),
            })
        }
    }

    fn parse_addr_spec(&mut self) -> ParsingResult<String> {
        // local-part is a phrase, but allows dots in atoms
        let local_part = match self.p.consume_phrase(true) {
            Some(x) => x,
            None => {
                return Err(ParsingError::new(
                    "Couldn't find local part while parsing address.".to_owned(),
                ))
            }
        };

        self.p.assert_char('@')?;
        self.p.consume_char();

        let domain = self.parse_domain()?;
        Ok(format!("{}@{}", local_part, domain))
    }

    fn parse_domain(&mut self) -> ParsingResult<String> {
        // TODO: support domain-literal
        match self.p.consume_atom(true) {
            Some(x) => Ok(x),
            None => Err(ParsingError::new("Failed to parse domain.".to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::header::Header;

    #[test]
    fn test_address_to_string() {
        let addr = Mailbox::new("foo@example.org".to_string());
        assert_eq!(addr.to_string(), "<foo@example.org>".to_string());

        let name_addr =
            Mailbox::new_with_name("Joe Blogs".to_string(), "foo@example.org".to_string());
        assert_eq!(
            name_addr.to_string(),
            "\"Joe Blogs\" <foo@example.org>".to_string()
        );
    }

    #[test]
    fn test_address_from_string() {
        let addr = "\"Joe Blogs\" <joe@example.org>"
            .parse::<Mailbox>()
            .unwrap();
        assert_eq!(addr.name.unwrap(), "Joe Blogs".to_string());
        assert_eq!(addr.address, "joe@example.org".to_string());

        assert!("Not an address".parse::<Mailbox>().is_err());
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

    #[test]
    fn test_invalid_address_parsing() {
        for a in vec!["<a@example.com", "a@"].iter() {
            AddressParser::new(a)
                .parse_mailbox()
                .expect_err("parse failure");
        }
    }

    #[test]
    fn test_address_group_to_string() {
        let addr = Address::new_group("undisclosed recipients".to_string(), vec![]);
        assert_eq!(addr.to_string(), "undisclosed recipients: ;".to_string());

        let addr = Address::new_group(
            "group test".to_string(),
            vec![
                Mailbox::new("joe@example.org".to_string()),
                Mailbox::new_with_name("John Doe".to_string(), "john@example.org".to_string()),
            ],
        );
        assert_eq!(
            addr.to_string(),
            "group test: <joe@example.org>, \"John Doe\" <john@example.org>;".to_string()
        );
    }

    #[test]
    fn test_address_group_parsing() {
        let mut parser =
            AddressParser::new("A Group:\"Joe Blogs\" <joe@example.org>,john@example.org;");
        let addr = parser.parse_group().unwrap();
        match addr {
            Address::Group(name, mboxes) => {
                assert_eq!(name, "A Group".to_string());
                assert_eq!(
                    mboxes,
                    vec![
                        Mailbox::new_with_name(
                            "Joe Blogs".to_string(),
                            "joe@example.org".to_string()
                        ),
                        Mailbox::new("john@example.org".to_string()),
                    ]
                );
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_address_list_parsing() {
        let mut parser =
            AddressParser::new("\"Joe Blogs\" <joe@example.org>, \"John Doe\" <john@example.org>");
        assert_eq!(
            parser.parse_address_list().unwrap(),
            vec![
                Address::new_mailbox_with_name(
                    "Joe Blogs".to_string(),
                    "joe@example.org".to_string()
                ),
                Address::new_mailbox_with_name(
                    "John Doe".to_string(),
                    "john@example.org".to_string()
                ),
            ]
        );
    }

    #[test]
    fn test_address_list_parsing_groups() {
        let mut parser = AddressParser::new("A Group:\"Joe Blogs\" <joe@example.org>, \"John Doe\" <john@example.org>; <third@example.org>, <fourth@example.org>");
        assert_eq!(
            parser.parse_address_list().unwrap(),
            vec![
                Address::new_group(
                    "A Group".to_string(),
                    vec![
                        Mailbox::new_with_name(
                            "Joe Blogs".to_string(),
                            "joe@example.org".to_string()
                        ),
                        Mailbox::new_with_name(
                            "John Doe".to_string(),
                            "john@example.org".to_string()
                        ),
                    ]
                ),
                Address::new_mailbox("third@example.org".to_string()),
                Address::new_mailbox("fourth@example.org".to_string()),
            ]
        );
    }

    #[test]
    fn test_from_header_parsing() {
        let header = Header::new(
            "From:".to_string(),
            "\"Joe Blogs\" <joe@example.org>, \"John Doe\" <john@example.org>".to_string(),
        );
        let addresses: Vec<Address> = header.get_value().unwrap();
        assert_eq!(
            addresses,
            vec![
                Address::new_mailbox_with_name(
                    "Joe Blogs".to_string(),
                    "joe@example.org".to_string()
                ),
                Address::new_mailbox_with_name(
                    "John Doe".to_string(),
                    "john@example.org".to_string()
                ),
            ]
        );
    }

    #[test]
    fn test_to_header_generation() {
        let addresses = vec![
            Address::new_mailbox_with_name("Joe Blogs".to_string(), "joe@example.org".to_string()),
            Address::new_mailbox_with_name("John Doe".to_string(), "john@example.org".to_string()),
        ];

        let header = Header::new_with_value("From:".to_string(), addresses).unwrap();
        assert_eq!(
            header.get_value::<String>().unwrap(),
            "\"Joe Blogs\" <joe@example.org>, \"John Doe\" <john@example.org>".to_string()
        );
    }

    #[test]
    fn test_to_header_line_wrap() {
        let addresses = vec![
            Address::new_mailbox_with_name("Joe Blogs".to_string(), "joe@example.org".to_string()),
            Address::new_mailbox_with_name("John Doe".to_string(), "john@example.org".to_string()),
            Address::new_mailbox_with_name(
                "Mr Black".to_string(),
                "mafia_black@example.org".to_string(),
            ),
        ];

        let header = Header::new_with_value("To".to_string(), addresses).unwrap();
        assert_eq!(
            &header.to_string()[..],
            "To: \"Joe Blogs\" <joe@example.org>, \"John Doe\" <john@example.org>, \r\n\
             \t\"Mr Black\" <mafia_black@example.org>"
        );
    }
}
