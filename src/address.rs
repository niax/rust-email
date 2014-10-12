use std::fmt;

/// Represents an RFC 822 address
pub struct Address<'s> {
    /// The given name for this address
    pub name: Option<&'s str>,
    /// The mailbox address
    pub address: &'s str,
}

impl<'s> Address<'s> {
    /// Create a new Address without a name
    pub fn new(address: &str) -> Address {
        Address {
            name: None,
            address: address
        }
    }

    /// Create a new Address with a name
    pub fn new_with_name(name: &'s str, address: &'s str) -> Address<'s> {
        Address {
            name: Some(name),
            address: address
        }
    }
}

impl<'s> fmt::Show for Address<'s> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.name {
            Some(ref name) => write!(fmt, "\"{}\" <{}>", name, self.address),
            None => write!(fmt, "<{}>", self.address),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_address_to_string() {
        let addr = Address::new("foo@example.org");
        assert_eq!(addr.to_string(), "<foo@example.org>".to_string());

        let name_addr = Address::new_with_name("Joe Blogs", "foo@example.org");
        assert_eq!(name_addr.to_string(), "\"Joe Blogs\" <foo@example.org>".to_string());
    }
}
