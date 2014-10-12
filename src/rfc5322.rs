//! Module with helpers for dealing with RFC 5322

trait Rfc5322Character {
    /// Is considered a special character by RFC 5322 Section 3.2.3
    fn is_special(&self) -> bool;
    fn is_printable(&self) -> bool;

    fn is_atext(&self) -> bool {
        self.is_printable() && !self.is_special()
    }
}

impl Rfc5322Character for char {
    fn is_special(&self) -> bool {
        match *self {
            '(' | ')' | '<' | '>' | '[' | ']' | ':' | ';' | '@' | '\\' | ',' | '.' | '\"' => true,
            _ => false
        }
    }

    fn is_printable(&self) -> bool {
        match *self {
            ' '...'~' => true,
            _ => false,
        }
    }
}

/// RFC 5322 base parser for parsing
///  `atom`, `dot-atom`, `quoted-string`, `phrase`
///
/// This should prove useful for parsing other things that appear in RFC 5322,
/// as most are based off these core items.
///
/// It also implements a stack for tracking the position.
/// This allows the simple implementation of backtracking, by pushing the position
/// before a test and popping it if the test should fail.
pub struct Rfc5322Parser<'s> {
    s: &'s str,
    pos: uint,
    pos_stack: Vec<uint>,
}

impl<'s> Rfc5322Parser<'s> {
    /// Make a new parser, initialized with the given string. 
    pub fn new(source: &'s str) -> Rfc5322Parser<'s> {
        Rfc5322Parser {
            s: source,
            pos: 0u,
            pos_stack: Vec::new(),
        }
    }

    /// Push the current position onto the stack.
    pub fn push_position(&mut self) {
        self.pos_stack.push(self.pos);
    }

    /// Move the position back to the last entry pushed
    pub fn pop_position(&mut self) {
        match self.pos_stack.pop() {
            Some(pos) => { self.pos = pos; },
            None => fail!("Popped position stack too far"),
        }
    }

    /// Consume a phrase from the input.
    ///
    /// A phrase is defined as:
    ///
    /// `word = atom / quoted-string
    /// phrase = 1*word`
    ///
    /// If `allow_dot_atom` is true, then `atom` can be a `dot-atom` in this phrase.
    pub fn consume_phrase(&mut self, allow_dot_atom: bool) -> Option<String> {
        let mut phrase = String::new();

        while !self.eof() {
            self.consume_linear_whitespace();
            let word = if self.peek() == '"' {
                // Word is a quoted string
                self.consume_quoted_string()
            } else if self.peek().is_atext() {
                self.consume_atom(allow_dot_atom)
            } else {
                // If it's not a quoted string, or an atom, it's no longer
                // in a phrase, so stop.
                break
            };

            if word.is_some() {
                phrase.push_str(word.unwrap().as_slice());
            } else {
                return None
            }
        }

        Some(phrase)
    }

    /// Consume a quoted string from the input
    pub fn consume_quoted_string(&mut self) -> Option<String> {
        if self.peek() != '"' {
            // Fail if we were called wrong
            None
        } else {
            let mut quoted_string = String::new();
            let mut inside_escape = false;
            let mut terminated = false;
            // Consume the leading "
            self.consume_char();
            while !terminated && !self.eof() {
                match self.peek() {
                    '\\' if !inside_escape => {
                        // If we were not already being escaped, consume the
                        // escape character and mark that we're being escaped.
                        self.consume_char();
                        inside_escape = true;
                    },
                    '"' if !inside_escape => {
                        // If this is a DQUOTE and we haven't seen an escape character,
                        // consume it and mark that we should break from the loop
                        self.consume_char();
                        terminated = true;
                    },
                    _ => {
                        // Any old character gets pushed in
                        quoted_string.push(self.consume_char());
                        // Clear any escape character state we have
                        inside_escape = false;
                    },
                }
            }

            if inside_escape || !terminated {
                // Return an error state if we're still expecting a character
                None
            } else {
                Some(quoted_string)
            }
        }
    }

    /// Consume an atom from the input.
    ///
    /// If `allow_dot` is true, then also allow '.' to be considered as an
    /// atext character.
    pub fn consume_atom(&mut self, allow_dot: bool) -> Option<String> {
        if !self.peek().is_atext() {
            None
        } else {
            Some(self.consume_while(|c| {
                c.is_atext() || (allow_dot && c == '.')
            }))
        }
    }

    /// Consume LWSP (Linear whitespace)
    pub fn consume_linear_whitespace(&mut self) {
        self.consume_while(|c| { c == '\t' || c == ' ' });
    }

    /// Consume a single character from the input.
    pub fn consume_char(&mut self) -> char {
        if self.eof() { 
            fail!("Consuming beyond end of input");
        }
        let ch_range = self.s.char_range_at(self.pos);
        self.pos = ch_range.next;
        ch_range.ch
    }

    /// Consume a set of characters, each passed to `test` until this function
    /// returns false.
    ///
    /// The position after calling this function will be pointing to the character
    /// which caused a false result from `test`.
    ///
    /// Returns the string of characters that returned true for the test function.
    pub fn consume_while(&mut self, test: |char| -> bool) -> String {
        let mut res = String::new();
        while !self.eof() && test(self.peek()) {
            res.push(self.consume_char())
        }
        res
    }

    /// Peek at the current character.
    ///
    /// Note that this does not do any bounds checking.
    pub fn peek(&self) -> char {
        self.s.char_at(self.pos)
    }

    /// Returns true if we have reached the end of the input.
    #[inline]
    pub fn eof(&self) -> bool {
        self.pos >= self.s.len()
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_qstring() {
        let mut p = Rfc5322Parser::new("\"test phrase\"");
        assert_eq!(p.consume_phrase(false).unwrap(), "test phrase".to_string());

    }

    #[test]
    fn test_escaped_qstring() {
        let mut p = Rfc5322Parser::new("\"test \\\"phrase\\\"\"");
        assert_eq!(p.consume_phrase(false).unwrap(), "test \"phrase\"".to_string());
    }

    #[test]
    fn test_multiple_words() {
        let mut p = Rfc5322Parser::new("atom words with \"quoted string words too\"");
        assert_eq!(p.consume_phrase(false).unwrap(), "atom words with quoted string words too".to_string());
    }

    #[test]
    fn test_sample_address() {
        let mut p = Rfc5322Parser::new("\"John Smith\" <test@example.org>");
        assert_eq!(p.consume_phrase(false).unwrap(), "John Smith".to_string());
    }
}
