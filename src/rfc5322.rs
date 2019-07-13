//! Module with helpers for dealing with RFC 5322

use super::header::{Header, HeaderMap};
use super::rfc2047::decode_rfc2047;
use super::results::{ParsingError, ParsingResult};

pub const MIME_LINE_LENGTH: usize = 78;

trait Rfc5322Character {
    /// Is considered a special character by RFC 5322 Section 3.2.3
    fn is_special(&self) -> bool;
    /// Is considered to be a VCHAR by RFC 5234 Appendix B.1
    fn is_vchar(&self) -> bool;
    /// Is considered to be field text as defined by RFC 5322 Section 3.6.8
    fn is_ftext(&self) -> bool;


    fn is_atext(&self) -> bool {
        self.is_vchar() && !self.is_special()
    }
}

impl Rfc5322Character for char {
    fn is_ftext(&self) -> bool {
        match *self {
            '!'...'9' | ';'...'~' => true,
            _ => false,
        }
    }

    fn is_special(&self) -> bool {
        match *self {
            '(' | ')' | '<' | '>' | '[' | ']' | ':' | ';' | '@' | '\\' | ',' | '.' | '\"' | ' ' => true,
            _ => false
        }
    }

    fn is_vchar(&self) -> bool {
        match *self {
            '!'...'~' => true,
            _ => false,
        }
    }
}

/// RFC 5322 base parser for parsing
///  `atom`, `dot-atom`, `quoted-string`, `phrase`, `message`
///
/// This should prove useful for parsing other things that appear in RFC 5322,
/// as most are based off these core items.
///
/// It also implements a stack for tracking the position.
/// This allows the simple implementation of backtracking, by pushing the position
/// before a test and popping it if the test should fail.
/// [unstable]
pub struct Rfc5322Parser<'s> {
    s: &'s str,
    pos: usize,
    pos_stack: Vec<usize>,
}

impl<'s> Rfc5322Parser<'s> {
    /// Make a new parser, initialized with the given string.
    /// [unstable]
    pub fn new(source: &'s str) -> Rfc5322Parser<'s> {
        Rfc5322Parser {
            s: source,
            pos: 0,
            pos_stack: Vec::new(),
        }
    }

    /// Push the current position onto the stack.
    /// [unstable]
    pub fn push_position(&mut self) {
        self.pos_stack.push(self.pos);
    }

    /// Move the position back to the last entry pushed
    /// [unstable]
    pub fn pop_position(&mut self) {
        match self.pos_stack.pop() {
            Some(pos) => { self.pos = pos; },
            None => panic!("Popped position stack too far"),
        }
    }

    /// Consume a message from the input.
    ///
    /// Returns as a map of the headers and the body text.
    ///
    /// A message is defined as:
    ///
    /// `fields = *field
    /// body = text
    /// message = fields CRLF body`
    /// [unstable]
    pub fn consume_message(&mut self) -> Option<(HeaderMap, String)> {
        let mut headers = HeaderMap::new();
        while !self.eof() {
            let header = self.consume_header();
            if let Some(header) = header {
                headers.insert(header);
            } else {
                // Check end of headers as marked by CRLF
                if !self.eof() && self.peek_linebreak() {
                    assert!(self.consume_linebreak());
                }

                break;
            }

        }


        // Whatever remains is the body
        let body = self.s[self.pos..].to_string();
        self.pos = self.s.len();

        Some((headers, body))
    }

    /// Consume a header from the input.
    ///
    /// A header is defined as:
    ///
    /// `ftext = "!".."9" / ";".."~"
    /// field-name = 1*ftext
    /// field = field-name *LWSP ":" unstructured`
    /// [unstable]
    pub fn consume_header(&mut self) -> Option<Header> {
        let last_pos = self.pos;
        // Parse field-name
        let field_name = self.consume_while(|c| { c.is_ftext() });
        self.consume_linear_whitespace();
        if field_name.len() == 0 || self.eof() || self.peek() != ':' {
            // Fail to parse if we didn't see a field, we're at the end of input
            // or we haven't just seen a ":"
            self.pos = last_pos;
            None
        } else {
            // Consume the ":" and any leading whitespace
            self.consume_char();
            self.consume_linear_whitespace();
            let field_value = self.consume_unstructured();

            // don't just panic!()
            if self.consume_linebreak() == false { return None };

            Some(Header::new(field_name, field_value))
        }
    }

    /// Consume an unstructured from the input.
    /// [unstable]
    pub fn consume_unstructured(&mut self) -> String {
        let mut result = String::new();
        while !self.eof() {
            if self.peek_linebreak() {
                // Check for folding whitespace, if it wasn't, then
                // we're done parsing
                if !self.consume_folding_whitespace() {
                    break;
                }
            }

            result.push_str(&self.consume_while(|c| {
                c.is_vchar() || c == ' ' || c == '\t'
            })[..])
        }
        result
    }

    /// Consume folding whitespace.
    ///
    /// This is a CRLF followed by one or more whitespace character.
    ///
    /// Returns true if whitespace was consume
    /// [unstable]
    pub fn consume_folding_whitespace(&mut self) -> bool {
        // Remember where we were, in case this isn't folding whitespace
        let current_position = self.pos;
        let is_fws = if !self.eof() && self.consume_linebreak() {
            match self.consume_char() {
                Some(' ') | Some('\t') => true,
                _ => false,
            }
        } else {
            false
        };

        if is_fws {
            // This was a folding whitespace, so consume all linear whitespace
            self.consume_linear_whitespace();
        } else {
            // Reset back if we didn't see a folding whitespace
            self.pos = current_position;
        }

        is_fws
    }

    /// Consume a word from the input.
    ///
    /// A word is defined as:
    ///
    /// `word = atom / quoted-string`
    ///
    /// If `allow_dot_atom` is true, then `atom` can be a `dot-atom` in this phrase.
    /// [unstable]
    pub fn consume_word(&mut self, allow_dot_atom: bool) -> Option<String> {
        let p = self.peek();
        if p == '"' {
            // Word is a quoted string
            self.consume_quoted_string()
        } else {
            // Word is an atom (or not a word)
            self.consume_atom(allow_dot_atom)
        }
    }

    /// Consume a phrase from the input.
    ///
    /// A phrase is defined as:
    ///
    /// `phrase = 1*word`
    ///
    /// If `allow_dot_atom` is true, then `atom` can be a `dot-atom` in this phrase.
    /// [unstable]
    pub fn consume_phrase(&mut self, allow_dot_atom: bool) -> Option<String> {
        let mut phrase = String::new();

        while !self.eof() {
            self.consume_linear_whitespace();

            let word = match self.consume_word(allow_dot_atom) {
                Some(x) => x,
                None => break // If it's not a word, it's no longer
                              // in a phrase, so stop.
            };

            let w_slice = &word[..];
            // RFC 2047 encoded words start with =?, end with ?=
            let decoded_word =
                if w_slice.starts_with("=?") && w_slice.ends_with("?=") {
                    match decode_rfc2047(w_slice) {
                        Some(w) => w,
                        None => w_slice.to_string(),
                    }
                } else {
                    w_slice.to_string()
                };

            // Make sure we put a leading space on, if this isn't the first insertion
            if phrase.len() > 0 {
                phrase.push_str(" ");
            }
            phrase.push_str(&decoded_word[..]);
        }

        if phrase.len() > 0 {
            Some(phrase)
        } else {
            None
        }
    }

    /// Consume a quoted string from the input
    /// [unstable]
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
                        if let Some(c) = self.consume_char() {
                            quoted_string.push(c);
                            // Clear any escape character state we have
                            inside_escape = false;
                        }
                        // TODO: Should this return a Result<> instead of an Option<>?
                        else {
                            return None;
                        }
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
    /// [unstable]
    pub fn consume_atom(&mut self, allow_dot: bool) -> Option<String> {
        if self.eof() || !self.peek().is_atext() {
            None
        } else {
            Some(self.consume_while(|c| {
                c.is_atext() || (allow_dot && c == '.')
            }))
        }
    }

    /// Consume LWSP (Linear whitespace)
    /// [unstable]
    pub fn consume_linear_whitespace(&mut self) {
        self.consume_while(|c| { c == '\t' || c == ' ' });
    }

    /// Consume a single character from the input.
    #[inline]
    /// [unstable]
    pub fn consume_char(&mut self) -> Option<char> {
        if self.eof() {
            return None
        }
        let c = self.peek();
        self.pos += c.len_utf8();
        Some(c)
    }

    // Consume a linebreak: \r\n, \r or \n
    /// [unstable]
    pub fn consume_linebreak(&mut self) -> bool {
        if self.eof() {
            return false;
        }

        let start_pos = self.pos;

        match self.consume_char() {
            Some('\r') => {
                // Try to consume a single \n following the \r
                if !self.eof() && self.peek() == '\n' {
                    self.consume_char();
                }
                true
            },
            Some('\n') => true,
            _ => { self.pos = start_pos; false }
        }
    }

    // Peek at the current character and determine whether it's (part of) a linebreak
    /// [unstable]
    pub fn peek_linebreak(&mut self) -> bool {
        match self.peek() {
            '\r' | '\n' => true,
            _ => false
        }
    }

    /// Consume a set of characters, each passed to `test` until this function
    /// returns false.
    ///
    /// The position after calling this function will be pointing to the character
    /// which caused a false result from `test`.
    ///
    /// Returns the string of characters that returned true for the test function.
    #[inline]
    /// [unstable]
    pub fn consume_while<F: Fn(char) -> bool>(&mut self, test: F) -> String {
        let start_pos = self.pos;
        while !self.eof() && test(self.peek()) {
            self.consume_char();
        }
        self.s[start_pos..self.pos].to_string()
    }

    /// Peek at the current character.
    ///
    /// Note that this does not do any bounds checking.
    #[inline]
    /// [unstable]
    pub fn peek(&self) -> char {
        self.s[self.pos..].chars().next().unwrap()
    }

    /// Check that `!self.eof() && self.peek() == c`
    #[inline]
    /// [unstable]
    pub fn assert_char(&self, c: char) -> ParsingResult<()> {
        try!(self.assert_not_eof());

        let actual_c = self.peek();
        if c == actual_c {
            Ok(())
        } else {
            Err(ParsingError::new(format!("Expected {}, got {}", c, actual_c)))
        }
    }

    /// Check that we have not reached the end of the input.
    #[inline]
    /// [unstable]
    pub fn assert_not_eof(&self) -> ParsingResult<()> {
        if self.eof() {
            Err(ParsingError::new("Reached EOF.".to_string()))
        } else {
            Ok(())
        }
    }

    /// Get the unconsumed string. Should only be used for debugging purposes!
    #[inline]
    /// [unstable]
    pub fn peek_to_end(&self) -> &str {
        &self.s[self.pos..]
    }

    /// Returns true if we have reached the end of the input.
    #[inline]
    /// [unstable]
    pub fn eof(&self) -> bool {
        self.pos >= self.s.len()
    }

}

/// Type for constructing RFC 5322 messages
pub struct Rfc5322Builder {
    result: String
}

impl Rfc5322Builder {
    /// Make a new builder, with an empty string
    pub fn new() -> Rfc5322Builder {
        Rfc5322Builder {
            result: "".to_string(),
        }
    }

    pub fn result(&self) -> &String {
        &self.result
    }

    pub fn emit_raw(&mut self, s: &str) {
        self.result.push_str(s);
    }

    pub fn emit_folded(&mut self, s: &str) {
       let mut cur_len = 0;
       let mut last_space = 0;
       let mut last_cut = 0;

       for (pos, c) in s.char_indices() {
           match c {
               ' ' => { last_space = pos; },
               '\r' => { cur_len = 0; },
               '\n' => { cur_len = 0; },
               _ => {},
           }

           cur_len += 1;
           // We've reached our line length, so
           if cur_len >= MIME_LINE_LENGTH && last_space > 0 {
               // Emit the string from the last place we cut it to the
               // last space that we saw
               self.emit_raw(&s[last_cut..last_space]);
               // ... and get us ready to put out the continuation
               self.emit_raw("\r\n\t");

               // Reset our counters
               cur_len = 0;
               last_cut = last_space + s[last_space..].chars().next().unwrap().len_utf8();
               last_space = 0;
           }
       }

       // Finally, emit everything left in the string
       self.emit_raw(&s[last_cut..]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct PhraseTestCase<'s> {
        input: &'s str,
        output: &'s str,
        name: &'s str,
    }

    #[test]
    fn test_parser() {
        let mut parser = Rfc5322Parser::new("");
        assert!(parser.consume_message().is_some());

        let mut parser = Rfc5322Parser::new("\r\n");
        assert!(parser.consume_message().is_some());

        let mut parser = Rfc5322Parser::new("From: Garbage@-\r\n");
        assert!(parser.consume_message().is_some());

        let mut parser = Rfc5322Parser::new("From: Garbage@");
        assert!(parser.consume_message().is_some());

        let mut parser = Rfc5322Parser::new("From: Garnage@-");
        assert!(parser.consume_message().is_some());
    }

    #[test]
    fn test_consume_phrase() {
        let tests = [
            PhraseTestCase {
                input: "\"test phrase\"", output: "test phrase",
                name: "Simple quoted-string"
            },
            PhraseTestCase {
                input: "\"test \\\"phrase\\\"\"", output: "test \"phrase\"",
                name: "quoted-string with escape character"
            },
            PhraseTestCase {
                input: "\"=?utf-8?q?encoded=20q-string?=\"", output: "encoded q-string",
                name: "Encoded quoted-string"
            },
            PhraseTestCase {
                input: "atom test", output: "atom test",
                name: "Collection of atoms"
            },
            PhraseTestCase {
                input: "=?utf-8?q?encoded=20atom?=", output: "encoded atom",
                name: "Encoded atom"
            },
            PhraseTestCase {
                input: "Mix of atoms \"and quoted strings\"", output: "Mix of atoms and quoted strings",
                name: "Mix of atoms and quoted strings"
            },
            PhraseTestCase {
                input: "=?utf-8?q?encoded=20atoms?= mixed with \"unencoded\" \"=?utf-8?b?YW5kIGVuY29kZWQgcS1zdHJpbmdz?=\"",
                output: "encoded atoms mixed with unencoded and encoded q-strings",
                name: "Mix of atoms, q-strings of differing encodings"
            },
            PhraseTestCase {
                input: "\"John Smith\" <test@example.org>", output: "John Smith",
                name: "Stop consuming phrase at \"special\" character",
            }
        ];

        for t in tests.iter() {
            let mut p = Rfc5322Parser::new(t.input);
            let phrase = p.consume_phrase(false);
            assert!(phrase.is_some(), format!("{} returned Some", t.name));
            let test_name = format!("{} == {} for {}", phrase.clone().unwrap(), t.output, t.name);
            assert!(phrase.unwrap() == t.output.to_string(), test_name);
        }
    }

    struct MessageTestCase<'s> {
        input: &'s str,
        headers: Vec<(&'s str, &'s str)>,
        body: &'s str,
    }

    #[test]
    fn test_consume_message() {
        let tests = vec![
            MessageTestCase {
                input: "From: \"Joe Blogs\" <joe@example.org>\r\n\r\nBody",
                headers: vec![
                    ("From", "\"Joe Blogs\" <joe@example.org>"),
                ],
                body: "Body",
            },
            // Support parsing messages with \n instead of \r\n
            MessageTestCase {
                input: "From: \"Joe Blogs\" <joe@example.org>\n\nBody",
                headers: vec![
                    ("From", "\"Joe Blogs\" <joe@example.org>"),
                ],
                body: "Body",
            },
            MessageTestCase {
                input: "From: \"Joe Blogs\" <joe@example.org>\r\n\r\nMultiline\r\nBody",
                headers: vec![
                    ("From", "\"Joe Blogs\" <joe@example.org>"),
                ],
                body: "Multiline\r\nBody",
            },
            MessageTestCase {
                input: "From: \"Joe Blogs\" <joe@example.org>\r\nTo: \"John Doe\" <john@example.org>\r\n\r\nMultiple headers",
                headers: vec![
                    ("From", "\"Joe Blogs\" <joe@example.org>"),
                    ("To", "\"John Doe\" <john@example.org>"),
                ],
                body: "Multiple headers",
            },
            MessageTestCase {
                input: "Folded-Header: Some content that is \r\n\t wrapped with a tab.\r\n\r\nFolding whitespace test",
                headers: vec![
                    ("Folded-Header", "Some content that is wrapped with a tab."),
                ],
                body: "Folding whitespace test",
            },
            MessageTestCase {
                input: "Folded-Header: Some content that is \r\n  wrapped with spaces.\r\n\r\nFolding whitespace test",
                headers: vec![
                    ("Folded-Header", "Some content that is wrapped with spaces."),
                ],
                body: "Folding whitespace test",
            },
        ];

        for test in tests.iter() {
            let mut p = Rfc5322Parser::new(test.input);
            let message = p.consume_message();
            match message {
                Some((headers, body)) => {
                    assert_eq!(body, test.body.to_string());
                    for &(header_title, header_value) in test.headers.iter() {
                        let matching_headers = headers.find(&header_title.to_string()).unwrap();
                        assert!(matching_headers.iter().filter(|h| {
                            let val: String = h.get_value().unwrap();
                            val == header_value.to_string()
                        }).count() > 0);
                    }
                },
                None => panic!("Failed to parse message"),
            };
        }
    }

    #[test]
    fn test_builder_folding() {
        struct BuildFoldTest<'s> {
            input: &'s str,
            expected: &'s str,
        }

        let tests = vec![
            BuildFoldTest {
                input: "A long line that should get folded on a space at some point around here, possibly at this point.",
                expected: "A long line that should get folded on a space at some point around here,\r\n\
                \tpossibly at this point.",
            },
            BuildFoldTest {
                input: "A long line that should get folded on a space at some point around here, possibly at this point. And yet more content that will get folded onto another line.",
                expected: "A long line that should get folded on a space at some point around here,\r\n\
                \tpossibly at this point. And yet more content that will get folded onto another\r\n\
                \tline.",
            },
        ];

        for test in tests.into_iter() {
            let mut gen = Rfc5322Builder::new();
            gen.emit_folded(test.input);
            assert_eq!(gen.result(), &test.expected.to_string());
        }

    }
}
