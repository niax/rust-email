//! Module for decoding RFC 2047 strings
// use for to_ascii_lowercase
use std::ascii::AsciiExt;
use std::num::from_str_radix;
use rustc_serialize::base64::FromBase64;

use encoding::label::encoding_from_whatwg_label;
use encoding::DecoderTrap;

/// Decode an RFC 2047 string (`s`) into a Rust String.
///
/// Will accept either "Q" encoding (RFC 2047 Section 4.2) or
/// "B" encoding (BASE64)
#[unstable]
pub fn decode_rfc2047(s: &str) -> Option<String> {
    let parts: Vec<&str> = s.split('?').collect();
    if parts.len() != 5 || parts[0] != "=" || parts[4] != "=" {
        None
    } else {
        let charset = parts[1].to_ascii_lowercase();
        let encoding = parts[2].to_ascii_lowercase();
        let content = parts[3];

        let bytes = match encoding.as_slice() {
            "q" => decode_q_encoding(content),
            "b" => decode_base64_encoding(content),
            _ => panic!("Unknown encoding type"),
        };

        // XXX: Relies on WHATWG labels, rather than MIME labels for
        // charset. Consider adding mapping upstream.
        let decoder = encoding_from_whatwg_label(charset.as_slice());

        match (bytes, decoder) {
            (Ok(b), Some(d)) => {
                d.decode(&b[], DecoderTrap::Replace).ok()
            }
            _ => None,
        }
    }
}

#[stable]
pub fn decode_q_encoding(s: &str) -> Result<Vec<u8>, String> {
    let mut result = Vec::new();

    let mut pos = 0us;

    while pos < s.len() {
        let c = s.char_range_at(pos);
        pos = match c.ch {
            '=' => {
                let mut inner_pos = c.next;
                let mut hex_string = String::new();
                for _ in 0..2 {
                    let hex_digit_char = s.char_range_at(inner_pos);
                    hex_string.push(hex_digit_char.ch);
                    inner_pos = hex_digit_char.next;
                }
                // = followed by a newline means a continuation
                if hex_string.as_slice() != "\r\n" {
                    match from_str_radix(hex_string.as_slice(), 16us) {
                        Some(char_val) => { result.push(char_val) },
                        None => { return Err(format!("'{}' is not a hex number", hex_string)) },
                    }
                }
                inner_pos
            }
            _ => {
                result.push(c.ch as u8);
                c.next
            }
        }
    }

    Ok(result)
}

fn decode_base64_encoding(s: &str) -> Result<Vec<u8>, String> {
    match s.from_base64() {
        Ok(bytes) => Ok(bytes),
        Err(_) => Err("Failed to base64 decode".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct DecodeTest<'s> {
        input: &'s str,
        output: &'s str,
    }

    #[test]
    fn test_decode() {
        let tests = [
            DecodeTest {
                input: "=?ISO-8859-1?Q?Test=20text?=", 
                output: "Test text"
            },
            DecodeTest {
                input: "=?ISO-8859-1?b?VGVzdCB0ZXh0?=", 
                output: "Test text"
            },
            DecodeTest {
                input: "=?utf-8?b?44GT44KT44Gr44Gh44Gv44CC?=",
                output: "こんにちは。"
            },
        ];

        for t in tests.iter() {
            assert_eq!(decode_rfc2047(t.input).unwrap(), t.output.to_string());
        }
    }

    #[test]
    fn test_decode_failure() {
        let tests = [
            // Invalid base64
            "=?ISO-8859-1?b?-?=",
            // Not valid RFC 2047
            "=?Doesn't end with equals",
            // Unknown charset
            "=?NOCHARSET?q?foo?=",
        ];

        for t in tests.iter() {
            println!("{}", t);
            assert!(decode_rfc2047(*t).is_none());
        }
    }
}
