use std::ascii::OwnedAsciiExt;
use std::collections::HashMap;

use chrono::{
    DateTime,
    FixedOffset,
    Offset,
};

use super::rfc5322::Rfc5322Parser;
use super::results::{ParsingError, ParsingResult};

static DAYS_OF_WEEK: [&'static str; 7] = [
    "mon", "tue", "wed", "thu",
    "fri", "sat", "sun",
];

static MONTHS: [&'static str; 12] = [
    "jan", "feb", "mar", "apr",
    "may", "jun", "jul", "aug",
    "sep", "oct", "nov", "dec"
];

// Lazily build TZ_DATA when we need it.
lazy_static!{
    static ref TZ_DATA: HashMap<&'static str, i32> = {
        let mut map = HashMap::new();
        map.insert("Z",    0); // Zulu
        map.insert("UT",   0);
        map.insert("GMT",  0);
        map.insert("PST", -28800); // UTC-8
        map.insert("PDT", -25200); // UTC-7
        map.insert("MST", -25200); // UTC-7
        map.insert("MDT", -21600); // UTC-6
        map.insert("CST", -21600); // UTC-6
        map.insert("CDT", -18000); // UTC-5
        map.insert("EST", -18000); // UTC-5
        map.insert("EDT", -14400); // UTC-4
        map
    };
}

/// Parser for RFC822 style dates, as defined by Section 5.
///
/// Note that this also supports the additions as specified in
/// RFC5322 Section 3.3 while still being backward compatible.
#[unstable]
pub struct Rfc822DateParser<'s> {
    parser: Rfc5322Parser<'s>,
}

impl<'s> Rfc822DateParser<'s> {
    #[unstable]
    pub fn new(s: &'s str) -> Rfc822DateParser<'s> {
        Rfc822DateParser {
            parser: Rfc5322Parser::new(s),
        }
    }

    #[inline]
    fn consume_u32(&mut self) -> Option<u32> {
        match self.parser.consume_word(false) {
            Some(s) => match s.parse() {  // FIXME
                Ok(x) => Some(x),
                Err(_) => None
            },
            None => None,
        }
    }

    fn consume_time(&mut self) -> ParsingResult<(u32, u32, u32)> {
        let hour = match self.consume_u32() {
            Some(x) => x,
            None => return Err(ParsingError::new(
                    "Failed to parse time: Expected hour, a number.".to_string()))
        };

        try!(self.parser.assert_char(':'));
        self.parser.consume_char();

        let minute = match self.consume_u32() {
            Some(x) => x,
            None => return Err(ParsingError::new("Failed to parse time: Expected minute.".to_string()))
        };

        // Seconds are optional, only try to parse if we see the next seperator.
        let second = match self.parser.assert_char(':') {
            Ok(_) => {
                self.parser.consume_char();
                self.consume_u32()
            },
            Err(_) => None
        }.unwrap_or(0);

        Ok((hour, minute, second))
    }

    fn consume_timezone_offset(&mut self) -> ParsingResult<i32> {
        match self.parser.consume_word(false) {
            Some(s) => {
                // from_str doesn't like leading '+' to indicate positive,
                // so strip it off if it's there.
                let mut s_slice = s.as_slice();
                s_slice = if s_slice.starts_with("+") {
                    &s_slice[1..]
                } else {
                    s_slice
                };
                // Try to parse zone as an int
                match s_slice.parse::<i32>() {
                    Ok(i) => {
                        let offset_hours = i / 100;
                        let offset_mins = i % 100;
                        Ok(offset_hours * 3600 + offset_mins * 60)
                    },
                    Err(_) => {
                        // Isn't an int, so try to use the strings->TZ hash.
                        match TZ_DATA.get(s_slice) {
                            Some(offset) => Ok(offset.clone()),
                            None => Err(ParsingError::new(format!("Invalid timezone: {}", s_slice)))
                        }
                    }
                }
            },
            None => Err(ParsingError::new("Expected timezone offset.".to_string()))
        }
    }

    /// Consume a DateTime from the input.
    ///
    /// If successful, returns a DateTime with a fixed offset based on the
    /// timezone parsed. You may wish to deal with this in UTC, in which case
    /// you may want something like
    ///
    /// ```
    /// extern crate chrono;
    /// extern crate email;
    ///
    /// use email::rfc822::Rfc822DateParser;
    /// use chrono::UTC;
    ///
    /// fn main() {
    ///     let mut p = Rfc822DateParser::new("Thu, 18 Dec 2014 21:07:22 +0100");
    ///     let d = p.consume_datetime().unwrap();
    ///     let as_utc = d.with_offset(UTC);
    ///
    ///     assert_eq!(d, as_utc);
    /// }
    /// ```
    #[unstable]
    pub fn consume_datetime(&mut self) -> ParsingResult<DateTime<FixedOffset>> {
        // Handle the optional day ","
        self.parser.push_position();
        let day_of_week = self.parser.consume_word(false);
        if day_of_week.is_some() {
            let lower_dow = day_of_week.unwrap().into_ascii_lowercase();
            if DAYS_OF_WEEK.position_elem(&lower_dow.as_slice()).is_some() {
                // Lose the ","
                self.parser.consume_while(|c| { c == ',' || c.is_whitespace() });
            } else {
                // What we read doesn't look like a day, so ignore it,
                // go back to the start and continue on.
                self.parser.pop_position();
            }
        } else {
            // We don't have a leading day "," so go back to the start.
            self.parser.pop_position();
        }

        let day_of_month = match self.consume_u32() {
            Some(x) => x,
            None => return Err(ParsingError::new("Expected day of month, a number.".to_string()))
        };
        self.parser.consume_linear_whitespace();

        let month = match self.parser.consume_word(false) {
            // FIXME: Move into consume_month?
            Some(s) => {
                let lower_month = s.into_ascii_lowercase();
                // Add one because months are 1 indexed, array is 0 indexed.
                match MONTHS.position_elem(&lower_month.as_slice()).map(|i| { (i + 1) as u32 }) {
                    Some(x) => x,
                    None => return Err(ParsingError::new(format!("Invalid month: {}", lower_month)))
                }
            },
            None => return Err(ParsingError::new("Expected month.".to_string()))
        };
        self.parser.consume_linear_whitespace();

        let year = match self.consume_u32() {
            Some(i) => {
                // See RFC5322 4.3 for justification of obsolete year format handling.
                match i {
                    // 2 digit year between 0 and 49 is assumed to be in the 2000s
                    0...49 => { i + 2000 },
                    // 2 digit year greater than 50 and 3 digit years are added to 1900
                    50...999 => { i + 1900 },
                    _ => { i },
                }
            },
            None => return Err(ParsingError::new("Expected year.".to_string())),
        };
        self.parser.consume_linear_whitespace();

        let time = try!(self.consume_time());
        self.parser.consume_linear_whitespace();

        let tz_offset = try!(self.consume_timezone_offset());

        let (hour, minute, second) = time;

        Ok(FixedOffset::east(tz_offset).ymd(
            year as i32, month, day_of_month
        ).and_hms(
            hour, minute, second
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{
        DateTime,
        FixedOffset,
        Offset,
    };

    #[test]
    fn test_time_parse() {
        struct TimeParseTest<'s> {
            input: &'s str,
            result: Option<DateTime<FixedOffset>>,
        }

        let edt = FixedOffset::east(-14400); // UTC-0400
        let cet = FixedOffset::east(3600);  // UTC+0100
        let napal = FixedOffset::east(20700); // UTC+0545
        let utc = FixedOffset::east(0); // UTC+0000
        let tests = vec![
            TimeParseTest {
                input: "Mon, 20 Jun 1982 10:01:59 EDT",
                result: Some(edt.ymd(1982, 6, 20).and_hms(10, 1, 59))
            },
            TimeParseTest {
                // Check the 2 digit date parsing logic, >=50
                input: "Mon, 20 Jun 82 10:01:59 EDT",
                result: Some(edt.ymd(1982, 6, 20).and_hms(10, 1, 59))
            },
            TimeParseTest {
                // Check the 2 digit date parsing logic, <50
                input: "Mon, 20 Jun 02 10:01:59 EDT",
                result: Some(edt.ymd(2002, 6, 20).and_hms(10, 1, 59))
            },
            TimeParseTest {
                // Check the optional seconds
                input: "Mon, 20 Jun 1982 10:01 EDT",
                result: Some(edt.ymd(1982, 6, 20).and_hms(10, 1, 0))
            },
            TimeParseTest {
                // Check different TZ parsing
                input: "Mon, 20 Jun 1982 10:01:59 +0100",
                result: Some(cet.ymd(1982, 6, 20).and_hms(10, 1, 59))
            },
            TimeParseTest {
                input: "Mon, 20 Jun 1982 10:01:59 -0400",
                result: Some(edt.ymd(1982, 6, 20).and_hms(10, 1, 59))
            },
            TimeParseTest {
                // Test for wierd minute offsets in TZ
                input: "Mon, 20 Jun 1982 10:01:59 +0545",
                result: Some(napal.ymd(1982, 6, 20).and_hms(10, 1, 59))
            },
            TimeParseTest {
                // Test for being able to skip day of week
                input: "09 Jan 2012 21:20:00 +0000",
                result: Some(utc.ymd(2012, 1, 9).and_hms(21, 20, 00))
            },
        ];

        for test in tests.into_iter() {
            let mut parser = Rfc822DateParser::new(test.input);
            assert_eq!(parser.consume_datetime().ok(), test.result);
        }
    }

}
