use std::ascii::OwnedAsciiExt;
use std::collections::HashMap;

use chrono::{
    DateTime,
    FixedOffset,
    Offset,
};

use super::rfc5322::Rfc5322Parser;

static DAYS_OF_WEEK: [&'static str, .. 7] = [
    "mon", "tue", "wed", "thu",
    "fri", "sat", "sun",
];

static MONTHS: [&'static str, .. 12] = [
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
            Some(s) => s.parse(),
            None => None,
        }
    }

    fn consume_time(&mut self) -> Option<(u32, u32, u32)> {
        let hour = self.consume_u32();
        if !self.parser.eof() && self.parser.peek() == ':' {
            self.parser.consume_char();
            let minute = self.consume_u32();
            // Seconds are optional, only try to parse if we see the next seperator.
            let second = if !self.parser.eof() && self.parser.peek() == ':' {
                self.parser.consume_char();
                self.consume_u32()
            } else {
                None
            };

            match (hour, minute, second) {
                (Some(hour), Some(minute), Some(second)) => Some((hour, minute, second)),
                // No seconds - default to 0
                (Some(hour), Some(minute), None) => Some((hour, minute, 0)),
                _ => None,
            }
        } else {
            None
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
    pub fn consume_datetime(&mut self) -> Option<DateTime<FixedOffset>> {
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

        let day_of_month = self.consume_u32();
        self.parser.consume_linear_whitespace();

        let month = match self.parser.consume_word(false) {
            Some(s) => {
                let lower_month = s.into_ascii_lowercase();
                // Add one because months are 1 indexed, array is 0 indexed.
                MONTHS.position_elem(&lower_month.as_slice()).map(|i| { (i + 1) as u32 })
            },
            None => None,
        };
        self.parser.consume_linear_whitespace();

        let year = match self.consume_u32() {
            Some(i) => {
                Some(
                    // See RFC5322 4.3 for justification of obsolete year format handling.
                    match i {
                        // 2 digit year between 0 and 49 is assumed to be in the 2000s
                        0...49 => { i + 2000 },
                        // 2 digit year greater than 50 and 3 digit years are added to 1900
                        50...999 => { i + 1900 },
                        _ => { i },
                    }
                )
            },
            None => None,
        };
        self.parser.consume_linear_whitespace();

        let time = self.consume_time();
        self.parser.consume_linear_whitespace();

        let tz_offset = match self.parser.consume_word(false) {
            Some(s) => {
                // from_str doesn't like leading '+' to indicate positive,
                // so strip it off if it's there.
                let mut s_slice = s.as_slice();
                s_slice = if s_slice.starts_with("+") {
                    s_slice.slice_from(1)
                } else {
                    s_slice
                };
                // Try to parse zone as an int
                match s_slice.parse::<i32>() {
                    Some(i) => {
                        let offset_hours = i / 100;
                        let offset_mins = i % 100;
                        Some(offset_hours * 3600 + offset_mins * 60)
                    },
                    None => {
                        // Isn't an int, so try to use the strings->TZ hash.
                        match TZ_DATA.get(s_slice) {
                            Some(offset) => Some(offset.clone()),
                            None => None
                        }
                    }
                }
            },
            None => None
        };

        match (year, month, day_of_month, time, tz_offset) {
            (Some(year), Some(month), Some(day_of_month), Some((hour, minute, second)), Some(tz_offset)) => {
                Some(FixedOffset::east(tz_offset).ymd(
                    year as i32, month, day_of_month
                ).and_hms(
                    hour, minute, second
                ))
            },
            _ => None
        }

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
            assert_eq!(parser.consume_datetime(), test.result);
        }
    }

}
