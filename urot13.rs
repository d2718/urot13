//! urot13.rs
//!
//! Perform a Unicode-aware rot13 transformation.
//!
//! last update: 2016-09-25
//!
//! This program manages to perform rot13 on a large number of precomposed
//! Unicode characters by decomposing said character into a base character
//! and a series of combining characters, rot13ing the base character, and
//! writing out the new, rotated base character followed by the series of
//! combining characters.

use std::io;
use std::io::BufRead;
use std::collections::HashMap;

/// Contains the raw data about decomposable Unicode characters.
mod unidata;

/// Unicode code points are represented internally as u32s. These two
/// functions translate between u32s and char primitives.
///
fn ch2code(c: char) -> u32 { c as u32 }
fn code2ch(n: u32) -> char { std::char::from_u32(n).unwrap() }

/// Any given Unicode character can be either atomic or decomposable, and
/// the program needs to deal with those cases differently
///
enum Unichar<'a> {
    Single(u32),
    Multi(&'a unidata::Multichar<'a>)
}

/// The lower end of each range of code points that could possibly be rot13'd.
///
static RANGES : [u32; 12] = [
    0x0041,     // A
    0x0061,     // a
    0x249c,     // parenthesized latin small letter a
    0x24b6,     // circled latin capital letter a
    0x24d0,     // circled latin small letter a
    0xff21,     // fullwidth latin capital letter a
    0xff41,     // fullwidth latin small letter a
    0x1f110,    // parenthesized latin capital letter a
    0x1f130,    // squared latin capital letter a
    0x1f150,    // negative circled latin capital letter a
    0x1f170,    // negative squared latin capital letter a
    0x1f1e6     // regional indicator symbol letter a
];

/// If a code point falls in a rotatable range, do so, otherwise just
/// pass it through unchanged.
///
fn range_rotate(n: u32) -> u32 {
    for &b in &RANGES {
        if n < b {
            return n;
        } else if n < b + 26 {
            return ((n - b + 13) % 26) + b;
        }
    }

    n
}

/// If the character at code point n decomposes, return an appropriate
/// unidata::Multichar, else just the single code point.
///
/// The dat argument is a reference to the hash containing Unicode
/// decomposition data (as returned by unidata::generate_hash()).
///
fn decompose<'a>(
        n: u32,
        dat: &'a HashMap<u32, unidata::Multichar<'a>>
) -> Unichar<'a>
{
    match dat.get(&n) {
        Some(x) => Unichar::Multi(x),
        None => Unichar::Single(n)
    }
}

/// Return a new String containing the urot13'd text from s.
///
/// The dat argument is a reference to the hash containing Unicode
/// decomposition data (as returned by unidata::generate_hash()).
///
fn urot13(
        s: &str,
        dat: &HashMap<u32, unidata::Multichar>
) -> String
{
    let mut rval = String::new();
    for c in s.chars() {
        match decompose(ch2code(c), dat) {
            Unichar::Single(n) => { rval.push(code2ch(range_rotate(n))) },
            Unichar::Multi(mc) => {
                rval.push(code2ch(range_rotate(mc.base)));
                for &n in mc.rest { rval.push(code2ch(n)); }
            },
        }
    }
    rval
}
                
/// Do the thing.
///
fn main() {
    let stdin = io::stdin();
    let unidat = unidata::generate_hash();

    for line in stdin.lock().lines() {
        println!("{}", urot13(&line.unwrap(), &unidat));
    }
}
