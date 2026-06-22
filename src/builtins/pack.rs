//! `pack` / `unpack` (experimental, `use experimental :pack`).
//!
//! Raku's pack template is a smaller, and quirkier, set than Perl's. The
//! letters mutsu supports mirror the table in `raku-doc` (Blob `unpack`):
//!
//! | Letter | Meaning                                                |
//! |--------|--------------------------------------------------------|
//! | A a Z  | string — each byte is one codepoint (pack: field bytes)|
//! | C      | one element as an 8-bit integer                        |
//! | H      | hex string                                             |
//! | S v    | two bytes, little-endian unsigned                      |
//! | n      | two bytes, big-endian ("network") unsigned             |
//! | L V    | four bytes, little-endian unsigned                     |
//! | N      | four bytes, big-endian ("network") unsigned            |
//! | x      | a null byte (pack) / drop a byte (unpack)              |
//!
//! Rakudo treats a numeric letter's count as a no-op for `pack` (e.g.
//! `pack("C3", 65, 66, 67)` packs a single byte `65`, and `pack("S2", a, b)`
//! packs only `a`); each numeric unit consumes exactly one item. Repetition is
//! expressed by repeating the letter (`"S" x $n`). mutsu matches that. For
//! `unpack` the count is meaningful (`"C*"` extracts every remaining byte).

use crate::value::{RuntimeError, Value};

/// A single parsed template unit: a letter plus an optional count.
struct Unit {
    letter: char,
    /// `Some(n)` for a fixed count, `None` for `*`, absent quantifier defaults
    /// to `Some(1)`.
    count: Option<usize>,
}

/// Parse a pack/unpack template string into units. Whitespace between units is
/// ignored; a quantifier is `*` or a positive integer.
fn parse_template(template: &str) -> Result<Vec<Unit>, RuntimeError> {
    let mut units = Vec::new();
    let mut chars = template.chars().peekable();
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
            continue;
        }
        if !c.is_ascii_alphabetic() {
            return Err(RuntimeError::new(format!(
                "Unrecognised pack template character: '{c}'"
            )));
        }
        let letter = c;
        chars.next();
        // Skip whitespace between the letter and its quantifier.
        while matches!(chars.peek(), Some(d) if d.is_whitespace()) {
            chars.next();
        }
        let count = match chars.peek() {
            Some('*') => {
                chars.next();
                None
            }
            Some(d) if d.is_ascii_digit() => {
                let mut n = 0usize;
                while let Some(d) = chars.peek() {
                    if let Some(digit) = d.to_digit(10) {
                        n = n.saturating_mul(10).saturating_add(digit as usize);
                        chars.next();
                    } else {
                        break;
                    }
                }
                Some(n)
            }
            _ => Some(1),
        };
        units.push(Unit { letter, count });
    }
    Ok(units)
}

fn item_to_u64(v: &Value) -> u64 {
    crate::runtime::to_int(v) as u64
}

/// `pack(Str $template, *@items) --> Buf`
pub(crate) fn pack(template: &str, items: &[Value]) -> Result<Value, RuntimeError> {
    let units = parse_template(template)?;
    let mut out: Vec<u8> = Vec::new();
    let mut it = items.iter();

    for unit in &units {
        match unit.letter {
            // String letters: one item, laid out as `count` bytes (pad/truncate).
            'A' | 'a' | 'Z' => {
                let s = it.next().map(Value::to_string_value).unwrap_or_default();
                let bytes = s.into_bytes();
                let pad = if unit.letter == 'A' { b' ' } else { 0u8 };
                match unit.count {
                    None => out.extend_from_slice(&bytes),
                    Some(width) => {
                        for i in 0..width {
                            out.push(bytes.get(i).copied().unwrap_or(pad));
                        }
                    }
                }
            }
            // Hex string: one item; `count` hex digits (`*` = all of them).
            'H' => {
                let s = it.next().map(Value::to_string_value).unwrap_or_default();
                let digits: Vec<u8> = s
                    .chars()
                    .filter_map(|c| c.to_digit(16).map(|d| d as u8))
                    .collect();
                let take = unit.count.unwrap_or(digits.len()).min(digits.len());
                let mut i = 0;
                while i < take {
                    let hi = digits[i];
                    let lo = if i + 1 < take { digits[i + 1] } else { 0 };
                    out.push((hi << 4) | lo);
                    i += 2;
                }
            }
            // Null bytes (pack); consumes no item.
            'x' => {
                out.resize(out.len() + unit.count.unwrap_or(1), 0);
            }
            // Numeric letters: one item each, count is a no-op (Rakudo quirk).
            'C' | 'c' => {
                let n = it.next().map_or(0, item_to_u64);
                out.push(n as u8);
            }
            'S' | 'v' => {
                let n = it.next().map_or(0, item_to_u64) as u16;
                out.extend_from_slice(&n.to_le_bytes());
            }
            'n' => {
                let n = it.next().map_or(0, item_to_u64) as u16;
                out.extend_from_slice(&n.to_be_bytes());
            }
            'L' | 'V' => {
                let n = it.next().map_or(0, item_to_u64) as u32;
                out.extend_from_slice(&n.to_le_bytes());
            }
            'N' => {
                let n = it.next().map_or(0, item_to_u64) as u32;
                out.extend_from_slice(&n.to_be_bytes());
            }
            other => {
                return Err(RuntimeError::new(format!(
                    "Unsupported pack template character: '{other}'"
                )));
            }
        }
    }

    Ok(crate::builtins::buf_write_num::make_buf_value("Buf", out))
}

/// `unpack(Blob $blob, Str $template) --> List`
pub(crate) fn unpack(bytes: &[u8], template: &str) -> Result<Value, RuntimeError> {
    let units = parse_template(template)?;
    let mut out: Vec<Value> = Vec::new();
    let mut pos = 0usize;

    for unit in &units {
        match unit.letter {
            // String: `count` bytes mapped to codepoints, joined into one Str.
            'A' | 'a' | 'Z' => {
                let take = match unit.count {
                    None => bytes.len().saturating_sub(pos),
                    Some(n) => n.min(bytes.len().saturating_sub(pos)),
                };
                let mut s = String::new();
                for _ in 0..take {
                    s.push(bytes[pos] as char);
                    pos += 1;
                }
                if unit.letter == 'Z' {
                    s.truncate(s.find('\0').unwrap_or(s.len()));
                }
                out.push(Value::str(s));
            }
            // Hex string of `count` nibbles (`*` = all remaining bytes).
            'H' => {
                let take_bytes = match unit.count {
                    None => bytes.len().saturating_sub(pos),
                    Some(n) => n.div_ceil(2).min(bytes.len().saturating_sub(pos)),
                };
                let mut s = String::new();
                for _ in 0..take_bytes {
                    s.push_str(&format!("{:02x}", bytes[pos]));
                    pos += 1;
                }
                if let Some(n) = unit.count {
                    s.truncate(n.min(s.len()));
                }
                out.push(Value::str(s));
            }
            'x' => {
                pos = (pos + unit.count.unwrap_or(1)).min(bytes.len());
            }
            // Numeric: count IS meaningful for unpack (`*` = rest of the blob).
            'C' | 'c' => unpack_ints(&mut out, bytes, &mut pos, unit.count, 1, false),
            'S' | 'v' => unpack_ints(&mut out, bytes, &mut pos, unit.count, 2, false),
            'n' => unpack_ints(&mut out, bytes, &mut pos, unit.count, 2, true),
            'L' | 'V' => unpack_ints(&mut out, bytes, &mut pos, unit.count, 4, false),
            'N' => unpack_ints(&mut out, bytes, &mut pos, unit.count, 4, true),
            other => {
                return Err(RuntimeError::new(format!(
                    "Unsupported unpack template character: '{other}'"
                )));
            }
        }
    }

    // Raku's unpack collapses a single extracted value to the bare element
    // (`Blob.new(0x34,0x12).unpack("S")` is an `Int`, not a one-element list).
    if out.len() == 1 {
        return Ok(out.into_iter().next().unwrap());
    }
    Ok(Value::array(out))
}

/// Extract integers of `width` bytes from `bytes` at `pos`. `count == None`
/// (`*`) consumes the rest of the blob; `Some(n)` extracts up to `n` elements.
fn unpack_ints(
    out: &mut Vec<Value>,
    bytes: &[u8],
    pos: &mut usize,
    count: Option<usize>,
    width: usize,
    big_endian: bool,
) {
    let mut remaining = match count {
        None => usize::MAX,
        Some(n) => n,
    };
    while remaining > 0 && *pos + width <= bytes.len() {
        let mut val: u64 = 0;
        if big_endian {
            for i in 0..width {
                val = (val << 8) | bytes[*pos + i] as u64;
            }
        } else {
            for i in 0..width {
                val |= (bytes[*pos + i] as u64) << (8 * i);
            }
        }
        out.push(Value::Int(val as i64));
        *pos += width;
        remaining -= 1;
    }
}
