//! Raku string increment/decrement (S03 "Autoincrement precedence").
//!
//! Implements the "magical" string succ/pred that operates on the last
//! contiguous segment of alphanumeric characters, treating '.' as a
//! decimal separator (only the integer part before the first '.' is
//! affected).
//!
//! Supports the Unicode ranges specified in S03:
//! - A-Z, a-z (ASCII)
//! - Greek uppercase/lowercase
//! - Hebrew
//! - Multiple Unicode digit ranges

/// Unicode character ranges for Raku string increment/decrement (S03).
/// Each range is (start, end) inclusive.
const MAGIC_RANGES: &[(char, char)] = &[
    ('A', 'Z'),
    ('a', 'z'),
    ('\u{0391}', '\u{03A9}'), // Greek uppercase
    ('\u{03B1}', '\u{03C9}'), // Greek lowercase
    ('\u{05D0}', '\u{05EA}'), // Hebrew
    ('0', '9'),
    ('\u{0660}', '\u{0669}'), // Arabic-Indic digits
    ('\u{0966}', '\u{096F}'), // Devanagari digits
    ('\u{09E6}', '\u{09EF}'), // Bengali digits
    ('\u{0A66}', '\u{0A6F}'), // Gurmukhi digits
    ('\u{0AE6}', '\u{0AEF}'), // Gujarati digits
    ('\u{0B66}', '\u{0B6F}'), // Oriya digits
    ('\u{0E50}', '\u{0E59}'), // Thai digits
];

/// Find which magic range a character belongs to.
fn char_magic_range(ch: char) -> Option<(char, char)> {
    for &(start, end) in MAGIC_RANGES {
        if ch >= start && ch <= end {
            return Some((start, end));
        }
    }
    None
}

/// Check if a character is in any magic range.
pub(crate) fn is_magic_char(ch: char) -> bool {
    char_magic_range(ch).is_some()
}

/// Check if a character is a valid member of its range (not an unassigned gap).
/// Known gaps: U+03A2 (between Greek uppercase letters Rho and Sigma).
fn is_valid_range_member(ch: char) -> bool {
    ch != '\u{03A2}'
}

/// Get the next valid character in a range, skipping gaps.
fn next_char_in_range(ch: char, end: char) -> Option<char> {
    let mut code = ch as u32 + 1;
    while code <= end as u32 {
        if let Some(next) = char::from_u32(code)
            && is_valid_range_member(next)
        {
            return Some(next);
        }
        code += 1;
    }
    None
}

/// Get the previous valid character in a range, skipping gaps.
fn prev_char_in_range(ch: char, start: char) -> Option<char> {
    if ch <= start {
        return None;
    }
    let mut code = ch as u32 - 1;
    while code >= start as u32 {
        if let Some(prev) = char::from_u32(code)
            && is_valid_range_member(prev)
        {
            return Some(prev);
        }
        if code == 0 {
            break;
        }
        code -= 1;
    }
    None
}

/// Check if a range is a digit range (needs '1' prefix on carry, not '0').
fn is_digit_range(ch: char) -> bool {
    matches!(
        char_magic_range(ch),
        Some(('0', '9'))
            | Some(('\u{0660}', '\u{0669}'))
            | Some(('\u{0966}', '\u{096F}'))
            | Some(('\u{09E6}', '\u{09EF}'))
            | Some(('\u{0A66}', '\u{0A6F}'))
            | Some(('\u{0AE6}', '\u{0AEF}'))
            | Some(('\u{0B66}', '\u{0B6F}'))
            | Some(('\u{0E50}', '\u{0E59}'))
    )
}

/// Get the carry prefix character for a given range.
fn carry_prefix(ch: char) -> char {
    if let Some((start, _)) = char_magic_range(ch) {
        if is_digit_range(ch) {
            // For digit ranges, carry prefix is start + 1 (e.g. '1' not '0')
            char::from_u32(start as u32 + 1).unwrap_or(start)
        } else {
            start
        }
    } else {
        ch
    }
}

/// Increment a segment of magic characters with carry.
fn increment_segment(chars: &[char]) -> String {
    let mut result: Vec<char> = chars.to_vec();
    let mut carry = true;
    for ch in result.iter_mut().rev() {
        if !carry {
            break;
        }
        if let Some((start, end)) = char_magic_range(*ch) {
            if let Some(next) = next_char_in_range(*ch, end) {
                *ch = next;
                carry = false;
            } else {
                // Wrap around to start of range
                *ch = start;
            }
        } else {
            *ch = char::from_u32(*ch as u32 + 1).unwrap_or(*ch);
            carry = false;
        }
    }
    if carry {
        let prefix = carry_prefix(result[0]);
        result.insert(0, prefix);
    }
    result.into_iter().collect()
}

/// Decrement a segment of magic characters with borrow.
/// Returns None if decrement underflows (all chars at range minimum).
fn decrement_segment(chars: &[char]) -> Option<String> {
    let mut result: Vec<char> = chars.to_vec();
    let mut borrow = true;
    for ch in result.iter_mut().rev() {
        if !borrow {
            break;
        }
        if let Some((start, end)) = char_magic_range(*ch) {
            if let Some(prev) = prev_char_in_range(*ch, start) {
                *ch = prev;
                borrow = false;
            } else {
                // At range minimum: wrap around to end of range
                *ch = end;
                // borrow continues
            }
        } else {
            if let Some(prev) = char::from_u32((*ch as u32).wrapping_sub(1)) {
                *ch = prev;
            }
            borrow = false;
        }
    }
    if borrow {
        // All characters were at their range minimum — underflow
        None
    } else {
        Some(result.into_iter().collect())
    }
}

/// Find the last contiguous run of magic characters in a char slice.
/// Returns (start_index, end_index) inclusive, or None.
fn find_last_magic_segment(chars: &[char]) -> Option<(usize, usize)> {
    let mut end = None;
    for i in (0..chars.len()).rev() {
        if is_magic_char(chars[i]) {
            if end.is_none() {
                end = Some(i);
            }
        } else if end.is_some() {
            return Some((i + 1, end.unwrap()));
        }
    }
    end.map(|e| (0, e))
}

/// Compute the successor of a string (Raku string increment).
pub(crate) fn string_succ(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    // Handle '.' as decimal separator
    if let Some(dot_pos) = s.find('.') {
        let integer_part = &s[..dot_pos];
        let fractional_part = &s[dot_pos..];
        let incremented = string_succ(integer_part);
        return format!("{}{}", incremented, fractional_part);
    }
    let chars: Vec<char> = s.chars().collect();
    if let Some((seg_start, seg_end)) = find_last_magic_segment(&chars) {
        let segment = &chars[seg_start..=seg_end];
        let incremented = increment_segment(segment);
        let mut result = String::new();
        for &ch in &chars[..seg_start] {
            result.push(ch);
        }
        result.push_str(&incremented);
        for &ch in &chars[seg_end + 1..] {
            result.push(ch);
        }
        result
    } else {
        // No magic characters; increment last char by 1
        let mut chars = chars;
        if let Some(last) = chars.last_mut() {
            *last = char::from_u32(*last as u32 + 1).unwrap_or(*last);
        }
        chars.into_iter().collect()
    }
}

/// Compute the predecessor of a string (Raku string decrement).
/// Returns None if the decrement would underflow (Failure in Raku).
pub(crate) fn string_pred_checked(s: &str) -> Option<String> {
    if s.is_empty() {
        return Some(String::new());
    }
    // Handle '.' as decimal separator
    if let Some(dot_pos) = s.find('.') {
        let integer_part = &s[..dot_pos];
        let fractional_part = &s[dot_pos..];
        let decremented = string_pred_checked(integer_part)?;
        return Some(format!("{}{}", decremented, fractional_part));
    }
    let chars: Vec<char> = s.chars().collect();
    if chars.len() == 1 {
        let ch = chars[0];
        if let Some((start, _)) = char_magic_range(ch) {
            if ch == start {
                return None;
            }
            return prev_char_in_range(ch, start).map(|c| c.to_string());
        }
        return char::from_u32(ch as u32 - 1).map(|c| c.to_string());
    }
    if let Some((seg_start, seg_end)) = find_last_magic_segment(&chars) {
        let segment = &chars[seg_start..=seg_end];
        let decremented = decrement_segment(segment)?;
        let mut result = String::new();
        for &ch in &chars[..seg_start] {
            result.push(ch);
        }
        result.push_str(&decremented);
        for &ch in &chars[seg_end + 1..] {
            result.push(ch);
        }
        Some(result)
    } else {
        let mut chars = chars;
        if let Some(last) = chars.last_mut() {
            *last = char::from_u32(*last as u32 - 1).unwrap_or(*last);
        }
        Some(chars.into_iter().collect())
    }
}

/// Compute the predecessor of a string.
/// Returns the original string unchanged if decrement underflows.
pub(crate) fn string_pred(s: &str) -> String {
    string_pred_checked(s).unwrap_or_else(|| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_succ() {
        assert_eq!(string_succ("a"), "b");
        assert_eq!(string_succ("z"), "aa");
        assert_eq!(string_succ("Z"), "AA");
        assert_eq!(string_succ("9"), "10");
        assert_eq!(string_succ("zz"), "aaa");
    }

    #[test]
    fn test_dot_separator() {
        assert_eq!(string_succ("123.456"), "124.456");
        assert_eq!(string_pred("124.456"), "123.456");
    }

    #[test]
    fn test_path_increment() {
        assert_eq!(string_succ("/tmp/pix000.jpg"), "/tmp/pix001.jpg");
        assert_eq!(string_pred("/tmp/pix001.jpg"), "/tmp/pix000.jpg");
    }

    #[test]
    fn test_ebcdic_chars() {
        assert_eq!(string_succ("zi"), "zj");
        assert_eq!(string_pred("zj"), "zi");
        assert_eq!(string_succ("zr"), "zs");
        assert_eq!(string_pred("zs"), "zr");
    }

    #[test]
    fn test_decrement_failure() {
        assert!(string_pred_checked("A00").is_none());
        assert!(string_pred_checked("a0").is_none());
        assert!(string_pred_checked("aaa").is_none());
    }

    #[test]
    fn test_greek_sigma_skip() {
        // U+03A1 (Rho) should skip U+03A2 and go to U+03A3 (Sigma)
        assert_eq!(string_succ("\u{03A1}"), "\u{03A3}");
    }

    #[test]
    fn test_greek_carry() {
        // K + omega -> L + alpha (omega wraps, carry to K->L)
        assert_eq!(string_succ("K\u{03C9}"), "L\u{03B1}");
    }
}
