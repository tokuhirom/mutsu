//! Numeric values of single Unicode characters: decimal digits of every
//! `Nd` block, vulgar fractions, and the `Nl`/`No`/Unihan numerals. The
//! parser reads them for numeric literals and the runtime for `.unival`
//! and friends, so they live below both (issue #10779).

/// Return the Rat (numerator, denominator) for a Unicode vulgar fraction character.
pub(crate) fn unicode_rat_value(c: char) -> Option<(i64, i64)> {
    match c {
        '¼' => Some((1, 4)),
        '½' => Some((1, 2)),
        '¾' => Some((3, 4)),
        '⅐' => Some((1, 7)),
        '⅑' => Some((1, 9)),
        '⅒' => Some((1, 10)),
        '⅓' => Some((1, 3)),
        '⅔' => Some((2, 3)),
        '⅕' => Some((1, 5)),
        '⅖' => Some((2, 5)),
        '⅗' => Some((3, 5)),
        '⅘' => Some((4, 5)),
        '⅙' => Some((1, 6)),
        '⅚' => Some((5, 6)),
        '⅛' => Some((1, 8)),
        '⅜' => Some((3, 8)),
        '⅝' => Some((5, 8)),
        '⅞' => Some((7, 8)),
        '༳' => Some((-1, 2)),
        '↉' => Some((0, 1)),
        _ => unicode_general_rat_value(c),
    }
}

/// General fallback: look up a character's rational value via the Unicode Nl/No table.
/// Handles characters not explicitly listed above (e.g. cuneiform fractions).
fn unicode_general_rat_value(c: char) -> Option<(i64, i64)> {
    let (n, d) = super::numval_table::lookup_nl_no_value(c)?;
    if d == 1 {
        return None; // Integer values handled by unicode_numeric_int_value
    }
    Some((n, d))
}

/// Return the integer value for a Unicode numeric character (superscripts, subscripts, etc.).
pub(crate) fn unicode_numeric_int_value(c: char) -> Option<i64> {
    match c {
        '²' => Some(2),
        '³' => Some(3),
        '¹' => Some(1),
        '⁰' => Some(0),
        '⁴' => Some(4),
        '⁵' => Some(5),
        '⁶' => Some(6),
        '⁷' => Some(7),
        '⁸' => Some(8),
        '⁹' => Some(9),
        '⅟' => Some(1),
        '𑁓' => Some(2),
        '౸' => Some(0),
        '㆒' => Some(1),
        '𐌣' => Some(50),
        '⓿' => Some(0),
        '፼' => Some(10000),
        'ↈ' => Some(100000),
        '𒐀' => Some(2),
        'Ⅰ' | 'ⅰ' => Some(1),
        'Ⅱ' | 'ⅱ' => Some(2),
        'Ⅲ' | 'ⅲ' => Some(3),
        'Ⅳ' | 'ⅳ' => Some(4),
        'Ⅴ' | 'ⅴ' => Some(5),
        'Ⅵ' | 'ⅵ' => Some(6),
        'Ⅶ' | 'ⅶ' => Some(7),
        'Ⅷ' | 'ⅷ' => Some(8),
        'Ⅸ' | 'ⅸ' => Some(9),
        'Ⅹ' | 'ⅹ' => Some(10),
        _ => unicode_general_int_value(c),
    }
}

/// General fallback: look up a character's integer value via the Unicode Nl/No table.
/// Handles characters not explicitly listed above (e.g. cuneiform integers).
fn unicode_general_int_value(c: char) -> Option<i64> {
    if let Some((n, d)) = super::numval_table::lookup_nl_no_value(c)
        && d == 1
    {
        return Some(n);
    }
    // Also check Lo (Letter, Other) characters with Unihan numeric values
    if let Some((n, d)) = super::numval_table::lookup_lo_value(c)
        && d == 1
    {
        return Some(n);
    }
    None
}

/// Return the decimal digit value (0-9) for any Unicode Nd (decimal digit) character.
/// For ASCII digits and also Thai, Arabic, Devanagari, NKo, and all other Unicode decimal digit blocks.
pub(crate) fn unicode_decimal_digit_value(c: char) -> Option<u32> {
    if c.is_ascii_digit() {
        return Some(c as u32 - '0' as u32);
    }
    if !c.is_numeric() {
        return None;
    }
    // Only allow Unicode Decimal_Number (Nd), not No/Nl.
    if super::gc::general_category(c) != super::gc::GeneralCategory::Nd {
        return None;
    }
    let cp = c as u32;
    // Nd (decimal digit) blocks are always exactly 10 consecutive codepoints
    // representing digits 0-9. However, multiple blocks can be packed
    // contiguously (e.g., MATHEMATICAL digit blocks). We find the start of
    // the full Nd run, then compute our block boundary as the nearest multiple
    // of 10 within that run.
    let mut run_start = cp;
    // Scan backwards to find the start of the contiguous Nd run (limit to 200)
    for _ in 0..200 {
        if run_start == 0 {
            break;
        }
        let prev = run_start - 1;
        let is_nd = char::from_u32(prev)
            .is_some_and(|ch| super::gc::general_category(ch) == super::gc::GeneralCategory::Nd);
        if !is_nd {
            break;
        }
        run_start = prev;
    }
    // Within the contiguous run, digit blocks are at 10-char boundaries
    let offset_in_run = cp - run_start;
    let digit_val = offset_in_run % 10;
    if digit_val <= 9 {
        Some(digit_val)
    } else {
        None
    }
}
