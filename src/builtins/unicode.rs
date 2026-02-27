/// Unicode titlecase for the first character of a string.
/// Titlecase differs from uppercase for certain characters:
/// - 'ss' -> "Ss" (not "SS")
/// - digraph ligatures: special titlecase forms
pub(crate) fn unicode_titlecase_first(ch: char) -> String {
    match ch {
        // Latin digraph titlecase pairs
        '\u{01C9}' | '\u{01C7}' => "\u{01C8}".to_string(), // Lj
        '\u{01CC}' | '\u{01CA}' => "\u{01CB}".to_string(), // Nj
        '\u{01F3}' | '\u{01F1}' => "\u{01F2}".to_string(), // Dz
        // Sharp S: titlecase is "Ss", not "SS"
        '\u{00DF}' => "Ss".to_string(),
        // Default: use uppercase
        _ => ch.to_uppercase().to_string(),
    }
}

pub(crate) fn titlecase_string(s: &str) -> String {
    let mut result = String::new();
    let mut first = true;
    for ch in s.chars() {
        if first {
            result.push_str(&unicode_titlecase_first(ch));
            first = false;
        } else {
            result.push(ch);
        }
    }
    result
}

pub(crate) fn samemark_string(target: &str, source: &str) -> String {
    use unicode_normalization::UnicodeNormalization;
    use unicode_segmentation::UnicodeSegmentation;

    if source.is_empty() {
        return target.to_string();
    }

    fn split_base_and_marks(grapheme: &str) -> (String, String) {
        let mut base = String::new();
        let mut marks = String::new();
        for ch in grapheme.nfd() {
            if unicode_normalization::char::is_combining_mark(ch) {
                marks.push(ch);
            } else {
                base.push(ch);
            }
        }
        (base, marks)
    }

    let source_parts: Vec<(String, String)> =
        source.graphemes(true).map(split_base_and_marks).collect();
    if source_parts.is_empty() {
        return target.to_string();
    }

    let mut result = String::new();
    for (idx, target_grapheme) in target.graphemes(true).enumerate() {
        let (target_base, _) = split_base_and_marks(target_grapheme);
        let source_idx = idx.min(source_parts.len() - 1);
        let (_, source_marks) = &source_parts[source_idx];
        let mut combined = target_base;
        combined.push_str(source_marks);
        result.push_str(&combined.nfc().collect::<String>());
    }
    result
}

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
        _ => None,
    }
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
        _ => None,
    }
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
    static ND_RE: std::sync::OnceLock<regex::Regex> = std::sync::OnceLock::new();
    let nd_re = ND_RE.get_or_init(|| regex::Regex::new(r"^\p{Nd}$").expect("valid Nd regex"));
    let mut tmp = [0u8; 4];
    if !nd_re.is_match(c.encode_utf8(&mut tmp)) {
        return None;
    }
    let cp = c as u32;
    // Nd blocks are contiguous digits 0..9. Recover the value by offset from zero.
    let offset16 = cp % 16;
    if offset16 <= 9 {
        let base = cp - offset16;
        let all_digits = (0..=9).all(|i| {
            char::from_u32(base + i).is_some_and(|ch| {
                let mut b = [0u8; 4];
                nd_re.is_match(ch.encode_utf8(&mut b))
            })
        });
        if all_digits {
            return Some(offset16);
        }
    }
    let offset10 = cp % 10;
    let base = cp - offset10;
    let all_digits = (0..=9).all(|i| {
        char::from_u32(base + i).is_some_and(|ch| {
            let mut b = [0u8; 4];
            nd_re.is_match(ch.encode_utf8(&mut b))
        })
    });
    if all_digits {
        return Some(offset10);
    }
    None
}

/// Return Unicode character name for a given character
/// Basic implementation covering ASCII and common Latin-1 characters
pub(crate) fn unicode_char_name(ch: char) -> String {
    match ch {
        '0' => "DIGIT ZERO".to_string(),
        '1' => "DIGIT ONE".to_string(),
        '2' => "DIGIT TWO".to_string(),
        '3' => "DIGIT THREE".to_string(),
        '4' => "DIGIT FOUR".to_string(),
        '5' => "DIGIT FIVE".to_string(),
        '6' => "DIGIT SIX".to_string(),
        '7' => "DIGIT SEVEN".to_string(),
        '8' => "DIGIT EIGHT".to_string(),
        '9' => "DIGIT NINE".to_string(),
        'A' => "LATIN CAPITAL LETTER A".to_string(),
        'B' => "LATIN CAPITAL LETTER B".to_string(),
        'C' => "LATIN CAPITAL LETTER C".to_string(),
        'D' => "LATIN CAPITAL LETTER D".to_string(),
        'E' => "LATIN CAPITAL LETTER E".to_string(),
        'F' => "LATIN CAPITAL LETTER F".to_string(),
        'G' => "LATIN CAPITAL LETTER G".to_string(),
        'H' => "LATIN CAPITAL LETTER H".to_string(),
        'I' => "LATIN CAPITAL LETTER I".to_string(),
        'J' => "LATIN CAPITAL LETTER J".to_string(),
        'K' => "LATIN CAPITAL LETTER K".to_string(),
        'L' => "LATIN CAPITAL LETTER L".to_string(),
        'M' => "LATIN CAPITAL LETTER M".to_string(),
        'N' => "LATIN CAPITAL LETTER N".to_string(),
        'O' => "LATIN CAPITAL LETTER O".to_string(),
        'P' => "LATIN CAPITAL LETTER P".to_string(),
        'Q' => "LATIN CAPITAL LETTER Q".to_string(),
        'R' => "LATIN CAPITAL LETTER R".to_string(),
        'S' => "LATIN CAPITAL LETTER S".to_string(),
        'T' => "LATIN CAPITAL LETTER T".to_string(),
        'U' => "LATIN CAPITAL LETTER U".to_string(),
        'V' => "LATIN CAPITAL LETTER V".to_string(),
        'W' => "LATIN CAPITAL LETTER W".to_string(),
        'X' => "LATIN CAPITAL LETTER X".to_string(),
        'Y' => "LATIN CAPITAL LETTER Y".to_string(),
        'Z' => "LATIN CAPITAL LETTER Z".to_string(),
        'a' => "LATIN SMALL LETTER A".to_string(),
        'b' => "LATIN SMALL LETTER B".to_string(),
        'c' => "LATIN SMALL LETTER C".to_string(),
        'd' => "LATIN SMALL LETTER D".to_string(),
        'e' => "LATIN SMALL LETTER E".to_string(),
        'f' => "LATIN SMALL LETTER F".to_string(),
        'g' => "LATIN SMALL LETTER G".to_string(),
        'h' => "LATIN SMALL LETTER H".to_string(),
        'i' => "LATIN SMALL LETTER I".to_string(),
        'j' => "LATIN SMALL LETTER J".to_string(),
        'k' => "LATIN SMALL LETTER K".to_string(),
        'l' => "LATIN SMALL LETTER L".to_string(),
        'm' => "LATIN SMALL LETTER M".to_string(),
        'n' => "LATIN SMALL LETTER N".to_string(),
        'o' => "LATIN SMALL LETTER O".to_string(),
        'p' => "LATIN SMALL LETTER P".to_string(),
        'q' => "LATIN SMALL LETTER Q".to_string(),
        'r' => "LATIN SMALL LETTER R".to_string(),
        's' => "LATIN SMALL LETTER S".to_string(),
        't' => "LATIN SMALL LETTER T".to_string(),
        'u' => "LATIN SMALL LETTER U".to_string(),
        'v' => "LATIN SMALL LETTER V".to_string(),
        'w' => "LATIN SMALL LETTER W".to_string(),
        'x' => "LATIN SMALL LETTER X".to_string(),
        'y' => "LATIN SMALL LETTER Y".to_string(),
        'z' => "LATIN SMALL LETTER Z".to_string(),
        ' ' => "SPACE".to_string(),
        '!' => "EXCLAMATION MARK".to_string(),
        '"' => "QUOTATION MARK".to_string(),
        '#' => "NUMBER SIGN".to_string(),
        '$' => "DOLLAR SIGN".to_string(),
        '%' => "PERCENT SIGN".to_string(),
        '&' => "AMPERSAND".to_string(),
        '\'' => "APOSTROPHE".to_string(),
        '(' => "LEFT PARENTHESIS".to_string(),
        ')' => "RIGHT PARENTHESIS".to_string(),
        '*' => "ASTERISK".to_string(),
        '+' => "PLUS SIGN".to_string(),
        ',' => "COMMA".to_string(),
        '-' => "HYPHEN-MINUS".to_string(),
        '.' => "FULL STOP".to_string(),
        '/' => "SOLIDUS".to_string(),
        ':' => "COLON".to_string(),
        ';' => "SEMICOLON".to_string(),
        '<' => "LESS-THAN SIGN".to_string(),
        '=' => "EQUALS SIGN".to_string(),
        '>' => "GREATER-THAN SIGN".to_string(),
        '?' => "QUESTION MARK".to_string(),
        '@' => "COMMERCIAL AT".to_string(),
        '[' => "LEFT SQUARE BRACKET".to_string(),
        '\\' => "REVERSE SOLIDUS".to_string(),
        ']' => "RIGHT SQUARE BRACKET".to_string(),
        '^' => "CIRCUMFLEX ACCENT".to_string(),
        '_' => "LOW LINE".to_string(),
        '`' => "GRAVE ACCENT".to_string(),
        '{' => "LEFT CURLY BRACKET".to_string(),
        '|' => "VERTICAL LINE".to_string(),
        '}' => "RIGHT CURLY BRACKET".to_string(),
        '~' => "TILDE".to_string(),
        '\n' => "LINE FEED (LF)".to_string(),
        '\r' => "CARRIAGE RETURN (CR)".to_string(),
        '\t' => "CHARACTER TABULATION".to_string(),
        _ => {
            // For other characters, return a generic name with codepoint
            format!("U+{:04X}", ch as u32)
        }
    }
}
