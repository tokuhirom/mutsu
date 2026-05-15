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
        // Latin ligature titlecase mappings
        '\u{FB00}' => "Ff".to_string(),  // ff
        '\u{FB01}' => "Fi".to_string(),  // fi
        '\u{FB02}' => "Fl".to_string(),  // fl
        '\u{FB03}' => "Ffi".to_string(), // ffi
        '\u{FB04}' => "Ffl".to_string(), // ffl
        '\u{FB05}' => "St".to_string(),  // long st
        '\u{FB06}' => "St".to_string(),  // st
        // Default: use uppercase
        _ => ch.to_uppercase().to_string(),
    }
}

pub(crate) fn titlecase_string(s: &str) -> String {
    use unicode_normalization::UnicodeNormalization;
    use unicode_segmentation::UnicodeSegmentation;

    let mut result = String::new();
    let mut graphemes = s.graphemes(true);
    if let Some(first_grapheme) = graphemes.next() {
        result.push_str(&case_convert_grapheme(first_grapheme, CaseOp::Title));
    }
    for g in graphemes {
        result.push_str(g);
    }
    result.nfc().collect()
}

/// Grapheme-aware uppercase conversion.
/// When a base character in a grapheme cluster expands during case conversion
/// (e.g., ligature ff -> FF), combining marks are placed after the first
/// resulting base character.
pub(crate) fn grapheme_uppercase(s: &str) -> String {
    use unicode_normalization::UnicodeNormalization;
    use unicode_segmentation::UnicodeSegmentation;

    let mut result = String::new();
    for g in s.graphemes(true) {
        result.push_str(&case_convert_grapheme(g, CaseOp::Upper));
    }
    result.nfc().collect()
}

/// Lowercase conversion with NFC normalization.
/// Lowercase rarely expands characters, and Rust's str::to_lowercase()
/// correctly handles context-sensitive rules like Greek final sigma.
pub(crate) fn grapheme_lowercase(s: &str) -> String {
    use unicode_normalization::UnicodeNormalization;
    s.to_lowercase().nfc().collect()
}

/// Grapheme-aware foldcase conversion.
/// Foldcase uses NFKD decomposition (not just NFD) to expand compatibility
/// characters like ligatures before applying case folding.
pub(crate) fn grapheme_foldcase(s: &str) -> String {
    use unicode_normalization::UnicodeNormalization;
    use unicode_segmentation::UnicodeSegmentation;

    let mut result = String::new();
    for g in s.graphemes(true) {
        result.push_str(&foldcase_grapheme(g));
    }
    result.nfc().collect()
}

enum CaseOp {
    Upper,
    Title,
}

/// Convert a single grapheme cluster with proper combining mark placement.
/// When a character expands during case conversion (e.g., ligature ff -> FF),
/// any immediately following combining marks are inserted after the first
/// character of the expansion.
///
/// Characters are processed in NFD order to keep combining marks with their
/// correct base character. Prepend characters and other non-cased characters
/// are preserved in their original positions.
fn case_convert_grapheme(grapheme: &str, op: CaseOp) -> String {
    use unicode_normalization::UnicodeNormalization;

    let nfd_chars: Vec<char> = grapheme.nfd().collect();
    if nfd_chars.is_empty() {
        return String::new();
    }

    // Track which base character is the "first cased" for Title mode
    let mut seen_cased = false;
    let mut result = String::new();
    let mut i = 0;
    while i < nfd_chars.len() {
        let ch = nfd_chars[i];
        if unicode_normalization::char::is_combining_mark(ch) {
            // Combining mark: keep as-is
            result.push(ch);
            i += 1;
            continue;
        }

        // Non-combining character: apply case conversion
        let converted = match op {
            CaseOp::Upper => ch.to_uppercase().to_string(),
            CaseOp::Title if !seen_cased && ch.is_alphabetic() => {
                seen_cased = true;
                unicode_titlecase_first(ch)
            }
            CaseOp::Title if seen_cased => ch.to_lowercase().to_string(),
            CaseOp::Title => ch.to_string(), // non-alphabetic before first letter
        };

        let expanded_len = converted.chars().count();
        if expanded_len > 1 {
            // Character expanded: collect following combining marks and insert
            // them after the first character of the expansion
            let mut combiners = Vec::new();
            let mut j = i + 1;
            while j < nfd_chars.len()
                && unicode_normalization::char::is_combining_mark(nfd_chars[j])
            {
                combiners.push(nfd_chars[j]);
                j += 1;
            }

            let mut chars_iter = converted.chars();
            result.push(chars_iter.next().unwrap());
            for &c in &combiners {
                result.push(c);
            }
            for c in chars_iter {
                result.push(c);
            }
            // Skip past the combining marks we already consumed
            i = j;
        } else {
            result.push_str(&converted);
            i += 1;
        }
    }
    result
}

/// Push a single character's foldcase form into the result string.
fn push_foldcase_char(result: &mut String, c: char) {
    match c {
        '\u{00DF}' | '\u{1E9E}' => result.push_str("ss"),
        '\u{0345}' => result.push('\u{03B9}'),
        _ => {
            for lc in c.to_lowercase() {
                result.push(lc);
            }
        }
    }
}

/// Foldcase a single grapheme cluster.
/// Uses NFD decomposition first (preserving compatibility characters), then
/// applies NFKD + lowercase per character. This ensures that when a
/// compatibility character (like a ligature) expands, combining marks from
/// the original grapheme are placed after the first resulting character.
fn foldcase_grapheme(grapheme: &str) -> String {
    use unicode_normalization::UnicodeNormalization;

    let nfd_chars: Vec<char> = grapheme.nfd().collect();
    if nfd_chars.is_empty() {
        return String::new();
    }

    let mut result = String::new();
    let mut i = 0;
    while i < nfd_chars.len() {
        let ch = nfd_chars[i];
        if unicode_normalization::char::is_combining_mark(ch) {
            push_foldcase_char(&mut result, ch);
            i += 1;
            continue;
        }

        // Non-combining character: expand via NFKD then foldcase each part
        let mut folded = String::new();
        for kd_ch in ch.to_string().nfkd() {
            push_foldcase_char(&mut folded, kd_ch);
        }

        let expanded_len = folded.chars().count();
        if expanded_len > 1 {
            // Character expanded: collect following combining marks and insert
            // them after the first character of the expansion
            let mut combiners = Vec::new();
            let mut j = i + 1;
            while j < nfd_chars.len()
                && unicode_normalization::char::is_combining_mark(nfd_chars[j])
            {
                combiners.push(nfd_chars[j]);
                j += 1;
            }

            let mut chars_iter = folded.chars();
            result.push(chars_iter.next().unwrap());
            for &c in &combiners {
                result.push(c);
            }
            for c in chars_iter {
                result.push(c);
            }
            i = j;
        } else {
            result.push_str(&folded);
            i += 1;
        }
    }
    result
}

/// Apply the case pattern of `pattern` to `source`.
/// Each character in the result takes the upper/lower case of the corresponding
/// character in `pattern`. When `pattern` is shorter, the last case applies to
/// all remaining characters.
pub(crate) fn samecase_string(source: &str, pattern: &str) -> String {
    if pattern.is_empty() {
        return source.to_string();
    }
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let mut result = String::new();
    for (pat_idx, ch) in source.chars().enumerate() {
        let pat_ch = pattern_chars[pat_idx.min(pattern_chars.len() - 1)];
        if pat_ch.is_uppercase() {
            for c in ch.to_uppercase() {
                result.push(c);
            }
        } else if pat_ch.is_lowercase() {
            for c in ch.to_lowercase() {
                result.push(c);
            }
        } else {
            // Non-cased pattern character: keep source as-is
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
        _ => unicode_general_rat_value(c),
    }
}

/// General fallback: look up a character's rational value via the Unicode Nl/No table.
/// Handles characters not explicitly listed above (e.g. cuneiform fractions).
fn unicode_general_rat_value(c: char) -> Option<(i64, i64)> {
    let (n, d) = super::unicode_numval_table::lookup_nl_no_value(c)?;
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
    if let Some((n, d)) = super::unicode_numval_table::lookup_nl_no_value(c)
        && d == 1
    {
        return Some(n);
    }
    // Also check Lo (Letter, Other) characters with Unihan numeric values
    if let Some((n, d)) = super::unicode_numval_table::lookup_lo_value(c)
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
    static ND_RE: std::sync::OnceLock<regex::Regex> = std::sync::OnceLock::new();
    let nd_re = ND_RE.get_or_init(|| regex::Regex::new(r"^\p{Nd}$").expect("valid Nd regex"));
    let mut tmp = [0u8; 4];
    if !nd_re.is_match(c.encode_utf8(&mut tmp)) {
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
        let is_nd = char::from_u32(prev).is_some_and(|ch| {
            let mut b = [0u8; 4];
            nd_re.is_match(ch.encode_utf8(&mut b))
        });
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

/// Check if a codepoint is a Unicode noncharacter.
fn is_noncharacter(cp: u32) -> bool {
    // U+FDD0..U+FDEF
    if (0xFDD0..=0xFDEF).contains(&cp) {
        return true;
    }
    // U+xFFFE and U+xFFFF for each plane (0x0..0x10)
    let low = cp & 0xFFFF;
    if low == 0xFFFE || low == 0xFFFF {
        let plane = cp >> 16;
        return plane <= 0x10;
    }
    false
}

/// Check if a codepoint is a Unicode control character (Cc category).
fn is_control_char(cp: u32) -> bool {
    cp <= 0x1F || (0x7F..=0x9F).contains(&cp)
}

/// Return the Unicode character name for a codepoint (u32).
/// Handles all special categories: control, noncharacter, reserved, etc.
pub(crate) fn unicode_char_name_by_codepoint(cp: u32) -> String {
    // Noncharacters
    if is_noncharacter(cp) {
        return format!("<noncharacter-{:04X}>", cp);
    }
    // Try to convert to char
    if let Some(ch) = char::from_u32(cp) {
        // Try unicode_names2 first
        if let Some(name) = unicode_names2::name(ch) {
            return name.to_string();
        }
        // Control characters without a name
        if is_control_char(cp) {
            return format!("<control-{:04X}>", cp);
        }
        // Has a char but no name and not control -> reserved/unassigned
        format!("<reserved-{:04X}>", cp)
    } else {
        // Invalid char (surrogates etc.)
        format!("<reserved-{:04X}>", cp)
    }
}

/// Return Unicode character name for a given character
pub(crate) fn unicode_char_name(ch: char) -> String {
    unicode_char_name_by_codepoint(ch as u32)
}

/// Return the uniname result for an integer codepoint.
/// Returns Err for out-of-range codepoints (>= 0x110000).
/// Returns `<illegal>` for negative codepoints.
pub(crate) fn uniname_from_int(codepoint: i64) -> Result<String, crate::value::RuntimeError> {
    if codepoint < 0 {
        return Ok("<illegal>".to_string());
    }
    let cp = codepoint as u64;
    if cp > 0x10FFFF {
        let msg = format!("Unassigned codepoint: 0x{:X}", cp);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
        let ex =
            crate::value::Value::make_instance(crate::symbol::Symbol::intern("X::AdHoc"), attrs);
        let mut err = crate::value::RuntimeError::new(&msg);
        err.exception = Some(Box::new(ex));
        return Err(err);
    }
    Ok(unicode_char_name_by_codepoint(cp as u32))
}

/// Return the Unicode General Category abbreviation for a character.
pub(crate) fn unicode_general_category(ch: char) -> String {
    use std::sync::OnceLock;

    // Build regex matchers for each General Category lazily.
    // We check the most common categories first.
    static CATEGORIES: OnceLock<Vec<(&'static str, regex::Regex)>> = OnceLock::new();
    let cats = CATEGORIES.get_or_init(|| {
        // Note: "Cs" (surrogates) is omitted because the regex crate does not support it,
        // and Rust chars cannot be surrogates anyway.
        let cats_list: &[&str] = &[
            "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps",
            "Pe", "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Co",
        ];
        cats_list
            .iter()
            .map(|cat| {
                let re = regex::Regex::new(&format!(r"^\p{{{cat}}}$")).expect("valid regex");
                (*cat, re)
            })
            .collect()
    });

    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    for (cat, re) in cats.iter() {
        if re.is_match(s) {
            return cat.to_string();
        }
    }
    "Cn".to_string()
}

/// Return the Unicode Script name for a character (e.g. "Latin", "Hangul").
pub(crate) fn unicode_script_name(ch: char) -> String {
    use std::sync::OnceLock;

    static SCRIPTS: OnceLock<Vec<(&'static str, regex::Regex)>> = OnceLock::new();
    let scripts = SCRIPTS.get_or_init(|| {
        let script_list: &[&str] = &[
            "Adlam",
            "Ahom",
            "Anatolian_Hieroglyphs",
            "Arabic",
            "Armenian",
            "Avestan",
            "Balinese",
            "Bamum",
            "Bassa_Vah",
            "Batak",
            "Bengali",
            "Bhaiksuki",
            "Bopomofo",
            "Brahmi",
            "Braille",
            "Buginese",
            "Buhid",
            "Canadian_Aboriginal",
            "Carian",
            "Caucasian_Albanian",
            "Chakma",
            "Cham",
            "Cherokee",
            "Chorasmian",
            "Common",
            "Coptic",
            "Cuneiform",
            "Cypriot",
            "Cypro_Minoan",
            "Cyrillic",
            "Deseret",
            "Devanagari",
            "Dives_Akuru",
            "Dogra",
            "Duployan",
            "Egyptian_Hieroglyphs",
            "Elbasan",
            "Elymaic",
            "Ethiopic",
            "Georgian",
            "Glagolitic",
            "Gothic",
            "Grantha",
            "Greek",
            "Gujarati",
            "Gunjala_Gondi",
            "Gurmukhi",
            "Han",
            "Hangul",
            "Hanifi_Rohingya",
            "Hanunoo",
            "Hatran",
            "Hebrew",
            "Hiragana",
            "Imperial_Aramaic",
            "Inherited",
            "Inscriptional_Pahlavi",
            "Inscriptional_Parthian",
            "Javanese",
            "Kaithi",
            "Kannada",
            "Katakana",
            "Kayah_Li",
            "Kharoshthi",
            "Khitan_Small_Script",
            "Khmer",
            "Khojki",
            "Khudawadi",
            "Lao",
            "Latin",
            "Lepcha",
            "Limbu",
            "Linear_A",
            "Linear_B",
            "Lisu",
            "Lycian",
            "Lydian",
            "Mahajani",
            "Makasar",
            "Malayalam",
            "Mandaic",
            "Manichaean",
            "Marchen",
            "Masaram_Gondi",
            "Medefaidrin",
            "Meetei_Mayek",
            "Mende_Kikakui",
            "Meroitic_Cursive",
            "Meroitic_Hieroglyphs",
            "Miao",
            "Modi",
            "Mongolian",
            "Mro",
            "Multani",
            "Myanmar",
            "Nabataean",
            "Nandinagari",
            "New_Tai_Lue",
            "Newa",
            "Nko",
            "Nushu",
            "Nyiakeng_Puachue_Hmong",
            "Ogham",
            "Ol_Chiki",
            "Old_Hungarian",
            "Old_Italic",
            "Old_North_Arabian",
            "Old_Permic",
            "Old_Persian",
            "Old_Sogdian",
            "Old_South_Arabian",
            "Old_Turkic",
            "Old_Uyghur",
            "Oriya",
            "Osage",
            "Osmanya",
            "Pahawh_Hmong",
            "Palmyrene",
            "Pau_Cin_Hau",
            "Phags_Pa",
            "Phoenician",
            "Psalter_Pahlavi",
            "Rejang",
            "Runic",
            "Samaritan",
            "Saurashtra",
            "Sharada",
            "Shavian",
            "Siddham",
            "SignWriting",
            "Sinhala",
            "Sogdian",
            "Sora_Sompeng",
            "Soyombo",
            "Sundanese",
            "Syloti_Nagri",
            "Syriac",
            "Tagalog",
            "Tagbanwa",
            "Tai_Le",
            "Tai_Tham",
            "Tai_Viet",
            "Takri",
            "Tamil",
            "Tangsa",
            "Tangut",
            "Telugu",
            "Thaana",
            "Thai",
            "Tibetan",
            "Tifinagh",
            "Tirhuta",
            "Toto",
            "Ugaritic",
            "Vai",
            "Vithkuqi",
            "Wancho",
            "Warang_Citi",
            "Yezidi",
            "Yi",
            "Zanabazar_Square",
        ];
        script_list
            .iter()
            .filter_map(|name| {
                let re = regex::Regex::new(&format!(r"^\p{{Script={name}}}$")).ok()?;
                Some((*name, re))
            })
            .collect()
    });

    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    for (name, re) in scripts.iter() {
        if re.is_match(s) {
            return name.to_string();
        }
    }
    "Unknown".to_string()
}
