use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static UNICODE_PROP_CACHE: RefCell<HashMap<String, Option<regex::Regex>>> =
        RefCell::new(HashMap::new());
}

pub(super) fn check_unicode_property(name: &str, c: char) -> bool {
    // Handle Unicode block properties (InXxx) separately since the regex crate
    // doesn't support the unicode-block feature
    if let Some(block_name) = name.strip_prefix("In")
        && let Some((start, end)) = unicode_block_range(block_name)
    {
        let cp = c as u32;
        return cp >= start && cp <= end;
    }
    // Handle parameterized properties: bc<L> → Bidi_Class=L, sc<Latin> → Script=Latin
    if let Some(open) = name.find('<')
        && let Some(close) = name.find('>')
    {
        let prop = &name[..open];
        let value = &name[open + 1..close];
        // BiDi class is not supported by the regex crate; handle manually
        if prop == "bc" {
            return check_bidi_class(value, c);
        }
        let full_prop = match prop {
            "sc" => format!("Script={}", value),
            "gc" => format!("General_Category={}", value),
            _ => format!("{}={}", prop, value),
        };
        return UNICODE_PROP_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            let entry = cache.entry(full_prop.clone()).or_insert_with(|| {
                let pattern = format!(r"^\p{{{}}}", full_prop);
                regex::Regex::new(&pattern).ok()
            });
            match entry {
                Some(re) => re.is_match(&c.to_string()),
                None => false,
            }
        });
    }
    UNICODE_PROP_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        let entry = cache.entry(name.to_string()).or_insert_with(|| {
            let pattern = format!(r"^\p{{{}}}", name);
            regex::Regex::new(&pattern).ok()
        });
        match entry {
            Some(re) => re.is_match(&c.to_string()),
            None => false,
        }
    })
}

/// Check a Unicode property with arguments, e.g. NumericValue(0 ^..^ 1) or name(/:s LATIN SMALL/).
pub(super) fn check_unicode_property_with_args(prop: &str, args: &str, c: char) -> bool {
    let prop_lower = prop.to_lowercase();
    match prop_lower.as_str() {
        "numericvalue" | "numeric_value" | "nv" => check_numeric_value_property(args, c),
        "name" | "na" => check_name_property(args, c),
        _ => {
            // Fall back to simple property check (ignore args)
            check_unicode_property(prop, c)
        }
    }
}

/// Check if a character's numeric value matches a range expression like "0 ^..^ 1".
fn check_numeric_value_property(args: &str, c: char) -> bool {
    let nv = match unicode_numeric_value(c) {
        Some(v) => v,
        None => return false,
    };
    // Parse range expression: "low ^..^ high", "low .. high", "low ^.. high", "low ..^ high"
    let args = args.trim();
    if let Some((low_str, rest)) = args.split_once("..") {
        let exclude_low = low_str.trim().ends_with('^');
        let exclude_high = rest.trim().starts_with('^');
        let low_str = low_str.trim().trim_end_matches('^').trim();
        let high_str = rest.trim().trim_start_matches('^').trim();
        let low: f64 = low_str.parse().unwrap_or(0.0);
        let high: f64 = high_str.parse().unwrap_or(0.0);
        let low_ok = if exclude_low { nv > low } else { nv >= low };
        let high_ok = if exclude_high { nv < high } else { nv <= high };
        low_ok && high_ok
    } else {
        // Single value comparison
        if let Ok(target) = args.parse::<f64>() {
            (nv - target).abs() < f64::EPSILON
        } else {
            false
        }
    }
}

/// Get the Unicode numeric value of a character, if any.
fn unicode_numeric_value(c: char) -> Option<f64> {
    // Common fractions
    match c {
        '¼' => return Some(0.25),
        '½' => return Some(0.5),
        '¾' => return Some(0.75),
        '⅐' => return Some(1.0 / 7.0),
        '⅑' => return Some(1.0 / 9.0),
        '⅒' => return Some(0.1),
        '⅓' => return Some(1.0 / 3.0),
        '⅔' => return Some(2.0 / 3.0),
        '⅕' => return Some(0.2),
        '⅖' => return Some(0.4),
        '⅗' => return Some(0.6),
        '⅘' => return Some(0.8),
        '⅙' => return Some(1.0 / 6.0),
        '⅚' => return Some(5.0 / 6.0),
        '⅛' => return Some(0.125),
        '⅜' => return Some(0.375),
        '⅝' => return Some(0.625),
        '⅞' => return Some(0.875),
        '↉' => return Some(0.0),
        '²' => return Some(2.0),
        '³' => return Some(3.0),
        '¹' => return Some(1.0),
        '⁰' => return Some(0.0),
        '⁴' => return Some(4.0),
        '⁵' => return Some(5.0),
        '⁶' => return Some(6.0),
        '⁷' => return Some(7.0),
        '⁸' => return Some(8.0),
        '⁹' => return Some(9.0),
        '₀' => return Some(0.0),
        '₁' => return Some(1.0),
        '₂' => return Some(2.0),
        '₃' => return Some(3.0),
        '₄' => return Some(4.0),
        '₅' => return Some(5.0),
        '₆' => return Some(6.0),
        '₇' => return Some(7.0),
        '₈' => return Some(8.0),
        '₉' => return Some(9.0),
        _ => {}
    }
    if c.is_ascii_digit() {
        return Some((c as u32 - '0' as u32) as f64);
    }
    // Check various Unicode digit ranges
    let cp = c as u32;
    let digit_ranges: &[(u32, u32)] = &[
        (0x0660, 0x0669),
        (0x06F0, 0x06F9),
        (0x0966, 0x096F),
        (0x09E6, 0x09EF),
        (0x0A66, 0x0A6F),
        (0x0AE6, 0x0AEF),
        (0x0B66, 0x0B6F),
        (0x0BE6, 0x0BEF),
        (0x0C66, 0x0C6F),
        (0x0CE6, 0x0CEF),
        (0x0D66, 0x0D6F),
        (0x0E50, 0x0E59),
        (0x0F20, 0x0F29),
        (0x1040, 0x1049),
        (0x17E0, 0x17E9),
        (0x1810, 0x1819),
        (0xFF10, 0xFF19),
    ];
    for &(start, end) in digit_ranges {
        if (start..=end).contains(&cp) {
            return Some((cp - start) as f64);
        }
    }
    None
}

/// Check if a character's Unicode name matches a regex pattern.
fn check_name_property(args: &str, c: char) -> bool {
    let args = args.trim();
    // The argument can be a regex like /:s LATIN SMALL LETTER/
    let pattern = if let Some(inner) = args.strip_prefix('/') {
        inner.strip_suffix('/').unwrap_or(inner)
    } else {
        args
    };
    // Get Unicode name of the character
    let name = unicode_char_name(c);
    if name.is_empty() {
        return false;
    }
    // Handle :s (sigspace) flag — replace whitespace in pattern with \s+
    let pattern = if let Some(rest) = pattern.strip_prefix(":s ") {
        rest.split_whitespace().collect::<Vec<_>>().join(r"\s+")
    } else if let Some(rest) = pattern.strip_prefix(":s\t") {
        rest.split_whitespace().collect::<Vec<_>>().join(r"\s+")
    } else {
        pattern.split_whitespace().collect::<Vec<_>>().join(r"\s+")
    };
    // Match against the name using a regex
    if let Ok(re) = regex::Regex::new(&pattern) {
        re.is_match(&name)
    } else {
        false
    }
}

/// Get the Unicode name of a character.
fn unicode_char_name(c: char) -> String {
    // Use the unicode_names2 crate if available, otherwise fall back to manual lookup
    unicode_names2::name(c)
        .map(|n| n.to_string())
        .unwrap_or_default()
}

/// Check BiDi class of a character (regex crate doesn't support Bidi_Class).
fn check_bidi_class(class: &str, c: char) -> bool {
    match class {
        "L" => {
            // Left-to-Right: most alphabetic, syllabic, Han ideographic, non-currency symbols
            c.is_alphabetic()
                && !matches!(
                    c as u32,
                    0x0590..=0x05FF
                        | 0x07C0..=0x089F
                        | 0xFB1D..=0xFDFF
                        | 0xFE70..=0xFEFF
                        | 0x10800..=0x10FFF
                        | 0x1E800..=0x1EFFF
                        | 0x0600..=0x07BF
                        | 0x08A0..=0x08FF
                )
        }
        "R" => {
            // Right-to-Left: Hebrew, NKo, etc.
            matches!(
                c as u32,
                0x0590..=0x05FF | 0x07C0..=0x089F | 0xFB1D..=0xFB4F | 0x10800..=0x10FFF | 0x1E800..=0x1EFFF
            )
        }
        "EN" => {
            // European Number
            c.is_ascii_digit()
                || matches!(
                    c as u32,
                    0x06F0..=0x06F9 | 0x2070..=0x2079 | 0x2080..=0x2089
                )
        }
        "ES" => {
            // European Number Separator
            matches!(c, '+' | '-')
        }
        "ET" => {
            // European Number Terminator: currency symbols, #, %, etc.
            matches!(
                c as u32,
                0x0023..=0x0025
                    | 0x00A2..=0x00A5
                    | 0x00B0..=0x00B1
                    | 0x0609..=0x060A
                    | 0x066A
                    | 0x09F2..=0x09F3
                    | 0x20A0..=0x20CF
            ) || c == '#'
                || c == '%'
                || c == '\u{B0}'
                || c == '\u{B1}'
        }
        "WS" => {
            // Whitespace
            matches!(
                c as u32,
                0x000C | 0x0020 | 0x1680 | 0x2000..=0x200A | 0x2028 | 0x205F | 0x3000
            )
        }
        "AN" => {
            // Arabic Number
            matches!(c as u32, 0x0600..=0x0605 | 0x0660..=0x0669 | 0x066B..=0x066C)
        }
        "CS" => {
            // Common Number Separator
            matches!(
                c,
                ',' | '.'
                    | ':'
                    | '\u{00A0}'
                    | '\u{060C}'
                    | '\u{202F}'
                    | '\u{2044}'
                    | '\u{FE50}'
                    | '\u{FE52}'
                    | '\u{FF0C}'
                    | '\u{FF0E}'
            )
        }
        "ON" => {
            // Other Neutral: not easily categorizable, approximate
            !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control()
        }
        _ => false,
    }
}

/// Look up a Unicode block range by name. Returns (start, end) inclusive.
/// Names are matched case-insensitively with non-alphanumeric chars stripped.
fn unicode_block_range(name: &str) -> Option<(u32, u32)> {
    let normalized: String = name
        .chars()
        .filter(|c| c.is_alphanumeric())
        .map(|c| c.to_ascii_lowercase())
        .collect();
    // Unicode blocks table (Unicode 15.0)
    match normalized.as_str() {
        "basiclatin" => Some((0x0000, 0x007F)),
        "latin1supplement" | "latin1" => Some((0x0080, 0x00FF)),
        "latinextendeda" => Some((0x0100, 0x017F)),
        "latinextendedb" => Some((0x0180, 0x024F)),
        "ipaextensions" => Some((0x0250, 0x02AF)),
        "spacingmodifierletters" => Some((0x02B0, 0x02FF)),
        "combiningdiacriticalmarks" => Some((0x0300, 0x036F)),
        "greekandcoptic" | "greek" => Some((0x0370, 0x03FF)),
        "cyrillic" => Some((0x0400, 0x04FF)),
        "cyrillicsupplement" | "cyrillicsupplementary" => Some((0x0500, 0x052F)),
        "armenian" => Some((0x0530, 0x058F)),
        "hebrew" => Some((0x0590, 0x05FF)),
        "arabic" => Some((0x0600, 0x06FF)),
        "syriac" => Some((0x0700, 0x074F)),
        "arabicsupplement" => Some((0x0750, 0x077F)),
        "thaana" => Some((0x0780, 0x07BF)),
        "nko" => Some((0x07C0, 0x07FF)),
        "samaritan" => Some((0x0800, 0x083F)),
        "mandaic" => Some((0x0840, 0x085F)),
        "devanagari" => Some((0x0900, 0x097F)),
        "bengali" => Some((0x0980, 0x09FF)),
        "gurmukhi" => Some((0x0A00, 0x0A7F)),
        "gujarati" => Some((0x0A80, 0x0AFF)),
        "oriya" => Some((0x0B00, 0x0B7F)),
        "tamil" => Some((0x0B80, 0x0BFF)),
        "telugu" => Some((0x0C00, 0x0C7F)),
        "kannada" => Some((0x0C80, 0x0CFF)),
        "malayalam" => Some((0x0D00, 0x0D7F)),
        "sinhala" => Some((0x0D80, 0x0DFF)),
        "thai" => Some((0x0E00, 0x0E7F)),
        "lao" => Some((0x0E80, 0x0EFF)),
        "tibetan" => Some((0x0F00, 0x0FFF)),
        "myanmar" => Some((0x1000, 0x109F)),
        "georgian" => Some((0x10A0, 0x10FF)),
        "hanguljamo" => Some((0x1100, 0x11FF)),
        "ethiopic" => Some((0x1200, 0x137F)),
        "ethiopicsupplement" => Some((0x1380, 0x139F)),
        "cherokee" => Some((0x13A0, 0x13FF)),
        "unifiedcanadianaboriginalsyllabics" => Some((0x1400, 0x167F)),
        "ogham" => Some((0x1680, 0x169F)),
        "runic" => Some((0x16A0, 0x16FF)),
        "tagalog" => Some((0x1700, 0x171F)),
        "hanunoo" => Some((0x1720, 0x173F)),
        "buhid" => Some((0x1740, 0x175F)),
        "tagbanwa" => Some((0x1760, 0x177F)),
        "khmer" => Some((0x1780, 0x17FF)),
        "mongolian" => Some((0x1800, 0x18AF)),
        "limbu" => Some((0x1900, 0x194F)),
        "taile" => Some((0x1950, 0x197F)),
        "newtailue" => Some((0x1980, 0x19DF)),
        "khmersymbols" => Some((0x19E0, 0x19FF)),
        "buginese" => Some((0x1A00, 0x1A1F)),
        "balinese" => Some((0x1B00, 0x1B7F)),
        "sundanese" => Some((0x1B80, 0x1BBF)),
        "lepcha" => Some((0x1C00, 0x1C4F)),
        "olchiki" => Some((0x1C50, 0x1C7F)),
        "phoneticextensions" => Some((0x1D00, 0x1D7F)),
        "phoneticextensionssupplement" => Some((0x1D80, 0x1DBF)),
        "combiningdiacriticalmarkssupplement" => Some((0x1DC0, 0x1DFF)),
        "latinextendedadditional" => Some((0x1E00, 0x1EFF)),
        "greekextended" => Some((0x1F00, 0x1FFF)),
        "generalpunctuation" => Some((0x2000, 0x206F)),
        "superscriptsandsubscripts" => Some((0x2070, 0x209F)),
        "currencysymbols" => Some((0x20A0, 0x20CF)),
        "combiningdiacriticalmarksforsymbols" | "combiningmarksforsymbols" => {
            Some((0x20D0, 0x20FF))
        }
        "letterlikesymbols" => Some((0x2100, 0x214F)),
        "numberforms" => Some((0x2150, 0x218F)),
        "arrows" => Some((0x2190, 0x21FF)),
        "mathematicaloperators" => Some((0x2200, 0x22FF)),
        "miscellaneoustechnical" => Some((0x2300, 0x23FF)),
        "controlpictures" => Some((0x2400, 0x243F)),
        "opticalcharacterrecognition" => Some((0x2440, 0x245F)),
        "enclosedalphanumerics" => Some((0x2460, 0x24FF)),
        "boxdrawing" => Some((0x2500, 0x257F)),
        "blockelements" => Some((0x2580, 0x259F)),
        "geometricshapes" => Some((0x25A0, 0x25FF)),
        "miscellaneoussymbols" => Some((0x2600, 0x26FF)),
        "dingbats" => Some((0x2700, 0x27BF)),
        "miscellaneousmathematicalsymbolsa" => Some((0x27C0, 0x27EF)),
        "supplementalarrowsa" => Some((0x27F0, 0x27FF)),
        "braillepatterns" => Some((0x2800, 0x28FF)),
        "supplementalarrowsb" => Some((0x2900, 0x297F)),
        "miscellaneousmathematicalsymbolsb" => Some((0x2980, 0x29FF)),
        "supplementalmathematicaloperators" => Some((0x2A00, 0x2AFF)),
        "miscellaneoussymbolsandarrows" => Some((0x2B00, 0x2BFF)),
        "cjkradicalssupplement" => Some((0x2E80, 0x2EFF)),
        "kangxiradicals" => Some((0x2F00, 0x2FDF)),
        "ideographicdescriptioncharacters" => Some((0x2FF0, 0x2FFF)),
        "cjksymbolsandpunctuation" => Some((0x3000, 0x303F)),
        "hiragana" => Some((0x3040, 0x309F)),
        "katakana" => Some((0x30A0, 0x30FF)),
        "bopomofo" => Some((0x3100, 0x312F)),
        "hangulcompatibilityjamo" => Some((0x3130, 0x318F)),
        "kanbun" => Some((0x3190, 0x319F)),
        "bopomofoextended" => Some((0x31A0, 0x31BF)),
        "katakanaphoneticextensions" => Some((0x31F0, 0x31FF)),
        "enclosedcjklettersandmonths" => Some((0x3200, 0x32FF)),
        "cjkcompatibility" => Some((0x3300, 0x33FF)),
        "cjkunifiedideographsextensiona" => Some((0x3400, 0x4DBF)),
        "yijinghexagramsymbols" => Some((0x4DC0, 0x4DFF)),
        "cjkunifiedideographs" => Some((0x4E00, 0x9FFF)),
        "yisyllables" => Some((0xA000, 0xA48F)),
        "yiradicals" => Some((0xA490, 0xA4CF)),
        "hangulsyllables" => Some((0xAC00, 0xD7AF)),
        "highsurrogates" => Some((0xD800, 0xDB7F)),
        "highprivateusearea"
        | "highprivateuseareausesurrogates"
        | "highprivateuserogatessurrogates"
        | "highprivateuseareauses"
        | "highprivateuseareausesurrogate"
        | "highprivateuseareause" => Some((0xDB80, 0xDBFF)),
        "lowsurrogates" => Some((0xDC00, 0xDFFF)),
        "privateusearea" => Some((0xE000, 0xF8FF)),
        "cjkcompatibilityideographs" => Some((0xF900, 0xFAFF)),
        "alphabeticpresentationforms" => Some((0xFB00, 0xFB4F)),
        "arabicpresentationformsa" => Some((0xFB50, 0xFDFF)),
        "variationselectors" => Some((0xFE00, 0xFE0F)),
        "combininghalfmarks" => Some((0xFE20, 0xFE2F)),
        "cjkcompatibilityforms" => Some((0xFE30, 0xFE4F)),
        "smallformvariants" => Some((0xFE50, 0xFE6F)),
        "arabicpresentationformsb" => Some((0xFE70, 0xFEFF)),
        "halfwidthandfullwidthforms" => Some((0xFF00, 0xFFEF)),
        "specials" => Some((0xFFF0, 0xFFFF)),
        "linearbsyllabary" => Some((0x10000, 0x1007F)),
        "linearbideograms" => Some((0x10080, 0x100FF)),
        "aegeannumbers" => Some((0x10100, 0x1013F)),
        "olditalic" => Some((0x10300, 0x1032F)),
        "gothic" => Some((0x10330, 0x1034F)),
        "ugaritic" => Some((0x10380, 0x1039F)),
        "deseret" => Some((0x10400, 0x1044F)),
        "shavian" => Some((0x10450, 0x1047F)),
        "osmanya" => Some((0x10480, 0x104AF)),
        "cypriotsyllabary" => Some((0x10800, 0x1083F)),
        "byzantinemusicalsymbols" => Some((0x1D000, 0x1D0FF)),
        "musicalsymbols" => Some((0x1D100, 0x1D1FF)),
        "taixuanjingsymbols" => Some((0x1D300, 0x1D35F)),
        "mathematicalalphanumericsymbols" => Some((0x1D400, 0x1D7FF)),
        "cjkunifiedideographsextensionb" => Some((0x20000, 0x2A6DF)),
        "cjkcompatibilityideographssupplement" => Some((0x2F800, 0x2FA1F)),
        "tags" => Some((0xE0000, 0xE007F)),
        "variationselectorssupplement" => Some((0xE0100, 0xE01EF)),
        "supplementaryprivateuseareaa" => Some((0xF0000, 0xFFFFF)),
        "supplementaryprivateuseareab" => Some((0x100000, 0x10FFFF)),
        _ => None,
    }
}
