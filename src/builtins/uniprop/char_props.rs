use crate::value::Value;
use std::sync::OnceLock;

/// Unicode Block lookup — delegates to the tables module.
pub(crate) fn unicode_block(ch: char) -> String {
    crate::builtins::uniprop_tables::unicode_block(ch)
}

/// Jamo short name lookup — delegates to the tables module.
pub(crate) fn unicode_jamo_short_name(ch: char) -> String {
    crate::builtins::uniprop_tables::unicode_jamo_short_name(ch)
}

/// Look up Numeric_Value for uniprop.
pub(crate) fn unicode_numeric_value(ch: char) -> Value {
    // Check if it's a digit (Nd)
    if let Some(d) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
        return Value::Int(d as i64);
    }
    // Check if it's a fraction
    if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
        return crate::value::make_rat(n, d);
    }
    // Check if it's an integer numeric
    if let Some(i) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
        return Value::Int(i);
    }
    Value::Num(f64::NAN)
}

/// Look up Numeric_Type for uniprop.
pub(crate) fn unicode_numeric_type(ch: char) -> String {
    // Check Nd (Decimal)
    static ND_RE: OnceLock<regex::Regex> = OnceLock::new();
    let nd_re = ND_RE.get_or_init(|| regex::Regex::new(r"^\p{Nd}$").unwrap());
    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    if nd_re.is_match(s) {
        return "Decimal".to_string();
    }
    // Digit type: superscripts/subscripts that are No but behave as Digit
    // U+00B2 SUPERSCRIPT TWO, U+00B3 SUPERSCRIPT THREE, U+00B9 SUPERSCRIPT ONE
    // U+2070-U+2079, U+2080-U+2089
    let cp = ch as u32;
    if cp == 0x00B2
        || cp == 0x00B3
        || cp == 0x00B9
        || (0x2070..=0x2079).contains(&cp)
        || (0x2080..=0x2089).contains(&cp)
    {
        return "Digit".to_string();
    }
    // Check No (Numeric — includes fractions and other numeric chars)
    static NO_RE: OnceLock<regex::Regex> = OnceLock::new();
    let no_re = NO_RE.get_or_init(|| regex::Regex::new(r"^\p{No}$").unwrap());
    if no_re.is_match(s) {
        return "Numeric".to_string();
    }
    // Check Nl (Letter number)
    static NL_RE: OnceLock<regex::Regex> = OnceLock::new();
    let nl_re = NL_RE.get_or_init(|| regex::Regex::new(r"^\p{Nl}$").unwrap());
    if nl_re.is_match(s) {
        return "Numeric".to_string();
    }
    "None".to_string()
}

/// Case mapping properties.
pub(crate) fn case_mapping(ch: char, prop: &str) -> String {
    match prop {
        "Lowercase_Mapping" | "lc" => ch.to_lowercase().to_string(),
        "Uppercase_Mapping" | "uc" => ch.to_uppercase().to_string(),
        "Titlecase_Mapping" | "tc" => crate::builtins::unicode::unicode_titlecase_first(ch),
        "Case_Folding" | "cf" => {
            // Use the same unicode_foldcase logic as .fc
            crate::builtins::methods_0arg::unicode_foldcase(&ch.to_string())
        }
        "Simple_Uppercase_Mapping" | "suc" => {
            let uc: String = ch.to_uppercase().collect();
            // Simple mapping: only valid if it maps to exactly one char
            if uc.chars().count() == 1 {
                uc
            } else {
                ch.to_string()
            }
        }
        "Simple_Lowercase_Mapping" | "slc" => {
            let lc: String = ch.to_lowercase().collect();
            if lc.chars().count() == 1 {
                lc
            } else {
                ch.to_string()
            }
        }
        "Simple_Titlecase_Mapping" | "stc" => {
            let tc = crate::builtins::unicode::unicode_titlecase_first(ch);
            if tc.chars().count() == 1 {
                tc
            } else {
                ch.to_string()
            }
        }
        _ => ch.to_string(),
    }
}

/// Bidi_Class property.
pub(crate) fn unicode_bidi_class(ch: char) -> String {
    let cp = ch as u32;
    // Common Bidi_Class values
    match cp {
        0x202A => "LRE",
        0x202B => "RLE",
        0x202C => "PDF",
        0x202D => "LRO",
        0x202E => "RLO",
        0x200E => "L",
        0x200F => "R",
        0x2066 => "LRI",
        0x2067 => "RLI",
        0x2068 => "FSI",
        0x2069 => "PDI",
        _ => {
            // Use General Category heuristics
            let gc = crate::builtins::unicode::unicode_general_category(ch);
            match gc.as_str() {
                "Lu" | "Ll" | "Lt" | "Lm" | "Lo" | "Nd" | "Nl" | "No" => "L",
                "Mn" | "Me" | "Cf" => "NSM",
                "Zs" => "WS",
                "Zl" | "Zp" => "B",
                "Cc" => {
                    if cp <= 0x001F || (0x007F..=0x009F).contains(&cp) {
                        if cp == 0x000A
                            || cp == 0x000D
                            || cp == 0x001C
                            || cp == 0x001D
                            || cp == 0x001E
                            || cp == 0x001F
                            || cp == 0x0085
                        {
                            "B"
                        } else {
                            "BN"
                        }
                    } else {
                        "BN"
                    }
                }
                _ => "L",
            }
        }
    }
    .to_string()
}

/// Decomposition_Type property.
pub(crate) fn unicode_decomposition_type(ch: char) -> String {
    use unicode_normalization::UnicodeNormalization;
    let nfd: String = ch.to_string().nfd().collect();
    if nfd.len() != ch.len_utf8() || nfd != ch.to_string() {
        // Has a canonical decomposition
        return "Canonical".to_string();
    }
    let nfkd: String = ch.to_string().chars().collect::<String>().nfkd().collect();
    if nfkd != ch.to_string() {
        // Has a compatibility decomposition (but not canonical)
        // Determine the specific type from the codepoint range
        return compatibility_decomposition_type(ch);
    }
    "None".to_string()
}

fn compatibility_decomposition_type(ch: char) -> String {
    let cp = ch as u32;
    // Heuristic mapping based on common compatibility decomposition types
    match cp {
        0x00A0 => "Nobreak".to_string(),                 // NO-BREAK SPACE
        0x00B2 | 0x00B3 | 0x00B9 => "Super".to_string(), // Superscripts
        0x2070..=0x207E => "Super".to_string(),
        0x2080..=0x208E => "Sub".to_string(),
        0x2100..=0x214F => "Compat".to_string(), // Letterlike symbols
        0x2150..=0x218F => "Fraction".to_string(), // Number forms
        0x2460..=0x24FF => "Circle".to_string(), // Enclosed alphanumerics
        0x3300..=0x33FF => "Square".to_string(), // CJK compat
        0xFB00..=0xFB06 => "Compat".to_string(), // Latin ligatures
        0xFB50..=0xFDFF => "Isolated".to_string(), // Arabic pres forms
        0xFE30..=0xFE4F => "Vertical".to_string(), // CJK compat forms
        0xFE70..=0xFEFF => "Isolated".to_string(), // Arabic pres forms B
        0xFF01..=0xFF5E => "Wide".to_string(),   // Fullwidth
        0xFF61..=0xFFDC => "Narrow".to_string(), // Halfwidth
        0xFFE0..=0xFFE6 => "Wide".to_string(),
        0xFFE8..=0xFFEE => "Narrow".to_string(),
        _ => "Compat".to_string(),
    }
}

/// East_Asian_Width property.
pub(crate) fn unicode_east_asian_width(ch: char) -> String {
    let cp = ch as u32;
    // Simplified East_Asian_Width lookup
    match cp {
        // Fullwidth
        0xFF01..=0xFF60 | 0xFFE0..=0xFFE6 => "Fullwidth",
        // Halfwidth
        0xFF61..=0xFFDC | 0xFFE8..=0xFFEE => "Halfwidth",
        // Wide (CJK)
        0x1100..=0x115F
        | 0x2329..=0x232A
        | 0x2E80..=0x303E
        | 0x3041..=0x33BF
        | 0x3400..=0x4DBF
        | 0x4E00..=0x9FFF
        | 0xA000..=0xA4CF
        | 0xA960..=0xA97F
        | 0xAC00..=0xD7A3
        | 0xF900..=0xFAFF
        | 0xFE10..=0xFE19
        | 0xFE30..=0xFE6F
        | 0x1F000..=0x1F02F
        | 0x1F030..=0x1F09F
        | 0x1F0A0..=0x1F0FF
        | 0x1F300..=0x1F5FF
        | 0x1F600..=0x1F64F
        | 0x1F680..=0x1F6FF
        | 0x1F900..=0x1F9FF
        | 0x20000..=0x2FA1F
        | 0x30000..=0x323AF => "Wide",
        // Ambiguous (common ones)
        0x00A1
        | 0x00A4
        | 0x00A7..=0x00A8
        | 0x00AA
        | 0x00AD..=0x00AE
        | 0x00B0..=0x00B4
        | 0x00B6..=0x00BA
        | 0x00BC..=0x00BF
        | 0x00C6
        | 0x00D0
        | 0x00D7..=0x00D8
        | 0x00DE..=0x00E1
        | 0x00E6
        | 0x00E8..=0x00EA
        | 0x00EC..=0x00ED
        | 0x00F0
        | 0x00F2..=0x00F3
        | 0x00F7..=0x00FA
        | 0x00FC
        | 0x00FE
        | 0x0101
        | 0x0111
        | 0x0113
        | 0x011B
        | 0x0126..=0x0127
        | 0x012B
        | 0x0131..=0x0133
        | 0x0138
        | 0x013F..=0x0142
        | 0x0144
        | 0x0148..=0x014B
        | 0x014D
        | 0x0152..=0x0153
        | 0x0166..=0x0167
        | 0x016B
        | 0x01CE
        | 0x01D0
        | 0x01D2
        | 0x01D4
        | 0x01D6
        | 0x01D8
        | 0x01DA
        | 0x01DC
        | 0x0251
        | 0x0261
        | 0x02C4
        | 0x02C7
        | 0x02C9..=0x02CB
        | 0x02CD
        | 0x02D0
        | 0x02D8..=0x02DB
        | 0x02DD
        | 0x02DF
        | 0x0300..=0x036F
        | 0x0391..=0x03A9
        | 0x03B1..=0x03C1
        | 0x03C3..=0x03C9
        | 0x0401
        | 0x0410..=0x044F
        | 0x0451
        | 0x2010..=0x2016
        | 0x2018..=0x2019
        | 0x201C..=0x201D
        | 0x2020..=0x2022
        | 0x2024..=0x2027
        | 0x2030
        | 0x2032..=0x2033
        | 0x2035
        | 0x203B
        | 0x203E
        | 0x2074
        | 0x207F
        | 0x2081..=0x2084
        | 0x20AC
        | 0x2103
        | 0x2105
        | 0x2109
        | 0x2113
        | 0x2116
        | 0x2121..=0x2122
        | 0x2126
        | 0x212B
        | 0x2153..=0x2154
        | 0x215B..=0x215E
        | 0x2160..=0x216B
        | 0x2170..=0x2179
        | 0x2189..=0x2199
        | 0x21B8..=0x21B9
        | 0x21D2
        | 0x21D4
        | 0x21E7
        | 0x2200
        | 0x2202..=0x2203
        | 0x2207..=0x2208
        | 0x220B
        | 0x220F
        | 0x2211
        | 0x2215
        | 0x221A
        | 0x221D..=0x2220
        | 0x2223
        | 0x2225
        | 0x2227..=0x222C
        | 0x222E
        | 0x2234..=0x2237
        | 0x223C..=0x223D
        | 0x2248
        | 0x224C
        | 0x2252
        | 0x2260..=0x2261
        | 0x2264..=0x2267
        | 0x226A..=0x226B
        | 0x226E..=0x226F
        | 0x2282..=0x2283
        | 0x2286..=0x2287
        | 0x2295
        | 0x2299
        | 0x22A5
        | 0x22BF
        | 0x2312
        | 0x2460..=0x24E9
        | 0x24EB..=0x254B
        | 0x2550..=0x2573
        | 0x2580..=0x258F
        | 0x2592..=0x2595
        | 0x25A0..=0x25A1
        | 0x25A3..=0x25A9
        | 0x25B2..=0x25B3
        | 0x25B6..=0x25B7
        | 0x25BC..=0x25BD
        | 0x25C0..=0x25C1
        | 0x25C6..=0x25C8
        | 0x25CB
        | 0x25CE..=0x25D1
        | 0x25E2..=0x25E5
        | 0x25EF
        | 0x2605..=0x2606
        | 0x2609
        | 0x260E..=0x260F
        | 0x261C
        | 0x261E
        | 0x2640
        | 0x2642
        | 0x2660..=0x2661
        | 0x2663..=0x2665
        | 0x2667..=0x266A
        | 0x266C..=0x266D
        | 0x266F
        | 0x269E..=0x269F
        | 0x26BF
        | 0x26C6..=0x26CD
        | 0x26CF..=0x26D3
        | 0x26D5..=0x26E1
        | 0x26E3
        | 0x26E8..=0x26E9
        | 0x26EB..=0x26F1
        | 0x26F4
        | 0x26F6..=0x26F9
        | 0x26FB..=0x26FC
        | 0x26FE..=0x26FF
        | 0x273D
        | 0x2776..=0x277F
        | 0xFFFD => "Ambiguous",
        // Narrow (Latin, etc. that aren't fullwidth/wide)
        _ => {
            if cp <= 0x007F {
                if (0x0020..=0x007E).contains(&cp) {
                    "Narrow"
                } else {
                    "Neutral"
                }
            } else if cp <= 0x00FF {
                // Latin-1 Supplement: mostly Neutral, some Ambiguous (handled above)
                "Neutral"
            } else {
                "Neutral"
            }
        }
    }
    .to_string()
}

/// Hangul_Syllable_Type property.
pub(crate) fn unicode_hangul_syllable_type(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x1100..=0x115F | 0xA960..=0xA97C => "L",
        0x1160..=0x11A7 | 0xD7B0..=0xD7C6 => "V",
        0x11A8..=0x11FF | 0xD7CB..=0xD7FB => "T",
        0xAC00..=0xD7A3 => {
            let s_index = cp - 0xAC00;
            if s_index.is_multiple_of(28) {
                "LV"
            } else {
                "LVT"
            }
        }
        _ => "Not_Applicable",
    }
    .to_string()
}
