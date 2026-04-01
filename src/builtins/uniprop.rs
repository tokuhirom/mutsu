use crate::value::Value;
use std::sync::OnceLock;

/// Check if NFKC case folding changes the character.
fn check_changes_when_nfkc_casefolded(ch: char) -> bool {
    use unicode_normalization::UnicodeNormalization;
    let s = ch.to_string();
    // Apply NFKC case folding: NFKD -> casefold -> NFC
    let folded = super::methods_0arg::unicode_foldcase(&s);
    let nfkc_cf: String = folded.nfkc().collect();
    nfkc_cf != s
}

/// Check if a character has full composition exclusion.
fn check_full_composition_exclusion(ch: char) -> bool {
    use unicode_normalization::UnicodeNormalization;
    // A character has Full_Composition_Exclusion if it decomposes canonically
    // and the decomposition is excluded from NFC composition.
    // In practice: if NFD(ch) != ch and NFC(NFD(ch)) != ch
    let s = ch.to_string();
    let nfd: String = s.nfd().collect();
    if nfd == s {
        return false; // No canonical decomposition
    }
    let nfc: String = nfd.nfc().collect();
    nfc != s
}

/// Check if a character has a binary Unicode property using regex \p{...}.
fn check_binary_property(ch: char, prop_regex_str: &str) -> bool {
    let re = regex::Regex::new(prop_regex_str).ok();
    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    re.is_some_and(|r| r.is_match(s))
}

/// Check a binary Unicode property by name, returning Some(bool) if it's
/// a known binary property, None otherwise.
fn try_binary_property(ch: char, prop: &str) -> Option<bool> {
    // Map property names to regex patterns.
    // The regex crate supports many Unicode binary properties via \p{...}.
    let pattern = match prop {
        "Alphabetic" | "Alpha" => r"^\p{Alphabetic}$",
        "Dash" => r"^\p{Dash}$",
        "Diacritic" | "Dia" => r"^\p{Diacritic}$",
        "Default_Ignorable_Code_Point" | "DI" => r"^\p{Default_Ignorable_Code_Point}$",
        "Extender" | "Ext" => r"^\p{Extender}$",
        "Grapheme_Base" | "Gr_Base" => r"^\p{Grapheme_Base}$",
        "Grapheme_Extend" | "Gr_Ext" => r"^\p{Grapheme_Extend}$",
        "Hex_Digit" | "Hex" => r"^\p{Hex_Digit}$",
        "ASCII_Hex_Digit" | "AHex" => r"^\p{ASCII_Hex_Digit}$",
        "ID_Continue" | "IDC" => r"^\p{ID_Continue}$",
        "ID_Start" | "IDS" => r"^\p{ID_Start}$",
        "Ideographic" | "Ideo" => r"^\p{Ideographic}$",
        "IDS_Binary_Operator" | "IDSB" => r"^\p{IDS_Binary_Operator}$",
        "IDS_Trinary_Operator" | "IDST" => r"^\p{IDS_Trinary_Operator}$",
        "Join_Control" | "Join_C" => r"^\p{Join_Control}$",
        "Math" => r"^\p{Math}$",
        "Radical" => r"^\p{Radical}$",
        "Soft_Dotted" | "SD" => r"^\p{Soft_Dotted}$",
        "Terminal_Punctuation" | "Term" => r"^\p{Terminal_Punctuation}$",
        "Variation_Selector" | "VS" => r"^\p{Variation_Selector}$",
        "White_Space" | "WSpace" | "space" => r"^\p{White_Space}$",
        "Uppercase" | "Upper" => r"^\p{Uppercase}$",
        "Lowercase" | "Lower" => r"^\p{Lowercase}$",
        "Bidi_Control" | "Bidi_C" => r"^\p{Bidi_Control}$",
        "Bidi_Mirrored" | "Bidi_M" => r"^\p{Bidi_Mirrored}$",
        "Case_Ignorable" | "CI" => r"^\p{Case_Ignorable}$",
        "Cased" => r"^\p{Cased}$",
        "Changes_When_Casefolded" | "CWCF" => r"^\p{Changes_When_Casefolded}$",
        "Changes_When_Casemapped" | "CWCM" => r"^\p{Changes_When_Casemapped}$",
        "Changes_When_Lowercased" | "CWL" => r"^\p{Changes_When_Lowercased}$",
        "Changes_When_Uppercased" | "CWU" => r"^\p{Changes_When_Uppercased}$",
        "Changes_When_Titlecased" | "CWT" => r"^\p{Changes_When_Titlecased}$",
        "Changes_When_NFKC_Casefolded" | "CWKCF" => {
            // regex crate may not support this; compute manually
            return Some(check_changes_when_nfkc_casefolded(ch));
        }
        "Deprecated" | "Dep" => r"^\p{Deprecated}$",
        "Grapheme_Link" | "Gr_Link" => r"^\p{Grapheme_Link}$",
        "Hyphen" => r"^\p{Hyphen}$",
        "Quotation_Mark" | "QMark" => r"^\p{Quotation_Mark}$",
        "Sentence_Terminal" | "STerm" => r"^\p{Sentence_Terminal}$",
        "Full_Composition_Exclusion" | "Comp_Ex" => {
            return Some(check_full_composition_exclusion(ch));
        }
        "Pattern_White_Space" => r"^\p{Pattern_White_Space}$",
        "Pattern_Syntax" => r"^\p{Pattern_Syntax}$",
        "Other_Alphabetic" | "OAlpha" => r"^\p{Other_Alphabetic}$",
        "Other_Lowercase" | "OLower" => r"^\p{Other_Lowercase}$",
        "Other_Uppercase" | "OUpper" => r"^\p{Other_Uppercase}$",
        "Other_Math" | "OMath" => r"^\p{Other_Math}$",
        "Unified_Ideograph" | "UIdeo" => r"^\p{Unified_Ideograph}$",
        "Noncharacter_Code_Point" | "NChar" => r"^\p{Noncharacter_Code_Point}$",
        "Other_Grapheme_Extend" | "OGr_Ext" => r"^\p{Other_Grapheme_Extend}$",
        "Other_ID_Continue" | "OIDC" => r"^\p{Other_ID_Continue}$",
        "Other_ID_Start" | "OIDS" => r"^\p{Other_ID_Start}$",
        "Other_Default_Ignorable_Code_Point" | "ODI" => r"^\p{Other_Default_Ignorable_Code_Point}$",
        "XID_Start" | "XIDS" => r"^\p{XID_Start}$",
        "XID_Continue" | "XIDC" => r"^\p{XID_Continue}$",
        "Emoji" => r"^\p{Emoji}$",
        "Emoji_Presentation" => r"^\p{Emoji_Presentation}$",
        "Emoji_Modifier" => r"^\p{Emoji_Modifier}$",
        "Emoji_Modifier_Base" => r"^\p{Emoji_Modifier_Base}$",
        "Emoji_Component" => r"^\p{Emoji_Component}$",
        "Extended_Pictographic" => r"^\p{Extended_Pictographic}$",
        "Regional_Indicator" => r"^\p{Regional_Indicator}$",
        _ => return None,
    };
    Some(check_binary_property(ch, pattern))
}

/// Unicode Block lookup — delegates to the tables module.
fn unicode_block(ch: char) -> String {
    super::uniprop_tables::unicode_block(ch)
}

/// Jamo short name lookup — delegates to the tables module.
fn unicode_jamo_short_name(ch: char) -> String {
    super::uniprop_tables::unicode_jamo_short_name(ch)
}

/// Look up Numeric_Value for uniprop.
fn unicode_numeric_value(ch: char) -> Value {
    // Check if it's a digit (Nd)
    if let Some(d) = super::unicode::unicode_decimal_digit_value(ch) {
        return Value::Int(d as i64);
    }
    // Check if it's a fraction
    if let Some((n, d)) = super::unicode::unicode_rat_value(ch) {
        return crate::value::make_rat(n, d);
    }
    // Check if it's an integer numeric
    if let Some(i) = super::unicode::unicode_numeric_int_value(ch) {
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
fn case_mapping(ch: char, prop: &str) -> String {
    match prop {
        "Lowercase_Mapping" | "lc" => ch.to_lowercase().to_string(),
        "Uppercase_Mapping" | "uc" => ch.to_uppercase().to_string(),
        "Titlecase_Mapping" | "tc" => super::unicode::unicode_titlecase_first(ch),
        "Case_Folding" | "cf" => {
            // Use the same unicode_foldcase logic as .fc
            super::methods_0arg::unicode_foldcase(&ch.to_string())
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
            let tc = super::unicode::unicode_titlecase_first(ch);
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
fn unicode_bidi_class(ch: char) -> String {
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
            let gc = super::unicode::unicode_general_category(ch);
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
fn unicode_decomposition_type(ch: char) -> String {
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
fn unicode_east_asian_width(ch: char) -> String {
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
fn unicode_hangul_syllable_type(ch: char) -> String {
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

/// Grapheme_Cluster_Break property.
fn unicode_grapheme_cluster_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => "CR",
        0x000A => "LF",
        0x200D => "ZWJ",
        _ => {
            // Use regex for Extend
            if check_binary_property(ch, r"^\p{Grapheme_Extend}$") {
                return "Extend".to_string();
            }
            let gc = super::unicode::unicode_general_category(ch);
            if gc == "Cc" || gc == "Cf" {
                return "Control".to_string();
            }
            return "Other".to_string();
        }
    }
    .to_string()
}

/// Joining_Type property.
fn unicode_joining_type(ch: char) -> String {
    // Simplified - use General Category heuristic
    let gc = super::unicode::unicode_general_category(ch);
    if gc == "Mn" || gc == "Me" || gc == "Cf" {
        // Most non-spacing marks are transparent
        if check_binary_property(ch, r"^\p{Join_Control}$") {
            return "C".to_string(); // Join_Causing
        }
        return "T".to_string(); // Transparent
    }
    // For Arabic/Syriac script chars, they are usually D (Dual_Joining)
    let script = super::unicode::unicode_script_name(ch);
    match script.as_str() {
        "Arabic" | "Syriac" | "Mandaic" | "Nko" | "Thaana" => "D".to_string(),
        _ => "U".to_string(), // Non_Joining
    }
}

/// Joining_Group property — derive from character name for Arabic/Syriac.
fn unicode_joining_group(ch: char) -> String {
    let script = super::unicode::unicode_script_name(ch);
    let name = super::unicode::unicode_char_name(ch);
    match script.as_str() {
        "Syriac" => {
            // "SYRIAC LETTER WAW" → "SYRIAC WAW"
            // "SYRIAC LETTER ALAPH" → "SYRIAC WAW"
            if let Some(rest) = name.strip_prefix("SYRIAC LETTER ") {
                format!("SYRIAC {}", rest)
            } else {
                "No_Joining_Group".to_string()
            }
        }
        "Arabic" => {
            // Arabic joining groups: extract the letter name
            // "ARABIC LETTER BEH" → "BEH"
            if let Some(rest) = name.strip_prefix("ARABIC LETTER ") {
                // Remove final form/isolated form suffixes
                let base = rest
                    .trim_end_matches(" FINAL FORM")
                    .trim_end_matches(" INITIAL FORM")
                    .trim_end_matches(" MEDIAL FORM")
                    .trim_end_matches(" ISOLATED FORM");
                base.to_string()
            } else {
                "No_Joining_Group".to_string()
            }
        }
        _ => "No_Joining_Group".to_string(),
    }
}

/// Sentence_Break property.
fn unicode_sentence_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => "CR".to_string(),
        0x000A => "LF".to_string(),
        0x0009 | 0x000B | 0x000C => "Sp".to_string(),
        _ => {
            let gc = super::unicode::unicode_general_category(ch);
            match gc.as_str() {
                "Lu" => "Upper".to_string(),
                "Ll" => "Lower".to_string(),
                "Nd" => "Numeric".to_string(),
                "Zs" => "Sp".to_string(),
                _ => {
                    // Check for ATerm (. and similar)
                    if cp == 0x002E || cp == 0x2024 {
                        "ATerm".to_string()
                    } else if check_binary_property(ch, r"^\p{Sentence_Terminal}$") {
                        "STerm".to_string()
                    } else {
                        "Other".to_string()
                    }
                }
            }
        }
    }
}

/// Word_Break property.
fn unicode_word_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => "CR".to_string(),
        0x000A | 0x000B | 0x000C | 0x0085 | 0x2028 | 0x2029 => "LF".to_string(),
        0x200D => "ZWJ".to_string(),
        _ => {
            let gc = super::unicode::unicode_general_category(ch);
            let script = super::unicode::unicode_script_name(ch);
            if script == "Hebrew" && (gc == "Lo" || gc == "Lm") {
                return "Hebrew_Letter".to_string();
            }
            match gc.as_str() {
                "Lu" | "Ll" | "Lt" | "Lm" | "Lo" => "ALetter".to_string(),
                "Nd" => "Numeric".to_string(),
                "Mn" | "Me" | "Mc" => {
                    if check_binary_property(ch, r"^\p{Grapheme_Extend}$") {
                        "Extend".to_string()
                    } else {
                        "Other".to_string()
                    }
                }
                _ => {
                    if check_binary_property(ch, r"^\p{Extender}$") {
                        "ExtendNumLet".to_string()
                    } else {
                        "Other".to_string()
                    }
                }
            }
        }
    }
}

/// Line_Break property.
fn unicode_line_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000A => "LF".to_string(),
        0x000D => "CR".to_string(),
        0x000B => "BK".to_string(),
        0x000C => "BK".to_string(),
        0x0085 => "NL".to_string(),
        0x2028 => "BK".to_string(),
        0x2029 => "BK".to_string(),
        0x0020 => "SP".to_string(),
        0x200D => "ZWJ".to_string(),
        0x200B => "ZW".to_string(),
        0x00AD => "BA".to_string(), // SOFT HYPHEN
        _ => {
            // Check for surrogates (not valid Rust chars but handle codepoints)
            if (0xD800..=0xDFFF).contains(&cp) {
                return "SG".to_string();
            }
            // Noncharacters
            if (cp & 0xFFFE == 0xFFFE) || (0xFDD0..=0xFDEF).contains(&cp) {
                return "XX".to_string();
            }
            let gc = super::unicode::unicode_general_category(ch);
            match gc.as_str() {
                "Ps" => "OP".to_string(),
                "Pe" => "CL".to_string(),
                "Zs" => "SP".to_string(),
                "Mn" | "Mc" | "Me" => {
                    // Southeast Asian scripts use SA for combining marks too
                    let script = super::unicode::unicode_script_name(ch);
                    match script.as_str() {
                        "Thai" | "Lao" | "Myanmar" | "Khmer" | "Javanese" | "Tai_Tham"
                        | "New_Tai_Lue" | "Tai_Le" => "SA".to_string(),
                        _ => "CM".to_string(),
                    }
                }
                "Nd" => "NU".to_string(),
                "Lu" | "Ll" | "Lt" | "Lm" | "Lo" => {
                    let script = super::unicode::unicode_script_name(ch);
                    match script.as_str() {
                        "Thai" | "Lao" | "Myanmar" | "Khmer" | "Javanese" | "Tai_Tham"
                        | "New_Tai_Lue" | "Tai_Le" => "SA".to_string(),
                        _ => "AL".to_string(),
                    }
                }
                "Sm" => "AL".to_string(),
                "Sc" => "PR".to_string(),
                "Sk" => "AL".to_string(),
                "So" => "AL".to_string(),
                "Pi" => "QU".to_string(),
                "Pf" => "QU".to_string(),
                "Pd" => "HY".to_string(),
                "Po" => {
                    if cp == 0x002E || cp == 0x002C || cp == 0x003A || cp == 0x003B {
                        "IS".to_string()
                    } else {
                        "AL".to_string()
                    }
                }
                "Pc" => "AL".to_string(),
                "Cc" => "CM".to_string(),
                "Cf" => "CM".to_string(),
                "Cn" => "XX".to_string(),
                "Co" => "XX".to_string(),
                _ => "XX".to_string(),
            }
        }
    }
}

/// NFC_Quick_Check property.
fn unicode_nfc_quick_check(ch: char) -> String {
    use unicode_normalization::UnicodeNormalization;
    let s = ch.to_string();
    let nfc: String = s.nfc().collect();
    if nfc == s {
        // Could be Yes or Maybe
        // Check if it's a combining character that might compose with a preceding char
        if unicode_normalization::char::is_combining_mark(ch) {
            // Check canonical combining class — characters with CCC > 0
            // that can compose are "Maybe"
            let nfd: String = s.nfd().collect();
            if nfd == s {
                "Maybe".to_string()
            } else {
                "No".to_string()
            }
        } else {
            "Yes".to_string()
        }
    } else {
        "No".to_string()
    }
}

/// NFD_Quick_Check property.
fn unicode_nfd_quick_check(ch: char) -> String {
    use unicode_normalization::UnicodeNormalization;
    let s = ch.to_string();
    let nfd: String = s.nfd().collect();
    if nfd == s { "Yes" } else { "No" }.to_string()
}

/// NFKC_Quick_Check property.
fn unicode_nfkc_quick_check(ch: char) -> String {
    use unicode_normalization::UnicodeNormalization;
    let s = ch.to_string();
    let nfkc: String = s.nfkc().collect();
    if nfkc == s { "Yes" } else { "No" }.to_string()
}

/// NFKD_Quick_Check property.
fn unicode_nfkd_quick_check(ch: char) -> String {
    use unicode_normalization::UnicodeNormalization;
    let s = ch.to_string();
    let nfkd: String = s.nfkd().collect();
    if nfkd == s { "Yes" } else { "No" }.to_string()
}

/// Canonical_Combining_Class property.
fn unicode_canonical_combining_class(ch: char) -> String {
    use unicode_normalization::char::canonical_combining_class;
    let ccc = canonical_combining_class(ch);
    match ccc {
        0 => "Not_Reordered",
        1 => "Overlay",
        7 => "Nukta",
        8 => "Kana_Voicing",
        9 => "Virama",
        200 => "Attached_Below_Left",
        202 => "Attached_Below",
        214 => "Attached_Above",
        216 => "Attached_Above_Right",
        218 => "Below_Left",
        220 => "Below",
        222 => "Below_Right",
        224 => "Left",
        226 => "Right",
        228 => "Above_Left",
        230 => "Above",
        232 => "Above_Right",
        233 => "Double_Below",
        234 => "Double_Above",
        240 => "Iota_Subscript",
        _ => return ccc.to_string(),
    }
    .to_string()
}

/// Bidi_Mirroring_Glyph property.
fn unicode_bidi_mirroring_glyph(ch: char) -> String {
    match ch {
        '(' => ")".to_string(),
        ')' => "(".to_string(),
        '<' => ">".to_string(),
        '>' => "<".to_string(),
        '[' => "]".to_string(),
        ']' => "[".to_string(),
        '{' => "}".to_string(),
        '}' => "{".to_string(),
        '\u{2039}' => "\u{203A}".to_string(),
        '\u{203A}' => "\u{2039}".to_string(),
        '\u{2045}' => "\u{2046}".to_string(),
        '\u{2046}' => "\u{2045}".to_string(),
        '\u{207D}' => "\u{207E}".to_string(),
        '\u{207E}' => "\u{207D}".to_string(),
        '\u{208D}' => "\u{208E}".to_string(),
        '\u{208E}' => "\u{208D}".to_string(),
        '\u{2308}' => "\u{2309}".to_string(),
        '\u{2309}' => "\u{2308}".to_string(),
        '\u{230A}' => "\u{230B}".to_string(),
        '\u{230B}' => "\u{230A}".to_string(),
        '\u{2329}' => "\u{232A}".to_string(),
        '\u{232A}' => "\u{2329}".to_string(),
        '\u{27C5}' => "\u{27C6}".to_string(),
        '\u{27C6}' => "\u{27C5}".to_string(),
        '\u{27E6}' => "\u{27E7}".to_string(),
        '\u{27E7}' => "\u{27E6}".to_string(),
        '\u{27E8}' => "\u{27E9}".to_string(),
        '\u{27E9}' => "\u{27E8}".to_string(),
        '\u{27EA}' => "\u{27EB}".to_string(),
        '\u{27EB}' => "\u{27EA}".to_string(),
        '\u{27EC}' => "\u{27ED}".to_string(),
        '\u{27ED}' => "\u{27EC}".to_string(),
        '\u{27EE}' => "\u{27EF}".to_string(),
        '\u{27EF}' => "\u{27EE}".to_string(),
        '\u{2983}' => "\u{2984}".to_string(),
        '\u{2984}' => "\u{2983}".to_string(),
        '\u{2985}' => "\u{2986}".to_string(),
        '\u{2986}' => "\u{2985}".to_string(),
        '\u{2987}' => "\u{2988}".to_string(),
        '\u{2988}' => "\u{2987}".to_string(),
        '\u{2989}' => "\u{298A}".to_string(),
        '\u{298A}' => "\u{2989}".to_string(),
        '\u{298B}' => "\u{298C}".to_string(),
        '\u{298C}' => "\u{298B}".to_string(),
        '\u{298D}' => "\u{2990}".to_string(),
        '\u{298E}' => "\u{298F}".to_string(),
        '\u{298F}' => "\u{298E}".to_string(),
        '\u{2990}' => "\u{298D}".to_string(),
        '\u{2991}' => "\u{2992}".to_string(),
        '\u{2992}' => "\u{2991}".to_string(),
        '\u{2993}' => "\u{2994}".to_string(),
        '\u{2994}' => "\u{2993}".to_string(),
        '\u{2995}' => "\u{2996}".to_string(),
        '\u{2996}' => "\u{2995}".to_string(),
        '\u{2997}' => "\u{2998}".to_string(),
        '\u{2998}' => "\u{2997}".to_string(),
        '\u{29D8}' => "\u{29D9}".to_string(),
        '\u{29D9}' => "\u{29D8}".to_string(),
        '\u{29DA}' => "\u{29DB}".to_string(),
        '\u{29DB}' => "\u{29DA}".to_string(),
        '\u{29FC}' => "\u{29FD}".to_string(),
        '\u{29FD}' => "\u{29FC}".to_string(),
        '\u{FE59}' => "\u{FE5A}".to_string(),
        '\u{FE5A}' => "\u{FE59}".to_string(),
        '\u{FE5B}' => "\u{FE5C}".to_string(),
        '\u{FE5C}' => "\u{FE5B}".to_string(),
        '\u{FE5D}' => "\u{FE5E}".to_string(),
        '\u{FE5E}' => "\u{FE5D}".to_string(),
        '\u{FF08}' => "\u{FF09}".to_string(),
        '\u{FF09}' => "\u{FF08}".to_string(),
        '\u{FF1C}' => "\u{FF1E}".to_string(),
        '\u{FF1E}' => "\u{FF1C}".to_string(),
        '\u{FF3B}' => "\u{FF3D}".to_string(),
        '\u{FF3D}' => "\u{FF3B}".to_string(),
        '\u{FF5B}' => "\u{FF5D}".to_string(),
        '\u{FF5D}' => "\u{FF5B}".to_string(),
        '\u{FF5F}' => "\u{FF60}".to_string(),
        '\u{FF60}' => "\u{FF5F}".to_string(),
        _ => "".to_string(),
    }
}

/// Emoji property (from emojis crate or regex).
fn is_emoji(ch: char) -> bool {
    // Use regex for Emoji property
    static EMOJI_RE: OnceLock<regex::Regex> = OnceLock::new();
    let re = EMOJI_RE.get_or_init(|| regex::Regex::new(r"^\p{Emoji}$").unwrap());
    let mut buf = [0u8; 4];
    re.is_match(ch.encode_utf8(&mut buf))
}

/// Emoji_Modifier property.
fn is_emoji_modifier(ch: char) -> bool {
    let cp = ch as u32;
    (0x1F3FB..=0x1F3FF).contains(&cp)
}

/// Emoji_Presentation property.
fn is_emoji_presentation(ch: char) -> bool {
    static EMOJI_PRES_RE: OnceLock<regex::Regex> = OnceLock::new();
    let re = EMOJI_PRES_RE.get_or_init(|| regex::Regex::new(r"^\p{Emoji_Presentation}$").unwrap());
    let mut buf = [0u8; 4];
    re.is_match(ch.encode_utf8(&mut buf))
}

/// Emoji_All: True if any Emoji property is true.
fn is_emoji_all(ch: char) -> bool {
    is_emoji(ch) && (is_emoji_presentation(ch) || is_emoji_modifier(ch))
}

/// Main entry point: look up a Unicode property value for a character.
/// Returns a Value (Bool for binary props, Str for string/enum props, etc.)
pub(crate) fn unicode_property_value(ch: char, prop: &str) -> Value {
    // Check binary properties first
    if let Some(b) = try_binary_property(ch, prop) {
        return Value::Bool(b);
    }

    match prop {
        // General Category
        "General_Category" | "gc" => Value::str(super::unicode::unicode_general_category(ch)),
        // Script
        "Script" | "sc" => Value::str(super::unicode::unicode_script_name(ch)),
        // Age
        "Age" => Value::str_from("1.1"), // Simplified; proper impl would need full data
        // Block
        "Block" | "blk" => Value::str(unicode_block(ch)),
        // Name
        "Name" | "na" => Value::str(super::unicode::unicode_char_name(ch)),
        // Unicode_1_Name
        "Unicode_1_Name" | "na1" => {
            // Unicode 1.0 names are mostly the same; some differ
            // For now return 0 (same as MoarVM behavior for most chars)
            Value::Int(0)
        }
        // Numeric properties
        "Numeric_Value" | "nv" => unicode_numeric_value(ch),
        "Numeric_Type" | "nt" => Value::str(unicode_numeric_type(ch)),
        // Case mappings
        "Lowercase_Mapping"
        | "lc"
        | "Uppercase_Mapping"
        | "uc"
        | "Titlecase_Mapping"
        | "tc"
        | "Case_Folding"
        | "cf"
        | "Simple_Uppercase_Mapping"
        | "suc"
        | "Simple_Lowercase_Mapping"
        | "slc"
        | "Simple_Titlecase_Mapping"
        | "stc" => Value::str(case_mapping(ch, prop)),
        // Bidi properties
        "Bidi_Class" | "bc" => Value::str(unicode_bidi_class(ch)),
        "Bidi_Mirroring_Glyph" | "bmg" => Value::str(unicode_bidi_mirroring_glyph(ch)),
        "Bidi_Paired_Bracket" | "bpb" => {
            // Same as mirroring glyph for paired brackets
            let mirror = unicode_bidi_mirroring_glyph(ch);
            if mirror.is_empty() {
                Value::str(ch.to_string())
            } else {
                Value::str(mirror)
            }
        }
        "Bidi_Paired_Bracket_Type" | "bpt" => {
            let gc = super::unicode::unicode_general_category(ch);
            let result = match gc.as_str() {
                "Ps" => "o", // Open
                "Pe" => "c", // Close
                _ => "n",    // None
            };
            Value::str_from(result)
        }
        // Decomposition
        "Decomposition_Type" | "dt" => Value::str(unicode_decomposition_type(ch)),
        // East Asian Width
        "East_Asian_Width" | "ea" => Value::str(unicode_east_asian_width(ch)),
        // Hangul
        "Hangul_Syllable_Type" | "hst" => Value::str(unicode_hangul_syllable_type(ch)),
        // Grapheme cluster break
        "Grapheme_Cluster_Break" | "GCB" => Value::str(unicode_grapheme_cluster_break(ch)),
        // Joining
        "Joining_Group" | "jg" => Value::str(unicode_joining_group(ch)),
        "Joining_Type" | "jt" => Value::str(unicode_joining_type(ch)),
        // Sentence Break
        "Sentence_Break" | "SB" => Value::str(unicode_sentence_break(ch)),
        // Word Break
        "Word_Break" | "WB" => Value::str(unicode_word_break(ch)),
        // Line Break
        "Line_Break" | "lb" => Value::str(unicode_line_break(ch)),
        // NFC/NFD/NFKC/NFKD Quick Check
        "NFC_Quick_Check" | "NFC_QC" => Value::str(unicode_nfc_quick_check(ch)),
        "NFD_Quick_Check" | "NFD_QC" => Value::str(unicode_nfd_quick_check(ch)),
        "NFKC_Quick_Check" | "NFKC_QC" => Value::str(unicode_nfkc_quick_check(ch)),
        "NFKD_Quick_Check" | "NFKD_QC" => Value::str(unicode_nfkd_quick_check(ch)),
        // Canonical Combining Class
        "Canonical_Combining_Class" | "ccc" => Value::str(unicode_canonical_combining_class(ch)),
        // Indic properties (partial)
        "Indic_Positional_Category" | "InPC" => {
            // Simplified: return NA for most, specific values for known chars
            Value::str_from("NA")
        }
        "Indic_Syllabic_Category" | "InSC" => Value::str_from("Other"),
        // ISO_Comment - always empty since Unicode 5.2.0
        "ISO_Comment" | "isc" => Value::str_from(""),
        // Jamo_Short_Name
        "Jamo_Short_Name" | "JSN" => Value::str(unicode_jamo_short_name(ch)),
        // Emoji properties
        "Emoji" => Value::Bool(is_emoji(ch)),
        "Emoji_Modifier" => Value::Bool(is_emoji_modifier(ch)),
        "Emoji_Presentation" => Value::Bool(is_emoji_presentation(ch)),
        "Emoji_All" => Value::Bool(is_emoji_all(ch)),
        // Default: general category
        _ => Value::str(super::unicode::unicode_general_category(ch)),
    }
}

/// Get the property value for a codepoint (as u32), handling
/// codepoints that may not be valid chars (like surrogates or
/// very high codepoints).
pub(crate) fn unicode_property_value_for_codepoint(cp: u32, prop: Option<&str>) -> Value {
    match char::from_u32(cp) {
        Some(ch) => match prop {
            Some(p) => unicode_property_value(ch, p),
            None => Value::str(super::unicode::unicode_general_category(ch)),
        },
        None => {
            // Invalid codepoint (surrogate or out of range)
            let is_surrogate = (0xD800..=0xDFFF).contains(&cp);
            match prop {
                Some("General_Category") | Some("gc") | None => {
                    if is_surrogate {
                        Value::str_from("Cs")
                    } else {
                        Value::str_from("Cn")
                    }
                }
                Some("Block") | Some("blk") => {
                    // Try to find block for surrogate codepoints
                    if (0xD800..=0xDB7F).contains(&cp) {
                        Value::str_from("High Surrogates")
                    } else if (0xDB80..=0xDBFF).contains(&cp) {
                        Value::str_from("High Private Use Surrogates")
                    } else if (0xDC00..=0xDFFF).contains(&cp) {
                        Value::str_from("Low Surrogates")
                    } else {
                        Value::str_from("No_Block")
                    }
                }
                _ => Value::str_from(""),
            }
        }
    }
}

/// Check if a character matches a Unicode property value.
/// `prop_value` is the value to match (e.g., "Nd", "L", "Hebrew", "Pattern_White_Space").
/// `prop_name` is an optional property name (e.g., "sc" for Script). If None, the function
/// tries general category, binary properties, script, and block in order.
pub(crate) fn unimatch(ch: char, prop_value: &str, prop_name: Option<&str>) -> bool {
    if let Some(pn) = prop_name {
        // Explicit property name given — check that specific property
        return unimatch_property(ch, prop_value, pn);
    }

    // No explicit property name — try matching in order:
    // 1. General Category (exact or parent match)
    if unimatch_general_category(ch, prop_value) {
        return true;
    }

    // 2. Binary properties
    if let Some(b) = try_binary_property(ch, prop_value) {
        return b;
    }

    // 3. Script name
    let script = super::unicode::unicode_script_name(ch);
    if script.eq_ignore_ascii_case(prop_value) {
        return true;
    }

    // 4. Block name (normalize underscores/spaces/case)
    let block = unicode_block(ch);
    if block_matches(&block, prop_value) {
        return true;
    }

    false
}

/// Match against a specific named property.
fn unimatch_property(ch: char, prop_value: &str, prop_name: &str) -> bool {
    match prop_name {
        "gc" | "General_Category" => unimatch_general_category(ch, prop_value),
        "sc" | "Script" => {
            let script = super::unicode::unicode_script_name(ch);
            script.eq_ignore_ascii_case(prop_value)
        }
        "blk" | "Block" => {
            let block = unicode_block(ch);
            block_matches(&block, prop_value)
        }
        _ => {
            // Try to get the property value and compare
            let val = unicode_property_value(ch, prop_name);
            let val_str = val.to_string_value();
            val_str.eq_ignore_ascii_case(prop_value)
        }
    }
}

/// Check if a character's general category matches the given value.
/// Supports exact matches ("Lu"), parent category matches ("L" matches "Lu", "Ll", etc.),
/// and the "LC" alias for cased letters (Lu, Ll, Lt).
fn unimatch_general_category(ch: char, cat: &str) -> bool {
    let gc = super::unicode::unicode_general_category(ch);

    // Exact match
    if gc == cat {
        return true;
    }

    // Parent category match: single-letter category matches any two-letter sub-category
    // starting with that letter (e.g., "L" matches "Lu", "Ll", "Lt", "Lm", "Lo")
    if cat.len() == 1 && gc.len() == 2 && gc.starts_with(cat) {
        return true;
    }

    // LC (Cased_Letter) matches Lu, Ll, Lt
    if cat == "LC" && (gc == "Lu" || gc == "Ll" || gc == "Lt") {
        return true;
    }

    false
}

/// Check if a block name matches, normalizing case, underscores, spaces, and hyphens.
fn block_matches(block: &str, target: &str) -> bool {
    let normalize = |s: &str| -> String {
        s.chars()
            .filter(|c| *c != '_' && *c != ' ' && *c != '-')
            .flat_map(|c| c.to_lowercase())
            .collect()
    };
    normalize(block) == normalize(target)
}

/// unimatch for a codepoint (u32), handling invalid chars.
pub(crate) fn unimatch_for_codepoint(cp: u32, prop_value: &str, prop_name: Option<&str>) -> Value {
    match char::from_u32(cp) {
        Some(ch) => Value::Bool(unimatch(ch, prop_value, prop_name)),
        None => Value::Bool(false),
    }
}
