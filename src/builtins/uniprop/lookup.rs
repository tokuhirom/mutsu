use crate::value::Value;
use std::sync::OnceLock;

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
    if let Some(b) = super::binary_props::try_binary_property(ch, prop) {
        return Value::Bool(b);
    }

    match prop {
        // General Category
        "General_Category" | "gc" => {
            Value::str(crate::builtins::unicode::unicode_general_category(ch))
        }
        // Script
        "Script" | "sc" => Value::str(crate::builtins::unicode::unicode_script_name(ch)),
        // Age
        "Age" => Value::str_from("1.1"), // Simplified; proper impl would need full data
        // Block
        "Block" | "blk" => Value::str(super::char_props::unicode_block(ch)),
        // Name
        "Name" | "na" => Value::str(crate::builtins::unicode::unicode_char_name(ch)),
        // Unicode_1_Name
        "Unicode_1_Name" | "na1" => {
            // Unicode 1.0 names are mostly the same; some differ
            // For now return 0 (same as MoarVM behavior for most chars)
            Value::Int(0)
        }
        // Numeric properties
        "Numeric_Value" | "nv" => super::char_props::unicode_numeric_value(ch),
        "Numeric_Type" | "nt" => Value::str(super::char_props::unicode_numeric_type(ch)),
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
        | "stc" => Value::str(super::char_props::case_mapping(ch, prop)),
        // Bidi properties
        "Bidi_Class" | "bc" => Value::str(super::char_props::unicode_bidi_class(ch)),
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
            let gc = crate::builtins::unicode::unicode_general_category(ch);
            let result = match gc.as_str() {
                "Ps" => "o", // Open
                "Pe" => "c", // Close
                _ => "n",    // None
            };
            Value::str_from(result)
        }
        // Decomposition
        "Decomposition_Type" | "dt" => {
            Value::str(super::char_props::unicode_decomposition_type(ch))
        }
        // East Asian Width
        "East_Asian_Width" | "ea" => Value::str(super::char_props::unicode_east_asian_width(ch)),
        // Hangul
        "Hangul_Syllable_Type" | "hst" => {
            Value::str(super::char_props::unicode_hangul_syllable_type(ch))
        }
        // Grapheme cluster break
        "Grapheme_Cluster_Break" | "GCB" => {
            Value::str(super::text_seg::unicode_grapheme_cluster_break(ch))
        }
        // Joining
        "Joining_Group" | "jg" => Value::str(super::text_seg::unicode_joining_group(ch)),
        "Joining_Type" | "jt" => Value::str(super::text_seg::unicode_joining_type(ch)),
        // Sentence Break
        "Sentence_Break" | "SB" => Value::str(super::text_seg::unicode_sentence_break(ch)),
        // Word Break
        "Word_Break" | "WB" => Value::str(super::text_seg::unicode_word_break(ch)),
        // Line Break
        "Line_Break" | "lb" => Value::str(super::text_seg::unicode_line_break(ch)),
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
        "Jamo_Short_Name" | "JSN" => Value::str(super::char_props::unicode_jamo_short_name(ch)),
        // Emoji properties
        "Emoji" => Value::Bool(is_emoji(ch)),
        "Emoji_Modifier" => Value::Bool(is_emoji_modifier(ch)),
        "Emoji_Presentation" => Value::Bool(is_emoji_presentation(ch)),
        "Emoji_All" => Value::Bool(is_emoji_all(ch)),
        // Default: general category
        _ => Value::str(crate::builtins::unicode::unicode_general_category(ch)),
    }
}

/// Get the property value for a codepoint (as u32), handling
/// codepoints that may not be valid chars (like surrogates or
/// very high codepoints).
pub(crate) fn unicode_property_value_for_codepoint(cp: u32, prop: Option<&str>) -> Value {
    match char::from_u32(cp) {
        Some(ch) => match prop {
            Some(p) => unicode_property_value(ch, p),
            None => Value::str(crate::builtins::unicode::unicode_general_category(ch)),
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
    if let Some(b) = super::binary_props::try_binary_property(ch, prop_value) {
        return b;
    }

    // 3. Script name
    let script = crate::builtins::unicode::unicode_script_name(ch);
    if script.eq_ignore_ascii_case(prop_value) {
        return true;
    }

    // 4. Block name (normalize underscores/spaces/case)
    let block = super::char_props::unicode_block(ch);
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
            let script = crate::builtins::unicode::unicode_script_name(ch);
            script.eq_ignore_ascii_case(prop_value)
        }
        "blk" | "Block" => {
            let block = super::char_props::unicode_block(ch);
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
    let gc = crate::builtins::unicode::unicode_general_category(ch);

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
