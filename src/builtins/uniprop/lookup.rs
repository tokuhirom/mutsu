use crate::value::Value;

/// Render a `*_Quick_Check` result as Raku's full property-value name. Unlike
/// MoarVM (which returns the short `Y`/`N`/`M` codes — a `#?rakudo.moar todo` in
/// roast S15-unicode-information/uniprop.t), mutsu returns the full names.
fn quick_check_name(result: unicode_normalization::IsNormalized) -> String {
    use unicode_normalization::IsNormalized;
    match result {
        IsNormalized::Yes => "Yes",
        IsNormalized::No => "No",
        IsNormalized::Maybe => "Maybe",
    }
    .to_string()
}

/// NFC_Quick_Check property. Uses the crate's authoritative `NFC_QC` data, so a
/// character that can compose with a following one (e.g. a Hangul trailing
/// consonant) is correctly `Maybe`, not `Yes`.
fn unicode_nfc_quick_check(ch: char) -> String {
    quick_check_name(unicode_normalization::is_nfc_quick(std::iter::once(ch)))
}

/// NFD_Quick_Check property.
fn unicode_nfd_quick_check(ch: char) -> String {
    quick_check_name(unicode_normalization::is_nfd_quick(std::iter::once(ch)))
}

/// NFKC_Quick_Check property.
fn unicode_nfkc_quick_check(ch: char) -> String {
    quick_check_name(unicode_normalization::is_nfkc_quick(std::iter::once(ch)))
}

/// NFKD_Quick_Check property.
fn unicode_nfkd_quick_check(ch: char) -> String {
    quick_check_name(unicode_normalization::is_nfkd_quick(std::iter::once(ch)))
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

/// Emoji property.
fn is_emoji(ch: char) -> bool {
    super::binary_props::check_binary_property(ch, "Emoji")
}

/// Emoji_Modifier property.
fn is_emoji_modifier(ch: char) -> bool {
    let cp = ch as u32;
    (0x1F3FB..=0x1F3FF).contains(&cp)
}

/// Emoji_Presentation property.
fn is_emoji_presentation(ch: char) -> bool {
    super::binary_props::check_binary_property(ch, "Emoji_Presentation")
}

/// Emoji_All: True if any Emoji property is true.
fn is_emoji_all(ch: char) -> bool {
    is_emoji(ch) && (is_emoji_presentation(ch) || is_emoji_modifier(ch))
}

/// Main entry point: look up a Unicode property value for a character.
/// Returns a Value (Bool for binary props, Str for string/enum props, etc.)
pub(crate) fn unicode_property_value(ch: char, prop: &str) -> Value {
    let prop = &*super::prop_names::canonical_property_name(prop);
    // Check binary properties first
    if let Some(b) = super::binary_props::try_binary_property(ch, prop) {
        return Value::truth(b);
    }

    match prop {
        // General Category
        "General_Category" | "gc" => {
            Value::str_from(crate::builtins::unicode::unicode_general_category(ch))
        }
        // Script
        "Script" | "sc" => Value::str_from(crate::builtins::unicode::unicode_script_name(ch)),
        // Age
        "Age" => Value::str_from(super::age_table::unicode_age(ch as u32)),
        // Block
        "Block" | "blk" => Value::str(super::char_props::unicode_block(ch)),
        // Name
        "Name" | "na" => Value::str(crate::builtins::unicode::unicode_char_name(ch)),
        // Unicode_1_Name
        "Unicode_1_Name" | "na1" => {
            // Unicode 1.0 names are mostly the same; some differ
            // For now return 0 (same as MoarVM behavior for most chars)
            Value::int(0)
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
            let result = match gc {
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
        "East_Asian_Width" | "ea" => Value::str_from(east_asian_width_short(
            &super::char_props::unicode_east_asian_width(ch),
        )),
        // Hangul
        "Hangul_Syllable_Type" | "hst" => {
            Value::str(super::char_props::unicode_hangul_syllable_type(ch))
        }
        // Grapheme cluster break
        "Grapheme_Cluster_Break" | "GCB" => {
            Value::str_from(super::text_seg::unicode_grapheme_cluster_break(ch))
        }
        // Joining
        "Joining_Group" | "jg" => Value::str_from(super::text_seg::unicode_joining_group(ch)),
        "Joining_Type" | "jt" => Value::str_from(super::text_seg::unicode_joining_type(ch)),
        // Sentence Break
        "Sentence_Break" | "SB" => Value::str_from(super::text_seg::unicode_sentence_break(ch)),
        // Word Break
        "Word_Break" | "WB" => Value::str_from(super::text_seg::unicode_word_break(ch)),
        // Line Break
        "Line_Break" | "lb" => Value::str_from(super::text_seg::unicode_line_break(ch)),
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
        "Emoji" => Value::truth(is_emoji(ch)),
        "Emoji_Modifier" => Value::truth(is_emoji_modifier(ch)),
        "Emoji_Presentation" => Value::truth(is_emoji_presentation(ch)),
        "Emoji_All" => Value::truth(is_emoji_all(ch)),
        // Default: general category
        _ => Value::str_from(crate::builtins::unicode::unicode_general_category(ch)),
    }
}

/// Get the property value for a codepoint (as u32), handling
/// codepoints that may not be valid chars (like surrogates or
/// very high codepoints).
// Cost: O(1) (a table lookup) plus the property name's parse.
pub(crate) fn unicode_property_value_for_codepoint(cp: u32, prop: Option<&str>) -> Value {
    if let Some(ch) = char::from_u32(cp) {
        return match prop {
            Some(p) => unicode_property_value(ch, p),
            None => Value::str_from(crate::builtins::unicode::unicode_general_category(ch)),
        };
    }
    let prop = prop.map(super::prop_names::canonical_property_name);
    let prop = prop.as_deref();
    if (0xD800..=0xDFFF).contains(&cp) {
        return match prop {
            Some("General_Category") | Some("gc") | None => Value::str_from("Cs"),
            Some("Block") | Some("blk") => Value::str_from(surrogate_block(cp)),
            _ => Value::str_from(""),
        };
    }
    // Past 0x10FFFF MoarVM has no Unicode data: each property answers its
    // type's default.
    match prop {
        Some(p) if super::binary_props::try_binary_property('\0', p).is_some() => Value::FALSE,
        Some("Numeric_Value") | Some("nv") => Value::NIL,
        Some("Name") | Some("na") => Value::str_from("<unassigned>"),
        _ => Value::str_from(""),
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

    // 4. Block name (case and underscores normalized)
    let block = unimatch_block(ch);
    if block_matches_bare(&block, prop_value) {
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
            let block = unimatch_block(ch);
            block_matches(&block, prop_value)
        }
        "ea" | "East_Asian_Width" => {
            let val = super::char_props::unicode_east_asian_width(ch);
            east_asian_width_matches(&val, prop_value)
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
    use crate::builtins::unicode_gc::{GeneralCategory, general_category};

    let gc = general_category(ch);

    // LC (Cased_Letter) matches Lu, Ll, Lt.
    if cat == "LC" {
        return gc.in_mask(GeneralCategory::CASED_LETTER);
    }

    // Parent category match: a single-letter category matches any two-letter
    // sub-category starting with that letter ("L" matches Lu, Ll, Lt, Lm, Lo).
    if let [letter] = cat.as_bytes() {
        return gc.major() == *letter;
    }

    // Exact match.
    GeneralCategory::from_name(cat) == Some(gc)
}

/// Match an East_Asian_Width value. Rakudo exposes East_Asian_Width using the
/// short property-value aliases (W, Na, F, H, A, N), so normalise our long-form
/// value to the short alias and compare against the target as given.
fn east_asian_width_matches(actual: &str, target: &str) -> bool {
    let short = east_asian_width_short(actual);
    !short.is_empty() && short.eq_ignore_ascii_case(target.trim())
}

/// The short East_Asian_Width alias (`W`, `Na`, ...) of a codepoint.
// Cost: O(1) (range table).
pub(crate) fn east_asian_width_abbrev(ch: char) -> &'static str {
    east_asian_width_short(&super::char_props::unicode_east_asian_width(ch))
}

/// The short property-value alias (`W`, `Na`, ...) of an East_Asian_Width
/// value, or `""` for an unknown one.
fn east_asian_width_short(value: &str) -> &'static str {
    match value.to_ascii_uppercase().as_str() {
        "N" | "NEUTRAL" => "N",
        "NA" | "NARROW" => "Na",
        "W" | "WIDE" => "W",
        "F" | "FULLWIDTH" => "F",
        "H" | "HALFWIDTH" => "H",
        "A" | "AMBIGUOUS" => "A",
        _ => "",
    }
}

/// The block `unimatch` reads for `ch`: an unassigned codepoint (`Cn`) is in
/// `No_Block`, though `uniprop("Block")` still answers its range's block.
// Cost: O(1) (a table lookup).
fn unimatch_block(ch: char) -> String {
    if unimatch_general_category(ch, "Cn") {
        "No_Block".to_string()
    } else {
        super::char_props::unicode_block(ch)
    }
}

/// Whether a property-less `unimatch` value names `block`: case and
/// underscores are ignored, but not spaces or hyphens (Rakudo matches
/// `Basic_Latin`, not `Basic Latin` or `Basic-Latin`).
// Cost: O(n), n = chars of the names.
fn block_matches_bare(block: &str, target: &str) -> bool {
    !target.contains([' ', '-']) && block_matches(block, target)
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

/// The block of a surrogate codepoint (`0xD800..=0xDFFF`).
// Cost: O(1).
fn surrogate_block(cp: u32) -> &'static str {
    if cp <= 0xDB7F {
        "High Surrogates"
    } else if cp <= 0xDBFF {
        "High Private Use Surrogates"
    } else {
        "Low Surrogates"
    }
}

/// An unassigned codepoint (`Cn`, script `Unknown`): `unimatch` reads the
/// properties of a codepoint no `char` can hold (a surrogate, or one past
/// `0x10FFFF`) from it, since MoarVM gives such a codepoint the same
/// defaults. Only its general category and block differ, and those are
/// matched separately.
const UNASSIGNED: char = '\u{0378}';

/// `unimatch` for an integer codepoint below `2**31`: a surrogate is `Cs` in
/// its surrogate block, a codepoint past `0x10FFFF` is `Cn` in `No_Block`, and
/// both take an unassigned codepoint's other property values.
// Cost: O(1) (a table lookup) plus the value name's parse.
pub(crate) fn unimatch_for_codepoint(cp: u32, prop_value: &str, prop_name: Option<&str>) -> Value {
    if let Some(ch) = char::from_u32(cp) {
        return Value::truth(unimatch(ch, prop_value, prop_name));
    }
    let (gc, block) = if (0xD800..=0xDFFF).contains(&cp) {
        ("Cs", surrogate_block(cp))
    } else {
        ("Cn", "No_Block")
    };
    // An exact category, or its major class ("C").
    let gc_matches = || prop_value == gc || (prop_value.len() == 1 && gc.starts_with(prop_value));
    Value::truth(match prop_name {
        Some("gc") | Some("General_Category") => gc_matches(),
        Some("blk") | Some("Block") => block_matches(block, prop_value),
        Some(name) => unimatch_property(UNASSIGNED, prop_value, name),
        // The order `unimatch` tries for a char.
        None => {
            gc_matches()
                || super::binary_props::try_binary_property(UNASSIGNED, prop_value).unwrap_or(false)
                || crate::builtins::unicode::unicode_script_name(UNASSIGNED)
                    .eq_ignore_ascii_case(prop_value)
                || block_matches_bare(block, prop_value)
        }
    })
}
