/// UAX #29 Grapheme_Cluster_Break=Prepend set.
fn is_gcb_prepend(cp: u32) -> bool {
    matches!(cp,
        0x0600..=0x0605 | 0x06DD | 0x070F | 0x08E2 | 0x0D4E | 0x110BD | 0x110CD
        | 0x111C2 | 0x111C3 | 0x1193F | 0x11941 | 0x11A3A | 0x11A84..=0x11A89 | 0x11D46)
}

/// Spacing marks (gc=Mc) that are NOT Grapheme_Cluster_Break=SpacingMark: they
/// are either Extend (handled earlier) or Other. Listing them keeps a plain
/// gc=Mc test from over-classifying them as SpacingMark.
fn is_gcb_spacingmark_exception(cp: u32) -> bool {
    matches!(cp,
        0x09BE | 0x09D7 | 0x0B3E | 0x0B57 | 0x0BBE | 0x0BD7 | 0x0CC2 | 0x0CD5 | 0x0CD6
        | 0x0D3E | 0x0D57 | 0x0DCF | 0x0DDF | 0x102B | 0x102C | 0x1038 | 0x1062..=0x1064
        | 0x1067..=0x106D | 0x1083 | 0x1087..=0x108C | 0x108F | 0x109A..=0x109C | 0x1A61
        | 0x1A63 | 0x1A64 | 0x1B35 | 0x302E | 0x302F | 0xAA7B | 0xAA7D | 0x1133E | 0x11357
        | 0x114B0 | 0x114BD | 0x115AF | 0x11930 | 0x1D165 | 0x1D16E..=0x1D172)
}

/// Grapheme_Cluster_Break property (UAX #29 GraphemeBreakProperty).
pub(crate) fn unicode_grapheme_cluster_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => return "CR".to_string(),
        0x000A => return "LF".to_string(),
        0x200D => return "ZWJ".to_string(),
        // Regional indicator symbols.
        0x1F1E6..=0x1F1FF => return "Regional_Indicator".to_string(),
        _ => {}
    }
    // Prepend must precede Extend/Control (some Prepend are gc=Cf).
    if is_gcb_prepend(cp) {
        return "Prepend".to_string();
    }
    if super::binary_props::check_binary_property(ch, r"^\p{Grapheme_Extend}$") {
        return "Extend".to_string();
    }
    // Hangul jamo / syllables.
    match cp {
        0x1100..=0x115F | 0xA960..=0xA97C => return "L".to_string(),
        0x1160..=0x11A7 | 0xD7B0..=0xD7C6 => return "V".to_string(),
        0x11A8..=0x11FF | 0xD7CB..=0xD7FB => return "T".to_string(),
        0xAC00..=0xD7A3 => {
            return if (cp - 0xAC00).is_multiple_of(28) {
                "LV"
            } else {
                "LVT"
            }
            .to_string();
        }
        _ => {}
    }
    // SpacingMark: spacing combining marks plus U+0E33 / U+0EB3.
    let gc = crate::builtins::unicode::unicode_general_category(ch);
    if (gc == "Mc" && !is_gcb_spacingmark_exception(cp)) || cp == 0x0E33 || cp == 0x0EB3 {
        return "SpacingMark".to_string();
    }
    // Control: line/paragraph separators, control and format characters.
    if gc == "Cc" || gc == "Cf" || gc == "Zl" || gc == "Zp" {
        return "Control".to_string();
    }
    "Other".to_string()
}

/// Joining_Type property.
pub(crate) fn unicode_joining_type(ch: char) -> String {
    // Simplified - use General Category heuristic
    let gc = crate::builtins::unicode::unicode_general_category(ch);
    if gc == "Mn" || gc == "Me" || gc == "Cf" {
        // Most non-spacing marks are transparent
        if super::binary_props::check_binary_property(ch, r"^\p{Join_Control}$") {
            return "C".to_string(); // Join_Causing
        }
        return "T".to_string(); // Transparent
    }
    // For Arabic/Syriac script chars, they are usually D (Dual_Joining)
    let script = crate::builtins::unicode::unicode_script_name(ch);
    match script.as_str() {
        "Arabic" | "Syriac" | "Mandaic" | "Nko" | "Thaana" => "D".to_string(),
        _ => "U".to_string(), // Non_Joining
    }
}

/// Joining_Group property — derive from character name for Arabic/Syriac.
pub(crate) fn unicode_joining_group(ch: char) -> String {
    let script = crate::builtins::unicode::unicode_script_name(ch);
    let name = crate::builtins::unicode::unicode_char_name(ch);
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

/// UAX #29 Sentence_Break=SContinue set.
fn is_sb_scontinue(cp: u32) -> bool {
    matches!(
        cp,
        0x002C
            | 0x002D
            | 0x003A
            | 0x055D
            | 0x060C
            | 0x060D
            | 0x07F8
            | 0x1802
            | 0x1808
            | 0x2013
            | 0x2014
            | 0x3001
            | 0xFE10
            | 0xFE11
            | 0xFE13
            | 0xFE31
            | 0xFE32
            | 0xFE50
            | 0xFE51
            | 0xFE55
            | 0xFF0C
            | 0xFF0D
            | 0xFF1A
            | 0xFF64
    )
}

/// Sentence_Break property (UAX #29 SentenceBreakProperty).
pub(crate) fn unicode_sentence_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => return "CR".to_string(),
        0x000A => return "LF".to_string(),
        0x0085 | 0x2028 | 0x2029 => return "Sep".to_string(),
        _ => {}
    }
    let gc = crate::builtins::unicode::unicode_general_category(ch);
    // Extend: grapheme-extending marks, spacing marks, and ZWJ.
    if gc == "Mc"
        || cp == 0x200D
        || super::binary_props::check_binary_property(ch, r"^\p{Grapheme_Extend}$")
    {
        return "Extend".to_string();
    }
    if gc == "Cf" {
        return "Format".to_string();
    }
    if super::binary_props::check_binary_property(ch, r"^\p{White_Space}$") {
        return "Sp".to_string();
    }
    if gc == "Nd" {
        return "Numeric".to_string();
    }
    if cp == 0x002E || cp == 0x2024 || cp == 0xFF0E {
        return "ATerm".to_string();
    }
    if super::binary_props::check_binary_property(ch, r"^\p{Sentence_Terminal}$") {
        return "STerm".to_string();
    }
    // Close: open/close/quotation punctuation plus straight quotes.
    if matches!(gc.as_str(), "Ps" | "Pe" | "Pi" | "Pf") || cp == 0x0022 || cp == 0x0027 {
        return "Close".to_string();
    }
    if is_sb_scontinue(cp) {
        return "SContinue".to_string();
    }
    if gc == "Lt" || super::binary_props::check_binary_property(ch, r"^\p{Uppercase}$") {
        return "Upper".to_string();
    }
    if super::binary_props::check_binary_property(ch, r"^\p{Lowercase}$") {
        return "Lower".to_string();
    }
    if super::binary_props::check_binary_property(ch, r"^\p{Alphabetic}$") {
        return "OLetter".to_string();
    }
    "Other".to_string()
}

/// Is `cp` in the UAX #29 Word_Break=Katakana set (includes Common-script
/// katakana-related marks alongside the Katakana script proper).
fn is_wb_katakana(cp: u32) -> bool {
    matches!(cp,
        0x3031..=0x3035 | 0x309B | 0x309C | 0x30A0..=0x30FA | 0x30FC..=0x30FF
        | 0x31F0..=0x31FF | 0x32D0..=0x32FE | 0x3300..=0x3357
        | 0xFF66..=0xFF9D | 0x1B000 | 0x1B164..=0x1B167)
}

/// Word_Break property (UAX #29 WordBreakProperty).
pub(crate) fn unicode_word_break(ch: char) -> String {
    let cp = ch as u32;
    // Line separators, joiners, and explicit punctuation classes.
    match cp {
        0x000D => return "CR".to_string(),
        0x000A => return "LF".to_string(),
        0x000B | 0x000C | 0x0085 | 0x2028 | 0x2029 => return "Newline".to_string(),
        0x200D => return "ZWJ".to_string(),
        0x0022 => return "Double_Quote".to_string(),
        0x0027 => return "Single_Quote".to_string(),
        // MidNumLet
        0x002E | 0x2018 | 0x2019 | 0x2024 | 0xFE52 | 0xFF07 | 0xFF0E => {
            return "MidNumLet".to_string();
        }
        // MidLetter
        0x003A | 0x00B7 | 0x0387 | 0x05F4 | 0x2027 | 0xFE13 | 0xFE55 | 0xFF1A => {
            return "MidLetter".to_string();
        }
        // MidNum
        0x002C | 0x003B | 0x037E | 0x0589 | 0x060C | 0x060D | 0x066C | 0x07F8 | 0x2044 | 0xFE10
        | 0xFE14 | 0xFE50 | 0xFE54 | 0xFF0C | 0xFF1B => {
            return "MidNum".to_string();
        }
        _ => {}
    }
    if is_wb_katakana(cp) {
        return "Katakana".to_string();
    }
    let gc = crate::builtins::unicode::unicode_general_category(ch);
    // Connector_Punctuation is exactly the ExtendNumLet base set.
    if gc == "Pc" {
        return "ExtendNumLet".to_string();
    }
    // Extend: grapheme-extending marks and ZWNJ.
    if super::binary_props::check_binary_property(ch, r"^\p{Grapheme_Extend}$") {
        return "Extend".to_string();
    }
    // Format controls (joiners and ZWSP already handled above).
    if gc == "Cf" && cp != 0x200B {
        return "Format".to_string();
    }
    match gc.as_str() {
        "Nd" => "Numeric".to_string(),
        "Lu" | "Ll" | "Lt" | "Lm" | "Lo" => {
            let script = crate::builtins::unicode::unicode_script_name(ch);
            if script == "Hebrew" && (gc == "Lo" || gc == "Lm") {
                return "Hebrew_Letter".to_string();
            }
            // ALetter excludes ideographs, Hiragana, and Complex_Context
            // (Line_Break=SA: Thai/Lao/Myanmar/Khmer/...) letters.
            if super::binary_props::check_binary_property(ch, r"^\p{Ideographic}$")
                || script == "Hiragana"
                || unicode_line_break(ch) == "SA"
            {
                "Other".to_string()
            } else {
                "ALetter".to_string()
            }
        }
        _ => "Other".to_string(),
    }
}

/// Line_Break property.
pub(crate) fn unicode_line_break(ch: char) -> String {
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
            let gc = crate::builtins::unicode::unicode_general_category(ch);
            match gc.as_str() {
                "Ps" => "OP".to_string(),
                "Pe" => "CL".to_string(),
                "Zs" => "SP".to_string(),
                "Mn" | "Mc" | "Me" => {
                    // Southeast Asian scripts use SA for combining marks too
                    let script = crate::builtins::unicode::unicode_script_name(ch);
                    match script.as_str() {
                        "Thai" | "Lao" | "Myanmar" | "Khmer" | "Javanese" | "Tai_Tham"
                        | "New_Tai_Lue" | "Tai_Le" => "SA".to_string(),
                        _ => "CM".to_string(),
                    }
                }
                "Nd" => "NU".to_string(),
                "Lu" | "Ll" | "Lt" | "Lm" | "Lo" => {
                    let script = crate::builtins::unicode::unicode_script_name(ch);
                    match script.as_str() {
                        "Thai" | "Lao" | "Myanmar" | "Khmer" | "Javanese" | "Tai_Tham"
                        | "New_Tai_Lue" | "Tai_Le" => "SA".to_string(),
                        // Ideographic scripts have Line_Break=ID (not AL), e.g. a
                        // CJK ideograph or a Hiragana/Katakana letter.
                        "Han"
                        | "Hiragana"
                        | "Katakana"
                        | "Bopomofo"
                        | "Yi"
                        | "Tangut"
                        | "Nushu"
                        | "Khitan_Small_Script" => "ID".to_string(),
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
