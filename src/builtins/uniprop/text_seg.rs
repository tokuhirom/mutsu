/// Grapheme_Cluster_Break property.
pub(crate) fn unicode_grapheme_cluster_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => "CR",
        0x000A => "LF",
        0x200D => "ZWJ",
        _ => {
            // Use regex for Extend
            if super::binary_props::check_binary_property(ch, r"^\p{Grapheme_Extend}$") {
                return "Extend".to_string();
            }
            let gc = crate::builtins::unicode::unicode_general_category(ch);
            if gc == "Cc" || gc == "Cf" {
                return "Control".to_string();
            }
            return "Other".to_string();
        }
    }
    .to_string()
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

/// Sentence_Break property.
pub(crate) fn unicode_sentence_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => "CR".to_string(),
        0x000A => "LF".to_string(),
        0x0009 | 0x000B | 0x000C => "Sp".to_string(),
        _ => {
            let gc = crate::builtins::unicode::unicode_general_category(ch);
            match gc.as_str() {
                "Lu" => "Upper".to_string(),
                "Ll" => "Lower".to_string(),
                "Nd" => "Numeric".to_string(),
                "Zs" => "Sp".to_string(),
                _ => {
                    // Check for ATerm (. and similar)
                    if cp == 0x002E || cp == 0x2024 {
                        "ATerm".to_string()
                    } else if super::binary_props::check_binary_property(
                        ch,
                        r"^\p{Sentence_Terminal}$",
                    ) {
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
pub(crate) fn unicode_word_break(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x000D => "CR".to_string(),
        0x000A | 0x000B | 0x000C | 0x0085 | 0x2028 | 0x2029 => "LF".to_string(),
        0x200D => "ZWJ".to_string(),
        _ => {
            let gc = crate::builtins::unicode::unicode_general_category(ch);
            let script = crate::builtins::unicode::unicode_script_name(ch);
            if script == "Hebrew" && (gc == "Lo" || gc == "Lm") {
                return "Hebrew_Letter".to_string();
            }
            match gc.as_str() {
                "Lu" | "Ll" | "Lt" | "Lm" | "Lo" => "ALetter".to_string(),
                "Nd" => "Numeric".to_string(),
                "Mn" | "Me" | "Mc" => {
                    if super::binary_props::check_binary_property(ch, r"^\p{Grapheme_Extend}$") {
                        "Extend".to_string()
                    } else {
                        "Other".to_string()
                    }
                }
                _ => {
                    if super::binary_props::check_binary_property(ch, r"^\p{Extender}$") {
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
