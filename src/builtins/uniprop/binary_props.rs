/// Check if NFKC case folding changes the character.
fn check_changes_when_nfkc_casefolded(ch: char) -> bool {
    use unicode_normalization::UnicodeNormalization;
    let s = ch.to_string();
    // Apply NFKC case folding: NFKD -> casefold -> NFC
    let folded = crate::builtins::methods_0arg::unicode_foldcase(&s);
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
pub(crate) fn check_binary_property(ch: char, prop_regex_str: &str) -> bool {
    let re = regex::Regex::new(prop_regex_str).ok();
    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    re.is_some_and(|r| r.is_match(s))
}

/// Check a binary Unicode property by name, returning Some(bool) if it's
/// a known binary property, None otherwise.
pub(crate) fn try_binary_property(ch: char, prop: &str) -> Option<bool> {
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
