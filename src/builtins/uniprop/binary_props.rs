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

/// Check if a character has a binary Unicode property, named as the body of
/// a `\p{...}` class (`Alphabetic`, `Word_Break=Extend`).
///
/// This used to compile a `regex::Regex` per pattern (and, before a cache,
/// per *call* -- [#8999](https://github.com/tokuhirom/mutsu/issues/8999));
/// it is now a binary search over the class's ranges (#10439).
// Cost: O(log R), R = ranges in the class (after its first resolution).
pub(crate) fn check_binary_property(ch: char, prop: &str) -> bool {
    crate::builtins::unicode_prop_class::in_property_class(prop, ch).unwrap_or(false)
}

/// Check a binary Unicode property by name, returning Some(bool) if it's
/// a known binary property, None otherwise.
pub(crate) fn try_binary_property(ch: char, prop: &str) -> Option<bool> {
    // Map property names to `\p{...}` class names.
    let pattern = match prop {
        "Alphabetic" | "Alpha" => "Alphabetic",
        "Dash" => "Dash",
        "Diacritic" | "Dia" => "Diacritic",
        "Default_Ignorable_Code_Point" | "DI" => "Default_Ignorable_Code_Point",
        "Extender" | "Ext" => "Extender",
        "Grapheme_Base" | "Gr_Base" => "Grapheme_Base",
        "Grapheme_Extend" | "Gr_Ext" => "Grapheme_Extend",
        "Hex_Digit" | "Hex" => "Hex_Digit",
        "ASCII_Hex_Digit" | "AHex" => "ASCII_Hex_Digit",
        "ID_Continue" | "IDC" => "ID_Continue",
        "ID_Start" | "IDS" => "ID_Start",
        "Ideographic" | "Ideo" => "Ideographic",
        "IDS_Binary_Operator" | "IDSB" => "IDS_Binary_Operator",
        "IDS_Trinary_Operator" | "IDST" => "IDS_Trinary_Operator",
        "Join_Control" | "Join_C" => "Join_Control",
        "Math" => "Math",
        "Radical" => "Radical",
        "Soft_Dotted" | "SD" => "Soft_Dotted",
        "Terminal_Punctuation" | "Term" => "Terminal_Punctuation",
        "Variation_Selector" | "VS" => "Variation_Selector",
        "White_Space" | "WSpace" | "space" => "White_Space",
        "Uppercase" | "Upper" => "Uppercase",
        "Lowercase" | "Lower" => "Lowercase",
        "Bidi_Control" | "Bidi_C" => "Bidi_Control",
        "Bidi_Mirrored" | "Bidi_M" => "Bidi_Mirrored",
        "Case_Ignorable" | "CI" => "Case_Ignorable",
        "Cased" => "Cased",
        "Changes_When_Casefolded" | "CWCF" => "Changes_When_Casefolded",
        "Changes_When_Casemapped" | "CWCM" => "Changes_When_Casemapped",
        "Changes_When_Lowercased" | "CWL" => "Changes_When_Lowercased",
        "Changes_When_Uppercased" | "CWU" => "Changes_When_Uppercased",
        "Changes_When_Titlecased" | "CWT" => "Changes_When_Titlecased",
        "Changes_When_NFKC_Casefolded" | "CWKCF" => {
            // `\p{...}` has no such class; compute manually
            return Some(check_changes_when_nfkc_casefolded(ch));
        }
        "Deprecated" | "Dep" => "Deprecated",
        "Grapheme_Link" | "Gr_Link" => "Grapheme_Link",
        "Hyphen" => "Hyphen",
        "Quotation_Mark" | "QMark" => "Quotation_Mark",
        "Sentence_Terminal" | "STerm" => "Sentence_Terminal",
        "Full_Composition_Exclusion" | "Comp_Ex" => {
            return Some(check_full_composition_exclusion(ch));
        }
        "Pattern_White_Space" => "Pattern_White_Space",
        "Pattern_Syntax" => "Pattern_Syntax",
        "Other_Alphabetic" | "OAlpha" => "Other_Alphabetic",
        "Other_Lowercase" | "OLower" => "Other_Lowercase",
        "Other_Uppercase" | "OUpper" => "Other_Uppercase",
        "Other_Math" | "OMath" => "Other_Math",
        "Unified_Ideograph" | "UIdeo" => "Unified_Ideograph",
        "Noncharacter_Code_Point" | "NChar" => "Noncharacter_Code_Point",
        "Other_Grapheme_Extend" | "OGr_Ext" => "Other_Grapheme_Extend",
        "Other_ID_Continue" | "OIDC" => "Other_ID_Continue",
        "Other_ID_Start" | "OIDS" => "Other_ID_Start",
        "Other_Default_Ignorable_Code_Point" | "ODI" => "Other_Default_Ignorable_Code_Point",
        "XID_Start" | "XIDS" => "XID_Start",
        "XID_Continue" | "XIDC" => "XID_Continue",
        "Emoji" => "Emoji",
        "Emoji_Presentation" => "Emoji_Presentation",
        "Emoji_Modifier" => "Emoji_Modifier",
        "Emoji_Modifier_Base" => "Emoji_Modifier_Base",
        "Emoji_Component" => "Emoji_Component",
        "Extended_Pictographic" => "Extended_Pictographic",
        "Regional_Indicator" => "Regional_Indicator",
        _ => return None,
    };
    Some(check_binary_property(ch, pattern))
}
