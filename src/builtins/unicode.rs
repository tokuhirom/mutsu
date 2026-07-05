/// Unicode titlecase for the first character of a string.
/// Titlecase differs from uppercase for certain characters:
/// - 'ss' -> "Ss" (not "SS")
/// - digraph ligatures: special titlecase forms
pub(crate) fn unicode_titlecase_first(ch: char) -> String {
    match ch {
        // Latin digraph titlecase triples (lower | upper | title) → title form.
        // The title form titlecases to ITSELF, not to the full uppercase digraph.
        '\u{01C6}' | '\u{01C4}' | '\u{01C5}' => "\u{01C5}".to_string(), // Dž
        '\u{01C9}' | '\u{01C7}' | '\u{01C8}' => "\u{01C8}".to_string(), // Lj
        '\u{01CC}' | '\u{01CA}' | '\u{01CB}' => "\u{01CB}".to_string(), // Nj
        '\u{01F3}' | '\u{01F1}' | '\u{01F2}' => "\u{01F2}".to_string(), // Dz
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

/// Characters whose Unicode full case fold expands to more than one codepoint,
/// or whose simple case fold differs from `char::to_lowercase` (µ, the Greek
/// iota-subscript vowels, ...). Generated from CaseFolding.txt. Returns `None`
/// when the simple lowercase is the fold.
pub(crate) fn full_case_fold(cp: u32) -> Option<&'static str> {
    Some(match cp {
        0x00B5 => "\u{3BC}",
        0x00DF => "\u{73}\u{73}",
        0x0130 => "\u{69}\u{307}",
        0x0149 => "\u{2BC}\u{6E}",
        0x0344 => "\u{308}\u{301}",
        0x0345 => "\u{3B9}",
        0x0587 => "\u{565}\u{582}",
        0x0958 => "\u{915}\u{93C}",
        0x0959 => "\u{916}\u{93C}",
        0x095A => "\u{917}\u{93C}",
        0x095B => "\u{91C}\u{93C}",
        0x095C => "\u{921}\u{93C}",
        0x095D => "\u{922}\u{93C}",
        0x095E => "\u{92B}\u{93C}",
        0x095F => "\u{92F}\u{93C}",
        0x09DC => "\u{9A1}\u{9BC}",
        0x09DD => "\u{9A2}\u{9BC}",
        0x09DF => "\u{9AF}\u{9BC}",
        0x0A33 => "\u{A32}\u{A3C}",
        0x0A36 => "\u{A38}\u{A3C}",
        0x0A59 => "\u{A16}\u{A3C}",
        0x0A5A => "\u{A17}\u{A3C}",
        0x0A5B => "\u{A1C}\u{A3C}",
        0x0A5E => "\u{A2B}\u{A3C}",
        0x0B5C => "\u{B21}\u{B3C}",
        0x0B5D => "\u{B22}\u{B3C}",
        0x0F43 => "\u{F42}\u{FB7}",
        0x0F4D => "\u{F4C}\u{FB7}",
        0x0F52 => "\u{F51}\u{FB7}",
        0x0F57 => "\u{F56}\u{FB7}",
        0x0F5C => "\u{F5B}\u{FB7}",
        0x0F69 => "\u{F40}\u{FB5}",
        0x0F73 => "\u{F71}\u{F72}",
        0x0F75 => "\u{F71}\u{F74}",
        0x0F76 => "\u{FB2}\u{F80}",
        0x0F78 => "\u{FB3}\u{F80}",
        0x0F81 => "\u{F71}\u{F80}",
        0x0F93 => "\u{F92}\u{FB7}",
        0x0F9D => "\u{F9C}\u{FB7}",
        0x0FA2 => "\u{FA1}\u{FB7}",
        0x0FA7 => "\u{FA6}\u{FB7}",
        0x0FAC => "\u{FAB}\u{FB7}",
        0x0FB9 => "\u{F90}\u{FB5}",
        0x1E9A => "\u{61}\u{2BE}",
        0x1E9E => "\u{73}\u{73}",
        0x1F80 => "\u{1F00}\u{3B9}",
        0x1F81 => "\u{1F01}\u{3B9}",
        0x1F82 => "\u{1F02}\u{3B9}",
        0x1F83 => "\u{1F03}\u{3B9}",
        0x1F84 => "\u{1F04}\u{3B9}",
        0x1F85 => "\u{1F05}\u{3B9}",
        0x1F86 => "\u{1F06}\u{3B9}",
        0x1F87 => "\u{1F07}\u{3B9}",
        0x1F88 => "\u{1F00}\u{3B9}",
        0x1F89 => "\u{1F01}\u{3B9}",
        0x1F8A => "\u{1F02}\u{3B9}",
        0x1F8B => "\u{1F03}\u{3B9}",
        0x1F8C => "\u{1F04}\u{3B9}",
        0x1F8D => "\u{1F05}\u{3B9}",
        0x1F8E => "\u{1F06}\u{3B9}",
        0x1F8F => "\u{1F07}\u{3B9}",
        0x1F90 => "\u{1F20}\u{3B9}",
        0x1F91 => "\u{1F21}\u{3B9}",
        0x1F92 => "\u{1F22}\u{3B9}",
        0x1F93 => "\u{1F23}\u{3B9}",
        0x1F94 => "\u{1F24}\u{3B9}",
        0x1F95 => "\u{1F25}\u{3B9}",
        0x1F96 => "\u{1F26}\u{3B9}",
        0x1F97 => "\u{1F27}\u{3B9}",
        0x1F98 => "\u{1F20}\u{3B9}",
        0x1F99 => "\u{1F21}\u{3B9}",
        0x1F9A => "\u{1F22}\u{3B9}",
        0x1F9B => "\u{1F23}\u{3B9}",
        0x1F9C => "\u{1F24}\u{3B9}",
        0x1F9D => "\u{1F25}\u{3B9}",
        0x1F9E => "\u{1F26}\u{3B9}",
        0x1F9F => "\u{1F27}\u{3B9}",
        0x1FA0 => "\u{1F60}\u{3B9}",
        0x1FA1 => "\u{1F61}\u{3B9}",
        0x1FA2 => "\u{1F62}\u{3B9}",
        0x1FA3 => "\u{1F63}\u{3B9}",
        0x1FA4 => "\u{1F64}\u{3B9}",
        0x1FA5 => "\u{1F65}\u{3B9}",
        0x1FA6 => "\u{1F66}\u{3B9}",
        0x1FA7 => "\u{1F67}\u{3B9}",
        0x1FA8 => "\u{1F60}\u{3B9}",
        0x1FA9 => "\u{1F61}\u{3B9}",
        0x1FAA => "\u{1F62}\u{3B9}",
        0x1FAB => "\u{1F63}\u{3B9}",
        0x1FAC => "\u{1F64}\u{3B9}",
        0x1FAD => "\u{1F65}\u{3B9}",
        0x1FAE => "\u{1F66}\u{3B9}",
        0x1FAF => "\u{1F67}\u{3B9}",
        0x1FB2 => "\u{1F70}\u{3B9}",
        0x1FB3 => "\u{3B1}\u{3B9}",
        0x1FB4 => "\u{3AC}\u{3B9}",
        0x1FB7 => "\u{1FB6}\u{3B9}",
        0x1FBC => "\u{3B1}\u{3B9}",
        0x1FBE => "\u{3B9}",
        0x1FC2 => "\u{1F74}\u{3B9}",
        0x1FC3 => "\u{3B7}\u{3B9}",
        0x1FC4 => "\u{3AE}\u{3B9}",
        0x1FC7 => "\u{1FC6}\u{3B9}",
        0x1FCC => "\u{3B7}\u{3B9}",
        0x1FF2 => "\u{1F7C}\u{3B9}",
        0x1FF3 => "\u{3C9}\u{3B9}",
        0x1FF4 => "\u{3CE}\u{3B9}",
        0x1FF7 => "\u{1FF6}\u{3B9}",
        0x1FFC => "\u{3C9}\u{3B9}",
        0x2ADC => "\u{2ADD}\u{338}",
        0xFB00 => "\u{66}\u{66}",
        0xFB01 => "\u{66}\u{69}",
        0xFB02 => "\u{66}\u{6C}",
        0xFB03 => "\u{66}\u{66}\u{69}",
        0xFB04 => "\u{66}\u{66}\u{6C}",
        0xFB05 => "\u{73}\u{74}",
        0xFB06 => "\u{73}\u{74}",
        0xFB13 => "\u{574}\u{576}",
        0xFB14 => "\u{574}\u{565}",
        0xFB15 => "\u{574}\u{56B}",
        0xFB16 => "\u{57E}\u{576}",
        0xFB17 => "\u{574}\u{56D}",
        0xFB1D => "\u{5D9}\u{5B4}",
        0xFB1F => "\u{5F2}\u{5B7}",
        0xFB2A => "\u{5E9}\u{5C1}",
        0xFB2B => "\u{5E9}\u{5C2}",
        0xFB2C => "\u{5E9}\u{5BC}\u{5C1}",
        0xFB2D => "\u{5E9}\u{5BC}\u{5C2}",
        0xFB2E => "\u{5D0}\u{5B7}",
        0xFB2F => "\u{5D0}\u{5B8}",
        0xFB30 => "\u{5D0}\u{5BC}",
        0xFB31 => "\u{5D1}\u{5BC}",
        0xFB32 => "\u{5D2}\u{5BC}",
        0xFB33 => "\u{5D3}\u{5BC}",
        0xFB34 => "\u{5D4}\u{5BC}",
        0xFB35 => "\u{5D5}\u{5BC}",
        0xFB36 => "\u{5D6}\u{5BC}",
        0xFB38 => "\u{5D8}\u{5BC}",
        0xFB39 => "\u{5D9}\u{5BC}",
        0xFB3A => "\u{5DA}\u{5BC}",
        0xFB3B => "\u{5DB}\u{5BC}",
        0xFB3C => "\u{5DC}\u{5BC}",
        0xFB3E => "\u{5DE}\u{5BC}",
        0xFB40 => "\u{5E0}\u{5BC}",
        0xFB41 => "\u{5E1}\u{5BC}",
        0xFB43 => "\u{5E3}\u{5BC}",
        0xFB44 => "\u{5E4}\u{5BC}",
        0xFB46 => "\u{5E6}\u{5BC}",
        0xFB47 => "\u{5E7}\u{5BC}",
        0xFB48 => "\u{5E8}\u{5BC}",
        0xFB49 => "\u{5E9}\u{5BC}",
        0xFB4A => "\u{5EA}\u{5BC}",
        0xFB4B => "\u{5D5}\u{5B9}",
        0xFB4C => "\u{5D1}\u{5BF}",
        0xFB4D => "\u{5DB}\u{5BF}",
        0xFB4E => "\u{5E4}\u{5BF}",
        0x1D15E => "\u{1D157}\u{1D165}",
        0x1D15F => "\u{1D158}\u{1D165}",
        0x1D160 => "\u{1D158}\u{1D165}\u{1D16E}",
        0x1D161 => "\u{1D158}\u{1D165}\u{1D16F}",
        0x1D162 => "\u{1D158}\u{1D165}\u{1D170}",
        0x1D163 => "\u{1D158}\u{1D165}\u{1D171}",
        0x1D164 => "\u{1D158}\u{1D165}\u{1D172}",
        0x1D1BB => "\u{1D1B9}\u{1D165}",
        0x1D1BC => "\u{1D1BA}\u{1D165}",
        0x1D1BD => "\u{1D1B9}\u{1D165}\u{1D16E}",
        0x1D1BE => "\u{1D1BA}\u{1D165}\u{1D16E}",
        0x1D1BF => "\u{1D1B9}\u{1D165}\u{1D16F}",
        0x1D1C0 => "\u{1D1BA}\u{1D165}\u{1D16F}",
        0x17F => "\u{0073}",
        0x3C2 => "\u{03C3}",
        0x3D0 => "\u{03B2}",
        0x3D1 => "\u{03B8}",
        0x3D5 => "\u{03C6}",
        0x3D6 => "\u{03C0}",
        0x3F0 => "\u{03BA}",
        0x3F1 => "\u{03C1}",
        0x3F5 => "\u{03B5}",
        0x13A0 => "\u{13A0}",
        0x13A1 => "\u{13A1}",
        0x13A2 => "\u{13A2}",
        0x13A3 => "\u{13A3}",
        0x13A4 => "\u{13A4}",
        0x13A5 => "\u{13A5}",
        0x13A6 => "\u{13A6}",
        0x13A7 => "\u{13A7}",
        0x13A8 => "\u{13A8}",
        0x13A9 => "\u{13A9}",
        0x13AA => "\u{13AA}",
        0x13AB => "\u{13AB}",
        0x13AC => "\u{13AC}",
        0x13AD => "\u{13AD}",
        0x13AE => "\u{13AE}",
        0x13AF => "\u{13AF}",
        0x13B0 => "\u{13B0}",
        0x13B1 => "\u{13B1}",
        0x13B2 => "\u{13B2}",
        0x13B3 => "\u{13B3}",
        0x13B4 => "\u{13B4}",
        0x13B5 => "\u{13B5}",
        0x13B6 => "\u{13B6}",
        0x13B7 => "\u{13B7}",
        0x13B8 => "\u{13B8}",
        0x13B9 => "\u{13B9}",
        0x13BA => "\u{13BA}",
        0x13BB => "\u{13BB}",
        0x13BC => "\u{13BC}",
        0x13BD => "\u{13BD}",
        0x13BE => "\u{13BE}",
        0x13BF => "\u{13BF}",
        0x13C0 => "\u{13C0}",
        0x13C1 => "\u{13C1}",
        0x13C2 => "\u{13C2}",
        0x13C3 => "\u{13C3}",
        0x13C4 => "\u{13C4}",
        0x13C5 => "\u{13C5}",
        0x13C6 => "\u{13C6}",
        0x13C7 => "\u{13C7}",
        0x13C8 => "\u{13C8}",
        0x13C9 => "\u{13C9}",
        0x13CA => "\u{13CA}",
        0x13CB => "\u{13CB}",
        0x13CC => "\u{13CC}",
        0x13CD => "\u{13CD}",
        0x13CE => "\u{13CE}",
        0x13CF => "\u{13CF}",
        0x13D0 => "\u{13D0}",
        0x13D1 => "\u{13D1}",
        0x13D2 => "\u{13D2}",
        0x13D3 => "\u{13D3}",
        0x13D4 => "\u{13D4}",
        0x13D5 => "\u{13D5}",
        0x13D6 => "\u{13D6}",
        0x13D7 => "\u{13D7}",
        0x13D8 => "\u{13D8}",
        0x13D9 => "\u{13D9}",
        0x13DA => "\u{13DA}",
        0x13DB => "\u{13DB}",
        0x13DC => "\u{13DC}",
        0x13DD => "\u{13DD}",
        0x13DE => "\u{13DE}",
        0x13DF => "\u{13DF}",
        0x13E0 => "\u{13E0}",
        0x13E1 => "\u{13E1}",
        0x13E2 => "\u{13E2}",
        0x13E3 => "\u{13E3}",
        0x13E4 => "\u{13E4}",
        0x13E5 => "\u{13E5}",
        0x13E6 => "\u{13E6}",
        0x13E7 => "\u{13E7}",
        0x13E8 => "\u{13E8}",
        0x13E9 => "\u{13E9}",
        0x13EA => "\u{13EA}",
        0x13EB => "\u{13EB}",
        0x13EC => "\u{13EC}",
        0x13ED => "\u{13ED}",
        0x13EE => "\u{13EE}",
        0x13EF => "\u{13EF}",
        0x13F0 => "\u{13F0}",
        0x13F1 => "\u{13F1}",
        0x13F2 => "\u{13F2}",
        0x13F3 => "\u{13F3}",
        0x13F4 => "\u{13F4}",
        0x13F5 => "\u{13F5}",
        0x13F8 => "\u{13F0}",
        0x13F9 => "\u{13F1}",
        0x13FA => "\u{13F2}",
        0x13FB => "\u{13F3}",
        0x13FC => "\u{13F4}",
        0x13FD => "\u{13F5}",
        0x1C80 => "\u{0432}",
        0x1C81 => "\u{0434}",
        0x1C82 => "\u{043E}",
        0x1C83 => "\u{0441}",
        0x1C84 => "\u{0442}",
        0x1C85 => "\u{0442}",
        0x1C86 => "\u{044A}",
        0x1C87 => "\u{0463}",
        0x1C88 => "\u{A64B}",
        0x1C89 => "\u{1C89}",
        0x1E9B => "\u{1E61}",
        0x2C2F => "\u{2C2F}",
        0xA7C0 => "\u{A7C0}",
        0xA7CB => "\u{A7CB}",
        0xA7CC => "\u{A7CC}",
        0xA7CE => "\u{A7CE}",
        0xA7D0 => "\u{A7D0}",
        0xA7D2 => "\u{A7D2}",
        0xA7D4 => "\u{A7D4}",
        0xA7D6 => "\u{A7D6}",
        0xA7D8 => "\u{A7D8}",
        0xA7DA => "\u{A7DA}",
        0xA7DC => "\u{A7DC}",
        0xAB70 => "\u{13A0}",
        0xAB71 => "\u{13A1}",
        0xAB72 => "\u{13A2}",
        0xAB73 => "\u{13A3}",
        0xAB74 => "\u{13A4}",
        0xAB75 => "\u{13A5}",
        0xAB76 => "\u{13A6}",
        0xAB77 => "\u{13A7}",
        0xAB78 => "\u{13A8}",
        0xAB79 => "\u{13A9}",
        0xAB7A => "\u{13AA}",
        0xAB7B => "\u{13AB}",
        0xAB7C => "\u{13AC}",
        0xAB7D => "\u{13AD}",
        0xAB7E => "\u{13AE}",
        0xAB7F => "\u{13AF}",
        0xAB80 => "\u{13B0}",
        0xAB81 => "\u{13B1}",
        0xAB82 => "\u{13B2}",
        0xAB83 => "\u{13B3}",
        0xAB84 => "\u{13B4}",
        0xAB85 => "\u{13B5}",
        0xAB86 => "\u{13B6}",
        0xAB87 => "\u{13B7}",
        0xAB88 => "\u{13B8}",
        0xAB89 => "\u{13B9}",
        0xAB8A => "\u{13BA}",
        0xAB8B => "\u{13BB}",
        0xAB8C => "\u{13BC}",
        0xAB8D => "\u{13BD}",
        0xAB8E => "\u{13BE}",
        0xAB8F => "\u{13BF}",
        0xAB90 => "\u{13C0}",
        0xAB91 => "\u{13C1}",
        0xAB92 => "\u{13C2}",
        0xAB93 => "\u{13C3}",
        0xAB94 => "\u{13C4}",
        0xAB95 => "\u{13C5}",
        0xAB96 => "\u{13C6}",
        0xAB97 => "\u{13C7}",
        0xAB98 => "\u{13C8}",
        0xAB99 => "\u{13C9}",
        0xAB9A => "\u{13CA}",
        0xAB9B => "\u{13CB}",
        0xAB9C => "\u{13CC}",
        0xAB9D => "\u{13CD}",
        0xAB9E => "\u{13CE}",
        0xAB9F => "\u{13CF}",
        0xABA0 => "\u{13D0}",
        0xABA1 => "\u{13D1}",
        0xABA2 => "\u{13D2}",
        0xABA3 => "\u{13D3}",
        0xABA4 => "\u{13D4}",
        0xABA5 => "\u{13D5}",
        0xABA6 => "\u{13D6}",
        0xABA7 => "\u{13D7}",
        0xABA8 => "\u{13D8}",
        0xABA9 => "\u{13D9}",
        0xABAA => "\u{13DA}",
        0xABAB => "\u{13DB}",
        0xABAC => "\u{13DC}",
        0xABAD => "\u{13DD}",
        0xABAE => "\u{13DE}",
        0xABAF => "\u{13DF}",
        0xABB0 => "\u{13E0}",
        0xABB1 => "\u{13E1}",
        0xABB2 => "\u{13E2}",
        0xABB3 => "\u{13E3}",
        0xABB4 => "\u{13E4}",
        0xABB5 => "\u{13E5}",
        0xABB6 => "\u{13E6}",
        0xABB7 => "\u{13E7}",
        0xABB8 => "\u{13E8}",
        0xABB9 => "\u{13E9}",
        0xABBA => "\u{13EA}",
        0xABBB => "\u{13EB}",
        0xABBC => "\u{13EC}",
        0xABBD => "\u{13ED}",
        0xABBE => "\u{13EE}",
        0xABBF => "\u{13EF}",
        0x10570 => "\u{10570}",
        0x10571 => "\u{10571}",
        0x10572 => "\u{10572}",
        0x10573 => "\u{10573}",
        0x10574 => "\u{10574}",
        0x10575 => "\u{10575}",
        0x10576 => "\u{10576}",
        0x10577 => "\u{10577}",
        0x10578 => "\u{10578}",
        0x10579 => "\u{10579}",
        0x1057A => "\u{1057A}",
        0x1057C => "\u{1057C}",
        0x1057D => "\u{1057D}",
        0x1057E => "\u{1057E}",
        0x1057F => "\u{1057F}",
        0x10580 => "\u{10580}",
        0x10581 => "\u{10581}",
        0x10582 => "\u{10582}",
        0x10583 => "\u{10583}",
        0x10584 => "\u{10584}",
        0x10585 => "\u{10585}",
        0x10586 => "\u{10586}",
        0x10587 => "\u{10587}",
        0x10588 => "\u{10588}",
        0x10589 => "\u{10589}",
        0x1058A => "\u{1058A}",
        0x1058C => "\u{1058C}",
        0x1058D => "\u{1058D}",
        0x1058E => "\u{1058E}",
        0x1058F => "\u{1058F}",
        0x10590 => "\u{10590}",
        0x10591 => "\u{10591}",
        0x10592 => "\u{10592}",
        0x10594 => "\u{10594}",
        0x10595 => "\u{10595}",
        0x10D50 => "\u{10D50}",
        0x10D51 => "\u{10D51}",
        0x10D52 => "\u{10D52}",
        0x10D53 => "\u{10D53}",
        0x10D54 => "\u{10D54}",
        0x10D55 => "\u{10D55}",
        0x10D56 => "\u{10D56}",
        0x10D57 => "\u{10D57}",
        0x10D58 => "\u{10D58}",
        0x10D59 => "\u{10D59}",
        0x10D5A => "\u{10D5A}",
        0x10D5B => "\u{10D5B}",
        0x10D5C => "\u{10D5C}",
        0x10D5D => "\u{10D5D}",
        0x10D5E => "\u{10D5E}",
        0x10D5F => "\u{10D5F}",
        0x10D60 => "\u{10D60}",
        0x10D61 => "\u{10D61}",
        0x10D62 => "\u{10D62}",
        0x10D63 => "\u{10D63}",
        0x10D64 => "\u{10D64}",
        0x10D65 => "\u{10D65}",
        0x16EA0 => "\u{16EA0}",
        0x16EA1 => "\u{16EA1}",
        0x16EA2 => "\u{16EA2}",
        0x16EA3 => "\u{16EA3}",
        0x16EA4 => "\u{16EA4}",
        0x16EA5 => "\u{16EA5}",
        0x16EA6 => "\u{16EA6}",
        0x16EA7 => "\u{16EA7}",
        0x16EA8 => "\u{16EA8}",
        0x16EA9 => "\u{16EA9}",
        0x16EAA => "\u{16EAA}",
        0x16EAB => "\u{16EAB}",
        0x16EAC => "\u{16EAC}",
        0x16EAD => "\u{16EAD}",
        0x16EAE => "\u{16EAE}",
        0x16EAF => "\u{16EAF}",
        0x16EB0 => "\u{16EB0}",
        0x16EB1 => "\u{16EB1}",
        0x16EB2 => "\u{16EB2}",
        0x16EB3 => "\u{16EB3}",
        0x16EB4 => "\u{16EB4}",
        0x16EB5 => "\u{16EB5}",
        0x16EB6 => "\u{16EB6}",
        0x16EB7 => "\u{16EB7}",
        0x16EB8 => "\u{16EB8}",
        _ => return None,
    })
}

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

        // Non-combining character: use the Unicode full case fold. A plain NFKD
        // would wrongly decompose non-cased compatibility characters (NBSP,
        // superscripts, Roman numerals, circled/fullwidth letters).
        let mut folded = String::new();
        match full_case_fold(ch as u32) {
            Some(f) => folded.push_str(f),
            None => push_foldcase_char(&mut folded, ch),
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
        // Codepoints beyond the Unicode maximum (U+10FFFF) have no name; Rakudo
        // returns "<unassigned>" rather than erroring (mirrors the in-range
        // unassigned "<reserved-XXXX>" / negative "<illegal>" sentinels).
        return Ok("<unassigned>".to_string());
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
