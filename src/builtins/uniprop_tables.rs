//! Unicode property data tables for uniprop.
//! Extracted from uniprop.rs to keep individual files under 500 lines.

/// Unicode Block lookup.
pub(super) fn unicode_block(ch: char) -> String {
    crate::builtins::unicode_blocks::block_index(ch as u32)
        .map_or("No_Block", |i| crate::builtins::unicode_blocks::BLOCKS[i].2)
        .to_string()
}

/// Jamo short name lookup.
pub(super) fn unicode_jamo_short_name(ch: char) -> String {
    let cp = ch as u32;
    match cp {
        0x1100 => "G",
        0x1101 => "GG",
        0x1102 => "N",
        0x1103 => "D",
        0x1104 => "DD",
        0x1105 => "R",
        0x1106 => "M",
        0x1107 => "B",
        0x1108 => "BB",
        0x1109 => "S",
        0x110A => "SS",
        0x110B => "",
        0x110C => "J",
        0x110D => "JJ",
        0x110E => "C",
        0x110F => "K",
        0x1110 => "T",
        0x1111 => "P",
        0x1112 => "H",
        0x1160 => "",
        0x1161 => "A",
        0x1162 => "AE",
        0x1163 => "YA",
        0x1164 => "YAE",
        0x1165 => "EO",
        0x1166 => "E",
        0x1167 => "YEO",
        0x1168 => "YE",
        0x1169 => "O",
        0x116A => "WA",
        0x116B => "WAE",
        0x116C => "OE",
        0x116D => "YO",
        0x116E => "U",
        0x116F => "WEO",
        0x1170 => "WE",
        0x1171 => "WI",
        0x1172 => "YU",
        0x1173 => "EU",
        0x1174 => "YI",
        0x1175 => "I",
        0x11A8 => "G",
        0x11A9 => "GG",
        0x11AA => "GS",
        0x11AB => "N",
        0x11AC => "NJ",
        0x11AD => "NH",
        0x11AE => "D",
        0x11AF => "L",
        0x11B0 => "LG",
        0x11B1 => "LM",
        0x11B2 => "LB",
        0x11B3 => "LS",
        0x11B4 => "LT",
        0x11B5 => "LP",
        0x11B6 => "LH",
        0x11B7 => "M",
        0x11B8 => "B",
        0x11B9 => "BS",
        0x11BA => "S",
        0x11BB => "SS",
        0x11BC => "NG",
        0x11BD => "J",
        0x11BE => "C",
        0x11BF => "K",
        0x11C0 => "T",
        0x11C1 => "P",
        0x11C2 => "H",
        _ => "",
    }
    .to_string()
}
