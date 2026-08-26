//! Auto-generated Unicode `NameAliases.txt` lookup table.
//!
//! Raku resolves `\c[NAME]` / `uniparse` against the UCD *name alias*
//! list as well as the immutable `Name` property, so a corrected
//! spelling (`LATIN CAPITAL LETTER GHA` for U+01A2, whose `Name` is
//! still `LATIN CAPITAL LETTER OI`), a control abbreviation (`LF`,
//! `NBSP`, `ZWJ`) and the `BYTE ORDER MARK` alternate all resolve.
//! The `unicode_names2` crate only indexes `Name`, hence this table.
//!
//! 217 entries (all alias kinds: correction, control,
//! abbreviation, figment, alternate). The `VS1`..`VS256` variation
//! selector abbreviations are resolved arithmetically instead.

/// Look up a character by a Unicode name alias (case-insensitive input is
/// expected to be upper-cased by the caller).
pub(crate) fn lookup_name_alias(upper_name: &str) -> Option<char> {
    if let Some(c) = lookup_variation_selector(upper_name) {
        return Some(c);
    }
    TABLE
        .binary_search_by_key(&upper_name, |&(name, _)| name)
        .ok()
        .and_then(|i| char::from_u32(TABLE[i].1))
}

/// `VS1`..`VS16` are U+FE00..U+FE0F; `VS17`..`VS256` are U+E0100..U+E01EF.
fn lookup_variation_selector(upper_name: &str) -> Option<char> {
    let n: u32 = upper_name.strip_prefix("VS")?.parse().ok()?;
    let cp = match n {
        1..=16 => 0xFE00 + (n - 1),
        17..=256 => 0xE0100 + (n - 17),
        _ => return None,
    };
    char::from_u32(cp)
}

static TABLE: &[(&str, u32)] = &[
    ("ACK", 0x0006),                                             // abbreviation
    ("ACKNOWLEDGE", 0x0006),                                     // control
    ("ALERT", 0x0007),                                           // control
    ("ALM", 0x061C),                                             // abbreviation
    ("APC", 0x009F),                                             // abbreviation
    ("APPLICATION PROGRAM COMMAND", 0x009F),                     // control
    ("ARABIC SMALL HIGH LIGATURE ALEF WITH YEH BARREE", 0x0616), // correction
    ("BACKSPACE", 0x0008),                                       // control
    ("BEL", 0x0007),                                             // abbreviation
    ("BOM", 0xFEFF),                                             // abbreviation
    ("BPH", 0x0082),                                             // abbreviation
    ("BREAK PERMITTED HERE", 0x0082),                            // control
    ("BS", 0x0008),                                              // abbreviation
    ("BYTE ORDER MARK", 0xFEFF),                                 // alternate
    (
        "BYZANTINE MUSICAL SYMBOL FTHORA SKLIRON CHROMA VASIS",
        0x1D0C5,
    ), // correction
    ("CAN", 0x0018),                                             // abbreviation
    ("CANCEL", 0x0018),                                          // control
    ("CANCEL CHARACTER", 0x0094),                                // control
    ("CARRIAGE RETURN", 0x000D),                                 // control
    ("CCH", 0x0094),                                             // abbreviation
    ("CGJ", 0x034F),                                             // abbreviation
    ("CHARACTER TABULATION", 0x0009),                            // control
    ("CHARACTER TABULATION SET", 0x0088),                        // control
    ("CHARACTER TABULATION WITH JUSTIFICATION", 0x0089),         // control
    ("CONTROL SEQUENCE INTRODUCER", 0x009B),                     // control
    ("CR", 0x000D),                                              // abbreviation
    ("CSI", 0x009B),                                             // abbreviation
    ("CUNEIFORM SIGN NU11 OVER NU11 BUR OVER BUR", 0x122D5),     // correction
    ("CUNEIFORM SIGN NU11 TENU", 0x122D4),                       // correction
    ("DATA LINK ESCAPE", 0x0010),                                // control
    ("DC1", 0x0011),                                             // abbreviation
    ("DC2", 0x0012),                                             // abbreviation
    ("DC3", 0x0013),                                             // abbreviation
    ("DC4", 0x0014),                                             // abbreviation
    ("DCS", 0x0090),                                             // abbreviation
    ("DEL", 0x007F),                                             // abbreviation
    ("DELETE", 0x007F),                                          // control
    ("DEVICE CONTROL FOUR", 0x0014),                             // control
    ("DEVICE CONTROL ONE", 0x0011),                              // control
    ("DEVICE CONTROL STRING", 0x0090),                           // control
    ("DEVICE CONTROL THREE", 0x0013),                            // control
    ("DEVICE CONTROL TWO", 0x0012),                              // control
    ("DLE", 0x0010),                                             // abbreviation
    ("EM", 0x0019),                                              // abbreviation
    ("END OF GUARDED AREA", 0x0097),                             // control
    ("END OF LINE", 0x000A),                                     // control
    ("END OF MEDIUM", 0x0019),                                   // control
    ("END OF PROTECTED AREA", 0x0097),                           // control
    ("END OF SELECTED AREA", 0x0087),                            // control
    ("END OF TEXT", 0x0003),                                     // control
    ("END OF TRANSMISSION", 0x0004),                             // control
    ("END OF TRANSMISSION BLOCK", 0x0017),                       // control
    ("ENQ", 0x0005),                                             // abbreviation
    ("ENQUIRY", 0x0005),                                         // control
    ("EOL", 0x000A),                                             // abbreviation
    ("EOM", 0x0019),                                             // abbreviation
    ("EOT", 0x0004),                                             // abbreviation
    ("EPA", 0x0097),                                             // abbreviation
    ("ESA", 0x0087),                                             // abbreviation
    ("ESC", 0x001B),                                             // abbreviation
    ("ESCAPE", 0x001B),                                          // control
    ("ETB", 0x0017),                                             // abbreviation
    ("ETX", 0x0003),                                             // abbreviation
    ("FF", 0x000C),                                              // abbreviation
    ("FILE SEPARATOR", 0x001C),                                  // control
    ("FORM FEED", 0x000C),                                       // control
    ("FS", 0x001C),                                              // abbreviation
    ("FSI", 0x2068),                                             // abbreviation
    ("FVS1", 0x180B),                                            // abbreviation
    ("FVS2", 0x180C),                                            // abbreviation
    ("FVS3", 0x180D),                                            // abbreviation
    ("FVS4", 0x180F),                                            // abbreviation
    ("GROUP SEPARATOR", 0x001D),                                 // control
    ("GS", 0x001D),                                              // abbreviation
    ("HANGUL JONGSEONG SSANGYESIEUNG", 0x11EE),                  // correction
    ("HANGUL JONGSEONG YESIEUNG-KHIEUKH", 0x11EF),               // correction
    ("HANGUL JONGSEONG YESIEUNG-KIYEOK", 0x11EC),                // correction
    ("HANGUL JONGSEONG YESIEUNG-SSANGKIYEOK", 0x11ED),           // correction
    ("HENTAIGANA LETTER E-1", 0x1B001),                          // correction
    ("HIGH OCTET PRESET", 0x0081),                               // figment
    ("HOP", 0x0081),                                             // abbreviation
    ("HORIZONTAL TABULATION", 0x0009),                           // control
    ("HORIZONTAL TABULATION SET", 0x0088),                       // control
    ("HORIZONTAL TABULATION WITH JUSTIFICATION", 0x0089),        // control
    ("HT", 0x0009),                                              // abbreviation
    ("HTJ", 0x0089),                                             // abbreviation
    ("HTS", 0x0088),                                             // abbreviation
    ("IND", 0x0084),                                             // abbreviation
    ("INDEX", 0x0084),                                           // control
    ("INFORMATION SEPARATOR FOUR", 0x001C),                      // control
    ("INFORMATION SEPARATOR ONE", 0x001F),                       // control
    ("INFORMATION SEPARATOR THREE", 0x001D),                     // control
    ("INFORMATION SEPARATOR TWO", 0x001E),                       // control
    ("KANNADA LETTER LLLA", 0x0CDE),                             // correction
    ("LAO LETTER FO FAY", 0x0E9F),                               // correction
    ("LAO LETTER FO FON", 0x0E9D),                               // correction
    ("LAO LETTER LO", 0x0EA5),                                   // correction
    ("LAO LETTER RO", 0x0EA3),                                   // correction
    ("LATIN CAPITAL LETTER GHA", 0x01A2),                        // correction
    ("LATIN SMALL LETTER GHA", 0x01A3),                          // correction
    (
        "LEFTWARDS TRIANGLE-HEADED ARROW WITH DOUBLE VERTICAL STROKE",
        0x2B7A,
    ), // correction
    ("LF", 0x000A),                                              // abbreviation
    ("LINE FEED", 0x000A),                                       // control
    ("LINE TABULATION", 0x000B),                                 // control
    ("LINE TABULATION SET", 0x008A),                             // control
    ("LOCKING-SHIFT ONE", 0x000E),                               // control
    ("LOCKING-SHIFT ZERO", 0x000F),                              // control
    ("LRE", 0x202A),                                             // abbreviation
    ("LRI", 0x2066),                                             // abbreviation
    ("LRM", 0x200E),                                             // abbreviation
    ("LRO", 0x202D),                                             // abbreviation
    ("MEDEFAIDRIN CAPITAL LETTER H", 0x16E56),                   // correction
    ("MEDEFAIDRIN CAPITAL LETTER NG", 0x16E57),                  // correction
    ("MEDEFAIDRIN SMALL LETTER H", 0x16E76),                     // correction
    ("MEDEFAIDRIN SMALL LETTER NG", 0x16E77),                    // correction
    ("MESSAGE WAITING", 0x0095),                                 // control
    ("MICR DASH SYMBOL", 0x2449),                                // correction
    ("MICR ON US SYMBOL", 0x2448),                               // correction
    ("MMSP", 0x205F),                                            // abbreviation
    ("MVS", 0x180E),                                             // abbreviation
    ("MW", 0x0095),                                              // abbreviation
    ("MYANMAR LETTER KHAMTI LLA", 0xAA6E),                       // correction
    ("NAK", 0x0015),                                             // abbreviation
    ("NBH", 0x0083),                                             // abbreviation
    ("NBSP", 0x00A0),                                            // abbreviation
    ("NEGATIVE ACKNOWLEDGE", 0x0015),                            // control
    ("NEL", 0x0085),                                             // abbreviation
    ("NEW LINE", 0x000A),                                        // control
    ("NEXT LINE", 0x0085),                                       // control
    ("NL", 0x000A),                                              // abbreviation
    ("NNBSP", 0x202F),                                           // abbreviation
    ("NO BREAK HERE", 0x0083),                                   // control
    ("NUL", 0x0000),                                             // abbreviation
    ("NULL", 0x0000),                                            // control
    ("OPERATING SYSTEM COMMAND", 0x009D),                        // control
    ("OSC", 0x009D),                                             // abbreviation
    ("PAD", 0x0080),                                             // abbreviation
    ("PADDING CHARACTER", 0x0080),                               // figment
    ("PARTIAL LINE BACKWARD", 0x008C),                           // control
    ("PARTIAL LINE DOWN", 0x008B),                               // control
    ("PARTIAL LINE FORWARD", 0x008B),                            // control
    ("PARTIAL LINE UP", 0x008C),                                 // control
    ("PDF", 0x202C),                                             // abbreviation
    ("PDI", 0x2069),                                             // abbreviation
    ("PLD", 0x008B),                                             // abbreviation
    ("PLU", 0x008C),                                             // abbreviation
    ("PM", 0x009E),                                              // abbreviation
    (
        "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET",
        0xFE18,
    ), // correction
    ("PRIVACY MESSAGE", 0x009E),                                 // control
    ("PRIVATE USE ONE", 0x0091),                                 // control
    ("PRIVATE USE TWO", 0x0092),                                 // control
    ("PRIVATE USE-1", 0x0091),                                   // control
    ("PRIVATE USE-2", 0x0092),                                   // control
    ("PU1", 0x0091),                                             // abbreviation
    ("PU2", 0x0092),                                             // abbreviation
    ("RECORD SEPARATOR", 0x001E),                                // control
    ("REVERSE INDEX", 0x008D),                                   // control
    ("REVERSE LINE FEED", 0x008D),                               // control
    ("RI", 0x008D),                                              // abbreviation
    (
        "RIGHTWARDS TRIANGLE-HEADED ARROW WITH DOUBLE VERTICAL STROKE",
        0x2B7C,
    ), // correction
    ("RLE", 0x202B),                                             // abbreviation
    ("RLI", 0x2067),                                             // abbreviation
    ("RLM", 0x200F),                                             // abbreviation
    ("RLO", 0x202E),                                             // abbreviation
    ("RS", 0x001E),                                              // abbreviation
    ("SCI", 0x009A),                                             // abbreviation
    ("SET TRANSMIT STATE", 0x0093),                              // control
    ("SGC", 0x0099),                                             // abbreviation
    ("SHIFT IN", 0x000F),                                        // control
    ("SHIFT OUT", 0x000E),                                       // control
    ("SHY", 0x00AD),                                             // abbreviation
    ("SI", 0x000F),                                              // abbreviation
    ("SINGLE CHARACTER INTRODUCER", 0x009A),                     // control
    ("SINGLE GRAPHIC CHARACTER INTRODUCER", 0x0099),             // figment
    ("SINGLE SHIFT THREE", 0x008F),                              // control
    ("SINGLE SHIFT TWO", 0x008E),                                // control
    ("SINGLE-SHIFT-2", 0x008E),                                  // control
    ("SINGLE-SHIFT-3", 0x008F),                                  // control
    ("SO", 0x000E),                                              // abbreviation
    ("SOH", 0x0001),                                             // abbreviation
    ("SOS", 0x0098),                                             // abbreviation
    ("SP", 0x0020),                                              // abbreviation
    ("SPA", 0x0096),                                             // abbreviation
    ("SS2", 0x008E),                                             // abbreviation
    ("SS3", 0x008F),                                             // abbreviation
    ("SSA", 0x0086),                                             // abbreviation
    ("ST", 0x009C),                                              // abbreviation
    ("START OF GUARDED AREA", 0x0096),                           // control
    ("START OF HEADING", 0x0001),                                // control
    ("START OF PROTECTED AREA", 0x0096),                         // control
    ("START OF SELECTED AREA", 0x0086),                          // control
    ("START OF STRING", 0x0098),                                 // control
    ("START OF TEXT", 0x0002),                                   // control
    ("STRING TERMINATOR", 0x009C),                               // control
    ("STS", 0x0093),                                             // abbreviation
    ("STX", 0x0002),                                             // abbreviation
    ("SUB", 0x001A),                                             // abbreviation
    ("SUBSTITUTE", 0x001A),                                      // control
    ("SUNDANESE LETTER ARCHAIC I", 0x1BBD),                      // correction
    ("SYN", 0x0016),                                             // abbreviation
    ("SYNCHRONOUS IDLE", 0x0016),                                // control
    ("SYRIAC SUBLINEAR COLON SKEWED LEFT", 0x0709),              // correction
    ("TAB", 0x0009),                                             // abbreviation
    ("TIBETAN MARK BKA- SHOG GI MGO RGYAN", 0x0FD0),             // correction
    ("UNIT SEPARATOR", 0x001F),                                  // control
    ("US", 0x001F),                                              // abbreviation
    ("VERTICAL TABULATION", 0x000B),                             // control
    ("VERTICAL TABULATION SET", 0x008A),                         // control
    ("VT", 0x000B),                                              // abbreviation
    ("VTS", 0x008A),                                             // abbreviation
    ("WEIERSTRASS ELLIPTIC FUNCTION", 0x2118),                   // correction
    ("WJ", 0x2060),                                              // abbreviation
    ("YI SYLLABLE ITERATION MARK", 0xA015),                      // correction
    ("ZWJ", 0x200D),                                             // abbreviation
    ("ZWNBSP", 0xFEFF),                                          // abbreviation
    ("ZWNJ", 0x200C),                                            // abbreviation
    ("ZWSP", 0x200B),                                            // abbreviation
];
