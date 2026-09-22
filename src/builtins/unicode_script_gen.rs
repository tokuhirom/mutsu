//! Generator and verifier for [`super::unicode_script_data`].
//!
//! Test-only; the shared machinery is in [`super::unicode_table_gen`], whose
//! header explains why deriving from `regex-syntax` makes the table's answers
//! identical to the ordered-regex probe's by construction. Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_script_gen
//! ```

use super::unicode_script_data as data;
use super::unicode_table_gen as tbl;
use std::fmt::Write as _;

/// Every Script `unicode_script_name` used to probe, in the order it probed
/// them (alphabetical, which is also the order the generated name table and
/// the codes follow). `Unknown` is the fallthrough and is not in this list.
const SCRIPTS: [&str; 161] = [
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

const HEADER: &str = "\
//! Generated Script tables. DO NOT EDIT BY HAND.
//!
//! Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_script_gen
//! ```
//!
//! Derived from `regex-syntax`'s Unicode tables -- the same data `regex`'s
//! `\\p{Script=...}` classes match against -- in the order the ordered-regex
//! probe tried them, so every answer is identical to the implementation this
//! replaced. `super::unicode_script_gen` re-derives them on every test run and
//! fails if this file has drifted.

";

fn patterns() -> Vec<String> {
    SCRIPTS
        .iter()
        .map(|name| format!(r"\p{{Script={name}}}"))
        .collect()
}

/// The name table, emitted after the code tables so the names and the codes
/// they index cannot be edited apart.
fn render_names() -> String {
    let mut out = String::from(
        "/// Every Script name, indexed by the code the tables above store.\n\
         #[rustfmt::skip]\n",
    );
    let _ = write!(
        out,
        "pub(super) static SCRIPT_NAMES: [&str; {}] = [\n",
        SCRIPTS.len() + 1
    );
    for chunk in SCRIPTS
        .iter()
        .copied()
        .chain(["Unknown"])
        .collect::<Vec<_>>()
        .chunks(4)
    {
        out.push_str("    ");
        for name in chunk {
            let _ = write!(out, "\"{name}\",");
        }
        out.push('\n');
    }
    out.push_str("];\n\n");
    let _ = write!(
        out,
        "/// The code of the `Unknown` fallthrough.\n\
         pub(super) const UNKNOWN_CODE: u8 = {};\n",
        SCRIPTS.len()
    );
    out
}

#[test]
fn scripts_are_disjoint() {
    let table = tbl::derive_table(&patterns());
    for (i, pattern) in patterns().iter().enumerate() {
        for (start, end) in tbl::ranges_for(pattern) {
            for cp in start..=end {
                assert_eq!(
                    table[cp as usize], i as u8,
                    "U+{cp:04X} is in {pattern} but folded elsewhere"
                );
            }
        }
    }
}

/// The committed tables must answer exactly what the Unicode data says, for
/// every one of the 1,114,112 codepoints.
#[test]
fn verify_committed_tables_match_unicode_data() {
    let table = tbl::derive_table(&patterns());
    let built = tbl::build_tables(&table);
    tbl::maybe_regenerate("unicode_script_data.rs", &built, HEADER, &render_names());

    tbl::assert_committed_matches(
        &tbl::Committed {
            ascii: data::ASCII_CATS.as_slice(),
            shift: data::BMP_SHIFT,
            index: data::BMP_INDEX.as_slice(),
            leaves: data::BMP_LEAVES.as_slice(),
            astral_starts: data::ASTRAL_STARTS.as_slice(),
            astral_cats: data::ASTRAL_CATS.as_slice(),
        },
        &built,
    );

    // The name table and the codes are two halves of one mapping.
    assert_eq!(data::SCRIPT_NAMES.len(), SCRIPTS.len() + 1);
    for (i, name) in SCRIPTS.iter().enumerate() {
        assert_eq!(data::SCRIPT_NAMES[i], *name);
    }
    assert_eq!(data::UNKNOWN_CODE as usize, SCRIPTS.len());
    assert_eq!(data::SCRIPT_NAMES[data::UNKNOWN_CODE as usize], "Unknown");

    tbl::assert_lookup_matches_table(&table, |ch| {
        let name = super::unicode_script::script_name(ch);
        data::SCRIPT_NAMES
            .iter()
            .position(|n| *n == name)
            .expect("a name the table can produce") as u8
    });
}

/// Tie the table back to the *original* implementation: the ordered
/// `regex::Regex` probe, run for real.
#[test]
fn matches_the_ordered_regex_probe_it_replaced() {
    let regexes: Vec<(&str, regex::Regex)> = SCRIPTS
        .iter()
        .map(|name| {
            (
                *name,
                regex::Regex::new(&format!(r"^\p{{Script={name}}}$")).expect("valid regex"),
            )
        })
        .collect();
    let probe = |ch: char| -> &'static str {
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        for (name, re) in &regexes {
            if re.is_match(s) {
                return name;
            }
        }
        "Unknown"
    };

    // All of ASCII, every run boundary in the derived table (and the codepoint
    // either side of it), plus a stride across the rest.
    let table = tbl::derive_table(&patterns());
    let mut probes: Vec<u32> = (0..0x80).collect();
    let mut prev = table[0];
    for cp in 1..0x110000u32 {
        if table[cp as usize] != prev {
            probes.extend([cp.saturating_sub(1), cp, cp + 1]);
            prev = table[cp as usize];
        }
    }
    probes.extend((0..0x110000u32).step_by(997));
    probes.sort_unstable();
    probes.dedup();

    for cp in probes {
        let Some(ch) = char::from_u32(cp) else {
            continue;
        };
        assert_eq!(
            super::unicode_script::script_name(ch),
            probe(ch),
            "U+{cp:04X}"
        );
    }
}
