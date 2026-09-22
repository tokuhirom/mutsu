//! Generator and verifier for [`super::unicode_gc_data`].
//!
//! Test-only; the shared machinery is in [`super::unicode_table_gen`], whose
//! header explains why deriving from `regex-syntax` makes the table's answers
//! identical to the ordered-regex probe's by construction. Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_gc_gen
//! ```

use super::unicode_gc::GeneralCategory;
use super::unicode_gc_data as data;
use super::unicode_table_gen as tbl;

/// The category abbreviations, in the order `unicode_general_category` used to
/// try them. `Cs` is absent for the reason [`GeneralCategory`] documents.
const CATS: [&str; 28] = [
    "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps", "Pe", "Pi",
    "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Co",
];

const HEADER: &str = "\
//! Generated General_Category tables. DO NOT EDIT BY HAND.
//!
//! Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_gc_gen
//! ```
//!
//! Derived from `regex-syntax`'s Unicode tables -- the same data `regex`'s
//! `\\p{...}` classes match against -- folded in the priority order the
//! ordered-regex probe used, so every answer is identical to the
//! implementation this replaced. `super::unicode_gc_gen` re-derives them on
//! every test run and fails if this file has drifted.

";

fn patterns() -> Vec<String> {
    CATS.iter().map(|cat| format!(r"\p{{{cat}}}")).collect()
}

#[test]
fn category_names_match_the_enum() {
    // The tables store indices into `GeneralCategory::ALL`, so the generator's
    // order and the enum's must agree, with `Cn` last.
    for (i, cat) in CATS.iter().enumerate() {
        assert_eq!(GeneralCategory::ALL[i].as_str(), *cat);
    }
    assert_eq!(GeneralCategory::ALL[CATS.len()], GeneralCategory::Cn);
    assert_eq!(GeneralCategory::ALL.len(), CATS.len() + 1);
}

#[test]
fn categories_are_disjoint() {
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
/// every one of the 1,114,112 codepoints. This is the whole correctness
/// argument for the lookup, and it is exhaustive rather than sampled.
#[test]
fn verify_committed_tables_match_unicode_data() {
    let table = tbl::derive_table(&patterns());
    let built = tbl::build_tables(&table);
    tbl::maybe_regenerate("unicode_gc_data.rs", &built, HEADER, "");

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

    tbl::assert_lookup_matches_table(&table, |ch| super::unicode_gc::general_category(ch) as u8);
}

/// Tie the tables back to the *original* implementation: the ordered
/// `regex::Regex` probe, run for real. `verify_committed_tables_match_unicode_data`
/// proves the tables match `regex-syntax`'s data; this proves that data is what
/// a compiled `regex` actually matches, which is the thing the old code did.
#[test]
fn matches_the_ordered_regex_probe_it_replaced() {
    let regexes: Vec<(&str, regex::Regex)> = CATS
        .iter()
        .map(|cat| {
            (
                *cat,
                regex::Regex::new(&format!(r"^\p{{{cat}}}$")).expect("valid regex"),
            )
        })
        .collect();
    let probe = |ch: char| -> &'static str {
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        for (cat, re) in &regexes {
            if re.is_match(s) {
                return cat;
            }
        }
        "Cn"
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
            super::unicode_gc::general_category(ch).as_str(),
            probe(ch),
            "U+{cp:04X}"
        );
    }
}
