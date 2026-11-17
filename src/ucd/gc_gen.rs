//! Generator and verifier for [`super::gc_data`].
//!
//! Test-only; the shared machinery is in [`super::table_gen`]. Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib ucd::gc_gen
//! ```

use super::gc::GeneralCategory;
use super::gc_data as data;
use super::table_gen as tbl;

/// The category abbreviations, ordered like [`GeneralCategory`]. `Cs` is
/// absent because Rust `char` cannot represent a surrogate.
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
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib ucd::gc_gen
//! ```
//!
//! Derived from Unicode 17.0.0 `extracted/DerivedGeneralCategory.txt`.
//! `super::gc_gen` re-derives and verifies every codepoint on each test run.

";

const UCD: &str = include_str!("data/DerivedGeneralCategory-17.0.0.txt");

fn table() -> Vec<u8> {
    let mut table = tbl::derive_ucd_table(UCD, &CATS, &["Cn", "Cs"]);
    assert_eq!(
        tbl::apply_moar_cjk_tails(
            &mut table,
            GeneralCategory::Cn as u8,
            GeneralCategory::Lo as u8
        ),
        22,
    );
    table
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
fn unicode_17_extension_j_is_lo() {
    let table = table();
    assert_eq!(table[0x323B0], GeneralCategory::Lo as u8);
}

/// The committed tables must answer exactly what the Unicode data says, for
/// every one of the 1,114,112 codepoints. This is the whole correctness
/// argument for the lookup, and it is exhaustive rather than sampled.
#[test]
fn verify_committed_tables_match_unicode_data() {
    let table = table();
    let built = tbl::build_tables(&table);
    tbl::maybe_regenerate("ucd/gc_data.rs", &built, HEADER, "");

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

    tbl::assert_lookup_matches_table(&table, |ch| super::gc::general_category(ch) as u8);
}
