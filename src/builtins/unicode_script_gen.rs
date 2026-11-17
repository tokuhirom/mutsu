//! Generator and verifier for [`super::unicode_script_data`].
//!
//! Test-only; the shared machinery is in [`crate::ucd::table_gen`]. Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_script_gen
//! ```

use super::unicode_script_data as data;
use crate::ucd::table_gen as tbl;
use std::fmt::Write as _;

/// Script names in the pinned UCD, sorted for stable compact codes.
const UCD: &str = include_str!("../ucd/data/Scripts-17.0.0.txt");

fn scripts() -> Vec<&'static str> {
    let mut names: Vec<_> = tbl::property_ranges(UCD)
        .into_iter()
        .map(|(_, _, name)| name)
        .collect();
    names.sort_unstable();
    names.dedup();
    names
}

const HEADER: &str = "\
//! Generated Script tables. DO NOT EDIT BY HAND.
//!
//! Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_script_gen
//! ```
//!
//! Derived from Unicode 17.0.0 `Scripts.txt`.
//! `super::unicode_script_gen` re-derives and verifies every codepoint.

";

fn table() -> Vec<u8> {
    let scripts = scripts();
    let mut table = tbl::derive_ucd_table(UCD, &scripts, &["Unknown"]);
    let han = scripts.iter().position(|name| *name == "Han").unwrap() as u8;
    assert_eq!(
        tbl::apply_moar_cjk_tails(&mut table, scripts.len() as u8, han),
        22
    );
    table
}

/// The name table, emitted after the code tables so the names and the codes
/// they index cannot be edited apart.
fn render_names() -> String {
    let scripts = scripts();
    let mut out = String::from(
        "/// Every Script name, indexed by the code the tables above store.\n\
         #[rustfmt::skip]\n",
    );
    let _ = writeln!(
        out,
        "pub(super) static SCRIPT_NAMES: [&str; {}] = [",
        scripts.len() + 1
    );
    for chunk in scripts
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
    out.push_str("];\n");
    out
}

#[test]
fn script_names_cover_ucd() {
    let scripts = scripts();
    assert_eq!(scripts.len(), 174);
    assert_eq!(
        scripts.iter().filter(|name| **name == "Beria_Erfe").count(),
        1
    );
}

/// The committed tables must answer exactly what the Unicode data says, for
/// every one of the 1,114,112 codepoints.
#[test]
fn verify_committed_tables_match_unicode_data() {
    let table = table();
    let built = tbl::build_tables(&table);
    tbl::maybe_regenerate(
        "builtins/unicode_script_data.rs",
        &built,
        HEADER,
        &render_names(),
    );

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
    let scripts = scripts();
    assert_eq!(data::SCRIPT_NAMES.len(), scripts.len() + 1);
    for (i, name) in scripts.iter().enumerate() {
        assert_eq!(data::SCRIPT_NAMES[i], *name);
    }
    assert_eq!(data::SCRIPT_NAMES[scripts.len()], "Unknown");

    tbl::assert_lookup_matches_table(&table, |ch| {
        let name = super::unicode_script::script_name(ch);
        data::SCRIPT_NAMES
            .iter()
            .position(|n| *n == name)
            .expect("a name the table can produce") as u8
    });
}
