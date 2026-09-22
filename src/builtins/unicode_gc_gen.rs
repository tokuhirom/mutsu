//! Generator and verifier for [`super::unicode_gc_data`].
//!
//! Test-only. The committed tables in `unicode_gc_data.rs` are derived from
//! `regex-syntax`'s Unicode tables -- the same data `regex`'s `\p{...}`
//! classes match against -- folded in the priority order the ordered-regex
//! probe in `unicode_general_category` used before
//! [#8999](https://github.com/tokuhirom/mutsu/issues/8999). That is what makes
//! the table's answers identical to the regexes' by construction rather than
//! by argument.
//!
//! [`verify_committed_tables_match_unicode_data`] re-derives them on every
//! `cargo test` run and fails if the committed file has drifted, so a
//! `regex-syntax` bump that carries a new Unicode version cannot land
//! silently. Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_gc_gen::regenerate
//! ```

use super::unicode_gc::GeneralCategory;
use super::unicode_gc_data as data;
use regex_syntax::hir::{Class, HirKind};
use std::fmt::Write as _;

/// The category abbreviations, in the order `unicode_general_category` used to
/// try them. `Cs` is absent for the reason [`GeneralCategory`] documents.
const CATS: [&str; 28] = [
    "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps", "Pe", "Pi",
    "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Co",
];
/// The code the unmatched fallthrough (`"Cn"`) gets.
const CN: u8 = CATS.len() as u8;

const SHIFT: u32 = 6;
const BLOCK: usize = 1 << SHIFT;

/// The codepoint ranges of one `\p{Xx}` class, straight out of `regex-syntax`.
fn ranges_for(cat: &str) -> Vec<(u32, u32)> {
    // The class-forcing brackets keep a single-codepoint category (`Zp`) from
    // being folded into a `Literal` by the HIR translator.
    let hir = regex_syntax::parse(&format!(r"[\p{{{cat}}}]")).expect("valid class");
    match hir.kind() {
        HirKind::Class(Class::Unicode(class)) => class
            .iter()
            .map(|r| (r.start() as u32, r.end() as u32))
            .collect(),
        HirKind::Literal(lit) => std::str::from_utf8(&lit.0)
            .expect("utf8 literal")
            .chars()
            .map(|c| (c as u32, c as u32))
            .collect(),
        other => panic!("unexpected HIR for \\p{{{cat}}}: {other:?}"),
    }
}

/// One category code per codepoint, folded in `CATS` order.
fn derive_table() -> Vec<u8> {
    let mut table = vec![CN; 0x110000];
    // Reverse order so an earlier category in `CATS` wins, exactly as the
    // first-match-wins regex probe did. (The classes are disjoint anyway --
    // `disjointness` below proves it.)
    for (i, cat) in CATS.iter().enumerate().rev() {
        for (start, end) in ranges_for(cat) {
            for cp in start..=end {
                table[cp as usize] = i as u8;
            }
        }
    }
    table
}

/// The three tiers, derived from `table`: ASCII, the BMP trie, the astral runs.
struct Tables {
    ascii: Vec<u8>,
    index: Vec<u16>,
    leaves: Vec<u8>,
    leaf_count: usize,
    astral_starts: Vec<u32>,
    astral_cats: Vec<u8>,
}

fn build_tables(table: &[u8]) -> Tables {
    let mut blocks: Vec<[u8; BLOCK]> = Vec::new();
    let mut index: Vec<u16> = Vec::new();
    for b in 0..(0x10000 / BLOCK) {
        let mut block = [0u8; BLOCK];
        block.copy_from_slice(&table[b * BLOCK..(b + 1) * BLOCK]);
        let pos = match blocks.iter().position(|l| *l == block) {
            Some(p) => p,
            None => {
                blocks.push(block);
                blocks.len() - 1
            }
        };
        index.push(u16::try_from(pos).expect("leaf index fits in u16"));
    }

    let mut astral_starts = Vec::new();
    let mut astral_cats = Vec::new();
    for cp in 0x10000u32..0x110000 {
        let cat = table[cp as usize];
        // Force a run boundary at 0x10000 so the lookup's `partition_point`
        // can never underflow.
        if cp == 0x10000 || astral_cats.last() != Some(&cat) {
            astral_starts.push(cp);
            astral_cats.push(cat);
        }
    }

    Tables {
        ascii: table[..128].to_vec(),
        index,
        leaves: blocks.iter().flat_map(|b| b.iter().copied()).collect(),
        leaf_count: blocks.len(),
        astral_starts,
        astral_cats,
    }
}

fn emit_u8(out: &mut String, values: &[u8], per_line: usize) {
    for chunk in values.chunks(per_line) {
        out.push_str("    ");
        for v in chunk {
            let _ = write!(out, "{v},");
        }
        out.push('\n');
    }
    out.push_str("];\n\n");
}

fn render(t: &Tables) -> String {
    let mut out = String::new();
    out.push_str(
        "//! Generated General_Category tables. DO NOT EDIT BY HAND.\n\
         //!\n\
         //! Regenerate with:\n\
         //!\n\
         //! ```text\n\
         //! MUTSU_UPDATE_GC_TABLE=1 cargo test --lib unicode_gc_gen::regenerate\n\
         //! ```\n\
         //!\n\
         //! Derived from `regex-syntax`'s Unicode tables -- the same data\n\
         //! `regex`'s `\\p{...}` classes match against -- folded in the priority\n\
         //! order the ordered-regex probe used, so every answer is identical to\n\
         //! the implementation this replaced. `super::unicode_gc_gen` re-derives\n\
         //! them on every test run and fails if this file has drifted.\n\n",
    );
    let _ = write!(
        out,
        "/// Category code of every ASCII codepoint, indexed directly.\n\
         ///\n\
         /// Almost all the text mutsu classifies is ASCII, so that case skips\n\
         /// the two-stage lookup below entirely: one load, no second dependent\n\
         /// load and no leaf index.\n\
         #[rustfmt::skip]\n\
         pub(super) static ASCII_CATS: [u8; {}] = [\n",
        t.ascii.len()
    );
    emit_u8(&mut out, &t.ascii, 32);

    let _ = write!(
        out,
        "/// Codepoints per BMP leaf block, as a shift.\npub(super) const BMP_SHIFT: u32 = {SHIFT};\n\n"
    );
    let _ = write!(
        out,
        "/// Leaf index for each {BLOCK}-codepoint block of the BMP.\n\
         #[rustfmt::skip]\n\
         pub(super) static BMP_INDEX: [u16; {}] = [\n",
        t.index.len()
    );
    for chunk in t.index.chunks(32) {
        out.push_str("    ");
        for v in chunk {
            let _ = write!(out, "{v},");
        }
        out.push('\n');
    }
    out.push_str("];\n\n");

    let _ = write!(
        out,
        "/// Deduplicated BMP leaf blocks: {} blocks of {BLOCK} category codes.\n\
         #[rustfmt::skip]\n\
         pub(super) static BMP_LEAVES: [u8; {}] = [\n",
        t.leaf_count,
        t.leaves.len()
    );
    emit_u8(&mut out, &t.leaves, 64);

    let _ = write!(
        out,
        "/// Start codepoint of each constant-category run above the BMP. The\n\
         /// first entry is exactly `0x10000`, so the lookup's `partition_point`\n\
         /// never underflows.\n\
         #[rustfmt::skip]\n\
         pub(super) static ASTRAL_STARTS: [u32; {}] = [\n",
        t.astral_starts.len()
    );
    for chunk in t.astral_starts.chunks(12) {
        out.push_str("    ");
        for v in chunk {
            let _ = write!(out, "0x{v:05X},");
        }
        out.push('\n');
    }
    out.push_str("];\n\n");

    let _ = write!(
        out,
        "/// Category code of each run in [`ASTRAL_STARTS`].\n\
         #[rustfmt::skip]\n\
         pub(super) static ASTRAL_CATS: [u8; {}] = [\n",
        t.astral_cats.len()
    );
    emit_u8(&mut out, &t.astral_cats, 64);
    out
}

#[test]
fn category_names_match_the_enum() {
    // The tables store indices into `GeneralCategory::ALL`, so the generator's
    // order and the enum's must agree, with `Cn` last.
    for (i, cat) in CATS.iter().enumerate() {
        assert_eq!(GeneralCategory::ALL[i].as_str(), *cat);
    }
    assert_eq!(GeneralCategory::ALL[CN as usize], GeneralCategory::Cn);
    assert_eq!(GeneralCategory::ALL.len(), CATS.len() + 1);
}

#[test]
fn categories_are_disjoint() {
    let table = derive_table();
    for (i, cat) in CATS.iter().enumerate() {
        for (start, end) in ranges_for(cat) {
            for cp in start..=end {
                assert_eq!(
                    table[cp as usize], i as u8,
                    "U+{cp:04X} is in \\p{{{cat}}} but folded elsewhere"
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
    let table = derive_table();
    let built = build_tables(&table);

    if std::env::var_os("MUTSU_UPDATE_GC_TABLE").is_some() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/builtins/unicode_gc_data.rs"
        );
        std::fs::write(path, render(&built)).expect("write generated table");
        eprintln!("regenerated {path}");
    }

    assert_eq!(data::ASCII_CATS.as_slice(), built.ascii, "ASCII_CATS");
    assert_eq!(data::BMP_SHIFT, SHIFT, "BMP_SHIFT");
    assert_eq!(data::BMP_INDEX.as_slice(), built.index, "BMP_INDEX");
    assert_eq!(data::BMP_LEAVES.as_slice(), built.leaves, "BMP_LEAVES");
    assert_eq!(
        data::ASTRAL_STARTS.as_slice(),
        built.astral_starts,
        "ASTRAL_STARTS"
    );
    assert_eq!(
        data::ASTRAL_CATS.as_slice(),
        built.astral_cats,
        "ASTRAL_CATS"
    );

    // And the lookup itself, end to end, over every codepoint a `char` can be.
    for cp in 0..0x110000u32 {
        let Some(ch) = char::from_u32(cp) else {
            continue; // surrogate: unreachable through `char`
        };
        assert_eq!(
            super::unicode_gc::general_category(ch),
            GeneralCategory::ALL[table[cp as usize] as usize],
            "U+{cp:04X}"
        );
    }
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
                return CATS[CATS.iter().position(|c| c == cat).unwrap()];
            }
        }
        "Cn"
    };

    // All of ASCII, every run boundary in the derived table (and the codepoint
    // either side of it), plus a stride across the rest.
    let table = derive_table();
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

/// Explicit regeneration entry point, so the doc comments can name a test.
#[test]
fn regenerate() {
    if std::env::var_os("MUTSU_UPDATE_GC_TABLE").is_none() {
        return;
    }
    let built = build_tables(&derive_table());
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/src/builtins/unicode_gc_data.rs"
    );
    std::fs::write(path, render(&built)).expect("write generated table");
    eprintln!("regenerated {path}");
}
