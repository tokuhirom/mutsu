//! Shared machinery for generating and verifying the Unicode property tables.
//!
//! Test-only. Both `ucd::gc_gen` and `builtins::unicode_script_gen` answer the same
//! question -- "which property does this codepoint have?" -- from the pinned
//! Unicode 17 UCD files. Both bake the answer into the same three-tier shape.

use std::fmt::Write as _;

/// Codepoints per BMP leaf block, as a shift.
pub(crate) const SHIFT: u32 = 6;
pub(crate) const BLOCK: usize = 1 << SHIFT;

/// Parse the range and property on each non-comment UCD line.
pub(crate) fn property_ranges(source: &str) -> Vec<(u32, u32, &str)> {
    source
        .lines()
        .filter_map(|line| {
            let (range, property) = line.split_once(';')?;
            let range = range.trim();
            if !range.starts_with(|c: char| c.is_ascii_hexdigit()) {
                return None;
            }
            let property = property.split('#').next().unwrap().trim();
            let (start, end) = range.split_once("..").unwrap_or((range, range));
            let start = u32::from_str_radix(start, 16).expect("UCD range start");
            let end = u32::from_str_radix(end, 16).expect("UCD range end");
            assert!(start <= end && end < 0x110000, "UCD range bounds");
            Some((start, end, property))
        })
        .collect()
}

/// One code per codepoint, indexed by `names`, or `names.len()` for the
/// property's default value. `ignored` names are explicit UCD defaults or
/// values unreachable through Rust `char` (surrogates).
pub(crate) fn derive_ucd_table(source: &str, names: &[&str], ignored: &[&str]) -> Vec<u8> {
    let fallback = u8::try_from(names.len()).expect("fallthrough code fits in u8");
    let mut table = vec![fallback; 0x110000];
    for (start, end, property) in property_ranges(source) {
        if ignored.contains(&property) {
            continue;
        }
        let code = names
            .iter()
            .position(|name| *name == property)
            .unwrap_or_else(|| panic!("unknown UCD property {property}")) as u8;
        for cp in start..=end {
            assert_eq!(
                table[cp as usize], fallback,
                "overlapping UCD range U+{cp:04X}"
            );
            table[cp as usize] = code;
        }
    }
    table
}

/// MoarVM classifies the still-reserved tails of CJK extensions D, F and G
/// as ideographs. Apply that block-level compatibility rule to either the GC
/// or Script table after reading the standard UCD property data.
pub(crate) fn apply_moar_cjk_tails(table: &mut [u8], fallback: u8, ideograph: u8) -> usize {
    const BLOCKS: &str = include_str!("data/Blocks-17.0.0.txt");
    let mut changed = 0;
    for (start, end, name) in property_ranges(BLOCKS) {
        if !matches!(
            name,
            "CJK Unified Ideographs Extension D"
                | "CJK Unified Ideographs Extension F"
                | "CJK Unified Ideographs Extension G"
        ) {
            continue;
        }
        for cp in start..=end {
            if table[cp as usize] == fallback {
                table[cp as usize] = ideograph;
                changed += 1;
            }
        }
    }
    changed
}

/// The three tiers derived from a per-codepoint table: ASCII, the BMP trie,
/// and the constant-category runs above the BMP.
pub(crate) struct Tables {
    pub(crate) ascii: Vec<u8>,
    pub(crate) index: Vec<u16>,
    pub(crate) leaves: Vec<u8>,
    pub(crate) leaf_count: usize,
    pub(crate) astral_starts: Vec<u32>,
    pub(crate) astral_cats: Vec<u8>,
}

pub(crate) fn build_tables(table: &[u8]) -> Tables {
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

/// The committed tables of one generated module, for the drift check.
pub(crate) struct Committed<'a> {
    pub(crate) ascii: &'a [u8],
    pub(crate) shift: u32,
    pub(crate) index: &'a [u16],
    pub(crate) leaves: &'a [u8],
    pub(crate) astral_starts: &'a [u32],
    pub(crate) astral_cats: &'a [u8],
}

/// Fail loudly, naming the array, if the committed file no longer matches the
/// Unicode data it was generated from.
pub(crate) fn assert_committed_matches(committed: &Committed<'_>, built: &Tables) {
    assert_eq!(committed.shift, SHIFT, "BMP_SHIFT");
    assert_eq!(committed.ascii, built.ascii, "ASCII table");
    assert_eq!(committed.index, built.index, "BMP_INDEX");
    assert_eq!(committed.leaves, built.leaves, "BMP_LEAVES");
    assert_eq!(
        committed.astral_starts, built.astral_starts,
        "ASTRAL_STARTS"
    );
    assert_eq!(committed.astral_cats, built.astral_cats, "ASTRAL_CATS");
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

/// Render the whole generated module. `header` is its `//!` doc block and
/// `extra` is appended verbatim (the script module adds its name table).
pub(crate) fn render(t: &Tables, header: &str, extra: &str) -> String {
    let mut out = String::from(header);
    let _ = write!(
        out,
        "/// Code of every ASCII codepoint, indexed directly.\n\
         ///\n\
         /// Almost all the text mutsu classifies is ASCII, so that case skips\n\
         /// the two-stage lookup below entirely: one load, no second dependent\n\
         /// load and no leaf index.\n\
         #[rustfmt::skip]\n\
         pub(crate) static ASCII_CATS: [u8; {}] = [\n",
        t.ascii.len()
    );
    emit_u8(&mut out, &t.ascii, 32);

    let _ = write!(
        out,
        "/// Codepoints per BMP leaf block, as a shift.\npub(crate) const BMP_SHIFT: u32 = {SHIFT};\n\n"
    );
    let _ = write!(
        out,
        "/// Leaf index for each {BLOCK}-codepoint block of the BMP.\n\
         #[rustfmt::skip]\n\
         pub(crate) static BMP_INDEX: [u16; {}] = [\n",
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
        "/// Deduplicated BMP leaf blocks: {} blocks of {BLOCK} codes.\n\
         #[rustfmt::skip]\n\
         pub(crate) static BMP_LEAVES: [u8; {}] = [\n",
        t.leaf_count,
        t.leaves.len()
    );
    emit_u8(&mut out, &t.leaves, 64);

    let _ = write!(
        out,
        "/// Start codepoint of each constant-code run above the BMP. The first\n\
         /// entry is exactly `0x10000`, so the lookup's `partition_point` never\n\
         /// underflows.\n\
         #[rustfmt::skip]\n\
         pub(crate) static ASTRAL_STARTS: [u32; {}] = [\n",
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
        "/// Code of each run in [`ASTRAL_STARTS`].\n\
         #[rustfmt::skip]\n\
         pub(crate) static ASTRAL_CATS: [u8; {}] = [\n",
        t.astral_cats.len()
    );
    emit_u8(&mut out, &t.astral_cats, 64);
    out.push_str(extra);
    while out.ends_with("\n\n") {
        out.pop();
    }
    out
}

/// Rewrite `<manifest>/src/builtins/<file>` when `MUTSU_UPDATE_GC_TABLE` is
/// set. A no-op otherwise, so the verify tests can call it unconditionally.
pub(crate) fn maybe_regenerate(file: &str, t: &Tables, header: &str, extra: &str) {
    if std::env::var_os("MUTSU_UPDATE_GC_TABLE").is_none() {
        return;
    }
    let path = format!("{}/src/{file}", env!("CARGO_MANIFEST_DIR"));
    std::fs::write(&path, render(t, header, extra)).expect("write generated table");
    eprintln!("regenerated {path}");
}

/// Drive `lookup` over every codepoint a `char` can be and compare it against
/// the derived table. Exhaustive, not sampled.
pub(crate) fn assert_lookup_matches_table<F: Fn(char) -> u8>(table: &[u8], lookup: F) {
    for cp in 0..0x110000u32 {
        let Some(ch) = char::from_u32(cp) else {
            continue; // surrogate: unreachable through `char`
        };
        assert_eq!(lookup(ch), table[cp as usize], "U+{cp:04X}");
    }
}
