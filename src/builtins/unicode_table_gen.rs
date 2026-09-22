//! Shared machinery for generating and verifying the Unicode property tables.
//!
//! Test-only. Both `unicode_gc_gen` and `unicode_script_gen` answer the same
//! question -- "which of these N disjoint `\p{...}` classes is this codepoint
//! in?" -- and both bake the answer into the same three-tier shape, so the
//! derivation, the tier construction, the emitter and the drift check live
//! here once.
//!
//! The tables are derived from `regex-syntax`'s Unicode data, which is the
//! very data `regex`'s `\p{...}` classes match against, folded in the same
//! first-match-wins order the ordered-regex probes used before
//! [#8999](https://github.com/tokuhirom/mutsu/issues/8999). That is what makes
//! each generated answer identical to the regexes' by construction rather than
//! by argument.

use regex_syntax::hir::{Class, HirKind};
use std::fmt::Write as _;

/// Codepoints per BMP leaf block, as a shift.
pub(super) const SHIFT: u32 = 6;
pub(super) const BLOCK: usize = 1 << SHIFT;

/// The codepoint ranges of one `\p{...}` class, straight out of `regex-syntax`.
pub(super) fn ranges_for(pattern: &str) -> Vec<(u32, u32)> {
    // The class-forcing brackets keep a single-codepoint class (`Zp`) from
    // being folded into a `Literal` by the HIR translator.
    let hir = regex_syntax::parse(&format!("[{pattern}]")).expect("valid class");
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
        other => panic!("unexpected HIR for {pattern}: {other:?}"),
    }
}

/// One code per codepoint: the index of the first pattern in `patterns` whose
/// class contains it, or `patterns.len()` (the fallthrough) where none does.
pub(super) fn derive_table(patterns: &[String]) -> Vec<u8> {
    let fallback = u8::try_from(patterns.len()).expect("fallthrough code fits in u8");
    let mut table = vec![fallback; 0x110000];
    // Reverse order so an earlier pattern wins, exactly as a first-match-wins
    // regex probe did.
    for (i, pattern) in patterns.iter().enumerate().rev() {
        for (start, end) in ranges_for(pattern) {
            for cp in start..=end {
                table[cp as usize] = i as u8;
            }
        }
    }
    table
}

/// The three tiers derived from a per-codepoint table: ASCII, the BMP trie,
/// and the constant-category runs above the BMP.
pub(super) struct Tables {
    pub(super) ascii: Vec<u8>,
    pub(super) index: Vec<u16>,
    pub(super) leaves: Vec<u8>,
    pub(super) leaf_count: usize,
    pub(super) astral_starts: Vec<u32>,
    pub(super) astral_cats: Vec<u8>,
}

pub(super) fn build_tables(table: &[u8]) -> Tables {
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
pub(super) struct Committed<'a> {
    pub(super) ascii: &'a [u8],
    pub(super) shift: u32,
    pub(super) index: &'a [u16],
    pub(super) leaves: &'a [u8],
    pub(super) astral_starts: &'a [u32],
    pub(super) astral_cats: &'a [u8],
}

/// Fail loudly, naming the array, if the committed file no longer matches the
/// Unicode data it was generated from.
pub(super) fn assert_committed_matches(committed: &Committed<'_>, built: &Tables) {
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
pub(super) fn render(t: &Tables, header: &str, extra: &str) -> String {
    let mut out = String::from(header);
    let _ = write!(
        out,
        "/// Code of every ASCII codepoint, indexed directly.\n\
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
        "/// Deduplicated BMP leaf blocks: {} blocks of {BLOCK} codes.\n\
         #[rustfmt::skip]\n\
         pub(super) static BMP_LEAVES: [u8; {}] = [\n",
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
        "/// Code of each run in [`ASTRAL_STARTS`].\n\
         #[rustfmt::skip]\n\
         pub(super) static ASTRAL_CATS: [u8; {}] = [\n",
        t.astral_cats.len()
    );
    emit_u8(&mut out, &t.astral_cats, 64);
    out.push_str(extra);
    out
}

/// Rewrite `<manifest>/src/builtins/<file>` when `MUTSU_UPDATE_GC_TABLE` is
/// set. A no-op otherwise, so the verify tests can call it unconditionally.
pub(super) fn maybe_regenerate(file: &str, t: &Tables, header: &str, extra: &str) {
    if std::env::var_os("MUTSU_UPDATE_GC_TABLE").is_none() {
        return;
    }
    let path = format!("{}/src/builtins/{file}", env!("CARGO_MANIFEST_DIR"));
    std::fs::write(&path, render(t, header, extra)).expect("write generated table");
    eprintln!("regenerated {path}");
}

/// Drive `lookup` over every codepoint a `char` can be and compare it against
/// the derived table. Exhaustive, not sampled.
pub(super) fn assert_lookup_matches_table<F: Fn(char) -> u8>(table: &[u8], lookup: F) {
    for cp in 0..0x110000u32 {
        let Some(ch) = char::from_u32(cp) else {
            continue; // surrogate: unreachable through `char`
        };
        assert_eq!(lookup(ch), table[cp as usize], "U+{cp:04X}");
    }
}
