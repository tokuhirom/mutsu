//! Generator and verifier for [`super::unicode_name_data`].
//!
//! Test-only. The source data is the `unicode_names2` crate (a
//! dev-dependency), whose names the runtime used directly before #10438; the
//! tests below pin that [`super::unicode_name`] answers exactly as it did, in
//! both directions, for every codepoint. Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_NAME_TABLE=1 cargo test --lib unicode_name_gen
//! ```

use super::unicode_name::{char_by_name, char_name, normalize_into};
use super::unicode_name_data as data;
use std::collections::BTreeMap;
use std::fmt::Write as _;

const HEADER: &str = "\
//! Generated Unicode character-name tables. DO NOT EDIT BY HAND.
//!
//! Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_NAME_TABLE=1 cargo test --lib unicode_name_gen
//! ```
//!
//! Every stored name is a run of `TOKENS`. A token's low 15 bits index a word
//! (`WORD_TEXT[WORD_START[w]..WORD_START[w + 1]]`); its top bit says the
//! separator after it is `-` rather than a space. Entry `i` is codepoint
//! `CODES[i]` (ascending) with tokens `NAME_START[i]..NAME_START[i + 1]`.
//! `BY_NAME` lists entry indices in order of their UAX #44 LM2-normalized
//! names, for the name -> codepoint binary search. CJK unified ideographs
//! (`CJK_RANGES`) and Hangul syllables are named algorithmically instead.

";

/// Every `(codepoint, name)` the crate stores literally.
type StoredNames = Vec<(u32, String)>;
/// Inclusive codepoint ranges.
type Ranges = Vec<(u32, u32)>;

/// Every name the crate stores literally, plus the CJK unified ideograph
/// ranges it names algorithmically.
fn source() -> (StoredNames, Ranges) {
    let mut names = Vec::new();
    let mut cjk: Ranges = Vec::new();
    for c in (0..=0x10FFFFu32).filter_map(char::from_u32) {
        let Some(name) = unicode_names2::name(c) else {
            continue;
        };
        let name = name.to_string();
        let cp = c as u32;
        if name.starts_with("CJK UNIFIED IDEOGRAPH-") {
            match cjk.last_mut() {
                Some((_, hi)) if *hi + 1 == cp => *hi = cp,
                _ => cjk.push((cp, cp)),
            }
        } else if !(0xAC00..=0xD7A3).contains(&cp) {
            names.push((cp, name));
        }
    }
    (names, cjk)
}

struct Tables {
    cjk: Ranges,
    codes: Vec<u32>,
    name_start: Vec<u32>,
    tokens: Vec<u16>,
    word_text: String,
    word_start: Vec<u32>,
    by_name: Vec<u16>,
}

fn build() -> Tables {
    let (names, cjk) = source();
    // Split each name into words at spaces and hyphens, remembering which.
    let split = |name: &str| -> Vec<(String, bool)> {
        let mut parts = Vec::new();
        let mut word = String::new();
        for ch in name.chars() {
            if ch == ' ' || ch == '-' {
                parts.push((std::mem::take(&mut word), ch == '-'));
            } else {
                word.push(ch);
            }
        }
        parts.push((word, false));
        parts
    };
    // Number words by descending frequency (ties alphabetically), so the
    // table is deterministic.
    let mut freq: BTreeMap<String, usize> = BTreeMap::new();
    for (_, name) in &names {
        for (w, _) in split(name) {
            *freq.entry(w).or_default() += 1;
        }
    }
    let mut words: Vec<(String, usize)> = freq.into_iter().collect();
    words.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    assert!(words.len() < 0x8000, "word index must fit 15 bits");
    let index: BTreeMap<&str, u16> = words
        .iter()
        .enumerate()
        .map(|(i, (w, _))| (w.as_str(), i as u16))
        .collect();
    let mut word_text = String::new();
    let mut word_start = vec![0u32];
    for (w, _) in &words {
        word_text.push_str(w);
        word_start.push(word_text.len() as u32);
    }
    let mut codes = Vec::new();
    let mut name_start = vec![0u32];
    let mut tokens = Vec::new();
    for (cp, name) in &names {
        codes.push(*cp);
        for (w, hyphen) in split(name) {
            tokens.push(index[w.as_str()] | if hyphen { 0x8000 } else { 0 });
        }
        name_start.push(tokens.len() as u32);
    }
    assert!(
        names.len() <= usize::from(u16::MAX),
        "entry index must fit u16"
    );
    let mut keyed: Vec<(Vec<u8>, u16)> = names
        .iter()
        .enumerate()
        .map(|(i, (cp, name))| {
            let mut key = Vec::new();
            assert!(normalize_into(name, *cp == 0x1180, &mut key), "{name}");
            (key, i as u16)
        })
        .collect();
    keyed.sort();
    for pair in keyed.windows(2) {
        assert_ne!(pair[0].0, pair[1].0, "LM2 keys must be unique");
    }
    Tables {
        cjk,
        codes,
        name_start,
        tokens,
        word_text,
        word_start,
        by_name: keyed.into_iter().map(|(_, i)| i).collect(),
    }
}

fn emit<T: std::fmt::Display>(out: &mut String, name: &str, ty: &str, values: &[T]) {
    let _ = writeln!(out, "#[rustfmt::skip]");
    let _ = writeln!(
        out,
        "pub(super) static {name}: [{ty}; {}] = [",
        values.len()
    );
    for chunk in values.chunks(16) {
        out.push_str("    ");
        for v in chunk {
            let _ = write!(out, "{v},");
        }
        out.push('\n');
    }
    out.push_str("];\n\n");
}

fn render(t: &Tables) -> String {
    let mut out = String::from(HEADER);
    let _ = writeln!(
        out,
        "pub(super) static CJK_RANGES: [(u32, u32); {}] = [",
        t.cjk.len()
    );
    for (lo, hi) in &t.cjk {
        let _ = writeln!(out, "    (0x{lo:05X}, 0x{hi:05X}),");
    }
    out.push_str("];\n\n");
    emit(&mut out, "CODES", "u32", &t.codes);
    emit(&mut out, "NAME_START", "u32", &t.name_start);
    emit(&mut out, "TOKENS", "u16", &t.tokens);
    emit(&mut out, "WORD_START", "u32", &t.word_start);
    emit(&mut out, "BY_NAME", "u16", &t.by_name);
    let _ = writeln!(out, "#[rustfmt::skip]");
    out.push_str("pub(super) static WORD_TEXT: &str = \"\\\n");
    let bytes = t.word_text.as_bytes();
    for chunk in bytes.chunks(96) {
        // Words are upper-case ASCII letters and digits; a line-continuation
        // escape would swallow a leading space, but no word has one.
        let _ = writeln!(out, "{}\\", std::str::from_utf8(chunk).expect("ascii"));
    }
    out.push_str("\";\n");
    out
}

#[test]
fn verify_committed_tables_match_unicode_names2() {
    let t = build();
    if std::env::var_os("MUTSU_UPDATE_NAME_TABLE").is_some() {
        let path = format!(
            "{}/src/builtins/unicode_name_data.rs",
            env!("CARGO_MANIFEST_DIR")
        );
        std::fs::write(&path, render(&t)).expect("write generated table");
        eprintln!("regenerated {path}");
        return;
    }
    assert_eq!(data::CJK_RANGES.as_slice(), t.cjk.as_slice());
    assert_eq!(data::CODES.as_slice(), t.codes.as_slice());
    assert_eq!(data::NAME_START.as_slice(), t.name_start.as_slice());
    assert_eq!(data::TOKENS.as_slice(), t.tokens.as_slice());
    assert_eq!(data::WORD_START.as_slice(), t.word_start.as_slice());
    assert_eq!(data::WORD_TEXT, t.word_text);
    assert_eq!(data::BY_NAME.as_slice(), t.by_name.as_slice());
}

#[test]
fn char_name_matches_unicode_names2_for_every_codepoint() {
    for c in (0..=0x10FFFFu32).filter_map(char::from_u32) {
        let expected = unicode_names2::name(c).map(|n| n.to_string());
        assert_eq!(char_name(c), expected, "U+{:04X}", c as u32);
    }
}

#[test]
fn char_by_name_matches_unicode_names2_for_every_name() {
    for c in (0..=0x10FFFFu32).filter_map(char::from_u32) {
        let Some(name) = char_name(c) else { continue };
        let loose = name.to_lowercase().replace(' ', "_");
        for query in [name.as_str(), loose.as_str()] {
            assert_eq!(
                char_by_name(query),
                unicode_names2::character(query),
                "{query}"
            );
        }
    }
}

#[test]
fn char_by_name_matches_unicode_names2_on_edge_spellings() {
    for query in [
        "",
        " ",
        "LATIN SMALL LETTER A ",
        "latinsmalllettera",
        "Black_Star",
        "BACKSPACE",
        "LF",
        "nbsp",
        "VS17",
        "VS256",
        "ZWJ",
        "BOM",
        "BYTE ORDER MARK",
        "LATIN CAPITAL LETTER GHA",
        "nonsense",
        "LATIN SMALL LETTER A!",
        "HANGUL JUNGSEONG O-E",
        "HANGUL JUNGSEONG OE",
        "HANGUL JUNGSEONG O -E",
        "hangul_jungseong_o-e_",
        "TIBETAN LETTER -A",
        "TIBETAN LETTER A",
        "HANGUL SYLLABLE GAG",
        "HANGUL SYLLABLE YEO",
        "HANGUL SYLLABLE X",
        "HANGUL SYLLABLE ",
        "CJK UNIFIED IDEOGRAPH-4E00",
        "cjk unified ideograph-4e00",
        "CJK UNIFIED IDEOGRAPH-04E00",
        "CJK UNIFIED IDEOGRAPH-",
        "CJK UNIFIED IDEOGRAPH-FFFFF",
        "CJK UNIFIED IDEOGRAPH-1234567",
        "CJK UNIFIED IDEOGRAPH-0041",
    ] {
        assert_eq!(
            char_by_name(query),
            unicode_names2::character(query),
            "{query:?}"
        );
    }
}
