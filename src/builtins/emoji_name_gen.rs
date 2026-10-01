//! Generator and verifier for [`super::emoji_name_table`].
//!
//! Test-only. The source data is the `emojis` crate (a dev-dependency), which
//! carries the CLDR short names. Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_EMOJI_TABLE=1 cargo test --lib emoji_name_gen && cargo fmt
//! ```
//!
//! The committed table is compared by *content*, not text, so `cargo fmt`
//! may reflow the generated file freely.

use super::emoji_name_table::{TABLE, lookup_emoji_by_normalized_name, normalize_emoji_name};
use std::fmt::Write as _;

const HEADER: &str = "\
//! Generated CLDR emoji short-name table. DO NOT EDIT BY HAND.
//!
//! Regenerate with:
//!
//! ```text
//! MUTSU_UPDATE_EMOJI_TABLE=1 cargo test --lib emoji_name_gen && cargo fmt
//! ```
//!
//! Keys are the names normalized by [`normalize_emoji_name`] (lowercase,
//! commas removed), sorted for binary search. When two emoji normalize to the
//! same key, the first in the `emojis` crate's order is kept.

/// Normalize a CLDR emoji short name for lookup: lowercase, commas removed.
///
/// A CLDR short name for a multi-person ZWJ sequence separates its parts with
/// commas (\"family: man, woman, girl, boy\"), but `\\c[...]` / `uniparse` split
/// their input on commas before the lookup, so the name that arrives has
/// already lost them (\"family: man woman girl boy\" -- exactly the spelling
/// Rakudo accepts). Dropping commas on both sides keeps those names resolving.
// Cost: O(n), n = name length in bytes.
pub(crate) fn normalize_emoji_name(name: &str) -> String {
    name.to_lowercase().replace(',', \"\")
}

/// Look up an emoji by a name already passed through [`normalize_emoji_name`].
// Cost: O(n log N), n = name length, N = table entries.
pub(crate) fn lookup_emoji_by_normalized_name(normalized: &str) -> Option<&'static str> {
    TABLE
        .binary_search_by_key(&normalized, |&(name, _)| name)
        .ok()
        .map(|i| TABLE[i].1)
}

";

/// Derive the `(normalized name, emoji)` table from the `emojis` crate:
/// first occurrence of each key wins, matching the linear scan this replaced.
fn derive() -> Vec<(String, String)> {
    let mut seen = std::collections::HashSet::new();
    let mut rows = Vec::new();
    for emoji in emojis::iter() {
        let key = normalize_emoji_name(emoji.name());
        if seen.insert(key.clone()) {
            rows.push((key, emoji.as_str().to_string()));
        }
    }
    rows.sort();
    rows
}

/// Render `s` as a Rust string literal with every non-ASCII char escaped, so
/// the generated source stays ASCII.
fn literal(s: &str) -> String {
    let mut out = String::from("\"");
    for c in s.chars() {
        match c {
            '"' | '\\' => {
                out.push('\\');
                out.push(c);
            }
            ' '..='~' => out.push(c),
            _ => {
                let _ = write!(out, "\\u{{{:04X}}}", c as u32);
            }
        }
    }
    out.push('"');
    out
}

fn render(rows: &[(String, String)]) -> String {
    let mut out = String::from(HEADER);
    let _ = writeln!(out, "pub(super) static TABLE: &[(&str, &str)] = &[");
    for (name, emoji) in rows {
        let _ = writeln!(out, "    ({}, {}),", literal(name), literal(emoji));
    }
    out.push_str("];\n");
    out
}

#[test]
fn verify_committed_table_matches_emoji_data() {
    let rows = derive();
    if std::env::var_os("MUTSU_UPDATE_EMOJI_TABLE").is_some() {
        let path = format!(
            "{}/src/builtins/emoji_name_table.rs",
            env!("CARGO_MANIFEST_DIR")
        );
        std::fs::write(&path, render(&rows)).expect("write generated table");
        eprintln!("regenerated {path}; run `cargo fmt`");
        return;
    }
    let committed: Vec<(String, String)> = TABLE
        .iter()
        .map(|&(n, e)| (n.to_string(), e.to_string()))
        .collect();
    assert_eq!(
        committed, rows,
        "emoji_name_table.rs is stale; regenerate with MUTSU_UPDATE_EMOJI_TABLE=1"
    );
}

#[test]
fn every_crate_name_resolves_like_the_linear_scan() {
    // The behaviour this table replaced: walk `emojis::iter()` and return the
    // first entry whose normalized name equals the normalized query.
    let all: Vec<(String, &str)> = emojis::iter()
        .map(|e| (normalize_emoji_name(e.name()), e.as_str()))
        .collect();
    for (key, _) in &all {
        let expected = all.iter().find(|(k, _)| k == key).map(|&(_, e)| e);
        assert_eq!(lookup_emoji_by_normalized_name(key), expected, "{key}");
    }
}
