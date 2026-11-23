//! The tags `use experimental` accepts.
//!
//! Rakudo's `experimental` module exports one tag per feature it gates, and an
//! import of any other tag dies at compile time with "no such tag" (#18983).
//! That includes `:macros`, which Rakudo 2026.09 removed
//! (`raku-doc/doc/Language/experimental.rakudoc`).

/// The tags rakudo 2026.11's `use experimental` exports.
const EXPERIMENTAL_TAGS: &[&str] = &["cached", "pack", "rakuast", "smallnatives", "will-complain"];

/// The compile-time error for the first tag of `use experimental` that the
/// module does not export, if any.
// Cost: O(t * k), t = tags of the `use`, k = known tags.
pub(super) fn unknown_experimental_tag_error(tags: &[String]) -> Option<String> {
    let unknown = tags.iter().find(|tag| !EXPERIMENTAL_TAGS.contains(&tag.as_str()))?;
    Some(format!(
        "Error while importing from 'experimental': no such tag '{unknown}'"
    ))
}
