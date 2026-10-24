//! The Unicode spelling of an infix operator (ADR-10723 Stage 2).
//!
//! The parser reads `×`, `≤`, `…`, `∈`, … as the operator they alias and
//! keeps the ASCII name (`*`, `<=`, `...`, `(elem)`), which is all execution
//! needs. Rakudo 2026.09 keeps the written symbol in the tree
//! (`Infix.new("×")`), and `.DEPARSE` writes it back. Once an infix
//! application is placed (`infix_origins`), the text between its operands
//! says which spelling was written; when it is one of the operator's Unicode
//! aliases, the `Infix` takes it. Exact or absent: an operator whose text
//! does not read as an alias keeps its ASCII name.

/// The Unicode spellings rakudo keeps for the infix the parser names `op`,
/// each measured on rakudo 2026.09.
// Cost: O(1).
pub(super) fn unicode_aliases(op: &str) -> &'static [&'static str] {
    match op {
        "*" => &["×"],
        "/" => &["÷"],
        "-" => &["−"],
        "<=" => &["≤"],
        ">=" => &["≥"],
        "!=" => &["≠"],
        "==" => &["⩵"],
        "===" => &["⩶"],
        "=~=" => &["≅"],
        "o" => &["∘"],
        "..." => &["…"],
        "...^" => &["…^"],
        "(elem)" => &["∈"],
        "(cont)" => &["∋"],
        "(<=)" => &["⊆", "≼"],
        "(>=)" => &["⊇", "≽"],
        "(<)" => &["⊂"],
        "(>)" => &["⊃"],
        "⚛-=" => &["⚛−="],
        _ => &[],
    }
}

/// The spelling of `op` that `text` starts with: `op` itself, or one of its
/// Unicode aliases.
// Cost: O(1).
pub(super) fn written_prefix<'a>(op: &'a str, text: &str) -> Option<&'a str> {
    if text.starts_with(op) {
        return Some(op);
    }
    unicode_aliases(op)
        .iter()
        .copied()
        .find(|alias| text.starts_with(alias))
}

/// The operator name an `Infix` written as `name` lowers to. The parser
/// dispatches `×`, `÷` and the set-relation symbols (`∈`, `⊆`, ...) by their
/// own names, so those stay as written (a user's `infix:<×>` keeps
/// overriding them); the other Unicode spellings have no operator of their
/// own in mutsu and lower to the ASCII operator they alias.
// Cost: O(1).
pub(super) fn lowered_name(name: &str) -> &str {
    match name {
        "−" => "-",
        "≤" => "<=",
        "≥" => ">=",
        "≠" => "!=",
        "⩵" => "==",
        "⩶" => "===",
        "≅" => "=~=",
        "∘" => "o",
        "…" => "...",
        "…^" => "...^",
        "≼" => "(<=)",
        "≽" => "(>=)",
        "⚛−=" => "⚛-=",
        other => other,
    }
}
