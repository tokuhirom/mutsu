//! `RakuAST::Term::Named`: a term routine written as a bareword (#14916).
//!
//! rakudo 2026.09 renders `now`, `time` and a unit's own `sub term:<forty-two>`
//! used as `forty-two` as `Term::Named.new("forty-two")`. The parser keeps such
//! a use as the argument-less call to `term:<forty-two>`; the view renders that
//! call as the term, and the lowering turns the term back into the call.

use super::{RakuAstClass, RakuAstNode};
use crate::value::Value;

/// The `Term::Named` a bare, argument-less call to `term:<NAME>` stands for.
// Cost: O(n), n = length of `name`.
pub(super) fn view(name: &str) -> Option<RakuAstNode> {
    let term = name.strip_prefix("term:<")?.strip_suffix('>')?;
    Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermNamed,
        fields: vec![super::view::leaf_field(None, Value::str(term.to_string()))],
    })
}

/// The routine name a `Term::Named` term calls: the setting's `now` / `time`
/// are routines of that name, any other term is `term:<NAME>`.
// Cost: O(n), n = length of `term`.
pub(super) fn routine_name(term: &str) -> String {
    match term {
        "now" | "time" => term.to_string(),
        _ => format!("term:<{term}>"),
    }
}
