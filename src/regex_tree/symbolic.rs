//! `<::(EXPR)>`: a subrule call whose rule name an expression computes.
//!
//! Measured on rakudo 2026.09, `/ <::("alpha")> /` is a
//! `Regex::Assertion::Named` whose name is `Name(Part::Empty,
//! Part::Expression(EXPR))`, `capturing => True` unless written `<.::(…)>`.
//! The matcher resolves the name at match time from the regex text, so the
//! node only records the expression and the source it was written as.

use super::RegexNode;

/// The node for the assertion contents `target` (after any leading `.`),
/// or `None` when they are not a symbolic call of one expression.
// Cost: O(n), n = length of the contents.
pub(super) fn parse(target: &str, capturing: bool) -> Option<RegexNode> {
    let inner = target.strip_prefix("::(")?.strip_suffix(')')?;
    let (rest, mut args) = crate::parser::parse_regex_call_arg_list(inner)?;
    if !rest.trim().is_empty() || args.len() != 1 {
        return None;
    }
    Some(RegexNode::SymbolicSubrule {
        expr: Box::new(args.pop()?),
        source: inner.to_string(),
        capturing,
    })
}
