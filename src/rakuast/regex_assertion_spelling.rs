//! How a few regex assertions are written, for the origin walk of a regex
//! body (`regex_origins`; #15780): a lookaround over a plain named assertion
//! (`<?alpha>`, `<!ws>`) and the word boundary `<|w>`.

use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// The identifier of `node`, a plain `Regex::Assertion::Named` (no
/// arguments), and whether it captures.
// Cost: O(f), f = fields of the node.
fn plain_named(node: &RakuAstNode) -> Option<(String, bool)> {
    if node.class != RakuAstClass::RegexAssertionNamed
        || node
            .fields
            .iter()
            .any(|f| !matches!(f.name, Some("name" | "capturing")))
    {
        return None;
    }
    let capturing = node.fields.iter().any(|f| f.name == Some("capturing"));
    match super::name_parts::name_shape(super::infix_origins::child(node, "name")?)? {
        super::name_parts::NameShape::Identifier(name) => Some((name, capturing)),
        _ => None,
    }
}

/// The length of `|w` at the start of `rest` when `node` is the word
/// boundary it spells (`Assertion::Named` of `wb`, not capturing); rakudo
/// gives its name no range.
// Cost: O(f), f = fields of the node.
pub(super) fn word_boundary_len(node: &RakuAstNode, rest: &str) -> Option<usize> {
    let (name, capturing) = plain_named(node)?;
    (name == "wb" && !capturing && rest.starts_with("|w")).then_some(2)
}

/// Place `inner`, the plain named assertion a lookaround holds, written at
/// the start of `rest` (byte `at` of the unit) after the `<?` / `<!`: the
/// assertion and its `Name` run over the name (`alpha` of `<?alpha>`), and
/// the word boundary `|w` is not placed. The length of its text.
// Cost: O(f + n), f = fields of the node, n = length of the name.
pub(super) fn lookaround_named(inner: &mut RakuAstNode, rest: &str, at: usize) -> Option<usize> {
    if let Some(len) = word_boundary_len(inner, rest) {
        return Some(len);
    }
    let (name, _) = plain_named(inner)?;
    let after = rest.strip_prefix(name.as_str())?;
    if after.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-') {
        return None;
    }
    let span = SrcSpan::new(at, at + name.len());
    super::origin::set_span(inner, span);
    super::infix_origins::with_child(inner, "name", |n| super::origin::set_span(n, span));
    Some(name.len())
}
