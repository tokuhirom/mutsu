//! How a regex's internal modifier (`:i`, `:!ignorecase`, `:s`, `:r`) is
//! written, for the origin walk of a regex body (`regex_origins`; #15780).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};

/// The length of the internal modifier `node` written at the start of `text`:
/// a colon, a `!` exactly when the node is negated, and the modifier's short
/// or long name, not followed by more of a word. `None` when `text` does not
/// spell it.
// Cost: O(1).
pub(super) fn written_len(node: &RakuAstNode, text: &str) -> Option<usize> {
    let (short, long) = match node.class {
        RakuAstClass::RegexInternalModifierIgnoreCase => ("i", "ignorecase"),
        RakuAstClass::RegexInternalModifierIgnoreMark => ("m", "ignoremark"),
        RakuAstClass::RegexInternalModifierSigspace => ("s", "sigspace"),
        RakuAstClass::RegexInternalModifierRatchet => ("r", "ratchet"),
        _ => return None,
    };
    let negated = node.fields.iter().any(|f| {
        f.name == Some("negated")
            && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
    });
    let rest = text.strip_prefix(':')?;
    let rest = match rest.strip_prefix('!') {
        Some(after) if negated => after,
        None if !negated => rest,
        _ => return None,
    };
    let name = [long, short].into_iter().find(|name| {
        rest.strip_prefix(name).is_some_and(|after| {
            !after.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
        })
    })?;
    Some(text.len() - rest.len() + name.len())
}
