//! `RakuAST::Term::True.new` / `RakuAST::Term::False.new`: rakudo 2026.11's
//! convenience classes whose only method, `new`, answers the same node as
//! `RakuAST::Term::Enum.from-identifier('True')` (`.raku` prints exactly that),
//! so `DEPARSE`, `EVAL` and a doc block's `set-config` see a plain enum term
//! (#18982).

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value};

/// The node `class_name.method(args)` builds, or `None` when `class_name` is
/// not `Term::True`/`Term::False` or `method` is not `new`.
// Cost: O(1).
pub(super) fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let identifier = match class_name {
        "RakuAST::Term::True" => "True",
        "RakuAST::Term::False" => "False",
        _ => return None,
    };
    if method != "new" {
        return None;
    }
    if !args.is_empty() {
        return Some(Err(RuntimeError::new(format!(
            "{class_name}.new expects no arguments"
        ))));
    }
    Some(Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermEnum,
        fields: vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(Value::str(identifier.to_string())),
        }],
    }))))
}
