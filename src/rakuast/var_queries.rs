//! `RakuAST::Var::Lexical.desigilname`: the variable's name without its sigil,
//! as a `Name` (rakudo 2026.09 stores the name with the sigil and derives this).

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// `desigilname`: `$foo` -> `Name.from-identifier("foo")`, `@a` -> `a`,
/// `$Foo::bar` -> `Foo::bar` (not split); a bare sigil answers the empty
/// name. `None` for a node that is not a lexical variable.
// Cost: O(n), n = length of the variable's spelling.
pub(crate) fn desigilname(node: &RakuAstNode) -> Option<Value> {
    if node.class != RakuAstClass::VarLexical {
        return None;
    }
    let spelling = node.fields.first().and_then(|f| match &f.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    })?;
    let mut chars = spelling.chars();
    chars.next()?;
    // One identifier leaf, `::` and all: rakudo does not split a variable's
    // name into parts here (`$Foo::bar` -> `from-identifier("Foo::bar")`).
    Some(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields: vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(Value::str(chars.as_str().to_string())),
        }],
    })))
}
