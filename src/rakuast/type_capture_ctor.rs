//! `RakuAST::Type::Capture.new($name, :smiley<D>)` and
//! `RakuAST::Type::Capture.from-identifier("T", "D")` (#18978): a type capture
//! with an optional definiteness smiley (`::T:D`), kept in its `smiley` field.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, named_arg};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

/// The capture node `class_name.method(args)` builds, `None` when this is
/// not a `Type::Capture` constructor.
// Cost: O(a), a = arguments.
pub(super) fn construct(
    names: &NameScope,
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if class_name != "RakuAST::Type::Capture" {
        return None;
    }
    let positional: Vec<Value> = args
        .iter()
        .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
        .cloned()
        .collect();
    let (name, smiley) = match method {
        "new" => {
            let [name] = positional.as_slice() else {
                return Some(Err(RuntimeError::new(
                    "RakuAST::Type::Capture.new expects a name",
                )));
            };
            (name.clone(), named_arg(args, "smiley"))
        }
        // `from-identifier("T", "D")`: the smiley is the second positional.
        "from-identifier" => {
            let (identifier, smiley) = match positional.as_slice() {
                [identifier] => (identifier, None),
                [identifier, smiley] => (identifier, Some(smiley.clone())),
                _ => return None,
            };
            let name = match super::construct_values(
                names,
                "RakuAST::Name",
                "from-identifier",
                std::slice::from_ref(identifier),
            ) {
                Ok(Some(name)) => name,
                Ok(None) => return None,
                Err(e) => return Some(Err(e)),
            };
            (name, smiley)
        }
        _ => return None,
    };
    let mut fields = vec![RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(name),
    }];
    if let Some(smiley) = smiley.filter(|s| s.as_str().is_some_and(|s| !s.is_empty())) {
        fields.push(RakuAstField {
            name: Some("smiley"),
            value: RakuAstFieldValue::Node(smiley),
        });
    }
    Some(Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TypeCapture,
        fields,
    }))))
}
