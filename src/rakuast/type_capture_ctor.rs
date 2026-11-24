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

/// The smiley on a `Parameter`'s one type capture (`::T:D $x` -> `'D'`), for
/// lowering it back into a parameter: the binder reads `:D`/`:U` as an
/// `Any:D`/`Any:U` type (#19020).
// Cost: O(f), f = the parameter's fields.
pub(super) fn parameter_capture_smiley(parameter: &RakuAstNode) -> Option<char> {
    let field = parameter
        .fields
        .iter()
        .find(|f| f.name == Some("type-captures"))?;
    let RakuAstFieldValue::List(items) = &field.value else {
        return None;
    };
    let [capture] = items.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(capture) = capture.view() else {
        return None;
    };
    let smiley = capture.fields.iter().find(|f| f.name == Some("smiley"))?;
    let RakuAstFieldValue::Node(value) = &smiley.value else {
        return None;
    };
    value.as_str().and_then(|s| s.chars().next())
}

/// `::T:D $x`: the parser turns the capture's smiley into an `Any:D` type for
/// the binder (#19020). The RakuAST view keeps the smiley on the
/// `Type::Capture` node instead and leaves the parameter's type the default,
/// so it views this copy of the parameter without that type. `None` when the
/// parameter has no such synthesized type.
// Cost: O(1), plus a clone of the parameter when it applies.
pub(super) fn param_without_capture_smiley_type(
    pd: &crate::ast::ParamDef,
) -> Option<crate::ast::ParamDef> {
    let smiley = pd.type_capture_smiley?;
    let synthesized = pd
        .type_constraint
        .as_deref()
        .and_then(|t| t.strip_prefix("Any:"))
        .is_some_and(|rest| rest.len() == 1 && rest.starts_with(smiley));
    synthesized.then(|| crate::ast::ParamDef {
        type_constraint: None,
        ..pd.clone()
    })
}

/// The view's `Type::Capture` node for the parameter `pd`'s capture `name`,
/// with the smiley written on the capture (`::T:D`) as its `smiley` field.
// Cost: O(|name|).
pub(super) fn capture_of(
    names: &NameScope,
    name: &str,
    pd: &crate::ast::ParamDef,
) -> Result<RakuAstNode, RuntimeError> {
    let mut node = super::view::type_capture_node(names, name)?;
    if let Some(smiley) = pd.type_capture_smiley {
        node.fields.push(RakuAstField {
            name: Some("smiley"),
            value: RakuAstFieldValue::Node(Value::str(smiley.to_string())),
        });
    }
    Ok(node)
}
