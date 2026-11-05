//! The invocant of a `RakuAST::ArgList` and the builders that replace its
//! arguments, as rakudo 2026.09 has them (`src/Raku/ast/call.rakumod`).
//!
//! `ArgList.from-invocant-list($colon-apply)` takes a colon-call's operand list
//! (`foo: 1, 2` is an `ApplyListInfix` over `:`): the first operand is the
//! invocant, the rest are the arguments. Rakudo keeps the invocant in a private
//! attribute that `.raku` and the constructor form leave out and `invocant`
//! answers; mutsu keeps it in the same hidden field ([`INVOCANT`]). A list with
//! none answers the `RakuAST::Expression` role's type object.
//!
//! `replace-args(@args)` and `set-arg-at-pos($pos, $arg)` change the arguments
//! of the node shared by every holder and answer `Nil`; a position counts from
//! the end when negative and past the end pads with `Mu` holes, as rakudo's
//! array does.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, node_accessor, origin};
use crate::value::{RuntimeError, Value, ValueView};

/// The hidden field holding an `ArgList`'s invocant.
const INVOCANT: &str = "invocant";

/// Whether `field` is the hidden invocant of an `ArgList`.
// Cost: O(1).
pub(super) fn is_hidden_invocant(node: &RakuAstNode, field: &RakuAstField) -> bool {
    node.class == RakuAstClass::ArgList && field.name == Some(INVOCANT)
}

/// `invocant`: the stored invocant, else the `Expression` type object.
// Cost: O(f), f = fields of the node.
pub(crate) fn invocant(node: &RakuAstNode) -> Value {
    node.fields
        .iter()
        .find(|f| is_hidden_invocant(node, f))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(value) => Some(value.clone()),
            _ => None,
        })
        .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("RakuAST::Expression")))
}

/// Whether `field` is one of the list's arguments (the hidden invocant is
/// named, so it never is).
fn positional(field: &RakuAstField) -> bool {
    field.name.is_none() && !origin::is_origin(field)
}

fn node_field(value: Value) -> RakuAstField {
    RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(value),
    }
}

fn mu() -> Value {
    Value::package(crate::symbol::Symbol::intern("Mu"))
}

/// `ArgList.from-invocant-list($colon-apply)`.
// Cost: O(o), o = operands of the colon list.
pub(crate) fn from_invocant_list(args: &[Value]) -> Result<Value, RuntimeError> {
    let [apply] = args else {
        return Err(RuntimeError::new(format!(
            "Too few positionals passed; expected 2 arguments but got {}",
            args.len() + 1
        )));
    };
    let operands = match apply.view() {
        ValueView::RakuAst(node) if node.class == RakuAstClass::ApplyListInfix => {
            node_accessor(node, "operands")
                .and_then(|v| v.as_list_items().map(<[Value]>::to_vec))
                .unwrap_or_default()
        }
        _ => {
            return Err(RuntimeError::new(format!(
                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
                 '$colon-apply'; expected RakuAST::ApplyListInfix but got {}",
                crate::value::type_name::value_type_name(apply)
            )));
        }
    };
    let mut operands = operands.into_iter();
    let Some(invocant) = operands.next() else {
        return Err(RuntimeError::new(
            "MVMArray: Can't shift from an empty array",
        ));
    };
    let mut fields = vec![RakuAstField {
        name: Some(INVOCANT),
        value: RakuAstFieldValue::Node(invocant),
    }];
    fields.extend(operands.map(node_field));
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ArgList,
        fields,
    })))
}

/// `replace-args(@args)`: the arguments become the elements of `list`.
// Cost: O(f + n), f = fields of the node, n = elements of the list.
pub(crate) fn replace_args(node: &mut RakuAstNode, list: &Value) -> Result<(), RuntimeError> {
    if node.class != RakuAstClass::ArgList {
        return Err(RuntimeError::new(
            "replace-args is only available on RakuAST::ArgList",
        ));
    }
    let Some(items) = list.as_list_items().map(<[Value]>::to_vec) else {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '@args'; expected List but got {}",
            crate::value::type_name::value_type_name(list)
        )));
    };
    let keep: Vec<RakuAstField> = std::mem::take(&mut node.fields)
        .into_iter()
        .filter(|f| !positional(f))
        .collect();
    node.fields = keep;
    node.fields.extend(items.into_iter().map(node_field));
    Ok(())
}

/// `set-arg-at-pos($pos, $arg)`.
// Cost: O(f + p), f = fields of the node, p = the position written.
pub(crate) fn set_arg_at_pos(
    node: &mut RakuAstNode,
    pos: &Value,
    arg: &Value,
) -> Result<(), RuntimeError> {
    if node.class != RakuAstClass::ArgList {
        return Err(RuntimeError::new(
            "set-arg-at-pos is only available on RakuAST::ArgList",
        ));
    }
    let ValueView::Int(pos) = pos.view() else {
        return Err(RuntimeError::new(format!(
            "This type cannot unbox to a native integer: P6opaque, {}",
            crate::value::type_name::value_type_name(pos)
        )));
    };
    if !matches!(arg.view(), ValueView::RakuAst(_)) {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$arg'; expected RakuAST::Expression but got {}",
            crate::value::type_name::value_type_name(arg)
        )));
    }
    let slots: Vec<usize> = (0..node.fields.len())
        .filter(|&i| positional(&node.fields[i]))
        .collect();
    let len = i64::try_from(slots.len()).unwrap_or(i64::MAX);
    let at = if pos < 0 { len + pos } else { pos };
    let Some(at) = usize::try_from(at).ok().filter(|_| at >= 0) else {
        return Err(RuntimeError::new("MVMArray: Index out of bounds"));
    };
    if let Some(&slot) = slots.get(at) {
        node.fields[slot].value = RakuAstFieldValue::Node(arg.clone());
        return Ok(());
    }
    // Past the end: holes of `Mu` up to the position, as rakudo's array has.
    for _ in slots.len()..at {
        node.fields.push(node_field(mu()));
    }
    node.fields.push(node_field(arg.clone()));
    Ok(())
}
