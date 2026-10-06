//! Method calls whose name is a value: `$o.$name()`, `$o.&f()` and their hyper
//! forms across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! $o.$n(1)     ApplyPostfix(operand, Call::TermAsMethod(callee => $n, args => ArgList(1)))
//! $o.?$n()     … Call::TermAsMethod(callee => $n, dispatch => ".?")
//! $o.&f(1)     ApplyPostfix(operand, Call::NameAsMethod(name => Name f, args => ArgList(1)))
//! @a>>.$n()    ApplyPostfix(operand, MetaPostfix::Hyper(Call::TermAsMethod(…)))
//! ```
//!
//! The parser keeps them as [`Expr::DynamicMethodCall`] (`name_expr` is the
//! variable, or a `CodeVar` for the `&f` spelling) and
//! [`Expr::HyperMethodCallDynamic`]. A `.^$n()` loses its `^` in rakudo's tree,
//! and a quoted name is a `Call::QuotedMethod` (see `convert.rs`).

use super::convert::{
    arg_list, convert_expr, leaf_field, name_from_identifier, node_field,
    unsupported as unsupported_expr,
};
use super::lower::{arg_list_exprs, call_name_str, lower_expr, named_child, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The `Call::TermAsMethod` / `Call::NameAsMethod` postfix of a dynamic call.
// Cost: O(n), n = nodes of the name and arguments.
fn postfix(
    name_expr: &Expr,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    if modifier == Some('^') {
        return Err(unsupported_expr("dynamic metamethod call"));
    }
    let dispatch = modifier.map(|m| leaf_field(Some("dispatch"), Value::str(format!(".{m}"))));
    let mut fields = Vec::new();
    let class = match name_expr {
        // `.&f`: the routine named `f`, called with the invocant first.
        Expr::CodeVar(name) => {
            if dispatch.is_some() {
                return Err(unsupported_expr("dispatch modifier on `.&f`"));
            }
            fields.push(node_field(Some("name"), name_from_identifier(name)));
            RakuAstClass::CallNameAsMethod
        }
        other => {
            fields.push(node_field(Some("callee"), convert_expr(other)?));
            RakuAstClass::CallTermAsMethod
        }
    };
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    fields.extend(dispatch);
    Ok(RakuAstNode { class, fields })
}

/// `$o.$name(args)` / `$o.&f(args)`.
// Cost: O(n), n = nodes of the target, name and arguments.
pub(super) fn convert(
    target: &Expr,
    name_expr: &Expr,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), convert_expr(target)?),
            node_field(Some("postfix"), postfix(name_expr, args, modifier)?),
        ],
    })
}

/// `@a>>.$name(args)` / `@a>>.&f(args)`.
// Cost: O(n), n = nodes of the target, name and arguments.
pub(super) fn convert_hyper(
    target: &Expr,
    name_expr: &Expr,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    let hyper = RakuAstNode {
        class: RakuAstClass::MetaPostfixHyper,
        fields: vec![node_field(None, postfix(name_expr, args, modifier)?)],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), convert_expr(target)?),
            node_field(Some("postfix"), hyper),
        ],
    })
}

/// Whether `postfix` is one of the two dynamic call classes.
// Cost: O(1).
pub(super) fn is_dynamic(postfix: &RakuAstNode) -> bool {
    matches!(
        postfix.class,
        RakuAstClass::CallTermAsMethod | RakuAstClass::CallNameAsMethod
    )
}

/// The name expression of a dynamic call node.
fn name_expr(postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match postfix.class {
        RakuAstClass::CallNameAsMethod => Ok(Expr::CodeVar(call_name_str(postfix)?)),
        _ => lower_expr(named_child(postfix, "callee")?),
    }
}

/// The modifier char of the `dispatch` field (`.?` -> `?`).
fn modifier(postfix: &RakuAstNode) -> Result<Option<char>, RuntimeError> {
    let Some(field) = postfix.fields.iter().find(|f| f.name == Some("dispatch")) else {
        return Ok(None);
    };
    let super::RakuAstFieldValue::Node(v) = &field.value else {
        return Err(unsupported(postfix));
    };
    let ValueView::Str(s) = v.view() else {
        return Err(unsupported(postfix));
    };
    match s
        .strip_prefix('.')
        .map(|rest| rest.chars().collect::<Vec<_>>())
    {
        Some(chars) if chars.len() == 1 => Ok(Some(chars[0])),
        _ => Err(unsupported(postfix)),
    }
}

fn args(postfix: &RakuAstNode) -> Result<Vec<Expr>, RuntimeError> {
    match postfix.fields.iter().find(|f| f.name == Some("args")) {
        Some(_) => arg_list_exprs(named_child(postfix, "args")?),
        None => Ok(Vec::new()),
    }
}

/// The parser's [`Expr::DynamicMethodCall`] over `operand`.
// Cost: O(n), n = nodes of the name and arguments.
pub(super) fn lower(operand: Expr, postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    Ok(Expr::DynamicMethodCall {
        target: Box::new(operand),
        name_expr: Box::new(name_expr(postfix)?),
        args: args(postfix)?,
        modifier: modifier(postfix)?,
        quoted: false,
    })
}

/// The parser's [`Expr::HyperMethodCallDynamic`] over `operand`.
// Cost: O(n), n = nodes of the name and arguments.
pub(super) fn lower_hyper(operand: Expr, postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    Ok(Expr::HyperMethodCallDynamic {
        target: Box::new(operand),
        name_expr: Box::new(name_expr(postfix)?),
        args: args(postfix)?,
        modifier: modifier(postfix)?,
    })
}
