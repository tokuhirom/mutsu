//! Method calls whose name is a value: `$o.$name()`, `$o.&f()` and their hyper
//! forms across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! $o.$n(1)     ApplyPostfix(operand, Call::TermAsMethod(callee => $n, args => ArgList(1)))
//! $o.?$n()     … Call::TermAsMethod(callee => $n, dispatch => ".?")
//! $o.&f(1)     ApplyPostfix(operand, Call::NameAsMethod(name => Name f, args => ArgList(1)))
//! $o.+&f()     … Call::TermAsMethod(callee => &f, dispatch => ".+")
//! @a>>.$n()    ApplyPostfix(operand, MetaPostfix::Hyper(Call::TermAsMethod(…)))
//! ```
//!
//! The parser keeps them as [`Expr::DynamicMethodCall`] (`name_expr` is the
//! variable, or a `CodeVar` for the `&f` spelling) and
//! [`Expr::HyperMethodCallDynamic`]. A `.^$n()` loses its `^` in rakudo's tree,
//! and a quoted name is a `Call::QuotedMethod` (see `convert.rs`).

use crate::scope::NameScope;
use super::convert::{
    arg_list, convert_expr, leaf_field, name_from_identifier, node_field, postfix_operand,
    unsupported as unsupported_expr,
};
use super::lower::{
    arg_exprs, arg_list_exprs, call_name_str, dispatch_modifier, lower_expr, named_child,
    unsupported,
};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The `Call::TermAsMethod` / `Call::NameAsMethod` postfix of a dynamic call.
// Cost: O(n), n = nodes of the name and arguments.
fn postfix(names: &NameScope, 
    name_expr: &Expr,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    postfix_of(names, name_expr, args, modifier, true)
}

/// [`postfix`]; `name_as_method` is false for the method of a `.=`, where
/// rakudo 2026.09 has even `&f` as the callee of a `Call::TermAsMethod`
/// (`$x .= &f`, `my $x .= &f`).
// Cost: O(n), n = nodes of the name and arguments.
fn postfix_of(names: &NameScope, 
    name_expr: &Expr,
    args: &[Expr],
    modifier: Option<char>,
    name_as_method: bool,
) -> Result<RakuAstNode, RuntimeError> {
    if modifier == Some('^') {
        return Err(unsupported_expr("dynamic metamethod call"));
    }
    let dispatch = modifier.map(|m| leaf_field(Some("dispatch"), Value::str(format!(".{m}"))));
    let mut fields = Vec::new();
    let class = match name_expr {
        // `.&f`: the routine named `f`, called with the invocant first.
        // With a dispatch modifier (`.+&f`) rakudo has the `&f` variable as the
        // callee of a `Call::TermAsMethod` instead.
        // `.&?BLOCK` / `.&*f` name a compile-time or dynamic routine, not an
        // identifier `Name.from-identifier` spells.
        Expr::CodeVar(name) if !name.starts_with(|c: char| c.is_alphabetic() || c == '_') => {
            fields.push(node_field(Some("block"), convert_expr(names, name_expr)?));
            RakuAstClass::CallBlockMethod
        }
        Expr::CodeVar(name) if dispatch.is_none() && name_as_method => {
            fields.push(node_field(Some("name"), name_from_identifier(name)));
            RakuAstClass::CallNameAsMethod
        }
        other => {
            fields.push(node_field(Some("callee"), convert_expr(names, other)?));
            RakuAstClass::CallTermAsMethod
        }
    };
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(names, args)?));
    }
    fields.extend(dispatch);
    Ok(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })
}

/// The `right` of a `.=` whose method is a term: `$x .= &f` / `$x .= $m`
/// (`Call::TermAsMethod`, `Call::NameAsMethod`) or an interpolated quoted
/// name, `$x .= "{...}"()` (`Call::QuotedMethod`). `call` is the marker's
/// call applied to the target; only its method part is rendered.
// Cost: O(n), n = nodes of the name and arguments.
pub(super) fn call_assign_postfix(names: &NameScope, call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match call {
        Expr::DynamicMethodCall {
            name_expr,
            args,
            modifier,
            quoted: false,
            ..
        } => postfix_of(names, name_expr, args, *modifier, false),
        Expr::DynamicMethodCall {
            name_expr,
            args,
            modifier: None,
            quoted: true,
            ..
        } => super::convert::call_quoted_method_expr(names, convert_expr(names, name_expr)?, args, None),
        _ => Err(unsupported_expr(
            "`.=` with a call that is not a method call",
        )),
    }
}

/// The `Initializer::CallAssign` call of `my $x .= &f(args)` / `my $x .= $m`.
// Cost: O(n), n = nodes of the callee and arguments.
pub(super) fn callee_postfix(names: &NameScope, callee: &Expr, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    postfix_of(names, callee, args, None, false)
}

/// A declaration's `Initializer::CallAssign` call, lowered to the parser's
/// pieces: the method name, or the term called as the method, and the
/// arguments. A dispatch modifier is not a declaration form.
// Cost: O(n), n = nodes of the call.
pub(super) fn call_assign_initializer(names: &NameScope, 
    call: &RakuAstNode,
) -> Result<(String, Option<Expr>, Vec<Expr>), RuntimeError> {
    match call.class {
        RakuAstClass::CallMethod if dispatch_modifier(call)?.is_none() => {
            Ok((call_name_str(call)?, None, arg_exprs(names, call)?))
        }
        _ if is_dynamic(call) && modifier(call)?.is_none() => {
            Ok((String::new(), Some(name_expr(names, call)?), args(names, call)?))
        }
        _ => Err(unsupported(call)),
    }
}

/// `$o.$name(args)` / `$o.&f(args)`.
// Cost: O(n), n = nodes of the target, name and arguments.
pub(super) fn convert(names: &NameScope, 
    target: &Expr,
    name_expr: &Expr,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), postfix_operand(names, target)?),
            node_field(Some("postfix"), postfix(names, name_expr, args, modifier)?),
        ],
    })
}

/// `@a>>.$name(args)` / `@a>>.&f(args)`.
// Cost: O(n), n = nodes of the target, name and arguments.
pub(super) fn convert_hyper(names: &NameScope, 
    target: &Expr,
    name_expr: &Expr,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    let hyper = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::MetaPostfixHyper,
        fields: vec![node_field(None, postfix(names, name_expr, args, modifier)?)],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), postfix_operand(names, target)?),
            node_field(Some("postfix"), hyper),
        ],
    })
}

/// Whether `postfix` is one of the two dynamic call classes.
// Cost: O(1).
pub(super) fn is_dynamic(postfix: &RakuAstNode) -> bool {
    matches!(
        postfix.class,
        RakuAstClass::CallTermAsMethod
            | RakuAstClass::CallNameAsMethod
            | RakuAstClass::CallBlockMethod
    )
}

/// The name expression of a dynamic call node.
fn name_expr(names: &NameScope, postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match postfix.class {
        RakuAstClass::CallNameAsMethod => Ok(Expr::CodeVar(call_name_str(postfix)?)),
        RakuAstClass::CallBlockMethod => lower_expr(names, named_child(postfix, "block")?),
        _ => lower_expr(names, named_child(postfix, "callee")?),
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

fn args(names: &NameScope, postfix: &RakuAstNode) -> Result<Vec<Expr>, RuntimeError> {
    match postfix.fields.iter().find(|f| f.name == Some("args")) {
        Some(_) => arg_list_exprs(names, named_child(postfix, "args")?),
        None => Ok(Vec::new()),
    }
}

/// The parser's [`Expr::DynamicMethodCall`] over `operand`.
// Cost: O(n), n = nodes of the name and arguments.
pub(super) fn lower(names: &NameScope, operand: Expr, postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    Ok(Expr::DynamicMethodCall {
        target: Box::new(operand),
        name_expr: Box::new(name_expr(names, postfix)?),
        args: args(names, postfix)?,
        modifier: modifier(postfix)?,
        quoted: false,
    })
}

/// The parser's [`Expr::HyperMethodCallDynamic`] over `operand`.
// Cost: O(n), n = nodes of the name and arguments.
pub(super) fn lower_hyper(names: &NameScope, operand: Expr, postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    Ok(Expr::HyperMethodCallDynamic {
        target: Box::new(operand),
        name_expr: Box::new(name_expr(names, postfix)?),
        args: args(names, postfix)?,
        modifier: modifier(postfix)?,
    })
}
