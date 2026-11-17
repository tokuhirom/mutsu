//! `$.meth(args)`, `@.meth(args)`, `%.meth: args` and `$.^meth`: a method call
//! on `self` written with a `.`-twigil variable, as rakudo's RakuAST frontend
//! keeps it (#14880).
//!
//! The parser runs `$.meth(1)` as `self.meth(1)` and `@.meth(1)` as
//! `self.meth(1).list`, marking the `self` (or, for `@`/`%`, the whole
//! contextualized call) with `Spelling::AttributeCall`. RakuAST keeps the
//! public attribute access with its arguments,
//! `Var::Attribute::Public(name => "$.meth", args => ArgList(1))`, and keeps
//! `$.^name` as `self.^name.item`. [`lower`] maps the attribute access with
//! arguments back to the call the parser builds.

use super::view::{arg_list, leaf_field, name_from_identifier, node_field, view_expr};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::spelled::Spelling;
use crate::scope::NameScope;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// The RakuAST node of `expr` when it is one of the spellings above.
// Cost: O(n), n = size of the call's arguments.
pub(super) fn view_node(
    names: &NameScope,
    expr: &Expr,
) -> Option<Result<RakuAstNode, RuntimeError>> {
    match expr {
        // `@.meth(args)` / `%.meth(args)`: the contextualized call.
        Expr::Spelled(spelled) => match spelled.spelling {
            Spelling::AttributeCall(sigil @ ('@' | '%')) => {
                let Expr::MethodCall { target, .. } = &spelled.expr else {
                    return None;
                };
                let Expr::MethodCall { name, args, .. } = &**target else {
                    return None;
                };
                Some(public(names, sigil, *name, args))
            }
            _ => None,
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted: false,
            ..
        } if is_marked_self(target) => match modifier {
            // `$.meth(args)` / `$.meth: args`.
            None => Some(public(names, '$', *name, args)),
            // `$.^name`: the meta-method call in item context.
            Some('^') => Some(item_meta_call(names, *name, args)),
            _ => None,
        },
        _ => None,
    }
}

/// Whether `target` is the `self` of a `$.meth(...)` call.
// Cost: O(1).
fn is_marked_self(target: &Expr) -> bool {
    matches!(target, Expr::Spelled(spelled) if matches!(spelled.spelling, Spelling::AttributeCall('$')))
}

/// `Var::Attribute::Public(name => "<sigil>.<name>", args => ArgList(...))`.
// Cost: O(n), n = size of the arguments.
fn public(
    names: &NameScope,
    sigil: char,
    name: Symbol,
    args: &[Expr],
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarAttributePublic,
        fields: vec![
            leaf_field(Some("name"), Value::str(format!("{sigil}.{name}"))),
            node_field(Some("args"), arg_list(names, args)?),
        ],
    })
}

/// `self.^name.item`.
// Cost: O(n), n = size of the arguments.
fn item_meta_call(
    names: &NameScope,
    name: Symbol,
    args: &[Expr],
) -> Result<RakuAstNode, RuntimeError> {
    let call = Expr::MethodCall {
        target: Box::new(Expr::BareWord("self".to_string())),
        name,
        args: args.to_vec(),
        modifier: Some('^'),
        quoted: false,
        sugar: false,
    };
    let item = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallMethod,
        fields: vec![node_field(Some("name"), name_from_identifier("item"))],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), view_expr(names, &call)?),
            node_field(Some("postfix"), item),
        ],
    })
}

/// The call a `Var::Attribute::Public` with an `args` field stands for:
/// `self.meth(args)`, put in list or hash context for `@` / `%`; `None` for
/// one without arguments.
// Cost: O(n), n = size of the lowered arguments.
pub(super) fn lower(
    node: &RakuAstNode,
    args: impl FnOnce(&RakuAstNode) -> Result<Vec<Expr>, RuntimeError>,
) -> Option<Result<Expr, RuntimeError>> {
    if node.class != RakuAstClass::VarAttributePublic {
        return None;
    }
    let arg_node = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some("args"), RakuAstFieldValue::Node(v)) => Some(v.clone()),
        _ => None,
    })?;
    let full = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some("name"), RakuAstFieldValue::Node(v)) => Some(v.to_string_value()),
        _ => None,
    })?;
    let (sigil, rest) = full.split_at(full.chars().next().map_or(0, char::len_utf8));
    let method = rest.strip_prefix('.')?;
    let crate::value::ValueView::RakuAst(arg_node) = arg_node.view() else {
        return Some(Err(super::view::unsupported("attribute call arguments")));
    };
    let args = match args(arg_node) {
        Ok(args) => args,
        Err(err) => return Some(Err(err)),
    };
    let call = Expr::MethodCall {
        target: Box::new(Expr::BareWord("self".to_string())),
        name: Symbol::intern(method),
        args,
        modifier: None,
        quoted: false,
        sugar: false,
    };
    let context = match sigil {
        "$" => return Some(Ok(call)),
        "@" => "list",
        "%" => "hash",
        _ => return Some(Err(super::view::unsupported("attribute call sigil"))),
    };
    Some(Ok(Expr::MethodCall {
        target: Box::new(call),
        name: Symbol::intern(context),
        args: Vec::new(),
        modifier: None,
        quoted: false,
        sugar: context == "list",
    }))
}
