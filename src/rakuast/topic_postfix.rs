//! `.++` / `.--`: a postfix increment or decrement of the topic, as rakudo's
//! RakuAST frontend keeps it (#14880).
//!
//! The parser runs `.++` as `$_++`, marking the postfix application with
//! `Spelling::TopicPostfix`. RakuAST keeps a `Term::TopicCall` whose call is
//! the `Postfix` itself (`Term::TopicCall.new(Postfix.new(operator => "++"))`),
//! where `$_++` is an `ApplyPostfix` over `Var::Lexical("$_")`. [`lower`] maps
//! the topic call back to the postfix application on `$_`. `.(args)` is marked
//! the same way and is `Term::TopicCall(Call::Term(args))`.

use super::view::{postfix_node, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::token_kind::TokenKind;
use crate::value::{RuntimeError, ValueView};

/// The `Term::TopicCall` of a marked `.++` / `.--` or `.&...` call; any other marked shape
/// renders as the expression it wraps.
// Cost: O(1).
pub(super) fn view_node(names: &NameScope, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::PostfixOp { op, expr: operand }
            if matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
                && matches!(operand.as_ref(), Expr::Var(name) if name == "_") =>
        {
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::TermTopicCall,
                fields: vec![super::view::node_field(None, postfix_node(op))],
            })
        }
        // `.(args)`: the call of the topic itself, `Term::TopicCall(Call::Term)`.
        Expr::CallOn { target, args } if matches!(target.as_ref(), Expr::Var(t) if t == "_") => {
            let mut call_term = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::CallTerm,
                fields: Vec::new(),
            };
            if !args.is_empty() {
                call_term.fields.push(super::view::node_field(
                    Some("args"),
                    super::view::arg_list(names, args)?,
                ));
            }
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::TermTopicCall,
                fields: vec![super::view::node_field(None, call_term)],
            })
        }
        // `.&foo(args)` / `.&{ ... }(args)`: the call of a routine or block with
        // the topic first. RakuAST keeps `Term::TopicCall(<dynamic call>)`.
        Expr::CallOn { target, args } => match args.split_first() {
            Some((Expr::Var(topic), rest)) if topic == "_" => {
                let postfix = super::dynamic_method::postfix(names, target, rest, None)?;
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TermTopicCall,
                    fields: vec![super::view::node_field(None, postfix)],
                })
            }
            _ => view_expr(names, expr),
        },
        other => view_expr(names, other),
    }
}

/// `Term::TopicCall(Postfix("++"))` back to `$_++` (and `--`): the parser's
/// postfix application on the topic. Another postfix is refused.
// Cost: O(1).
pub(super) fn lower(postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let operator = postfix
        .fields
        .iter()
        .find(|f| f.name == Some("operator"))
        .and_then(|f| match &f.value {
            super::RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        });
    let op = match operator.as_deref() {
        Some("++") => TokenKind::PlusPlus,
        Some("--") => TokenKind::MinusMinus,
        _ => return Err(super::lower::unsupported(postfix)),
    };
    Ok(Expr::PostfixOp {
        op,
        expr: Box::new(Expr::Var("_".to_string())),
    })
}
