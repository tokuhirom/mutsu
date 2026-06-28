use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{AssignOp, Expr, ParamDef, Stmt, collect_placeholders_shallow};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::decl::parse_array_shape_suffix;
use super::sub;
use super::{block, block_inner, ident, keyword, parse_comma_or_expr, statement, var_name};
fn condition_has_assignment_tail(rest: &str) -> bool {
    if rest.starts_with(":=") || rest.starts_with("::=") || rest.starts_with("⚛=") {
        return true;
    }
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        return true;
    }
    super::assign::parse_compound_assign_op(rest).is_some()
        || super::assign::parse_custom_compound_assign_op(rest).is_some()
}

/// Split an inline no-initializer declaration out of a `while`/`until`
/// condition so it is declared once at loop entry instead of re-declared (and
/// reset to its default) on every iteration. `until my $done { ... CLOSE {
/// $done = True } }` relies on `$done` persisting across iterations.
///
/// Only the no-initializer form is hoisted: `while my $x = expr { }` must keep
/// re-running `= expr` each iteration (that is the whole point of the idiom), so
/// it is left as-is. Returns the hoisted declaration (if any) and the condition
/// to test each iteration (a plain read of the declared variable).
fn split_loop_cond_decl(cond: Expr) -> (Option<Stmt>, Expr) {
    if let Expr::DoStmt(boxed) = &cond
        && let Stmt::VarDecl {
            name,
            custom_traits,
            ..
        } = boxed.as_ref()
        && !custom_traits.iter().any(|(t, _)| t == "__has_initializer")
    {
        let read = if let Some(rest) = name.strip_prefix('@') {
            Expr::ArrayVar(rest.to_string())
        } else if let Some(rest) = name.strip_prefix('%') {
            Expr::HashVar(rest.to_string())
        } else {
            Expr::Var(name.clone())
        };
        let decl = (**boxed).clone();
        return (Some(decl), read);
    }
    (None, cond)
}

fn condition_expr(input: &str) -> PResult<'_, Expr> {
    match expression(input) {
        Ok((rest, cond)) => {
            let (tail, _) = ws(rest)?;
            if condition_has_assignment_tail(tail)
                && let Ok((assign_rest, assign_cond)) = super::assign::try_parse_assign_expr(input)
            {
                return Ok((assign_rest, assign_cond));
            }
            Ok((rest, cond))
        }
        Err(_) => super::assign::try_parse_assign_expr(input),
    }
}

fn render_expr_brief(expr: &Expr) -> Option<String> {
    match expr {
        Expr::BareWord(s) => Some(s.clone()),
        Expr::Var(s) => Some(format!("${s}")),
        Expr::ArrayVar(s) => Some(format!("@{s}")),
        Expr::HashVar(s) => Some(format!("%{s}")),
        Expr::Literal(v) => Some(v.to_string_value()),
        Expr::Grouped(inner) => render_expr_brief(inner),
        Expr::Unary { expr, .. } => render_expr_brief(expr),
        Expr::Binary { left, right, .. } => {
            let l = render_expr_brief(left)?;
            let r = render_expr_brief(right)?;
            Some(format!("{l} {r}"))
        }
        _ => None,
    }
}

pub(super) fn make_block_gobbled_group_error(what: &str) -> PError {
    let sorrow_message =
        format!("Expression '{what}' was gobbled by a block; parenthesize it or add a separator");
    let mut sorrow_attrs = std::collections::HashMap::new();
    sorrow_attrs.insert("what".to_string(), Value::str(what.to_string()));
    sorrow_attrs.insert("message".to_string(), Value::str(sorrow_message.clone()));
    let sorrow = Value::make_instance(Symbol::intern("X::Syntax::BlockGobbled"), sorrow_attrs);

    let mut panic_attrs = std::collections::HashMap::new();
    panic_attrs.insert("what".to_string(), Value::str("block".to_string()));
    panic_attrs.insert(
        "message".to_string(),
        Value::str("Missing block".to_string()),
    );
    let panic = Value::make_instance(Symbol::intern("X::Syntax::Missing"), panic_attrs);

    let group_message = format!("{sorrow_message}\nMissing block");
    let mut group_attrs = std::collections::HashMap::new();
    group_attrs.insert("sorrows".to_string(), Value::array(vec![sorrow]));
    group_attrs.insert("worries".to_string(), Value::array(vec![]));
    group_attrs.insert("panic".to_string(), panic);
    group_attrs.insert("message".to_string(), Value::str(group_message.clone()));
    let exception = Value::make_instance(Symbol::intern("X::Comp::Group"), group_attrs);
    PError::fatal_with_exception(group_message, Box::new(exception))
}

pub(super) fn block_gobbled_group_from_expr(expr: &Expr) -> Option<PError> {
    render_expr_brief(expr).map(|what| make_block_gobbled_group_error(&what))
}

/// Build a fatal, structured `X::Obsolete` parse error carrying `old` /
/// `replacement` attributes so that `throws-like` can match them.
pub(in crate::parser) fn make_obsolete_error(
    old: &str,
    replacement: Option<&str>,
    message: &str,
) -> PError {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("old".to_string(), Value::str(old.to_string()));
    if let Some(rep) = replacement {
        attrs.insert("replacement".to_string(), Value::str(rep.to_string()));
    }
    attrs.insert("message".to_string(), Value::str(message.to_string()));
    let exception = Value::make_instance(crate::symbol::Symbol::intern("X::Obsolete"), attrs);
    PError::fatal_with_exception(message.to_string(), Box::new(exception))
}

/// Detect the Perl 5 `for my $x (LIST) { }` foreach pattern: a `my`/`our`/
/// `state` declaration of a single variable immediately followed by a
/// parenthesized list. In Raku the topic variable goes in a pointy block
/// (`for LIST -> $x { }`), so this is X::Syntax::P5.
fn looks_like_p5_foreach(input: &str) -> bool {
    let rest = keyword("my", input)
        .or_else(|| keyword("our", input))
        .or_else(|| keyword("state", input));
    let Some(rest) = rest else { return false };
    let Ok((rest, _)) = ws1(rest) else {
        return false;
    };
    // A single sigil-variable: `$x`, `@x`, `%x`.
    let Some(after_sigil) = rest
        .strip_prefix('$')
        .or_else(|| rest.strip_prefix('@'))
        .or_else(|| rest.strip_prefix('%'))
    else {
        return false;
    };
    let Ok((rest, _name)) = ident(after_sigil) else {
        return false;
    };
    let (rest, _) = ws(rest).unwrap_or((rest, ()));
    rest.starts_with('(')
}

fn p5_foreach_error() -> PError {
    let msg = "This appears to be Perl code".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
    let ex =
        crate::value::Value::make_instance(crate::symbol::Symbol::intern("X::Syntax::P5"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// Build the head statement that binds a pointy-block parameter to the topic.
///
/// A plain (aliasing) parameter uses `:=` so the compiler/VM writes the
/// parameter's final value back to the topic source — `given @a -> @p { @p.push }`
/// aliases `@a`, matching Raku. `is copy` uses `=` (a fresh copy) so nothing is
/// written back. The compiler only treats a `:=`-to-`$_` head as a pointy alias.
fn pointy_topic_bind(pd: &ParamDef) -> Stmt {
    let topic = Expr::Var("_".to_string());
    if pd.traits.iter().any(|t| t == "copy") {
        // `is copy` is a fresh, writable copy with no link to the source. Use a
        // plain assignment (not `:=`, so the compiler does not treat it as a
        // writeback alias). For `@`/`%` params flatten the topic first
        // (`@p = $_.list` / `%p = $_.hash`) so a copy of an array/hash topic
        // does not nest inside a single element (`$_` is an itemized container).
        let expr = if pd.name.starts_with('@') {
            method_call(topic, "list")
        } else if pd.name.starts_with('%') {
            method_call(topic, "hash")
        } else {
            topic
        };
        Stmt::Assign {
            name: pd.name.clone(),
            op: AssignOp::Assign,
            expr,
        }
    } else {
        // Aliasing parameter: `:=` so the compiler/VM writes the parameter's
        // final value back to the topic source.
        Stmt::Assign {
            name: pd.name.clone(),
            op: AssignOp::Bind,
            expr: topic,
        }
    }
}

/// Build a zero-argument method call expression (`$expr.NAME`).
fn method_call(target: Expr, name: &str) -> Expr {
    Expr::MethodCall {
        target: Box::new(target),
        name: Symbol::intern(name),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    }
}

/// Build a `$_ = <expr>` assignment statement for topicalization.
fn topicalize(expr: &Expr) -> Stmt {
    Stmt::Assign {
        name: "_".to_string(),
        op: AssignOp::Assign,
        expr: expr.clone(),
    }
}

mod conditionals;
mod for_loops;
mod for_params;
mod given_when;
mod labeled_loop;
mod loop_repeat;
mod pointy_param;
mod react;
mod while_until;
mod with_stmt;

// Re-exports preserving each function's original visibility (all `pub(super)`).
pub(super) use conditionals::{if_stmt, unless_stmt};
pub(super) use for_loops::{for_stmt, foreach_stmt, hyper_for_stmt, lazy_for_body, race_for_stmt};
pub(super) use for_params::parse_for_params;
pub(super) use given_when::{default_stmt, given_stmt, when_stmt};
pub(super) use labeled_loop::labeled_loop_stmt;
pub(super) use loop_repeat::{loop_stmt, repeat_stmt};
pub(super) use pointy_param::parse_pointy_param;
pub(super) use react::{react_stmt, whenever_stmt};
pub(super) use while_until::{until_stmt, while_stmt};
pub(super) use with_stmt::with_stmt;

// Cross-submodule helpers: originally private `fn`s in the if/unless block that
// are also called from `with_stmt` (for `orwith` elsif-chain lowering). Bumped
// to `pub(crate)` in `conditionals` and re-exported here so `with_stmt` reaches
// them via `use super::*`.
pub(in crate::parser) use conditionals::{lower_if_chain, parse_elsif_chain};
