//! The argument-taking forms of the loop-control words `last` / `next` /
//! `redo`: `next(FOO)`, `next |c`, and v6.e's value form `last VALUE`.
//!
//! Rakudo declares each word as a multi routine. Through v6.d the candidates
//! are `( --> Nil)` and `(Label:D $x --> Nil)`; v6.e adds `last(\x)` and
//! `next(\x)`, which end the loop (or the iteration) and make `x` the
//! iteration's value (#11073). So the parse of an argument list depends on the
//! language version in force:
//!
//! - v6.e, one argument to `last`/`next`: an [`Expr::ControlFlow`] carrying the
//!   value. Whether the value is a `Label` (the labelled form spelled as an
//!   argument) is decided by the opcode at run time.
//! - an argument list whose types are all known at compile time and none of
//!   which can be a `Label` (`last(5)` under v6.d): the compile-time
//!   `X::TypeCheck::Argument` rakudo's optimizer raises.
//! - anything else: a call of the routine, resolved at run time by
//!   `builtins/label.rs` (a `Label` value, or `X::Multi::NoMatch`).

use crate::ast::{ControlFlowKind, Expr};
use crate::parser::expr::{expression, term_expr};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// `last |c` / `next |c` / `redo |c`: the loop-control term applied to a slipped
/// argument list (Rakudo resolves it as `last(|c)`). Returns the slipped term.
pub(in crate::parser) fn control_flow_slip_args(input: &str) -> PResult<'_, Option<Expr>> {
    let (after_ws, _) = ws(input)?;
    if !after_ws.starts_with('|') {
        return Ok((input, None));
    }
    let (rest, slip) = term_expr(after_ws)?;
    Ok((rest, Some(slip)))
}

/// The routine forms of `last` / `next` / `redo`: `next(FOO)` with a `Label`
/// value, `next |c` slipping a capture that may hold one, and v6.e's
/// `last VALUE`. An empty `next()` stays the plain control flow term (the
/// caller's ordinary path), as does every other form.
pub(super) fn loop_control_call_form<'a>(name: &str, input: &'a str) -> PResult<'a, Option<Expr>> {
    if let (rest, Some(slip)) = control_flow_slip_args(input)? {
        return Ok((rest, Some(loop_control_call(name, vec![slip]))));
    }
    let (after_ws, _) = ws(input)?;
    // `last $res` / `last 42`: a listop argument, not a label. Under v6.e a
    // spaced `last (1, 2)` is the same listop applied to one parenthesized
    // term (the value is the list), not a two-argument call.
    if loop_control_listop_arg_start(input, after_ws)
        || (after_ws.len() < input.len()
            && after_ws.starts_with('(')
            && !opens_empty_parens(after_ws)
            && v6e_loop_values())
    {
        let (rest, arg) = expression(after_ws)?;
        let expr = loop_control_with_args(name, vec![arg], after_ws)?;
        return Ok((rest, Some(expr)));
    }
    let Some(after_paren) = after_ws.strip_prefix('(') else {
        return Ok((input, None));
    };
    let (args_start, _) = ws(after_paren)?;
    if args_start.starts_with(')') {
        return Ok((input, None));
    }
    let (rest, args) = crate::parser::primary::parse_call_arg_list(args_start)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = parse_char(rest, ')')?;
    let expr = loop_control_with_args(name, args, after_ws)?;
    Ok((rest, Some(expr)))
}

/// Whether a loop-control keyword is followed, after whitespace, by a term
/// that can only be its argument: a variable, a literal, or a prefix operator
/// glued to its operand (`next -1`, as for any listop). A bare word stays a
/// label (`last OUTER`), and `(` is the parenthesized call form.
pub(in crate::parser) fn loop_control_listop_arg_start(input: &str, after_ws: &str) -> bool {
    after_ws.len() < input.len()
        && (after_ws.starts_with(['$', '@', '%'])
            || crate::parser::term_boundary::starts_with_unambiguous_term(after_ws)
            || starts_with_glued_prefix_op(after_ws)
            || starts_with_v6e_bareword_value(after_ws))
}

/// v6.e's `last Nil` / `next Empty`: a bare word that is not a loop label is
/// the value term. A statement modifier (`last if ...`), a word infix
/// (`last and ...`) and a pair key (`next => 1`) still end the term.
fn starts_with_v6e_bareword_value(input: &str) -> bool {
    if !input.starts_with(crate::parser::helpers::is_raku_identifier_start)
        || crate::parser::primary::ident::predicates::is_stmt_modifier_ahead(input)
        || !v6e_loop_values()
    {
        return false;
    }
    let Ok((rest, word)) = crate::parser::stmt::ident_pub(input) else {
        return false;
    };
    !crate::parser::helpers::is_loop_label_name(&word)
        && !crate::parser::primary::ident::predicates::is_infix_word_op(&word)
        && !rest.trim_start().starts_with("=>")
}

/// `-1`, `+$x`, `!$ok`, `~$s`, `?$v`: a symbolic prefix operator directly
/// followed by its operand (not by whitespace, `=` or `>`, which would make it
/// an infix or an arrow).
fn starts_with_glued_prefix_op(input: &str) -> bool {
    let mut chars = input.chars();
    matches!(chars.next(), Some('-' | '+' | '!' | '~' | '?'))
        && chars
            .next()
            .is_some_and(|c| !c.is_whitespace() && c != '=' && c != '>')
}

fn opens_empty_parens(input: &str) -> bool {
    input
        .strip_prefix('(')
        .and_then(|r| ws(r).ok())
        .is_some_and(|(r, _)| r.starts_with(')'))
}

/// Whether the language version in force has the value-taking `last` /
/// `next` candidates (v6.e and later).
fn v6e_loop_values() -> bool {
    let version = crate::parser::current_language_version();
    version
        .strip_prefix("6.")
        .and_then(|s| s.chars().next())
        .is_some_and(|letter| letter >= 'e')
}

/// The expression the parser builds for the call form `last(ARGS)` /
/// `next(ARGS)`, for the RakuAST lowering of the same call. `None` when the
/// compile-time check refuses the arguments (the parser reports that as an
/// error; the lowering leaves the call to run time).
// Cost: O(a), a = number of arguments.
pub(crate) fn loop_control_expr(name: &str, args: Vec<Expr>) -> Option<Expr> {
    loop_control_with_args(name, args, "").ok()
}

/// Build the node for `name` applied to `args` (`at` is where the argument
/// list starts, for the compile-time diagnostic's position).
fn loop_control_with_args(name: &str, args: Vec<Expr>, at: &str) -> Result<Expr, PError> {
    let kind = match name {
        "last" => Some(ControlFlowKind::Last),
        "next" => Some(ControlFlowKind::Next),
        _ => None,
    };
    if let Some(kind) = kind
        && args.len() == 1
        && v6e_loop_values()
    {
        let value = args.into_iter().next().map(Box::new);
        return Ok(Expr::ControlFlow {
            kind,
            label: None,
            value,
            take_value: false,
        });
    }
    if let Some(types) = static_non_label_arg_types(&args) {
        return Err(never_work_error(name, &types, at));
    }
    Ok(loop_control_call(name, args))
}

/// The type names of `args` when every one is a literal whose type is known
/// at compile time and cannot be a `Label` — the calls rakudo's optimizer
/// refutes against the `( --> Nil)` / `(Label:D $x --> Nil)` candidates.
fn static_non_label_arg_types(args: &[Expr]) -> Option<Vec<String>> {
    args.iter()
        .map(|arg| match arg {
            Expr::Literal(v)
                if matches!(
                    v.view(),
                    ValueView::Int(_)
                        | ValueView::BigInt(_)
                        | ValueView::Num(_)
                        | ValueView::Rat(..)
                        | ValueView::Str(_)
                        | ValueView::Bool(_)
                ) =>
            {
                Some(crate::value::what_type_name(v))
            }
            _ => None,
        })
        .collect()
}

/// Rakudo's compile-time `X::TypeCheck::Argument` for a loop-control call no
/// candidate can bind.
fn never_work_error(name: &str, types: &[String], at: &str) -> PError {
    let mut signatures = String::from("    ( --> Nil)\n    (Label:D $x --> Nil)");
    if name != "redo" && v6e_loop_values() {
        signatures.push_str("\n    (\\x --> Nil)");
    }
    let message = format!(
        "Calling {name}({}) will never work with any of these multi signatures:\n{signatures}",
        types.join(", ")
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("objname".to_string(), Value::str(name.to_string()));
    attrs.insert(
        "arguments".to_string(),
        Value::array(types.iter().map(|t| Value::str(t.clone())).collect()),
    );
    let exception = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
    PError::fatal_with_exception_at(message, Box::new(exception), at)
}

fn loop_control_call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern(name),
        args,
        listop: false,
    }
}
