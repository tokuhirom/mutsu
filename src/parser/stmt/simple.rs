use std::cell::RefCell;
use std::collections::HashSet;

use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, merge_expected_messages, opt_char, parse_char};

use crate::ast::{AssignOp, Expr, PhaserKind, Stmt};
use crate::value::Value;

thread_local! {
    static USER_DECLARED_SUBS: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    static IMPORTED_FUNCTIONS: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
}

/// Register a user-declared sub name so it can be recognized as a call without parens.
pub(in crate::parser) fn register_user_sub(name: &str) {
    USER_DECLARED_SUBS.with(|s| {
        s.borrow_mut().insert(name.to_string());
    });
}

/// Reset declared sub names (called at parse start).
pub(in crate::parser) fn reset_user_subs() {
    USER_DECLARED_SUBS.with(|s| {
        s.borrow_mut().clear();
    });
    IMPORTED_FUNCTIONS.with(|s| {
        s.borrow_mut().clear();
    });
}

pub(in crate::parser) fn is_user_declared_sub(name: &str) -> bool {
    USER_DECLARED_SUBS.with(|s| s.borrow().contains(name))
}

/// Check if a function name was registered via `use` module import.
pub(crate) fn is_imported_function(name: &str) -> bool {
    IMPORTED_FUNCTIONS.with(|s| s.borrow().contains(name))
}

/// Register exported function names for a module (called when parsing `use` statements).
pub(in crate::parser) fn register_module_exports(module: &str) {
    let exports: &[&str] = match module {
        "Test" => TEST_EXPORTS,
        "Test::Util" => TEST_UTIL_EXPORTS,
        _ => return,
    };
    IMPORTED_FUNCTIONS.with(|s| {
        let mut set = s.borrow_mut();
        for name in exports {
            set.insert((*name).to_string());
        }
    });
}

/// Functions exported by `use Test`.
const TEST_EXPORTS: &[&str] = &[
    "ok",
    "nok",
    "is",
    "isnt",
    "is-deeply",
    "is-approx",
    "cmp-ok",
    "like",
    "unlike",
    "isa-ok",
    "does-ok",
    "can-ok",
    "lives-ok",
    "dies-ok",
    "eval-lives-ok",
    "eval-dies-ok",
    "throws-like",
    "fails-like",
    "pass",
    "flunk",
    "skip",
    "skip-rest",
    "todo",
    "diag",
    "plan",
    "done-testing",
    "bail-out",
    "subtest",
    "use-ok",
    "force_todo",
    "force-todo",
    "tap-ok",
];

/// Functions exported by `use Test::Util`.
const TEST_UTIL_EXPORTS: &[&str] = &[
    "is_run",
    "get_out",
    "warns-like",
    "doesn't-warn",
    "is-eqv",
    "group-of",
];

use super::{
    block, ident, is_stmt_modifier_keyword, keyword, parse_comma_or_expr, parse_statement_modifier,
    parse_stmt_call_args, parse_stmt_call_args_no_paren, statement,
};

/// Check if `say`/`print`/`put` is used bare (no arguments) — this is a compile error in Raku.
fn check_bare_io_func<'a>(name: &str, rest: &'a str) -> PResult<'a, ()> {
    let trimmed = rest.trim_start();
    if trimmed.is_empty() || trimmed.starts_with(';') || trimmed.starts_with('}') {
        return Err(PError::fatal(format!(
            "X::Comp: Unsupported use of bare \"{}\". \
             In Raku please use: .{} if you meant to call it as a method on $_, \
             or use an explicit invocant or argument, \
             or use &{} to refer to the function as a noun.",
            name, name, name
        )));
    }
    Ok((rest, ()))
}

/// Check if `say`/`print`/`put` is followed by `for`/`while`/`until` — X::Obsolete error.
fn check_io_func_followed_by_loop<'a>(name: &str, rest_after_ws: &'a str) -> PResult<'a, ()> {
    for kw in &["for", "while", "until"] {
        if let Some(r) = keyword(kw, rest_after_ws) {
            let next_char = r.chars().next();
            if next_char.is_none()
                || next_char == Some(' ')
                || next_char == Some('\t')
                || next_char == Some('\n')
            {
                return Err(PError::fatal(format!(
                    "X::Obsolete: Unsupported use of bare \"{}\". \
                     In Raku please use: .{} if you meant to call it as a method on $_, \
                     or use an explicit invocant or argument, \
                     or use &{} to refer to the function as a noun.",
                    name, name, name
                )));
            }
        }
    }
    Ok((rest_after_ws, ()))
}

/// Parse a `say` statement.
pub(super) fn say_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("say", input).ok_or_else(|| PError::expected("say statement"))?;
    check_bare_io_func("say", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("say", rest)?;
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Say(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `print` statement.
pub(super) fn print_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("print", input).ok_or_else(|| PError::expected("print statement"))?;
    check_bare_io_func("print", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("print", rest)?;
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Print(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `put` statement.
pub(super) fn put_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("put", input).ok_or_else(|| PError::expected("put statement"))?;
    check_bare_io_func("put", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("put", rest)?;
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Say(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `note` statement.
pub(super) fn note_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("note", input).ok_or_else(|| PError::expected("note statement"))?;
    // `note` with no arguments is valid (prints "Noted\n")
    if let Ok((rest2, _)) = ws1(rest)
        && let Ok((rest3, args)) = parse_expr_list(rest2)
    {
        return parse_statement_modifier(rest3, Stmt::Note(args));
    }
    // Bare `note` with no args
    parse_statement_modifier(rest, Stmt::Note(vec![]))
}

/// Parse a comma-separated expression list.
pub(super) fn parse_expr_list(input: &str) -> PResult<'_, Vec<Expr>> {
    let (input, first) = expression(input)?;
    let mut items = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            return Ok((r, items));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        // Check for end of list
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            return Ok((r, items));
        }
        // Check for statement modifier keywords
        if is_stmt_modifier_keyword(r) {
            return Ok((r, items));
        }
        let (r, next) = expression(r)?;
        items.push(next);
        rest = r;
    }
}

/// Parse `return` statement.
pub(super) fn return_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("return", input).ok_or_else(|| PError::expected("return statement"))?;
    let (rest, _) = ws(rest)?;
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Return(Expr::Literal(Value::Nil))));
    }
    let (rest, expr) = expression(rest)?;
    let stmt = Stmt::Return(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse `last` / `next` / `redo`.
pub(super) fn last_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("last", input).ok_or_else(|| PError::expected("last statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: last LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase())
        && let Ok((r, label)) = ident(rest)
        && label.chars().all(|c| c.is_ascii_uppercase() || c == '_')
    {
        return parse_statement_modifier(r, Stmt::Last(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Last(None))
}

pub(super) fn next_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("next", input).ok_or_else(|| PError::expected("next statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: next LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase())
        && let Ok((r, label)) = ident(rest)
        && label.chars().all(|c| c.is_ascii_uppercase() || c == '_')
    {
        return parse_statement_modifier(r, Stmt::Next(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Next(None))
}

pub(super) fn redo_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("redo", input).ok_or_else(|| PError::expected("redo statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: redo LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase())
        && let Ok((r, label)) = ident(rest)
        && label.chars().all(|c| c.is_ascii_uppercase() || c == '_')
    {
        return parse_statement_modifier(r, Stmt::Redo(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Redo(None))
}

/// Parse `die` statement.
pub(super) fn die_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, is_fail) = if let Some(r) = keyword("fail", input) {
        (r, true)
    } else {
        let r = keyword("die", input).ok_or_else(|| PError::expected("die/fail statement"))?;
        (r, false)
    };
    let (rest, _) = ws(rest)?;
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        let (rest, _) = opt_char(rest, ';');
        let default_msg = if is_fail { "Failed" } else { "Died" };
        let stmt = if is_fail {
            Stmt::Fail(Expr::Literal(Value::Str(default_msg.to_string())))
        } else {
            Stmt::Die(Expr::Literal(Value::Str(default_msg.to_string())))
        };
        return Ok((rest, stmt));
    }
    let (rest, expr) = expression(rest)?;
    let stmt = if is_fail {
        Stmt::Fail(expr)
    } else {
        Stmt::Die(expr)
    };
    parse_statement_modifier(rest, stmt)
}

/// Parse `take` statement.
pub(super) fn take_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("take", input).ok_or_else(|| PError::expected("take statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, expr) = expression(rest)?;
    parse_statement_modifier(rest, Stmt::Take(expr))
}

/// Parse CATCH/CONTROL blocks.
pub(super) fn catch_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CATCH", input).ok_or_else(|| PError::expected("CATCH block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Catch(body)))
}

pub(super) fn control_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CONTROL", input).ok_or_else(|| PError::expected("CONTROL block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Control(body)))
}

/// Parse phasers: BEGIN, END, etc.
pub(super) fn phaser_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, kind) = if let Some(r) = keyword("BEGIN", input) {
        (r, PhaserKind::Begin)
    } else if let Some(r) = keyword("CHECK", input) {
        (r, PhaserKind::Check)
    } else if let Some(r) = keyword("INIT", input) {
        (r, PhaserKind::Init)
    } else if let Some(r) = keyword("END", input) {
        (r, PhaserKind::End)
    } else if let Some(r) = keyword("ENTER", input) {
        (r, PhaserKind::Enter)
    } else if let Some(r) = keyword("LEAVE", input) {
        (r, PhaserKind::Leave)
    } else if let Some(r) = keyword("KEEP", input) {
        (r, PhaserKind::Keep)
    } else if let Some(r) = keyword("UNDO", input) {
        (r, PhaserKind::Undo)
    } else if let Some(r) = keyword("FIRST", input) {
        (r, PhaserKind::First)
    } else if let Some(r) = keyword("NEXT", input) {
        (r, PhaserKind::Next)
    } else if let Some(r) = keyword("LAST", input) {
        (r, PhaserKind::Last)
    } else if let Some(r) = keyword("PRE", input) {
        (r, PhaserKind::Pre)
    } else if let Some(r) = keyword("POST", input) {
        (r, PhaserKind::Post)
    } else if let Some(r) = keyword("QUIT", input) {
        (r, PhaserKind::Quit)
    } else if let Some(r) = keyword("CLOSE", input) {
        (r, PhaserKind::Close)
    } else {
        return Err(PError::expected("phaser keyword"));
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = if rest.starts_with('{') {
        block(rest)?
    } else {
        let (r, s) = statement(rest)?;
        (r, vec![s])
    };
    Ok((rest, Stmt::Phaser { kind, body }))
}

/// Parse `subtest` declaration.
pub(super) fn subtest_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subtest", input).ok_or_else(|| PError::expected("subtest statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    // Expect =>
    if !rest.starts_with("=>") {
        return Err(PError::expected("'=>' in subtest"));
    }
    let rest = &rest[2..];
    let (rest, _) = ws(rest)?;
    // Optional 'sub' keyword
    let rest = if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws(r)?;
        r
    } else {
        rest
    };
    let (rest, body) = block(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Subtest { name, body }))
}

/// Parse a block statement: { ... }
pub(super) fn block_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, body) = block(input)?;
    Ok((rest, Stmt::Block(body)))
}

/// Parse an expression statement (fallback).
pub(super) fn expr_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, expr) = expression(input).map_err(|err| PError {
        messages: merge_expected_messages("expected expression statement", &err.messages),
        remaining_len: err.remaining_len.or(Some(input.len())),
    })?;

    // Check for index assignment after expression
    let (rest, _) = ws(rest)?;
    if matches!(expr, Expr::Index { .. }) && rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after index assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        if let Expr::Index { target, index } = expr {
            let stmt = Stmt::Expr(Expr::IndexAssign {
                target,
                index,
                value: Box::new(value),
            });
            return parse_statement_modifier(rest, stmt);
        }
    }

    // Check for assignment after parenthesized assign expression: ($x = $y) = 5
    if let Expr::AssignExpr { ref name, .. } = expr {
        if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
            let var_name = name.clone();
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after '='",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?;
            let stmts = vec![
                Stmt::Expr(expr),
                Stmt::Assign {
                    name: var_name,
                    expr: rhs,
                    op: AssignOp::Assign,
                },
            ];
            return parse_statement_modifier(r, Stmt::Block(stmts));
        }
        if let Some((stripped, op)) = super::assign::parse_compound_assign_op(rest) {
            let var_name = name.clone();
            let (r, _) = ws(stripped)?;
            let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after compound assignment",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?;
            let var_expr = Expr::Var(var_name.clone());
            let stmts = vec![
                Stmt::Expr(expr),
                Stmt::Assign {
                    name: var_name,
                    expr: Expr::Binary {
                        left: Box::new(var_expr),
                        op: op.token_kind(),
                        right: Box::new(rhs),
                    },
                    op: AssignOp::Assign,
                },
            ];
            return parse_statement_modifier(r, Stmt::Block(stmts));
        }
    }

    // Check for comma-separated expressions (e.g., "1,2, until $++")
    // The statement modifier applies only to the last expression
    if rest.starts_with(',') && !rest.starts_with(",,") {
        let mut exprs = vec![expr];
        let mut r = rest;

        // Collect comma-separated expressions
        while r.starts_with(',') && !r.starts_with(",,") {
            let r2 = &r[1..];
            let (r2, _) = ws(r2)?;

            // Check if we hit a statement modifier - if so, stop parsing exprs
            if is_stmt_modifier_keyword(r2) {
                r = r2;
                break;
            }

            // Stop at semicolon, closing brace, or end of input
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                r = r2;
                break;
            }

            let (r2, next_expr) = expression(r2)?;
            exprs.push(next_expr);
            let (r2, _) = ws(r2)?;
            r = r2;
        }

        // Convert all but last expr to Stmt::Expr
        let mut stmts = Vec::new();
        let last_expr = exprs.pop().unwrap();
        for e in exprs {
            stmts.push(Stmt::Expr(e));
        }

        // Apply statement modifier to the last expression
        let last_stmt = Stmt::Expr(last_expr);
        let (r, last_stmt_with_modifier) = parse_statement_modifier(r, last_stmt)?;
        stmts.push(last_stmt_with_modifier);

        // Return as a block
        return Ok((r, Stmt::Block(stmts)));
    }

    let stmt = Stmt::Expr(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse `let` statement: `let $var = expr`, `let $var`, `let @arr[idx] = expr`.
pub(super) fn let_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("let", input).ok_or_else(|| PError::expected("let statement"))?;
    let (rest, _) = ws1(rest)?;
    // Parse sigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after let"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after let"));
    }
    let rest_after_sigil = &rest[1..];
    let (rest, var_name) = super::ident(rest_after_sigil)?;
    // Env key: scalars strip $, arrays/hashes keep sigil
    let full_name = if sigil == '$' {
        var_name.clone()
    } else {
        format!("{}{}", sigil, var_name)
    };
    let (rest, _) = ws(rest)?;

    // Check for index: @arr[idx]
    if let Some(idx_rest) = rest.strip_prefix('[') {
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, idx_expr) = expression(idx_rest)?;
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, _) = parse_char(idx_rest, ']')?;
        let (idx_rest, _) = ws(idx_rest)?;
        if idx_rest.starts_with('=') && !idx_rest.starts_with("==") {
            let val_rest = &idx_rest[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: Some(Box::new(idx_expr)),
                    value: Some(Box::new(val_expr)),
                },
            );
        }
        return parse_statement_modifier(
            idx_rest,
            Stmt::Let {
                name: full_name,
                index: Some(Box::new(idx_expr)),
                value: None,
            },
        );
    }

    // Check for assignment: let $var = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
            },
        );
    }

    // Bare let: let $var / let @arr / let %hash
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
        },
    )
}

/// Parse `temp` statement — same semantics as `let` (save/restore at scope exit).
pub(super) fn temp_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("temp", input).ok_or_else(|| PError::expected("temp statement"))?;
    let (rest, _) = ws1(rest)?;
    // Parse sigil + optional twigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after temp"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after temp"));
    }
    let after_sigil = &rest[1..];
    // Handle twigils: $*CWD, $?FILE, etc.
    let (after_twigil, twigil) = if after_sigil.starts_with('*')
        || after_sigil.starts_with('?')
        || after_sigil.starts_with('!')
    {
        (&after_sigil[1..], &after_sigil[..1])
    } else {
        (after_sigil, "")
    };
    let (rest, var_name) = super::ident(after_twigil)?;
    // Build full env key including twigil
    let full_name = if sigil == '$' {
        if twigil.is_empty() {
            var_name.clone()
        } else {
            format!("{}{}", twigil, var_name)
        }
    } else {
        format!("{}{}{}", sigil, twigil, var_name)
    };
    let (rest, _) = ws(rest)?;
    // Check for assignment: temp $*CWD = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
            },
        );
    }
    // Bare temp: temp $var
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
        },
    )
}

/// Known function names that get Stmt::Call treatment at statement level.
/// Test/Test::Util functions are NOT listed here — they are registered dynamically
/// via `register_module_exports()` when `use Test` / `use Test::Util` is parsed.
pub(super) const KNOWN_CALLS: &[&str] = &[
    "dd", "exit", "proceed", "succeed", "push", "pop", "shift", "unshift", "append", "prepend",
    "elems", "chars", "defined", "warn", "EVAL", "EVALFILE", "substr",
];

/// Check if a name is a known statement-level function call.
pub(super) fn is_known_call(name: &str) -> bool {
    KNOWN_CALLS.contains(&name) || is_imported_function(name)
}

/// Parse a known function call as statement.
pub(super) fn known_call_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    if !is_known_call(&name) {
        return Err(PError::expected("known function call"));
    }
    let had_ws = rest.starts_with(' ') || rest.starts_with('\t') || rest.starts_with('\n');
    let (rest, _) = ws(rest)?;

    // Special handling for proceed/succeed with no args
    if name == "proceed" {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Proceed));
    }
    if name == "succeed" {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Succeed));
    }

    // In Raku, `foo(args)` (no space) = paren call, but `foo (expr)` (space) = listop call.
    // When there was whitespace before `(`, treat `(` as expression grouping, not call parens.
    let (rest, args) = if had_ws {
        parse_stmt_call_args_no_paren(rest).map_err(|err| PError {
            messages: merge_expected_messages("expected known call arguments", &err.messages),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?
    } else {
        parse_stmt_call_args(rest).map_err(|err| PError {
            messages: merge_expected_messages("expected known call arguments", &err.messages),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?
    };
    let stmt = Stmt::Call { name, args };
    parse_statement_modifier(rest, stmt)
}
