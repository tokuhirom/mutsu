use super::*;

/// Parse `return` statement.
pub(crate) fn return_stmt(input: &str) -> PResult<'_, Stmt> {
    // If "return" is a declared term symbol (e.g. sigilless variable \return),
    // don't parse it as the return keyword.
    if keyword("return", input).is_some()
        && match_user_declared_term_symbol(input).is_some_and(|(name, _, _)| name == "return")
    {
        return Err(PError::expected("return statement"));
    }
    let rest = keyword("return", input).ok_or_else(|| PError::expected("return statement"))?;
    let (rest, _) = ws(rest)?;
    if is_stmt_modifier_keyword(rest) {
        return parse_statement_modifier(rest, Stmt::Return(Expr::Literal(Value::Nil)));
    }
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Return(Expr::Literal(Value::Nil))));
    }
    let (rest, expr) = parse_comma_or_expr(rest).map_err(|err| PError {
        messages: merge_expected_messages("expected return value expression", &err.messages),
        remaining_len: err.remaining_len.or(Some(rest.len())),
        exception: None,
    })?;
    let stmt = Stmt::Return(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse `last` / `next` / `redo`.
pub(crate) fn last_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("last", input).ok_or_else(|| PError::expected("last statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: last LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase() || c == '_')
        && let Ok((r, label)) = ident(rest)
        && is_loop_label_name(&label)
    {
        return parse_statement_modifier(r, Stmt::Last(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Last(None))
}

pub(crate) fn next_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("next", input).ok_or_else(|| PError::expected("next statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: next LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase() || c == '_')
        && let Ok((r, label)) = ident(rest)
        && is_loop_label_name(&label)
    {
        return parse_statement_modifier(r, Stmt::Next(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Next(None))
}

pub(crate) fn redo_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("redo", input).ok_or_else(|| PError::expected("redo statement"))?;
    let (rest, _) = ws(rest)?;
    // Check for label: redo LABEL
    if rest.starts_with(|c: char| c.is_ascii_uppercase() || c == '_')
        && let Ok((r, label)) = ident(rest)
        && is_loop_label_name(&label)
    {
        return parse_statement_modifier(r, Stmt::Redo(Some(label)));
    }
    parse_statement_modifier(rest, Stmt::Redo(None))
}

/// Parse `goto` statement.
pub(crate) fn goto_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("goto", input).ok_or_else(|| PError::expected("goto statement"))?;
    let (rest, _) = ws1(rest)?;
    // Bare identifier labels are treated as label names, not variable lookups.
    if let Ok((r, label)) = ident(rest) {
        return parse_statement_modifier(r, Stmt::Goto(Expr::Literal(Value::str(label))));
    }
    let (rest, expr) = expression(rest)?;
    parse_statement_modifier(rest, Stmt::Goto(expr))
}

/// Parse `die` statement.
pub(crate) fn die_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, is_fail) = if let Some(r) = keyword("fail", input) {
        (r, true)
    } else {
        let r = keyword("die", input).ok_or_else(|| PError::expected("die/fail statement"))?;
        (r, false)
    };
    let (rest, _) = ws(rest)?;
    // `die`/`fail` with no argument: followed by `;`, end, `}`, or a statement modifier
    let no_arg = rest.starts_with(';')
        || rest.is_empty()
        || rest.starts_with('}')
        || is_stmt_modifier_keyword(rest);
    if no_arg {
        let (rest, _) = opt_char(rest, ';');
        // `die`/`fail` with no argument should reuse current `$!` when present.
        // A later runtime fallback handles Nil -> default text.
        let stmt = if is_fail {
            Stmt::Fail(Expr::Var("!".to_string()))
        } else {
            Stmt::Die(Expr::Var("!".to_string()))
        };
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, expr) = expression(rest)?;
    let stmt = if is_fail {
        Stmt::Fail(expr)
    } else {
        Stmt::Die(expr)
    };
    parse_statement_modifier(rest, stmt)
}

/// Parse `take` or `take-rw` statement.
pub(crate) fn take_stmt(input: &str) -> PResult<'_, Stmt> {
    // Try `take-rw` first (longer match wins)
    if let Some(rest) = keyword("take-rw", input)
        && let Ok((rest, _)) = ws1(rest)
    {
        let (rest, expr) = parse_comma_or_expr(rest)?;
        // `take-rw`: is_rw=true. The compiler captures the source container so the
        // gathered value keeps container identity (`=:=`) with the original lvalue.
        return parse_statement_modifier(rest, Stmt::Take(expr, true));
    }
    let rest = keyword("take", input).ok_or_else(|| PError::expected("take statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, expr) = parse_comma_or_expr(rest)?;
    parse_statement_modifier(rest, Stmt::Take(expr, false))
}

/// Parse CATCH/CONTROL blocks.
pub(crate) fn catch_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CATCH", input).ok_or_else(|| PError::expected("CATCH block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Catch(body)))
}

pub(crate) fn control_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CONTROL", input).ok_or_else(|| PError::expected("CONTROL block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Control(body)))
}

/// Parse phasers: BEGIN, END, etc.
pub(crate) fn phaser_stmt(input: &str) -> PResult<'_, Stmt> {
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
    } else if let Some(r) = keyword("TEMP", input) {
        // `TEMP { ... }` is the temporization phaser. It is NYI in Rakudo: the
        // block is parsed but its body is never executed (and no restoration is
        // performed), so `TEMP {{ $x = $y }}` is effectively a no-op. Match that
        // behavior — consume the block and emit nothing — rather than running the
        // block inline (which `BareWord("TEMP")` + a bare `{ ... }` block would
        // do, wrongly executing the body).
        let (r, _) = ws(r)?;
        let (r, _body) = if r.starts_with('{') {
            block(r)?
        } else {
            let (r, s) = statement(r)?;
            (r, vec![s])
        };
        return parse_statement_modifier(r, Stmt::Block(Vec::new()));
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
    // `BEGIN $*RAKU.version` (and `$*PERL.version`) is a compile-time constant
    // equal to the language version of the current compilation unit. The parser
    // knows the active `use v6.X` version, so fold it to a Version literal here.
    // BEGIN of a pure constant has no side effects, so the phaser wrapper is
    // dropped — the value flows through directly (e.g. as a sub's return value).
    if kind == PhaserKind::Begin
        && body.len() == 1
        && let Stmt::Expr(e) = &body[0]
        && let Some(v) = fold_compile_time_version(e)
    {
        return Ok((rest, Stmt::Expr(Expr::Literal(v))));
    }
    Ok((rest, Stmt::Phaser { kind, body }))
}

/// Fold `$*RAKU.version` / `$*PERL.version` to a Version literal of the current
/// compilation unit's language version. These reflect the `use v6.X` pragma in
/// effect and are constant per compunit.
fn fold_compile_time_version(expr: &Expr) -> Option<Value> {
    let Expr::MethodCall {
        target, name, args, ..
    } = expr
    else {
        return None;
    };
    if !args.is_empty() || name.resolve() != "version" {
        return None;
    }
    let Expr::Var(v) = &**target else {
        return None;
    };
    if v != "*RAKU" && v != "*PERL" {
        return None;
    }
    let ver = current_language_version();
    let parts: Vec<crate::value::VersionPart> = ver
        .split('.')
        .map(|s| {
            if let Ok(n) = s.parse::<i64>() {
                crate::value::VersionPart::Num(n)
            } else {
                crate::value::VersionPart::Str(s.to_string())
            }
        })
        .collect();
    Some(Value::Version {
        parts,
        plus: false,
        minus: false,
    })
}

/// Parse `subtest` declaration.
pub(crate) fn subtest_stmt(input: &str) -> PResult<'_, Stmt> {
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
/// If the block is followed by `.method(...)`, treat it as a block expression
/// with postfix operators (e.g. `{ $^a }.assuming(123)()`).
pub(crate) fn block_stmt(input: &str) -> PResult<'_, Stmt> {
    // Try to parse as a hash expression first (e.g. `{:a(4)}`, `{a => 1}`)
    if let Ok((rest, hash_expr)) = crate::parser::primary::misc::block_or_hash_expr(input)
        && matches!(hash_expr, Expr::Hash(_))
    {
        // A statement-leading hash literal may carry postfix operators
        // (`{a => 1}.keys`, `{a => 1}<b>`, `{a => 1}.map(...)`). Without this the
        // hash terminated the statement and a following `.keys` was mis-parsed as
        // a new `$_.keys` statement. `postfix_expr_continue` is a no-op when no
        // postfix follows, so a bare `{a => 1}` statement is unchanged.
        let (rest, expr) = crate::parser::expr::postfix_expr_continue(rest, hash_expr)?;
        return parse_statement_modifier(rest, Stmt::Expr(expr));
    }
    let (rest, body) = block(input)?;
    let (r_ws, _) = ws(rest)?;
    // Check for postfix operators on the block:
    // - `.method(...)` after optional whitespace
    // - `()` immediately after the block (no newline between `}` and `(`)
    let has_postfix_dot = r_ws.starts_with('.') && !r_ws.starts_with("..");
    let has_immediate_call = rest.starts_with('(')
        || (rest.starts_with([' ', '\t'])
            && rest.trim_start_matches([' ', '\t']).starts_with('(')
            && !rest.contains('\n'));
    if has_postfix_dot || has_immediate_call {
        // Use AnonSub so the block compiles as a closure value, not inline code.
        // Handles `{ ... }.method(...)` and `{ ... }()` (immediate closure call).
        let block_expr = Expr::AnonSub {
            body,
            is_rw: false,
            is_block: true,
        };
        let (rest, expr) = crate::parser::expr::postfix_expr_continue(rest, block_expr)?;
        return parse_statement_modifier(rest, Stmt::Expr(expr));
    }
    parse_statement_modifier(rest, Stmt::Block(body))
}

/// Known function names that get Stmt::Call treatment at statement level.
/// Test/Test::Util functions are NOT listed here — they are registered dynamically
/// via `register_module_exports()` when `use Test` / `use Test::Util` is parsed.
pub(crate) const KNOWN_CALLS: &[&str] = &[
    "dd", "exit", "proceed", "succeed", "done", "emit", "push", "pop", "shift", "unshift",
    "append", "prepend", "elems", "chars", "defined", "warn", "leave", "EVAL", "EVALFILE",
];

/// Check if a name is a known statement-level function call.
pub(crate) fn is_known_call(name: &str) -> bool {
    // Keywords must never be treated as function calls, even if a module
    // accidentally registers them as imported functions (e.g. via regex
    // fallback scanning of string literals in source).
    if crate::parser::primary::ident::is_keyword(name) {
        return false;
    }
    KNOWN_CALLS.contains(&name) || is_imported_function(name)
}

/// Return true when parsing this known call as a standalone statement would
/// truncate a larger expression (e.g. `defined($x) ?? 1 !! 2`).
fn known_call_is_expression_prefix(input: &str, rest_after_call: &str) -> bool {
    let Ok((rest_after_call, _)) = ws(rest_after_call) else {
        return false;
    };
    if rest_after_call.is_empty()
        || rest_after_call.starts_with(';')
        || rest_after_call.starts_with('}')
    {
        return false;
    }
    if is_stmt_modifier_keyword(rest_after_call) {
        return false;
    }
    if let Ok((expr_rest, _)) = expression(input) {
        return expr_rest.len() < rest_after_call.len();
    }
    false
}

/// Parse a known function call as statement.
pub(crate) fn known_call_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    if !is_known_call(&name) {
        return Err(PError::expected("known function call"));
    }
    let had_ws = rest.starts_with(' ') || rest.starts_with('\t') || rest.starts_with('\n');
    let (rest, _) = ws(rest)?;
    if name == "int" && had_ws && !is_user_declared_sub("int") {
        return Err(PError::expected("known function call"));
    }

    // Special handling for proceed/succeed with no args
    if name == "proceed" {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Proceed));
    }
    if name == "succeed" {
        // `succeed EXPR` should call the builtin function with payload;
        // only bare `succeed` maps to the control-flow statement form.
        if rest.is_empty() || rest.starts_with(';') || is_stmt_modifier_keyword(rest) {
            let (rest, _) = opt_char(rest, ';');
            return Ok((rest, Stmt::Succeed));
        }
    }
    if name == "done" {
        // `done()` is the explicit call form (it takes no payload). Consume the
        // empty argument list so a trailing statement modifier applies to the
        // whole `done`, not to a stray `()` term: `done() if $_ == 3` must parse
        // as `(done()) if $_ == 3`, not `done; () if $_ == 3`.
        let rest = if !had_ws && rest.starts_with('(') {
            let after = skip_balanced_parens(rest);
            let (after, _) = ws(after)?;
            after
        } else {
            rest
        };
        // Support statement modifiers like `done if $v >= 2`
        return parse_statement_modifier(rest, Stmt::ReactDone);
    }

    // A parenthesised call (`foo(...)`) whose result is immediately followed by a
    // postfix method chain or subscript (`foo(...).bar`, `foo(...)[0]`) is an
    // *expression*, not a statement-level call: parsing it here would drop the
    // postfix (e.g. `make-temp-file(:content($_)).open` losing `.open`). Bail so
    // the expression-statement parser handles the whole postfix chain.
    let is_paren_call = !had_ws && rest.starts_with('(');
    if is_paren_call {
        let after_call = skip_balanced_parens(rest);
        let after_call = after_call.trim_start();
        if (after_call.starts_with('.') && !after_call.starts_with(".."))
            || after_call.starts_with('[')
        {
            return Err(PError::expected("known function call"));
        }
    }

    // In Raku, `foo(args)` (no space) = paren call, but `foo (expr)` (space) = listop call.
    // When there was whitespace before `(`, treat `(` as expression grouping, not call parens.
    let (rest, args) = if had_ws {
        parse_stmt_call_args_no_paren(rest).map_err(|err| PError {
            messages: merge_expected_messages("expected known call arguments", &err.messages),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?
    } else {
        parse_stmt_call_args(rest).map_err(|err| PError {
            messages: merge_expected_messages("expected known call arguments", &err.messages),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?
    };
    let mut args = args;
    // Test assertion calls (e.g. `is [1], [1], 'desc'`) can start with bracketed
    // arguments and be mistaken for an expression prefix. Keep them as calls.
    if !is_test_assertion_callable(&name)
        && KNOWN_CALLS.contains(&name.as_str())
        && known_call_is_expression_prefix(input, rest)
    {
        return Err(PError::expected("known function call"));
    }
    if is_test_assertion_callable(&name) {
        args.push(crate::ast::CallArg::Named {
            name: "__mutsu_test_callsite_line".to_string(),
            value: Some(Expr::Literal(Value::Int(
                crate::parser::primary::current_line_number(input),
            ))),
        });
    }
    let stmt = Stmt::Call {
        name: Symbol::intern(&name),
        args,
    };
    parse_statement_modifier(rest, stmt)
}
