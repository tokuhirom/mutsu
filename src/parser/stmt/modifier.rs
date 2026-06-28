use super::super::expr::{expression, expression_no_sequence};
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, merge_expected_messages};

use crate::ast::{CallArg, Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::super::helpers::is_raku_identifier_start;
use super::{keyword, parse_comma_or_expr};

/// After parsing a postfix modifier condition, check if the remaining input
/// starts on a new line with a bare word that is not a statement modifier.
/// This detects "two terms in a row across lines" errors like:
///   42 if 23
///   is 50; 1
/// where `is` on the next line is confused with a continuation.
fn check_two_terms_across_lines(r: &str) -> Result<(), PError> {
    // Only check if there's content after the condition
    if r.is_empty() || r.starts_with(';') || r.starts_with('}') {
        return Ok(());
    }
    // Check if whitespace before remaining contains a newline
    let trimmed = r.trim_start();
    let gap = &r[..r.len() - trimmed.len()];
    if !gap.contains('\n') {
        return Ok(());
    }
    // If the next token after the newline is a bare word that's not a
    // statement modifier keyword, it's "two terms in a row across lines"
    if trimmed.is_empty() || trimmed.starts_with(';') || trimmed.starts_with('}') {
        return Ok(());
    }
    if is_stmt_modifier_keyword(trimmed) {
        return Ok(());
    }
    let first_ch = trimmed.chars().next().unwrap_or('\0');
    if is_raku_identifier_start(first_ch) {
        return Err(PError::fatal(
            "Confused. Two terms in a row across lines (missing semicolon or comma?)".to_string(),
        ));
    }
    Ok(())
}

/// A `my`/`our` declaration carrying a conditional statement modifier
/// (`my $x = 5 if COND`, `my $x unless COND`) keeps its *declaration* lexically
/// unconditional — in Raku declarations take effect at compile time regardless
/// of the runtime modifier; only the *initializer* is gated. So split such a
/// declaration into an always-run declaration plus a conditional assignment:
///
///   `my $x = INIT if COND`  ->  `my $x; ($x = INIT) if COND`
///   `my $x if COND`         ->  `my $x`            (no init to gate)
///
/// `effective_cond` is the condition the surrounding modifier would test (already
/// negated for `unless`). Returns `None` for anything that is not a hoistable
/// scalar/array/hash `my`/`our` declaration (e.g. `state`, routines), leaving the
/// generic modifier wrapping in place.
fn try_split_decl_modifier(stmt: &Stmt, effective_cond: &Expr) -> Option<Stmt> {
    let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        is_export,
        export_tags,
        custom_traits,
        where_constraint,
    } = stmt
    else {
        return None;
    };
    // `state` has once-only initialization semantics that this split would
    // break, so leave it to the generic modifier wrapping.
    if *is_state {
        return None;
    }
    // Only sigil'd variables are hoistable here; a sigilless `\x` or other form
    // is left untouched.
    let is_array = name.starts_with('@');
    let is_hash = name.starts_with('%');
    let has_init = custom_traits.iter().any(|(n, _)| n == "__has_initializer");
    let decl = Stmt::VarDecl {
        name: name.clone(),
        expr: super::decl::default_decl_expr(is_array, is_hash, None, type_constraint.as_deref()),
        type_constraint: type_constraint.clone(),
        is_state: false,
        is_our: *is_our,
        is_dynamic: *is_dynamic,
        is_export: *is_export,
        export_tags: export_tags.clone(),
        custom_traits: custom_traits
            .iter()
            .filter(|(n, _)| n != "__has_initializer")
            .cloned()
            .collect(),
        where_constraint: where_constraint.clone(),
    };
    if !has_init {
        // No initializer to gate: the declaration is simply unconditional.
        return Some(decl);
    }
    let init = Stmt::If {
        cond: effective_cond.clone(),
        then_branch: vec![Stmt::Assign {
            name: name.clone(),
            expr: expr.clone(),
            op: crate::ast::AssignOp::Assign,
        }],
        else_branch: Vec::new(),
        binding_var: None,
    };
    Some(Stmt::SyntheticBlock(vec![decl, init]))
}

fn rewrite_placeholder_block_modifier_stmt(stmt: Stmt, cond: &Expr) -> Stmt {
    if let Stmt::Block(body) = &stmt
        && let placeholders = crate::ast::collect_placeholders_shallow(body)
        && !placeholders.is_empty()
    {
        let mut rewritten = Vec::new();
        for (idx, name) in placeholders.into_iter().enumerate() {
            rewritten.push(Stmt::VarDecl {
                name,
                expr: if idx == 0 {
                    cond.clone()
                } else {
                    Expr::Literal(Value::Nil)
                },
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
        rewritten.extend(body.clone());
        return Stmt::Block(rewritten);
    }
    stmt
}

pub(super) fn is_stmt_modifier_keyword(input: &str) -> bool {
    leading_modifier_keyword(input).is_some()
}

/// The statement-modifier keyword at the start of `input`, if any.
fn leading_modifier_keyword(input: &str) -> Option<&'static str> {
    [
        "if", "unless", "for", "while", "until", "given", "when", "with", "without",
    ]
    .into_iter()
    .find(|kw| keyword(kw, input).is_some())
}

/// Whether a second statement modifier `next` is legal after a first `first`.
/// Raku only allows a *conditional* modifier (`if`/`unless`/`with`/`without`)
/// followed by a *loop* modifier (`for`/`while`/`until`/`given`), e.g.
/// `EXPR if COND for LIST`. Everything else (two conditionals, two loops, a loop
/// then a conditional) is X::Syntax::Confused.
fn second_modifier_allowed(first: &str, next: &str) -> bool {
    let first_is_cond = matches!(first, "if" | "unless" | "with" | "without");
    let next_is_loop = matches!(next, "for" | "while" | "until" | "given");
    first_is_cond && next_is_loop
}

/// Check if an expression ends with a block body (e.g., `try { ... }`,
/// `do { ... }`, `gather { ... }`, or a call whose final argument is a bare
/// block such as `$lock.protect: { ... }` / `@a.map: { ... }`). Such
/// expressions should not have statement modifiers attached across a newline:
/// in Raku a statement ending in a `}` block at end of line is self-terminating.
fn expr_ends_with_block(expr: &Expr) -> bool {
    match expr {
        Expr::Try { .. } | Expr::Gather(_) | Expr::DoBlock { .. } | Expr::DoStmt(_) => true,
        Expr::MethodCall { args, .. }
        | Expr::HyperMethodCall { args, .. }
        | Expr::DynamicMethodCall { args, .. }
        | Expr::Call { args, .. } => {
            matches!(args.last(), Some(Expr::AnonSub { is_block: true, .. }))
        }
        _ => false,
    }
}

/// After a statement-modifier keyword's condition, a `{` block (or `-> ... {`
/// pointy header) means this is actually a full control statement (`if COND
/// { ... }`), not a postfix modifier — modifiers never take a block. Mirrors
/// the guard the `for`/`with` modifiers already apply to their own headers.
fn block_follows_modifier_condition(r: &str) -> bool {
    let r = r.trim_start();
    r.starts_with('{') || r.starts_with("->")
}

/// Parse statement modifier (postfix if/unless/for/while/until/given/when).
/// Supports chaining: `expr if cond for list` parses as `for list { expr if cond }`.
pub(crate) fn parse_statement_modifier(input: &str, stmt: Stmt) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;
    // Blocks: never attach a statement modifier across a newline.
    if matches!(stmt, Stmt::Block(_)) {
        let consumed_len = input.len().saturating_sub(rest.len());
        if input[..consumed_len].contains('\n') {
            return Ok((input, stmt));
        }
    }
    // Block-valued expressions (`try { ... }`, `do { ... }`, etc.):
    // a newline after the closing brace should terminate the statement,
    // preventing the next line's `if`/`for`/etc. from being treated as a
    // statement modifier.
    if let Stmt::Expr(ref expr) = stmt
        && expr_ends_with_block(expr)
    {
        let consumed_len = input.len().saturating_sub(rest.len());
        if input[..consumed_len].contains('\n') {
            return Ok((input, stmt));
        }
    }
    let mut current_stmt = stmt;
    let mut rest = rest;
    // The keywords of the modifiers parsed so far, in order.
    let mut parsed_kinds: Vec<&str> = Vec::new();

    loop {
        // If there's a semicolon, the statement is terminated — no more modifiers
        if let Some(stripped) = rest.strip_prefix(';') {
            return Ok((stripped, current_stmt));
        }

        // If at end of input or block, return as-is
        if rest.is_empty() || rest.starts_with('}') {
            return Ok((rest, current_stmt));
        }

        // A second modifier is only legal as `conditional THEN loop`
        // (`EXPR if COND for LIST`). Any other chain — two conditionals, two
        // loops, a loop then a conditional, or a third modifier — needs a `;`
        // and so is X::Syntax::Confused ("Missing semicolon").
        if let Some(next_kw) = leading_modifier_keyword(rest)
            && let Some(&first) = parsed_kinds.first()
            && (parsed_kinds.len() >= 2 || !second_modifier_allowed(first, next_kw))
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                crate::value::Value::str("Missing semicolon".to_string()),
            );
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Syntax::Confused"),
                attrs,
            );
            return Err(PError::fatal_with_exception(
                "Missing semicolon".to_string(),
                Box::new(ex),
            ));
        }

        let kw = leading_modifier_keyword(rest);
        match parse_single_modifier(rest, current_stmt.clone())? {
            Some((r, modified)) => {
                current_stmt = modified;
                if let Some(k) = kw {
                    parsed_kinds.push(k);
                }
                let (r, _) = ws(r)?;
                rest = r;
            }
            None => {
                return Ok((rest, current_stmt));
            }
        }
    }
}

/// Try to parse a single statement modifier. Returns None if no modifier matched.
fn parse_single_modifier(rest: &str, stmt: Stmt) -> Result<Option<(&str, Stmt)>, PError> {
    // Whether the statement being modified itself ends in a `{ ... }` block
    // (`$lock.protect: { ... }`). Only then does a `{` after the modifier's
    // condition mean "this `if` starts a new control statement" rather than a
    // postfix modifier. For a non-block statement (`die X if COND { ... }`) the
    // `if` IS a modifier and the trailing block is a separate statement.
    let modified_ends_block = matches!(&stmt, Stmt::Expr(e) if expr_ends_with_block(e));
    // Try statement modifiers
    if let Some(r) = keyword("if", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'if'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        // A `{` block after the condition means this is a full `if COND { ... }`
        // control statement, not a postfix modifier (modifiers take no block).
        // This arises after a block-final statement on the previous line whose
        // trailing newline was consumed (`$lock.protect: { ... }` then `if ...`).
        if modified_ends_block && block_follows_modifier_condition(r) {
            return Ok(None);
        }
        check_two_terms_across_lines(r)?;
        let then_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &cond);
        if let Some(split) = try_split_decl_modifier(&then_stmt, &cond) {
            return Ok(Some((r, split)));
        }
        return Ok(Some((
            r,
            Stmt::If {
                cond,
                then_branch: vec![then_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            },
        )));
    }
    if let Some(r) = keyword("unless", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'unless'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        if modified_ends_block && block_follows_modifier_condition(r) {
            return Ok(None);
        }
        check_two_terms_across_lines(r)?;
        let then_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &cond);
        let neg_cond = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond),
        };
        if let Some(split) = try_split_decl_modifier(&then_stmt, &neg_cond) {
            return Ok(Some((r, split)));
        }
        return Ok(Some((
            r,
            Stmt::If {
                cond: neg_cond,
                then_branch: vec![then_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            },
        )));
    }
    if let Some(r) = keyword("for", rest) {
        if matches!(stmt, Stmt::For { .. }) {
            return Err(PError::raw(
                "double statement-modifying for is not allowed".to_string(),
                Some(rest.len()),
            ));
        }
        let (r, _) = ws1(r)?;
        let (r, first) = expression_no_sequence(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected iterable expression after 'for'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        // Parse comma-separated list for `for` modifier: `expr for 1, 2, 3`
        let (r, iterable) = {
            let mut items = vec![first];
            let mut r = r;
            loop {
                let (r2, _) = ws(r)?;
                if !r2.starts_with(',') {
                    break;
                }
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                if r2.is_empty() || r2.starts_with('}') || r2.starts_with(';') {
                    r = r2;
                    break;
                }
                let (r2, next) = expression_no_sequence(r2)?;
                items.push(next);
                r = r2;
            }
            if items.len() == 1 {
                (r, items.into_iter().next().unwrap())
            } else {
                (r, Expr::ArrayLiteral(items))
            }
        };
        let (r, _) = ws(r)?;
        // Detect "Two terms in a row" on the same line after the for iterable.
        // e.g., `.say for (1, 2, 3)«~» "!"` — the `"!"` is a term without
        // an infix operator separating it from the iterable.
        if !r.is_empty()
            && !r.starts_with(';')
            && !r.starts_with('}')
            && !r.starts_with(')')
            && !r.starts_with("->")
            && !r.starts_with('{')
            && !is_stmt_modifier_keyword(r)
            && starts_with_term_char(r)
        {
            return Err(PError::fatal("Confused. Two terms in a row".to_string()));
        }
        // Do not consume full loop headers as statement modifiers.
        // This preserves parsing of:
        //   { ... }
        //   for @values -> $v { ... }
        // as two statements, rather than block + postfix modifier + lambda.
        if r.starts_with("->") || r.starts_with('{') {
            return Ok(None);
        }
        let loop_stmt = match stmt {
            Stmt::Expr(expr @ Expr::AnonSubParams { .. })
            | Stmt::Expr(expr @ Expr::Lambda { .. }) => {
                let target = Expr::CallOn {
                    target: Box::new(expr),
                    args: vec![Expr::Var("_".to_string())],
                };
                Stmt::Expr(target)
            }
            other => other,
        };
        return Ok(Some((
            r,
            Stmt::For {
                iterable,
                param: None,
                param_def: Box::new(None),
                params: Vec::new(),
                params_def: Vec::new(),
                body: vec![loop_stmt],
                label: None,
                mode: crate::ast::ForMode::Normal,
                rw_block: false,
                explicit_zero_params: false,
            },
        )));
    }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'while'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        if modified_ends_block && block_follows_modifier_condition(r) {
            return Ok(None);
        }
        return Ok(Some((
            r,
            Stmt::While {
                cond,
                body: vec![stmt],
                label: None,
            },
        )));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'until'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        if modified_ends_block && block_follows_modifier_condition(r) {
            return Ok(None);
        }
        return Ok(Some((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body: vec![stmt],
                label: None,
            },
        )));
    }
    if let Some(r) = keyword("given", rest) {
        let (r, _) = ws1(r)?;
        let (r, topic) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected topic expression after 'given'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        if modified_ends_block && block_follows_modifier_condition(r) {
            return Ok(None);
        }
        let given_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &topic);
        return Ok(Some((
            r,
            Stmt::Given {
                topic,
                body: vec![given_stmt],
            },
        )));
    }

    if let Some(r) = keyword("with", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        // Do not consume a full `with EXPR -> $param { ... }` block header
        // as a statement modifier. This preserves parsing of:
        //   subtest 'sub' => { ... }
        //   with make-temp-dir() -> $dir { ... }
        // as two statements, rather than stmt + postfix modifier + lambda.
        let (r_ws, _) = ws(r)?;
        if r_ws.starts_with("->") || r_ws.starts_with('{') {
            return Ok(None);
        }
        let mut stmt_for_branch = stmt.clone();
        let mut r_tail = r;
        if let Stmt::Call { name, args } = &stmt_for_branch {
            let mut call_args = args.clone();
            loop {
                let (r_ws, _) = ws(r_tail)?;
                if !r_ws.starts_with(',') {
                    r_tail = r_ws;
                    break;
                }
                let after_comma = &r_ws[1..];
                let (after_comma, _) = ws(after_comma)?;
                let (after_comma, arg_expr) = expression(after_comma)?;
                call_args.push(CallArg::Positional(arg_expr));
                r_tail = after_comma;
            }
            stmt_for_branch = Stmt::Call {
                name: *name,
                args: call_args,
            };
        }
        // `stmt with expr` is like `given expr { if .defined { stmt } }`.
        // When the statement is a block with placeholders, rewrite them.
        let stmt_for_branch =
            rewrite_placeholder_block_modifier_stmt(stmt_for_branch, &Expr::Var("_".to_string()));
        // When the modified statement is an expression statement, preserve
        // expression semantics via do-given.
        if matches!(stmt, Stmt::Expr(_)) {
            let if_stmt = Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: Symbol::intern("defined"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                },
                then_branch: vec![stmt_for_branch.clone()],
                else_branch: Vec::new(),
                binding_var: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: Symbol::intern("defined"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                },
                then_branch: vec![stmt_for_branch],
                else_branch: Vec::new(),
                binding_var: None,
            }],
        };
        return Ok(Some((r_tail, given_stmt)));
    }
    if let Some(r) = keyword("without", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        // Same as `with` — do not consume a full block header as modifier.
        let (r_ws, _) = ws(r)?;
        if r_ws.starts_with("->") || r_ws.starts_with('{') {
            return Ok(None);
        }
        let mut stmt_for_branch = stmt.clone();
        let mut r_tail = r;
        if let Stmt::Call { name, args } = &stmt_for_branch {
            let mut call_args = args.clone();
            loop {
                let (r_ws, _) = ws(r_tail)?;
                if !r_ws.starts_with(',') {
                    r_tail = r_ws;
                    break;
                }
                let after_comma = &r_ws[1..];
                let (after_comma, _) = ws(after_comma)?;
                let (after_comma, arg_expr) = expression(after_comma)?;
                call_args.push(CallArg::Positional(arg_expr));
                r_tail = after_comma;
            }
            stmt_for_branch = Stmt::Call {
                name: *name,
                args: call_args,
            };
        }
        // `stmt without expr` is like `given expr { unless .defined { stmt } }`.
        // Sets $_ to the condition value, then runs stmt if $_ is not defined.
        let not_defined = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name: Symbol::intern("defined"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            }),
        };
        let modified_stmt = rewrite_placeholder_block_modifier_stmt(stmt_for_branch.clone(), &cond);
        if matches!(stmt, Stmt::Expr(_)) {
            let if_stmt = Stmt::If {
                cond: not_defined,
                then_branch: vec![modified_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: not_defined,
                then_branch: vec![modified_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            }],
        };
        return Ok(Some((r_tail, given_stmt)));
    }

    Ok(None)
}

/// Check if the input starts with a character that unambiguously begins a term
/// (string literal, number, etc.). Used to detect "Two terms in a row" errors.
fn starts_with_term_char(input: &str) -> bool {
    let Some(ch) = input.chars().next() else {
        return false;
    };
    ch.is_ascii_digit()
        || matches!(
            ch,
            '\'' | '"'
                | '\u{2018}'
                | '\u{2019}'
                | '\u{201A}'
                | '\u{201C}'
                | '\u{201D}'
                | '\u{201E}'
        )
}
