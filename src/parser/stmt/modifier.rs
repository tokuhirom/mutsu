use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{
    PError, PResult, TWO_TERMS_ACROSS_LINES, merge_expected_messages,
};

use crate::ast::{CallArg, Expr, GivenWithKind, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::super::helpers::is_raku_identifier_start;
use super::modifier_decl_split::{split_decl_for_topic_modifier, try_split_decl_modifier};
use super::{keyword, parse_comma_or_expr};

thread_local! {
    /// The "Missing semicolon" error for a modifier chain this thread stopped
    /// short of, keyed by the address of the offending keyword.
    ///
    /// A statement takes at most one conditional modifier and then at most one
    /// loop modifier; a further one is not this statement's business, it is the
    /// *enclosing* statement's. `do STMT` is the construct that has one:
    /// `do return False unless %h<auth> ~~ $!auth if $!auth;` (Pakku::Spec) is a
    /// `do`-wrapped statement carrying `unless …`, and the `if …` modifies the
    /// `do` statement itself. Raising the error where the chain is detected
    /// therefore rejects legal code, so the chain merely *ends* there and the
    /// error is handed to whoever discovers the keyword is unconsumable — the
    /// statement list, see `pending_extra_modifier_error`.
    static PENDING_EXTRA_MODIFIER: std::cell::RefCell<Option<(crate::parser::memo::MemoKey, PError)>> =
        const { std::cell::RefCell::new(None) };
}

/// The deferred "Missing semicolon" error iff it belongs to `at`, i.e. the
/// statement really did end leaving a modifier keyword nobody could consume.
///
/// Reading it does NOT consume the record: a block body is parsed more than once
/// (speculatively as a hash composer first, then as a block, the second time as a
/// pure memo hit that re-runs no modifier loop), so a record taken by the
/// discarded attempt would be missing from the one that survives. Only an
/// enclosing statement actually consuming the keyword clears it, in
/// `clear_pending_extra_modifier`.
pub(crate) fn pending_extra_modifier_error(at: &str) -> Option<PError> {
    let key = crate::parser::memo::memo_key(at);
    PENDING_EXTRA_MODIFIER.with(|p| match p.borrow().as_ref() {
        Some((k, e)) if *k == key => Some(e.clone()),
        _ => None,
    })
}

/// Drop the deferred error once an enclosing statement has consumed the keyword
/// it was recorded for.
fn clear_pending_extra_modifier(at: &str) {
    let key = crate::parser::memo::memo_key(at);
    PENDING_EXTRA_MODIFIER.with(|p| {
        let mut slot = p.borrow_mut();
        if slot.as_ref().is_some_and(|(k, _)| *k == key) {
            *slot = None;
        }
    });
}

/// After parsing a postfix modifier condition, check if the remaining input
/// starts on a new line with a bare word that is not a statement modifier.
/// This detects "two terms in a row across lines" errors like:
///   42 if 23
///   is 50; 1
/// where `is` on the next line is confused with a continuation.
fn check_two_terms_across_lines(cond_input: &str, r: &str) -> Result<(), PError> {
    // Only check if there's content after the condition
    if r.is_empty() || r.starts_with(';') || r.starts_with('}') {
        return Ok(());
    }
    // A condition that ends in a `}` block is self-terminating at end of line,
    // exactly like any block statement: `say 1 if @a.grep: { ... }` followed by a
    // statement on the next line is legitimate, not "two terms in a row". Detect
    // it from the consumed condition text (ends in `}`).
    if cond_input[..cond_input.len() - r.len()]
        .trim_end()
        .ends_with('}')
    {
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
        // `trimmed` is the unconsumed rest at the offending second term, so the
        // reported position lands on that term rather than on the whole file.
        // `render_parse_error` separately re-derives the `------>` echo's own
        // (different) position from this exact message (#8329).
        return Err(PError::fatal_at(
            TWO_TERMS_ACROSS_LINES.to_string(),
            trimmed,
        ));
    }
    Ok(())
}

/// A compound-assignment declaration is represented as a scopeless synthetic
/// block containing its declaration and assignment. For a postfix `for`, the
/// declaration belongs outside the loop while the assignment runs once per
/// item; otherwise the synthetic block would redeclare and reset the variable
/// on every iteration (`my $product *= $_ for @values`).
fn split_compound_decl_for_modifier(stmt: Stmt) -> (Option<Stmt>, Stmt) {
    let Stmt::SyntheticBlock(mut stmts) = stmt else {
        return (None, stmt);
    };
    if stmts.len() < 2 || !matches!(stmts.first(), Some(Stmt::VarDecl { .. })) {
        return (None, Stmt::SyntheticBlock(stmts));
    }
    let declaration = stmts.remove(0);
    // `my $x = 1, $y for ...`: the trailing items' value list is headed by the
    // declared scalar (see `consume_scalar_decl_trailing_comma`). Hoisted out
    // of the loop body, that head would read as a user-written `$x` sunk per
    // iteration, so leave only the trailing items in the body.
    if let Stmt::VarDecl { name, .. } = &declaration {
        stmts = stmts
            .into_iter()
            .flat_map(|s| match s {
                Stmt::Expr(Expr::ArrayLiteral(items))
                    if matches!(items.first(), Some(Expr::Var(head)) if head == name) =>
                {
                    items.into_iter().skip(1).map(Stmt::Expr).collect()
                }
                other => vec![other],
            })
            .collect();
    }
    (Some(declaration), Stmt::SyntheticBlock(stmts))
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
                    Expr::Literal(Value::NIL)
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

/// A statement-modifier keyword sitting right after a *trailing comma*
/// (`die "x", if @c;` / `return 1, if 0;` — legal Raku, the trailing comma is an
/// empty list slot). Rejects a same-named **pair key** (`1, with => 2`) so a
/// legal comma list is never truncated at its last element.
pub(in crate::parser) fn is_stmt_modifier_after_trailing_comma(input: &str) -> bool {
    let Some(kw) = leading_modifier_keyword(input) else {
        return false;
    };
    !input[kw.len()..].trim_start().starts_with("=>")
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
/// Raku only allows a *conditional* modifier (`if`/`unless`/`with`/`without`/`when`)
/// followed by a *loop* modifier (`for`/`while`/`until`/`given`), e.g.
/// `EXPR if COND for LIST` or `EXPR when COND given TOPIC`. Everything else (two
/// conditionals, two loops, a loop then a conditional) is X::Syntax::Confused.
fn second_modifier_allowed(first: &str, next: &str) -> bool {
    let first_is_cond = matches!(first, "if" | "unless" | "with" | "without" | "when");
    let next_is_loop = matches!(next, "for" | "while" | "until" | "given");
    first_is_cond && next_is_loop
}

/// A bare block to the left of `while`/`until` is the modifier's operand: the
/// loop repeatedly evaluates the Block value without invoking its body.
fn while_modifier_operand(stmt: Stmt) -> Stmt {
    match stmt {
        Stmt::Block(body) => Stmt::Expr(crate::ast::make_anon_sub(body)),
        other => other,
    }
}

/// Parse statement modifier (postfix if/unless/for/while/until/given/when).
/// Supports chaining: `expr if cond for list` parses as `for list { expr if cond }`.
pub(crate) fn parse_statement_modifier(input: &str, stmt: Stmt) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;
    // A trailing comma in the statement's argument list is an empty list slot, not
    // a syntax error: `die "x", if @c;` is legal Raku. Statements that parse a
    // a statement modifier follows. A comma before anything else is left alone so
    // real errors still surface. Comma-list statements parse their arguments
    // before reaching this point; the comma here is only the empty trailing slot
    // accepted by Raku (`die "x", if @c`).
    let rest = if let Some(after_comma) = rest.strip_prefix(',') {
        let (after_ws, _) = ws(after_comma)?;
        if is_stmt_modifier_after_trailing_comma(after_ws) {
            after_ws
        } else {
            rest
        }
    } else {
        rest
    };
    // A block's `}` at end of line ends the statement (rakudo's `$*ENDSTMT`):
    // `my @a = gather { ... }` / `try { ... }` / `my $h = {a => 1}` / a bare
    // block, followed on the next line by `if COND { ... }`, is two statements,
    // not a modifier on the first. The brace parsers record that position
    // (`parser::stmt_ending_brace`); `@a = @a.grep({ ... })` ends in `)`, so
    // its next-line `if` still IS a modifier.
    if crate::parser::stmt_ending_brace::at_stmt_ending_brace(rest) {
        return Ok((input, stmt));
    }
    let mut current_stmt = stmt;
    let mut rest = rest;
    // The keywords of the modifiers parsed so far, in order.
    let mut parsed_kinds: Vec<&str> = Vec::new();
    // Whether the previous modifier's *condition* ended with a `{ ... }` block
    // (`return if @a.first: { ... }`) followed by a newline. A block that ends a
    // statement is itself a statement terminator in Raku, so the next line's
    // `if`/`for`/etc. begins a NEW statement rather than a second (illegal)
    // modifier — do not raise "Missing semicolon" for it.
    let mut prev_cond_block_terminated = false;

    loop {
        // A block-terminated condition followed by a newline ends the statement;
        // whatever comes next (including another modifier keyword) is a new one.
        if prev_cond_block_terminated {
            return Ok((rest, current_stmt));
        }

        // If there's a semicolon, the statement is terminated — no more modifiers
        if let Some(stripped) = rest.strip_prefix(';') {
            return Ok((stripped, current_stmt));
        }

        // If at end of input or block, return as-is
        if rest.is_empty() || rest.starts_with('}') {
            return Ok((rest, current_stmt));
        }

        // An enclosing statement that can take this keyword has consumed it, so
        // the deferred error recorded for it is moot.
        clear_pending_extra_modifier(rest);

        // A second modifier is only legal as `conditional THEN loop`
        // (`EXPR if COND for LIST`). Any other chain — two conditionals, two
        // loops, a loop then a conditional, or a third modifier — ends this
        // statement, and needs a `;` unless an enclosing statement takes it.
        if let Some(next_kw) = leading_modifier_keyword(rest)
            && let Some(&first) = parsed_kinds.first()
            && (parsed_kinds.len() >= 2 || !second_modifier_allowed(first, next_kw))
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                crate::value::Value::str("Missing semicolon".to_string()),
            );
            attrs.insert(
                "reason".to_string(),
                crate::value::Value::str("Missing semicolon".to_string()),
            );
            // Reconstruct the source spans around the eject point (`rest` sits at
            // the offending second modifier) so the error carries `pre`/`post`.
            if let Some((pre, post)) = crate::parser::primary::source_span_at(rest) {
                attrs.insert("pre".to_string(), crate::value::Value::str(pre));
                attrs.insert("post".to_string(), crate::value::Value::str(post));
            }
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Syntax::Confused"),
                attrs,
            );
            let mut err =
                PError::fatal_with_exception("Missing semicolon".to_string(), Box::new(ex));
            err.remaining_len = Some(rest.len());
            let key = crate::parser::memo::memo_key(rest);
            PENDING_EXTRA_MODIFIER.with(|p| {
                *p.borrow_mut() = Some((key, err));
            });
            return Ok((rest, current_stmt));
        }

        let kw = leading_modifier_keyword(rest);
        match parse_single_modifier(rest, current_stmt.clone())? {
            Some((r, modified)) => {
                // Did this modifier's condition end with a `{ ... }` block?
                let cond_consumed = &rest[..rest.len() - r.len()];
                let cond_ends_block = cond_consumed.trim_end().ends_with('}')
                    && !super::modifier_tail::modifier_operand_ends_with_subscript(&modified);
                current_stmt = modified;
                if let Some(k) = kw {
                    parsed_kinds.push(k);
                }
                let (r_ws, _) = ws(r)?;
                let gap_has_newline = r[..r.len() - r_ws.len()].contains('\n');
                prev_cond_block_terminated = cond_ends_block && gap_has_newline;
                rest = r_ws;
            }
            None => {
                return Ok((rest, current_stmt));
            }
        }
    }
}

/// The `(param, param_def, params, params_def, rw_block, explicit_zero_params)`
/// shape `Stmt::For` uses to describe a loop's own signature.
type ForParamShape = (
    Option<String>,
    Box<Option<ParamDef>>,
    Vec<String>,
    Vec<ParamDef>,
    bool,
    bool,
);

/// Turn a closure's own explicit signature into the shape `Stmt::For` uses, so
/// a pointy-block/`sub (...) { ... }` operand of the `for` statement modifier
/// consumes N elements per iteration according to its own arity -- exactly like
/// `for LIST -> SIG { ... }` already does for the identical signature written
/// the other way round. Mirrors how `parse_for_params` (for_params.rs) and
/// `arrow_lambda_inner` (lambda.rs) shape a single named param into the
/// singular `param`/`param_def` slot and a multi-param signature into the
/// plural `params`/`params_def` slot; an explicit empty signature (`-> {}`,
/// `sub () { }`) sets `explicit_zero_params` instead.
fn closure_signature_as_for_params(
    params: Vec<String>,
    param_defs: Vec<ParamDef>,
    is_rw: bool,
) -> ForParamShape {
    match params.len() {
        0 => (None, Box::new(None), Vec::new(), Vec::new(), is_rw, true),
        // A lone sigil'd slurpy (`*@all { ... } for @list`) binds a *list* of the
        // iteration chunk rather than the chunk element itself, so it needs the
        // plural shape whose binder knows about slurpies — exactly the routing
        // `parse_for_params` uses for the `for @list -> *@all { }` spelling.
        1 if param_defs[0].is_variadic() && !param_defs[0].sigilless => {
            (None, Box::new(None), params, param_defs, is_rw, false)
        }
        1 => {
            let param = params.into_iter().next();
            let param_def = param_defs.into_iter().next();
            (
                param,
                Box::new(param_def),
                Vec::new(),
                Vec::new(),
                is_rw,
                false,
            )
        }
        _ => (None, Box::new(None), params, param_defs, is_rw, false),
    }
}

/// Try to parse a single statement modifier. Returns None if no modifier matched.
fn parse_single_modifier(rest: &str, stmt: Stmt) -> Result<Option<(&str, Stmt)>, PError> {
    // Try statement modifiers
    if let Some(r) = keyword("if", rest) {
        let (r, _) = ws1(r)?;
        let cond_input = r;
        let (r, cond) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'if'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        check_two_terms_across_lines(cond_input, r)?;
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
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            },
        )));
    }
    if let Some(r) = keyword("unless", rest) {
        let (r, _) = ws1(r)?;
        let cond_input = r;
        let (r, cond) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'unless'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        check_two_terms_across_lines(cond_input, r)?;
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
                is_statement_modifier: true,
                is_unless: true,
                with_kind: None,
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
        let (hoisted_decl, stmt) = split_compound_decl_for_modifier(stmt);
        let (r, _) = ws1(r)?;
        // Sequence operators absorb a comma-separated seed list on their left:
        // `for 1, { $_ + 1 } ... 3` iterates one sequence, not an array whose
        // second item is another sequence.  This is the same special case used
        // by listop and assignment parsing.
        let (r, iterable) =
            if let Some(result) = crate::parser::primary::try_parse_sequence_arg_list(r) {
                result?
            } else {
                let (r, first) = expression(r).map_err(|err| PError {
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
                    let mut trailing_comma = false;
                    loop {
                        let (r2, _) = ws(r)?;
                        if !r2.starts_with(',') {
                            break;
                        }
                        let r2 = &r2[1..];
                        let (r2, _) = ws(r2)?;
                        if r2.is_empty() || r2.starts_with('}') || r2.starts_with(';') {
                            r = r2;
                            trailing_comma = true;
                            break;
                        }
                        let (r2, next) = expression(r2)?;
                        items.push(next);
                        r = r2;
                    }
                    // A trailing comma builds a 1-element list even with a single item:
                    // `for @a,` iterates once over `(@a,)` (the array itemized), matching
                    // the parenthesized `for (@a,)` form, rather than flattening `@a`.
                    if items.len() == 1 && !trailing_comma {
                        (r, items.into_iter().next().unwrap())
                    } else {
                        (r, Expr::ArrayLiteral(items))
                    }
                };
                (r, iterable)
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
            return Err(PError::fatal_at(
                "Confused. Two terms in a row".to_string(),
                r,
            ));
        }
        // Do not consume full loop headers as statement modifiers.
        // This preserves parsing of:
        //   { ... }
        //   for @values -> $v { ... }
        // as two statements, rather than block + postfix modifier + lambda.
        if r.starts_with("->") || r.starts_with('{') {
            return Ok(None);
        }
        let (param, param_def, params, params_def, rw_block, explicit_zero_params, body) =
            match stmt {
                // `{ ... } for LIST` is the very same loop as `for LIST { ... }`,
                // so the bare block gives the loop its implicit placeholder
                // signature exactly as a `for LIST { ... }` body block does.
                // Without this the loop stayed signature-less and `$^a`/`$^b` never
                // got bound (`{ $^a ~ $^b } for (1,2),(3,4)` yielded `True/True`).
                Stmt::Block(ref body) => {
                    let (param, params) =
                        crate::parser::stmt::control::placeholder_loop_params(body)
                            .unwrap_or((None, Vec::new()));
                    (param, Box::new(None), params, Vec::new(), false, false, vec![stmt])
                }
                // ADR-0033 Phase 1: a bare Whatever-curried statement (`* + 1 for
                // @a`) is now `WhateverCurry` rather than a built `Lambda`/
                // `AnonSubParams`, but still needs the same "call it with $_"
                // treatment as a single-param pointy block, to keep `* + 1 for
                // @a` meaning `($_ + 1) for @a` rather than discarding an
                // uncalled closure value. It is always arity-1 (one `*`
                // placeholder threads through the whole expression), so it must
                // NOT become the loop's own signature.
                Stmt::Expr(expr @ Expr::WhateverCurry(_))
                // The single-param pointy block form (`-> $x { ... }`) is
                // already exactly arity 1 under a plain call with the topic, so
                // it keeps working unchanged.
                | Stmt::Expr(expr @ Expr::Lambda { .. }) => {
                    let target = Expr::CallOn {
                        target: Box::new(expr),
                        args: vec![Expr::Var("_".to_string())],
                    };
                    (None, Box::new(None), Vec::new(), Vec::new(), false, false, vec![Stmt::Expr(target)])
                }
                // A genuine multi-/zero-/slurpy-param signature written as a
                // pointy block or `sub (...) { ... }`: make the closure's own
                // signature the loop's signature and its body the loop's body,
                // so `Stmt::For`'s existing multi-param handling consumes N
                // elements per iteration -- the same lowering the bare
                // placeholder-block case above already uses. Excludes the
                // implicit-`@_` bare-block shape (`{ @_ } for LIST`, guarded
                // below): rakudo invokes that one element at a time even though
                // its only parameter is a synthesized slurpy `*@_`, so its
                // signature must NOT become the loop's own (mirrors
                // `bare_block_body` in meta_ops.rs, which excludes this exact
                // shape from the placeholder-block conversion for the same
                // reason).
                Stmt::Expr(Expr::AnonSubParams {
                    params,
                    param_defs,
                    body,
                    is_rw,
                    ..
                }) if !(params.len() == 1
                    && params[0] == "@_"
                    && param_defs.first().is_some_and(|d| d.block_param)) =>
                {
                    let (param, param_def, params, params_def, rw_block, explicit_zero_params) =
                        closure_signature_as_for_params(params, param_defs, is_rw);
                    (
                        param,
                        param_def,
                        params,
                        params_def,
                        rw_block,
                        explicit_zero_params,
                        body,
                    )
                }
                Stmt::Expr(expr @ Expr::AnonSubParams { .. }) => {
                    let target = Expr::CallOn {
                        target: Box::new(expr),
                        args: vec![Expr::Var("_".to_string())],
                    };
                    (None, Box::new(None), Vec::new(), Vec::new(), false, false, vec![Stmt::Expr(target)])
                }
                other => (None, Box::new(None), Vec::new(), Vec::new(), false, false, vec![other]),
            };
        let loop_stmt = Stmt::For {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body,
            label: None,
            mode: crate::ast::ForMode::Normal,
            rw_block,
            explicit_zero_params,
            is_statement_modifier: true,
            uses_block_magic: false,
        };
        let stmt = if let Some(declaration) = hoisted_decl {
            Stmt::SyntheticBlock(vec![declaration, loop_stmt])
        } else {
            loop_stmt
        };
        return Ok(Some((r, stmt)));
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
        return Ok(Some((
            r,
            Stmt::While {
                cond,
                body: vec![while_modifier_operand(stmt)],
                label: None,
                is_statement_modifier: true,
                is_until: false,
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
        return Ok(Some((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body: vec![while_modifier_operand(stmt)],
                label: None,
                is_statement_modifier: true,
                is_until: true,
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
        let given_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &topic);
        return Ok(Some((
            r,
            Stmt::Given {
                topic,
                body: vec![given_stmt],
                is_statement_modifier: true,
                with_kind: None,
            },
        )));
    }

    if let Some(r) = keyword("when", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected match expression after 'when'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        // `When` signals a match by raising `succeed`, which only an enclosing topicalizer
        // catches. Wrapping in a `given $_` gives it that catcher without changing the topic,
        // and composes with a following `given` modifier, which re-topicalizes `$_` first.
        return Ok(Some((
            r,
            Stmt::Given {
                topic: Expr::Var("_".to_string()),
                body: vec![Stmt::When {
                    cond,
                    body: vec![stmt],
                    is_statement_modifier: true,
                }],
                is_statement_modifier: true,
                with_kind: None,
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
                then_branch: vec![stmt_for_branch],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
                is_statement_modifier: true,
                with_kind: Some(GivenWithKind::With),
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        // A declaration stays unconditional; only its initializer is gated.
        let (hoisted_decl, stmt_for_branch) = match split_decl_for_topic_modifier(&stmt_for_branch)
        {
            Some((decl, assign)) => (Some(decl), assign),
            None => (None, Some(stmt_for_branch)),
        };
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
                then_branch: stmt_for_branch.into_iter().collect(),
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            }],
            is_statement_modifier: true,
            with_kind: Some(GivenWithKind::With),
        };
        if let Some(decl) = hoisted_decl {
            return Ok(Some((r_tail, Stmt::SyntheticBlock(vec![decl, given_stmt]))));
        }
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
        let modified_stmt = rewrite_placeholder_block_modifier_stmt(stmt_for_branch, &cond);
        if matches!(stmt, Stmt::Expr(_)) {
            let if_stmt = Stmt::If {
                cond: not_defined,
                then_branch: vec![modified_stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
                is_statement_modifier: true,
                with_kind: Some(GivenWithKind::Without),
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        let (hoisted_decl, modified_stmt) = match split_decl_for_topic_modifier(&modified_stmt) {
            Some((decl, assign)) => (Some(decl), assign),
            None => (None, Some(modified_stmt)),
        };
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: not_defined,
                then_branch: modified_stmt.into_iter().collect(),
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            }],
            is_statement_modifier: true,
            with_kind: Some(GivenWithKind::Without),
        };
        if let Some(decl) = hoisted_decl {
            return Ok(Some((r_tail, Stmt::SyntheticBlock(vec![decl, given_stmt]))));
        }
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
