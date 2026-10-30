use super::*;

pub(crate) fn or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = assign_or_and_expr(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::Or | LogicalOp::XorXor | LogicalOp::OrElse), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let sigil =
                crate::ast::left_sigil::after(crate::ast::left_sigil::LeftSigil::Unset, &left);
            let (r, right) = crate::ast::left_sigil::inherit(r, sigil, || {
                if mode == ExprMode::Full {
                    assign_or_and_expr(r, mode).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after 'or'/'xor'/'orelse'",
                            r.len(),
                        )
                    })
                } else {
                    assign_or_and_expr(r, mode)
                }
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
                form: Default::default(),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Low-precedence OR/XOR chain for ternary condition parsing.
/// This intentionally excludes assignment expressions so:
/// `a = b ?? c !! d` parses as `a = (b ?? c !! d)`.
pub(crate) fn or_expr_no_assign_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_expr_no_assign_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::Or | LogicalOp::XorXor | LogicalOp::OrElse), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = and_expr_no_assign_mode(r, mode)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
                form: Default::default(),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Assignment expressions at the or/and level: $var = expr
/// This sits between or/and and not in precedence.
pub(crate) fn assign_or_and_expr(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    and_expr_mode(input, mode)
}

/// Continue parsing a statement-level word-logical chain, given an
/// already-parsed leftmost operand (`seed`) and the input positioned *at* the
/// first word-logical operator.
///
/// Used by the hand-rolled statement-level assignment / declaration / `return`
/// parsers: after the RHS is parsed with `expression_no_word_logical` (which
/// stops before the loose word-logicals), the trailing `... and ...` is parsed
/// here with `seed` (typically a re-read of the just-assigned variable) as the
/// left operand. The precedence mirrors `or_expr_mode` wrapping `and_expr_mode`
/// (`or`/`xor`/`orelse` looser than `and`/`andthen`/`notandthen`), but — because
/// this runs at *statement* level, where the comma is looser than `=` yet the
/// word-logicals are looser still — each operand absorbs a full comma list
/// (`X = 1, 2 and 3, 4` → `(X = 1, 2) and (3, 4)`).
pub(crate) fn word_logical_tail(input: &str, seed: Expr) -> PResult<'_, Expr> {
    let (rest, left) = and_chain_seeded(input, seed)?;
    or_chain_continue(rest, left)
}

/// Word-logical operand at statement level: a comma list whose elements stop
/// before the word-logicals.
fn wl_operand(input: &str) -> PResult<'_, Expr> {
    crate::parser::stmt::assign::parse_comma_or_expr_no_word_logical(input)
}

/// Consume a chain of `and`/`andthen`/`notandthen` starting from `seed`.
fn and_chain_seeded(input: &str, seed: Expr) -> PResult<'_, Expr> {
    let mut rest = input;
    let mut left = seed;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = wl_operand(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
                form: Default::default(),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Continue a chain of `or`/`xor`/`orelse` from an already-parsed `and`-level
/// left operand. Each `or`-operand is itself a full `and` chain.
fn or_chain_continue(input: &str, left0: Expr) -> PResult<'_, Expr> {
    let mut rest = input;
    let mut left = left0;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::Or | LogicalOp::XorXor | LogicalOp::OrElse), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, first) = wl_operand(r)?;
            let (r, right) = and_chain_seeded(r, first)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
                form: Default::default(),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Low-precedence: and / andthen / notandthen
pub(crate) fn and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    // The list-infix operators (`Z`/`X`/meta/`...`/feed/`minmax`) sit just below
    // the loose word-logicals, so their operand — everything down to the item
    // level — is parsed here, tighter than `and`/`andthen`/`notandthen`.
    let (mut rest, mut left) = list_infix_top(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let sigil =
                crate::ast::left_sigil::after(crate::ast::left_sigil::LeftSigil::Unset, &left);
            let (r, right) = crate::ast::left_sigil::inherit(r, sigil, || {
                if mode == ExprMode::Full {
                    list_infix_top(r, mode).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after 'and'/'andthen'/'notandthen'",
                            r.len(),
                        )
                    })
                } else {
                    list_infix_top(r, mode)
                }
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
                form: Default::default(),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

pub(crate) fn and_expr_no_assign_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = not_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = not_expr_mode(r, mode)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
                form: Default::default(),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

pub(crate) fn assign_not_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, expr) = not_expr_mode(input, mode)?;
    let (r, _) = ws(rest)?;

    // Longest-token rule (Raku LTM): a user-declared *symbol* infix operator that
    // starts here and is LONGER than the assignment operator that would otherwise
    // match (`infix:<-=->` vs the compound `-=`; `infix:<=...>` vs `=`) is that
    // infix, not an assignment. Leave it unconsumed so the enclosing list-infix
    // layer parses it. Before the list-infix layer moved above item assignment,
    // this collision was resolved for free — the infix was consumed inside the
    // operand chain (below this compound-assignment check).
    let assign_len = if let Some((after_op, _)) = parse_compound_assign_op(r) {
        r.len() - after_op.len()
    } else if r.starts_with(":=") && !r.starts_with("::=") {
        2
    } else if r.starts_with('=')
        && !r.starts_with("==")
        && !r.starts_with("=>")
        && !r.starts_with("=:=")
        && !r.starts_with("=~=")
    {
        1
    } else {
        0
    };
    if assign_len > 0
        && let Some((_, ulen)) = crate::parser::stmt::simple::match_user_declared_infix_symbol_op(r)
        && ulen > assign_len
    {
        return Ok((rest, expr));
    }

    // Try compound assignment operators (+=, *=, //=, etc.) before simple =
    if !matches!(mode, ExprMode::NoSequenceNoFeed | ExprMode::ListopArg)
        && let Some((after_op, op)) = parse_compound_assign_op(r)
    {
        let (after_ws, _) = ws(after_op)?;
        let comma_level = matches!(op, crate::parser::stmt::assign::CompoundAssignOp::Comma);
        if let Ok((r, rhs)) = parse_compound_assign_rhs_mode(after_ws, comma_level, mode)
            && let Ok(compound) = crate::parser::stmt::assign::written_compound_assignment(
                expr.clone(),
                op,
                crate::ast::parsed::CompoundMeta::Plain,
                rhs,
            )
        {
            return Ok((r, compound));
        }
    }
    // `$s (|)= 1`: a set operator's compound assignment.
    if !matches!(mode, ExprMode::NoSequenceNoFeed | ExprMode::ListopArg)
        && let Some((after_op, spelling)) =
            crate::parser::stmt::assign::parse_set_compound_assign_op(r)
    {
        let (after_ws, _) = ws(after_op)?;
        if let Ok((r, rhs)) = parse_compound_assign_rhs_mode(after_ws, false, mode) {
            return Ok((
                r,
                crate::parser::stmt::assign::written_set_assignment(expr, spelling, rhs),
            ));
        }
    }

    // Bracket meta-op compound assignment on an *indexed* lvalue: `%h<k> [R//]= $v`,
    // `@a[$i] [+]= 1`. A bare-variable LHS is handled by the dedicated
    // statement-level / call-argument assign paths (which need `expression()` to
    // NOT swallow the `[op]=`, so their comma-boundary logic keeps working) — only
    // subscripted lvalues, which reach here as the parsed `expr`, need this branch.
    // The list-infix / arithmetic loops above bail on `[op]=` so `r` still starts
    // with it.
    if matches!(expr, Expr::Index { .. } | Expr::MultiDimIndex { .. })
        && let Some((after_op, meta, op)) =
            crate::parser::stmt::assign::parse_bracket_meta_assign_op(r)
    {
        let depth = r.chars().take_while(|ch| *ch == '[').count();
        let (after_ws, _) = ws(after_op)?;
        if let Ok((r, rhs)) = parse_assignment_rhs_mode(after_ws, mode)
            && let Ok(result) = crate::parser::stmt::assign::written_bracketed_meta_assignment(
                expr.clone(),
                &meta,
                depth,
                op,
                rhs,
            )
        {
            return Ok((r, result));
        }
    }

    // Word-operator compound assignment (`div=`, `mod=`, `gcd=`, `x=`, and
    // user-declared operators). Handled here — not only at the statement level —
    // so it parses as the operand of a looser operator, e.g. the right side of
    // `and`/`or`: `@f.push($p) and $n div= $p while ...` (surfaced by
    // Prime::Factor). The multiplicative parser leaves `div=` alone (see
    // `word_infix_at`), so `not_expr_mode` above returns the bare lvalue here.
    if let Some((after_op, op_name)) = parse_custom_compound_assign_op(r) {
        let (after_ws, _) = ws(after_op)?;
        if let Ok((r, rhs)) = parse_compound_assign_rhs_mode(after_ws, false, mode)
            && let Ok(result) = crate::parser::stmt::assign::written_routine_compound_assignment(
                expr.clone(),
                op_name,
                rhs,
            )
        {
            return Ok((r, result));
        }
    }

    // Binding (`:=`) is an expression-level operator at item-assignment
    // precedence (raku), so it must work unparenthesized after `return`, in a
    // declarator RHS, etc. -- e.g. `return @!specs := @specs`. mutsu otherwise
    // only handled `:=` at the statement level. Compile-time `::=` is left to
    // its own statement-level handler. Only simple sigil-variable lvalues bind
    // here; other lvalue shapes (bareword pseudo-packages like `OUTER`, call
    // results, indexed elements) fall through to the statement-level handler,
    // which validates them and throws X::Bind where appropriate.
    if r.starts_with(":=") && !r.starts_with("::=") {
        let after = &r[2..];
        let (after, _) = ws(after)?;
        if let Ok((r2, rhs)) = ternary_mode(after, mode) {
            match unwrap_grouped_lvalue(expr.clone()) {
                Expr::Var(name) => {
                    return Ok((
                        r2,
                        Expr::AssignExpr {
                            name,
                            expr: Box::new(rhs),
                            is_bind: true,
                        },
                    ));
                }
                Expr::ArrayVar(name) => {
                    return Ok((
                        r2,
                        Expr::AssignExpr {
                            name: format!("@{name}"),
                            expr: Box::new(rhs),
                            is_bind: true,
                        },
                    ));
                }
                Expr::HashVar(name) => {
                    return Ok((
                        r2,
                        Expr::AssignExpr {
                            name: format!("%{name}"),
                            expr: Box::new(rhs),
                            is_bind: true,
                        },
                    ));
                }
                Expr::MultiDimIndex {
                    target,
                    dimensions,
                    is_positional,
                } if matches!(
                    target.as_ref(),
                    Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::Var(_) | Expr::PseudoStash(_)
                ) =>
                {
                    return Ok((
                        r2,
                        crate::parser::index_bind_expr(
                            target,
                            Box::new(Expr::ArrayLiteral(dimensions)),
                            is_positional,
                            crate::ast::IndexSpelling::SemicolonDimensions,
                            rhs,
                        ),
                    ));
                }
                // Indexed element bind: `%h<k> := v`, `@a[i] := v`. Desugar to an
                // IndexAssign whose value is an `Expr::IndexBindValue`
                // node so the VM takes *bind* (not assign) semantics — matching
                // the statement-level handler. This lets an indexed bind parse in
                // expression context (`my $c = %h<k> := v`, `$c = %h<k> := X.new: ...`,
                // `return @a[i] := v`); previously only simple sigil variables bound
                // here and indexed lvalues were left to the statement-level handler,
                // which is absent in expression context.
                //
                // Only a single-element bind on a plain sigil-variable target is
                // handled here. Slice binds (`@a[0,1] := 4,5`), Whatever-index binds
                // (`@a[*-1] := 42`, illegal → X::Bind::Slice), and binds into a
                // non-variable target (`(1,2)[0] := 3`, `10[0] := 1`, → X::Bind) must
                // keep falling through to the statement-level handler, which
                // distributes/validates them. Guard on those so this fast path does
                // not shadow that handling.
                Expr::Index {
                    target,
                    index,
                    is_positional,
                    spelling,
                } if matches!(
                    target.as_ref(),
                    // A pseudo-stash target is the `Pkg::<@a> := v` spelling
                    // of a package-variable bind (#10546).
                    Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::Var(_) | Expr::PseudoStash(_)
                ) && !matches!(
                    index.as_ref(),
                    Expr::ArrayLiteral(_)
                        | Expr::Whatever
                        // ADR-0033 Phase 1: a Whatever-index (`@a[*-1] := ...`) is
                        // still an un-expanded `WhateverCurry` marker at this
                        // (pre-compile) parse-time point, not yet the built
                        // `Lambda`/`AnonSubParams` closure below (kept as a
                        // belt-and-suspenders match; only reachable pre-ADR-0033).
                        | Expr::WhateverCurry(_)
                        | Expr::Lambda {
                            is_whatever_code: true,
                            ..
                        }
                        | Expr::AnonSubParams {
                            is_whatever_code: true,
                            ..
                        }
                ) =>
                {
                    // Indexed bind RHSs are values too: preserve a compound
                    // `*` expression as WhateverCode instead of evaluating it
                    // as Numeric(Whatever). This expression-context path is
                    // reached before the statement-level indexed-bind parser.
                    return Ok((
                        r2,
                        crate::parser::index_bind_expr(target, index, is_positional, spelling, rhs),
                    ));
                }
                _ => {}
            }
        }
    }

    // Atomic compound assignment on an array / hash element: `@a[0] ⚛+= 5`,
    // `%h<k> ⚛-= 2` (#12005). A variable target is parsed by the statement-level
    // sites (`stmt/assign/`); an element target is only ever reached here, as the
    // parsed `expr`. Like `+=` it is an item assignment, so the right-hand side
    // stops before the comma.
    if let Some((after_atomic, negate)) =
        crate::parser::stmt::assign::strip_atomic_compound_assign(r)
    {
        let target = unwrap_grouped_lvalue(expr.clone());
        if crate::parser::expr::is_atomic_elem_target(&target) {
            let (r2, _) = ws(after_atomic)?;
            let (r2, rhs) = item_expr(r2, mode)?;
            return Ok((
                r2,
                crate::parser::stmt::assign::atomic_compound_call(target, rhs, negate),
            ));
        }
    }

    // Atomic store in EXPRESSION position: `return unless $!stale ⚛== 1`
    // (Selkie). `⚛=` was recognised only by the statement-level and
    // parenthesized assignment parsers, so a bare `$x ⚛= 1` as a statement
    // modifier's condition — or anywhere else an expression is wanted — did
    // not parse at all. It is an item assignment like `=`, so its right-hand
    // side stops before the comma. `strip_atomic_store_assign` also accepts
    // the `⚛==` metaoperator spelling.
    if let Some(after_atomic) = crate::parser::stmt::assign::strip_atomic_store_assign(r) {
        let (r2, _) = ws(after_atomic)?;
        let (r2, rhs) = item_expr(r2, mode)?;
        return match unwrap_grouped_lvalue(expr) {
            Expr::Var(name) => Ok((r2, crate::ast::atomic_op::store_var(name, rhs))),
            target => crate::parser::stmt::assign::atomic_elem_store_call(&target, rhs)
                .map(|store| (r2, store))
                .ok_or_else(|| PError::expected_at("atomic assignment target", r)),
        };
    }

    if !(r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>")) {
        return Ok((rest, expr));
    }

    let r = &r[1..];
    let (r, _) = ws(r)?;
    // Item assignment (`=`) to a bare scalar variable binds TIGHTER than the
    // comma operator, so an embedded `$x = 1, 2` parses as `($x = 1), 2`. List
    // assignment to an `@`/`%` container — and the parenthesized list-lvalue form
    // `($x) = 1, 2` — keeps the comma-absorbing RHS. We test the pre-unwrap
    // expression so a grouped `($x)` stays on the list-assignment path. (The
    // statement-level counterpart lives in `stmt/assign.rs::assign_stmt`.)
    // A symbolic scalar deref (`$::('name') = ...`) is an item assignment too, so
    // it binds tighter than the comma (`flat $::('b') = l(), l()` is
    // `flat (($::('b') = l()), l())`). An `@`/`%` symbolic deref keeps the
    // comma-absorbing list-assignment RHS.
    let scalar_item_assign = matches!(&expr, Expr::Var(_))
        || matches!(&expr, Expr::SymbolicDeref { sigil, .. } if sigil == "$")
        || matches!(&expr, Expr::IndirectTypeLookup(_));
    let (r, rhs) = if scalar_item_assign {
        // Item assignment (`$x = ...`) binds TIGHTER than the comma operator and
        // therefore tighter than the list-infix operators: `$x = 1 Z 2` is
        // `($x = 1) Z 2`. Parse the RHS at the item level (which still includes
        // the conditional `?? !!`), leaving any trailing `Z`/`X`/`...` for the
        // enclosing list-infix layer.
        item_expr(r, mode)?
    } else {
        parse_assignment_rhs_mode(r, mode)?
    };

    // Not a target here: the `=` is left to the statement parser, which sees
    // the target without its parentheses. A `.AST` parse keeps the
    // parentheses of a target the statement parser writes through
    // (`(-$x) = 5`), which builds the assignment node from it as written.
    if !crate::rakuast::is_expression_assign_target(&expr) {
        if crate::rakuast::is_callable_lvalue_assign_target(&expr)
            || crate::rakuast::is_item_assign_target(&expr)
        {
            return Ok((rest, expr));
        }
        return Ok((rest, unwrap_grouped_lvalue(expr)));
    }
    // The target as written, parentheses included; a `.AST` parse keeps the
    // assignment in its RakuAST shape (ADR-10723 Stage 2).
    let node = crate::ast::parsed::Assign::written_at(Some(input), expr, rhs);
    Ok((
        r,
        Expr::Ast(Box::new(crate::ast::parsed::ParsedExpr::Assign(node))),
    ))
}
