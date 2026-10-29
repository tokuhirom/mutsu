//! The one lowering of statement modifiers (ADR-10723 Stage 2): a
//! [`Modified`] statement the parser built, or one read off a hand-built
//! `Statement::Expression` with a `condition-modifier` / `loop-modifier`,
//! becomes the statement the compiler runs.
//!
//! This is the desugaring the parser used to do while it parsed: `S if C` as
//! an `If` flagged as a modifier, `unless` / `until` over a negated condition,
//! `S when M` inside a `given $_` that catches its `succeed`, `S with T` as a
//! once-topicalized `.defined` test, and `S for L` as a `For` whose body is
//! `S`. The condition modifier applies first: `S if C for L` loops over
//! `S if C`.

use crate::ast::parsed::{Modified, ModifierKind, StatementModifier};
use crate::ast::{Expr, ForMode, GivenWithKind, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::{RuntimeError, Value};

/// Lower a statement with its modifiers.
// Cost: O(n), n = size of the statement and its modifiers' operands.
pub(crate) fn lower(node: Modified) -> Result<Stmt, RuntimeError> {
    let Modified {
        stmt,
        condition,
        loop_,
        ..
    } = node;
    let mut stmt = *stmt;
    // `sub a() { } given 3`: the declaration is compile-time and happens
    // anyway; the modifier only governs evaluating the statement's (sunk)
    // value, so its operand still runs (#10257).
    if let Stmt::SubDecl { .. } = stmt {
        let value = lower(Modified {
            stmt: Box::new(Stmt::Expr(Expr::Literal(Value::NIL))),
            condition,
            loop_,
            body: crate::ast::SrcSpan::NONE,
        })?;
        return Ok(Stmt::SyntheticBlock(vec![stmt, value]));
    }
    if let Some(modifier) = condition {
        stmt = condition_modifier(modifier, stmt)?;
    }
    // A signature declaration no condition took is lowered whole.
    stmt = super::signature_expand::lower_pending(stmt)?;
    if let Some(modifier) = loop_ {
        stmt = loop_modifier(modifier, stmt);
    }
    Ok(stmt)
}

/// Whether `stmt` is an attribute declaration, alone or followed by the
/// assignment of its compound initializer.
// Cost: O(1).
fn is_attribute_declaration(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::HasDecl { .. } => true,
        Stmt::SyntheticBlock(parts) => matches!(parts.first(), Some(Stmt::HasDecl { .. })),
        _ => false,
    }
}

/// `S if C`, `S unless C`, `S when M`, `S with T`, `S without T`.
// Cost: O(n), n = size of `stmt` and the operand.
fn condition_modifier(modifier: StatementModifier, stmt: Stmt) -> Result<Stmt, RuntimeError> {
    let StatementModifier { kind, expr, .. } = modifier;
    // An attribute is composed with its class, so a modifier cannot gate it:
    // the declaration stays and the modifier only evaluates its operand.
    // `has $.g //= EXPR if C` is the declaration and its assignment, which
    // stay in the one flat block.
    if is_attribute_declaration(&stmt)
        && let Some(kind) = declaration_modifier(kind)
    {
        return Ok(crate::ast::attribute_modifier::expand(stmt, expr, kind));
    }
    match kind {
        ModifierKind::If | ModifierKind::Unless => {
            let is_unless = kind == ModifierKind::Unless;
            if is_pointy_block(&stmt) {
                return Ok(call_with_condition(stmt, expr, is_unless));
            }
            let stmt = rewrite_placeholder_block(stmt, &expr);
            // A declaration takes effect unconditionally; only its
            // initializer is gated.
            if let Some(split) = super::decl_modifier_split::conditional_declaration(&stmt, &expr, is_unless)? {
                return Ok(split);
            }
            let stmt = super::signature_expand::lower_pending(stmt)?;
            Ok(Stmt::If {
                cond: negate_if(expr, is_unless),
                then_branch: vec![stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless,
                with_kind: None,
            })
        }
        // `When` signals a match by raising `succeed`, which only an enclosing
        // topicalizer catches. Wrapping in a `given $_` gives it that catcher
        // without changing the topic, and composes with a following `given`
        // modifier, which re-topicalizes `$_` first.
        ModifierKind::When => Ok(Stmt::Given {
            topic: Expr::Var("_".to_string()),
            body: vec![Stmt::When {
                cond: expr,
                body: vec![super::signature_expand::lower_pending(stmt)?],
                is_statement_modifier: true,
            }],
            is_statement_modifier: true,
            with_kind: None,
        }),
        ModifierKind::With | ModifierKind::Without => {
            with_modifier(kind == ModifierKind::Without, expr, stmt)
        }
        _ => unreachable!("a loop modifier is not a condition modifier"),
    }
}

/// `S with T` is `given T { if .defined { S } }` (`!.defined` for `without`).
// Cost: O(n), n = size of `stmt` and the topic.
fn with_modifier(is_without: bool, topic: Expr, stmt: Stmt) -> Result<Stmt, RuntimeError> {
    let is_expression = matches!(stmt, Stmt::Expr(_));
    // A pointy block takes the tested value as its argument.
    let stmt = if is_pointy_block(&stmt) {
        match stmt {
            Stmt::Expr(closure) => call_with_topic(closure),
            other => other,
        }
    } else {
        stmt
    };
    // A bare block's placeholders take the tested value: the topic for
    // `with`, the written operand for `without`.
    let placeholder_value = if is_without {
        topic.clone()
    } else {
        Expr::Var("_".to_string())
    };
    let stmt = rewrite_placeholder_block(stmt, &placeholder_value);
    let defined = Expr::MethodCall {
        target: Box::new(Expr::Var("_".to_string())),
        name: Symbol::intern("defined"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
        sugar: false,
    };
    let test = negate_if(defined, is_without);
    let with_kind = if is_without {
        GivenWithKind::Without
    } else {
        GivenWithKind::With
    };
    let if_stmt = |stmt| Stmt::If {
        cond: test,
        then_branch: vec![stmt],
        else_branch: Vec::new(),
        binding_var: None,
        is_statement_modifier: true,
        is_unless: false,
        with_kind: None,
    };
    // An expression statement keeps its value: the conditional is a `do`.
    if is_expression {
        let given = Stmt::Given {
            topic,
            body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt(stmt))))],
            is_statement_modifier: true,
            with_kind: Some(with_kind),
        };
        return Ok(Stmt::Expr(Expr::DoStmt(Box::new(given))));
    }
    if let Some(expanded) = super::decl_modifier_split::topical_declaration(&stmt, &topic, is_without)? {
        return Ok(expanded);
    }
    let stmt = super::signature_expand::lower_pending(stmt)?;
    Ok(Stmt::Given {
        topic,
        body: vec![if_stmt(stmt)],
        is_statement_modifier: true,
        with_kind: Some(with_kind),
    })
}

/// `S for L`, `S while C`, `S until C`, `S given T`.
// Cost: O(n), n = size of `stmt` and the operand.
fn loop_modifier(modifier: StatementModifier, stmt: Stmt) -> Stmt {
    let StatementModifier { kind, expr, .. } = modifier;
    match kind {
        ModifierKind::For => for_modifier(expr, stmt),
        ModifierKind::While | ModifierKind::Until => {
            let is_until = kind == ModifierKind::Until;
            Stmt::While {
                cond: negate_if(expr, is_until),
                body: vec![while_body(stmt)],
                label: None,
                is_statement_modifier: true,
                is_until,
                is_bare_term: false,
            }
        }
        ModifierKind::Given => {
            let stmt = rewrite_placeholder_block(stmt, &expr);
            Stmt::Given {
                topic: expr,
                body: vec![stmt],
                is_statement_modifier: true,
                with_kind: None,
            }
        }
        _ => unreachable!("a condition modifier is not a loop modifier"),
    }
}

/// The declaration modifier an attribute keeps for a condition modifier.
// Cost: O(1).
fn declaration_modifier(
    kind: ModifierKind,
) -> Option<crate::ast::decl_modifier::DeclarationModifier> {
    use crate::ast::decl_modifier::DeclarationModifier as D;
    Some(match kind {
        ModifierKind::If => D::If,
        ModifierKind::Unless => D::Unless,
        ModifierKind::With => D::With,
        ModifierKind::Without => D::Without,
        ModifierKind::When => D::When,
        _ => return None,
    })
}

/// `expr`, or `!expr` when `negate` is set.
// Cost: O(1).
fn negate_if(expr: Expr, negate: bool) -> Expr {
    if negate {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(expr),
            word: false,
        }
    } else {
        expr
    }
}

/// A bare block to the left of `while`/`until` runs on each pass, and its value
/// is the pass's value, so it is a `do` block.
// Cost: O(1).
fn while_body(stmt: Stmt) -> Stmt {
    match stmt {
        Stmt::Block(body) => Stmt::Expr(Expr::DoStmt(Box::new(Stmt::Block(body)))),
        other => other,
    }
}

/// A bare block under a modifier binds its placeholders: the first takes
/// `value`, the rest are declared undefined.
// Cost: O(n), n = size of the block.
fn rewrite_placeholder_block(stmt: Stmt, value: &Expr) -> Stmt {
    let Stmt::Block(body) = stmt else {
        return stmt;
    };
    let placeholders = crate::ast::collect_placeholders_shallow(&body);
    if placeholders.is_empty() {
        return Stmt::Block(body);
    }
    let mut rewritten = Vec::with_capacity(placeholders.len() + body.len());
    for (idx, name) in placeholders.into_iter().enumerate() {
        rewritten.push(Stmt::VarDecl {
            marks: Default::default(),
            anonymous: false,
            name,
            expr: if idx == 0 {
                value.clone()
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
            has_initializer: false,
            constant: None,
            bind: None,
            where_constraint: None,
        });
    }
    rewritten.extend(body);
    Stmt::Block(rewritten)
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

/// `S for L`: a loop over `L` whose body is `S`.
// Cost: O(n), n = size of `stmt`.
fn for_modifier(iterable: Expr, stmt: Stmt) -> Stmt {
    let (hoisted_decl, stmt) = split_compound_decl(stmt);
    let no_signature =
        || -> ForParamShape { (None, Box::new(None), Vec::new(), Vec::new(), false, false) };
    let ((param, param_def, params, params_def, rw_block, explicit_zero_params), body) = match stmt
    {
        // `{ ... } for LIST` is the very same loop as `for LIST { ... }`,
        // so the bare block gives the loop its implicit placeholder
        // signature exactly as a `for LIST { ... }` body block does.
        Stmt::Block(_) => {
            let (param, params) = loop_params(&stmt);
            (
                (param, Box::new(None), params, Vec::new(), false, false),
                vec![stmt],
            )
        }
        // A Whatever-curried statement (`* + 1 for @a`) is an ordinary
        // expression evaluated once per iteration, so the loop yields one
        // WhateverCode per element and calls nothing (#13308); it falls
        // through to the generic arm below. A pointy block is the
        // statement itself and is called per element.
        Stmt::Expr(
            expr @ Expr::Lambda {
                is_whatever_code: false,
                ..
            },
        ) => (no_signature(), vec![call_with_topic(expr)]),
        // A routine (`sub (...) { ... }`, `method (...) { ... }`) is a value
        // like any other expression: the loop builds it once per element
        // and calls nothing (rakudo 2026.09).
        routine @ Stmt::Expr(Expr::AnonSubParams { declarator, .. }) if declarator.is_routine() => {
            (no_signature(), vec![routine])
        }
        // A multi-/zero-/slurpy-param signature written as a pointy block:
        // the block's signature is the loop's, so the loop consumes N
        // elements per iteration, as `for LIST -> SIG { ... }` does. The
        // implicit-`@_` bare block (`{ @_ } for LIST`) is invoked one
        // element at a time instead.
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
            (
                closure_signature_as_for_params(params, param_defs, is_rw),
                body,
            )
        }
        Stmt::Expr(expr @ Expr::AnonSubParams { .. }) => {
            (no_signature(), vec![call_with_topic(expr)])
        }
        other => (no_signature(), vec![other]),
    };
    let loop_stmt = Stmt::For {
        iterable,
        param,
        param_def,
        params,
        params_def,
        body,
        label: None,
        mode: ForMode::Normal,
        rw_block,
        explicit_zero_params,
        is_statement_modifier: true,
        uses_block_magic: false,
    };
    match hoisted_decl {
        // A recorded compound declaration hoists its record with it.
        Some(Stmt::SyntheticBlock(mut hoisted)) => {
            hoisted.push(loop_stmt);
            Stmt::SyntheticBlock(hoisted)
        }
        Some(declaration) => Stmt::SyntheticBlock(vec![declaration, loop_stmt]),
        None => loop_stmt,
    }
}

/// A pointy block `-> $x { ... }` used as the modified statement.
// Cost: O(1).
fn is_pointy_block(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Expr(Expr::Lambda {
            is_whatever_code: false,
            ..
        })
    )
}

/// `-> $x { ... } if C`: the pointy block is called with the value of `C`,
/// which is only evaluated once (rakudo 2026.09).
// Cost: O(1).
fn call_with_condition(closure: Stmt, cond: Expr, is_unless: bool) -> Stmt {
    let Stmt::Expr(closure) = closure else {
        return closure;
    };
    let test = negate_if(Expr::Var("_".to_string()), is_unless);
    let call = Stmt::If {
        cond: test,
        then_branch: vec![call_with_topic(closure)],
        else_branch: Vec::new(),
        binding_var: None,
        is_statement_modifier: true,
        is_unless: false,
        with_kind: None,
    };
    Stmt::Expr(Expr::DoStmt(Box::new(Stmt::Given {
        topic: cond,
        body: vec![Stmt::Expr(Expr::DoStmt(Box::new(call)))],
        is_statement_modifier: true,
        with_kind: None,
    })))
}

/// `CLOSURE($_)`: the per-element call of a closure under `for`.
// Cost: O(1).
fn call_with_topic(closure: Expr) -> Stmt {
    Stmt::Expr(Expr::CallOn {
        target: Box::new(closure),
        args: vec![Expr::Var("_".to_string())],
    })
}

/// The loop signature `S for LIST` gives `stmt` as its own: a bare block's
/// placeholders (`{ $^a ~ $^b } for ...`), none for any other statement.
// Cost: O(n), n = size of the block.
fn loop_params(stmt: &Stmt) -> (Option<String>, Vec<String>) {
    match stmt {
        Stmt::Block(body) => {
            crate::parser::placeholder_loop_params(body).unwrap_or((None, Vec::new()))
        }
        _ => (None, Vec::new()),
    }
}

/// A closure's own explicit signature in the shape `Stmt::For` uses: one
/// named parameter in the singular `param` slot, several (or a lone sigilled
/// slurpy) in the plural `params` slot, an explicit empty signature as
/// `explicit_zero_params`.
// Cost: O(p), p = parameters.
fn closure_signature_as_for_params(
    params: Vec<String>,
    param_defs: Vec<ParamDef>,
    is_rw: bool,
) -> ForParamShape {
    match params.len() {
        0 => (None, Box::new(None), Vec::new(), Vec::new(), is_rw, true),
        // A lone sigil'd slurpy (`*@all { ... } for @list`) binds a *list* of
        // the iteration chunk, so it needs the plural shape whose binder knows
        // about slurpies.
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

/// `my $x OP= EXPR for LIST`: the declaration belongs outside the loop while
/// the assignment runs once per item; otherwise the loop body would redeclare
/// and reset the variable on every iteration (`my $product *= $_ for @values`).
// Cost: O(s), s = statements of a synthetic block.
fn split_compound_decl(stmt: Stmt) -> (Option<Stmt>, Stmt) {
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
