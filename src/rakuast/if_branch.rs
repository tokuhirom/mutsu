//! The branch lowering of `if` / `unless` / `else -> $x`: a pointy clause's
//! parameters bound to the tested value, an `elsif` / `orwith` chain folded into
//! nested `Stmt::If`s, and `unless` as an `if` over a negated condition.
//! `conditional_lower` is the one caller.

use crate::ast::{Expr, ParamDef, Stmt, WithBlockKind};
use crate::parser::simple_pointy_bind;
use crate::token_kind::TokenKind;
use std::sync::atomic::{AtomicUsize, Ordering};

static IF_BIND_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub(crate) struct IfChainClause {
    pub(crate) cond: Expr,
    pub(crate) then_branch: Vec<Stmt>,
    pub(crate) binding_var: Option<String>,
    /// `Some(WithBlockKind::Orwith)` for an `orwith` clause, which desugars
    /// into the same `.defined` conditional an `elsif` would but which raku
    /// models as its own `Statement::Orwith`. See `Stmt::If`'s `with_kind`.
    pub(crate) with_kind: Option<WithBlockKind>,
}

pub(crate) struct ElseClause {
    pub(crate) binding_params: Option<Vec<ParamDef>>,
    pub(crate) body: Vec<Stmt>,
}

fn is_simple_if_binding(param: &ParamDef) -> bool {
    param.traits.is_empty()
        && param.shape_constraints.is_none()
        && !param.named
        && !param.slurpy
        && !param.double_slurpy
        // `+@a` (the single-argument rule) is a slurpy too: it must reach the
        // real signature binder rather than the simple `my @a := COND`
        // desugar, which cannot apply the one-arg rule.
        && !param.onearg
        && param.default.is_none()
        && !param.optional_marker
        && param.type_constraint.is_none()
        && param.sub_signature.is_none()
        && param.outer_sub_signature.is_none()
        && param.code_signature.is_none()
}

fn next_if_bind_tmp_name() -> String {
    let tmp_idx = IF_BIND_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("$__mutsu_if_bind_{tmp_idx}")
}

/// The expansion of `if COND -> PARAMS { BODY }`: the `binding_var` the
/// compiler binds the condition to, and the then-branch. `rakuast::lower`
/// calls it too, so a hand-built `Statement::If` over a `PointyBlock` expands
/// the same way.
// Cost: O(n), n = size of the body.
pub(crate) fn if_pointy_clause(
    param_defs: Vec<ParamDef>,
    then_branch: Vec<Stmt>,
) -> Result<(Option<String>, Vec<Stmt>), crate::value::RuntimeError> {
    if param_defs.is_empty() {
        return Ok((None, then_branch));
    }
    Ok(expand_if_pointy(param_defs, then_branch))
}

fn expand_if_pointy(
    param_defs: Vec<ParamDef>,
    then_branch: Vec<Stmt>,
) -> (Option<String>, Vec<Stmt>) {
    if param_defs.len() == 1 && is_simple_if_binding(&param_defs[0]) {
        // A sigilless pointy (`if EXPR -> \r { }`) is marked with a leading
        // `\` so the compiler binds the value itself (no scalar itemization)
        // and resolves the name as a bare word — see
        // `Compiler::compile_if_binding_decl`.
        let p = &param_defs[0];
        if !p.sigilless && p.name.trim_start_matches('$') == "_" {
            // `if COND -> $_ { BODY }` binds a FRESH topic for the block, not an
            // ordinary lexical: declaring it as one (`my $_ = COND`) writes the
            // enclosing scope's topic slot and leaves the bound value behind
            // after the branch. Evaluate the condition once into a hidden temp
            // and topicalize the body through `given`, whose own topic opcodes
            // establish and restore the scope — the same reason the `orwith`
            // arm below lowers through `given`.
            let source_binding = next_if_bind_tmp_name();
            let source_expr = Expr::Var(source_binding.trim_start_matches('$').to_string());
            return (
                Some(source_binding),
                vec![Stmt::Given {
                    topic: source_expr,
                    body: then_branch,
                    is_statement_modifier: false,
                    with_kind: None,
                }],
            );
        }
        let name = if p.sigilless {
            format!("\\{}", p.name)
        } else {
            p.name.clone()
        };
        return (Some(name), then_branch);
    }

    let source_binding = next_if_bind_tmp_name();
    let source_expr = Expr::Var(source_binding.trim_start_matches('$').to_string());
    // The condition is ONE argument. `if (1, 2) -> $a, $b` is "expected 2
    // arguments but got 1" in rakudo, not a two-way bind -- the clause receives
    // the condition value itself, and it is the *signature* that decides what
    // to do with it. Slipping it (`|$tmp`) made a list condition bind several
    // parameters, which no source ever asked for.
    //
    // The slurpy spellings in `roast/S04-statements/if.t` all follow from this
    // one rule rather than needing their own: `*@a` flattens the single list
    // argument, `**@a` keeps it whole, `+@a` applies the one-argument rule to
    // it. `**@a` used to be special-cased here for exactly that reason.
    let args = vec![source_expr];
    let call_expr = Expr::CallOn {
        target: Box::new(Expr::AnonSubParams {
            params: param_defs.iter().map(|p| p.name.clone()).collect(),
            param_defs,
            return_type: None,
            body: then_branch,
            is_rw: false,
            is_raw: false,
            custom_traits: Default::default(),
            is_whatever_code: false,
            declarator: crate::ast::RoutineDeclarator::PointyBlock,
        }),
        args,
    };
    (Some(source_binding), vec![Stmt::Expr(call_expr)])
}

fn ensure_last_clause_binding_var(clauses: &mut [IfChainClause]) -> Option<String> {
    let last_clause = clauses.last_mut()?;
    Some(if let Some(existing) = &last_clause.binding_var {
        existing.clone()
    } else {
        let generated = next_if_bind_tmp_name();
        last_clause.binding_var = Some(generated.clone());
        generated
    })
}

/// Read the value the last clause's binding variable holds.
///
/// `binding_var` keeps the declaration's own spelling, and a sigilless
/// `if COND -> \\a { }` is recorded as `\\a` — so the `\\` has to come off before
/// the name is read, and the read itself is the bare word a sigilless binding
/// is spelled as. Stripping only `$` left `Expr::Var("\\a")`, a name nothing
/// declares, so `if 0 -> \\a { } else -> $x { }` handed the else clause Nil
/// instead of the condition value.
fn binding_var_read(source_binding: &str) -> Expr {
    match source_binding.strip_prefix('\\') {
        Some(bare) => Expr::BareWord(bare.to_string()),
        None => Expr::Var(source_binding.trim_start_matches('$').to_string()),
    }
}

fn lower_else_binding(
    source_binding: &str,
    else_clause: ElseClause,
) -> Result<Vec<Stmt>, crate::value::RuntimeError> {
    Ok(expand_else_binding(source_binding, else_clause))
}

fn expand_else_binding(source_binding: &str, else_clause: ElseClause) -> Vec<Stmt> {
    let Some(param_defs) = else_clause.binding_params else {
        return else_clause.body;
    };
    if param_defs.is_empty() {
        return else_clause.body;
    }
    if param_defs.len() == 1 && is_simple_if_binding(&param_defs[0]) {
        if param_defs[0].name.trim_start_matches('$') == "_" && !param_defs[0].sigilless {
            // `else -> $_ { }` topicalizes through `given` for the same reason
            // as the then-branch form — see `lower_if_clause_binding`.
            return vec![Stmt::Given {
                topic: binding_var_read(source_binding),
                body: else_clause.body,
                is_statement_modifier: false,
                with_kind: None,
            }];
        }
        let mut body = Vec::with_capacity(else_clause.body.len() + 1);
        body.push(simple_pointy_bind(
            &param_defs[0].name,
            &binding_var_read(source_binding),
            param_defs[0].sigilless,
        ));
        body.extend(else_clause.body);
        return body;
    }

    let call_expr = Expr::CallOn {
        target: Box::new(Expr::AnonSubParams {
            params: param_defs.iter().map(|p| p.name.clone()).collect(),
            param_defs,
            return_type: None,
            body: else_clause.body,
            is_rw: false,
            is_raw: false,
            custom_traits: Default::default(),
            is_whatever_code: false,
            declarator: crate::ast::RoutineDeclarator::PointyBlock,
        }),
        // One argument, as in `lower_if_clause_binding` -- `else -> ...` shares
        // the rule.
        args: vec![binding_var_read(source_binding)],
    };
    vec![Stmt::Expr(call_expr)]
}

pub(crate) fn lower_if_chain(
    mut clauses: Vec<IfChainClause>,
    else_clause: Option<ElseClause>,
) -> Result<Stmt, crate::value::RuntimeError> {
    let mut else_branch = if let Some(else_clause) = else_clause {
        let mut body = lower_else_clause(&mut clauses, else_clause)?;
        // An explicit `else {}` with an empty body should evaluate to Nil,
        // not be indistinguishable from a missing else clause.
        if body.is_empty() {
            body.push(Stmt::Expr(Expr::Literal(crate::value::Value::NIL)));
        }
        body
    } else {
        Vec::new()
    };

    while let Some(clause) = clauses.pop() {
        else_branch = vec![Stmt::If {
            cond: clause.cond,
            then_branch: clause.then_branch,
            else_branch,
            binding_var: clause.binding_var,
            is_statement_modifier: false,
            is_unless: false,
            with_kind: clause.with_kind,
        }];
    }

    else_branch
        .pop()
        .ok_or_else(|| crate::value::RuntimeError::new("if chain must have at least one clause"))
}

fn lower_else_clause(
    clauses: &mut [IfChainClause],
    else_clause: ElseClause,
) -> Result<Vec<Stmt>, crate::value::RuntimeError> {
    if else_clause.binding_params.is_none() {
        return Ok(else_clause.body);
    }
    let Some(source_binding) = ensure_last_clause_binding_var(clauses) else {
        return Ok(else_clause.body);
    };
    lower_else_binding(&source_binding, else_clause)
}

/// Expand the written unless clause through the same conditional binder.
// Cost: O(n), n = size of the signature and body.
pub(crate) fn unless_clause(
    cond: Expr,
    binding_params: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
) -> Result<Stmt, crate::value::RuntimeError> {
    expand_unless_clause(cond, binding_params, body)
}

fn expand_unless_clause(
    cond: Expr,
    binding_params: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
) -> Result<Stmt, crate::value::RuntimeError> {
    // `unless COND -> $x { BODY }` binds the condition's OWN value (rakudo:
    // `unless 0 -> $_ { $_.say }` prints `0`, not the negation). Lower it as
    // the *else* branch of an un-negated `if`, which is exactly the machinery
    // `if COND { } else -> $x { }` already uses to hand the else clause the
    // condition value — rather than negating the condition and binding that.
    if let Some(params) = binding_params {
        let clauses = vec![IfChainClause {
            cond,
            then_branch: Vec::new(),
            binding_var: None,
            with_kind: None,
        }];
        let else_clause = ElseClause {
            binding_params: Some(params),
            body,
        };
        return lower_if_chain(clauses, Some(else_clause));
    }
    Ok(Stmt::If {
        cond: Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond),
            word: false,
        },
        then_branch: body,
        else_branch: Vec::new(),
        binding_var: None,
        is_statement_modifier: false,
        is_unless: true,
        with_kind: None,
    })
}
