//! The one lowering of the conditional family (ADR-10723 Stage 2): a
//! [`Conditional`] the parser built, or one read off a hand-built
//! `Statement::If` / `::Unless` / `::With` / `::Without`, becomes the `Stmt::If`
//! the compiler runs.
//!
//! This is the desugaring the parser used to do while it parsed: `unless` as an
//! `if` over a negated condition, `with` as a once-evaluated `.defined` test
//! with a topicalized body, each `elsif` / `orwith` nested in the `else` of the
//! clause before it, and a pointy block's parameters bound to the tested value.

use crate::ast::parsed::{
    BlockParams, ClauseKind, Conditional, ConditionalKind, ParsedBlock, ParsedStmt,
};
use crate::ast::{Expr, ParamDef, Stmt, WithBlockKind};
use crate::parser::{ElseClause, IfChainClause};
use crate::value::RuntimeError;

/// Lower a construct of a migrated statement family to the statement the
/// compiler runs.
// Cost: O(n), n = size of the construct (each block body is moved once).
pub(crate) fn lower_parsed(node: ParsedStmt) -> Result<Stmt, RuntimeError> {
    match node {
        ParsedStmt::Conditional(c) => lower(c),
        ParsedStmt::Given { topic, block } => {
            let body = match block.params {
                BlockParams::Topic { def: Some(def), .. } => {
                    crate::parser::given_pointy_body(*def, block.body)
                }
                _ => block.body,
            };
            Ok(Stmt::Given {
                topic,
                body,
                is_statement_modifier: false,
                with_kind: None,
            })
        }
        ParsedStmt::When { condition, block } => Ok(Stmt::When {
            cond: condition,
            body: block.body,
            is_statement_modifier: false,
        }),
        ParsedStmt::Default(block) => Ok(Stmt::Default(block.body)),
        ParsedStmt::BareBlock(block) => Ok(Stmt::Block(block.body)),
        ParsedStmt::Loop(l) => lower_loop(l),
        ParsedStmt::For { stmt, .. } => Ok(*stmt),
        ParsedStmt::DoesDecl { decl, role, init } => {
            let name = match decl.as_ref() {
                Stmt::VarDecl { name, .. } => name.clone(),
                _ => return Err(RuntimeError::new("does declaration without a variable")),
            };
            let init = init.map(|expr| Stmt::Assign {
                name: name.clone(),
                expr,
                op: crate::ast::AssignOp::Assign,
                target_is_sigilless: false,
            });
            Ok(crate::ast::var_does::expand(
                &name, *decl, "does", role, init,
            ))
        }
        ParsedStmt::WillDecl { decl, phasers } => {
            let name = match decl.as_ref() {
                Stmt::VarDecl { name, .. } => name.clone(),
                _ => return Err(RuntimeError::new("will declaration without a variable")),
            };
            Ok(crate::parser::wrap_with_will_phasers(*decl, &name, phasers))
        }
        ParsedStmt::DeclTail { head, tail } => Ok(crate::ast::decl_tail::expand(*head, tail)),
        ParsedStmt::PackageHeader { decl, name, header } => {
            Ok(crate::ast::package_header::wrap(*decl, &name, header))
        }
        ParsedStmt::TempDecl(decl) => {
            let name = match decl.as_ref() {
                Stmt::VarDecl { name, .. } => name.clone(),
                _ => return Err(RuntimeError::new("temp declaration without a variable")),
            };
            let save = crate::ast::temporize::save(&name, None, None, true);
            Ok(Stmt::SyntheticBlock(vec![*decl, save]))
        }
        ParsedStmt::BarePhaser(phaser) => Ok(*phaser),
        ParsedStmt::TakeLogicalTail { value, op, right } => {
            Ok(crate::ast::expand_take_logical_tail(value, op, right))
        }
        ParsedStmt::CommaDecls(declarations) => {
            let mut expanded = Vec::with_capacity(declarations.len());
            for declaration in declarations {
                match declaration {
                    Stmt::SyntheticBlock(parts) => expanded.extend(parts),
                    other => expanded.push(other),
                }
            }
            Ok(Stmt::SyntheticBlock(expanded))
        }
        ParsedStmt::ImmediateCall { decl, args } => {
            if !matches!(decl.as_ref(), Stmt::SubDecl { .. }) {
                return Err(RuntimeError::new("immediate call without a named sub"));
            }
            Ok(crate::ast::immediate_call::expand(*decl, args))
        }
        ParsedStmt::AttributeModifier {
            decl,
            condition,
            kind,
        } => Ok(crate::ast::attribute_modifier::expand(
            *decl, condition, kind,
        )),
        ParsedStmt::SigillessDecl(d) => {
            lower_sigilless(*d, crate::parser::is_user_declared_value_term)
        }
        ParsedStmt::BindDecl(decl) => Ok(crate::ast::bind_decl::expand(*decl)),
        ParsedStmt::Modified(m) => super::modifier_lower::lower(m),
        ParsedStmt::WhileLoop(w) => {
            let defs = match w.block.params {
                BlockParams::None => None,
                BlockParams::Topic { def, .. } => Some(def.into_iter().map(|d| *d).collect()),
                BlockParams::List(defs) => Some(defs),
            };
            let mut stmt = crate::parser::loop_pointy_clause(
                w.condition,
                defs,
                w.block.body,
                w.until,
                w.label,
            )?;
            if w.bare_term
                && let Stmt::While { is_bare_term, .. } = &mut stmt
            {
                *is_bare_term = true;
            }
            Ok(stmt)
        }
    }
}

/// Lower `node` to the statement the compiler runs.
// Cost: O(n), n = size of the conditional (each block body is moved once).
pub(crate) fn lower(node: Conditional) -> Result<Stmt, RuntimeError> {
    match node.kind {
        ConditionalKind::If => lower_if(node),
        ConditionalKind::Unless => {
            let (params, body) = block_parts(node.then)?;
            crate::parser::unless_clause(node.condition, params, body)
        }
        ConditionalKind::With | ConditionalKind::Without => lower_with(node),
    }
}

// Cost: O(n), n = size of the conditional.
fn lower_if(node: Conditional) -> Result<Stmt, RuntimeError> {
    let (params, body) = block_parts(node.then)?;
    let (binding_var, then_branch) = match params {
        Some(defs) => crate::parser::if_pointy_clause(defs, body)?,
        None => (None, body),
    };
    let mut clauses = vec![IfChainClause {
        cond: node.condition,
        then_branch,
        binding_var,
        with_kind: None,
    }];
    let (mut chain, else_clause) = lower_chain(node.clauses, node.else_block)?;
    clauses.append(&mut chain);
    crate::parser::lower_if_chain(clauses, else_clause)
}

/// `with` / `without`: the condition is evaluated once into a hidden temp,
/// tested with `.defined`, and topicalizes the body.
// Cost: O(n), n = size of the conditional.
fn lower_with(node: Conditional) -> Result<Stmt, RuntimeError> {
    let is_without = node.kind == ConditionalKind::Without;
    let tmp_name = crate::with_desugar::next_tmp_name();
    let tmp_var = Expr::Var(tmp_name.clone());
    let (name, def) = topic_param(&node.then.params)?;
    let with_body =
        crate::parser::with_then_branch(&node.condition, &tmp_var, &name, &def, node.then.body)?;
    let cond = crate::with_desugar::defined_condition(is_without, &tmp_name, node.condition);
    let else_branch = if !node.clauses.is_empty() {
        let (clauses, else_clause) = lower_chain(node.clauses, node.else_block)?;
        vec![crate::parser::lower_if_chain(clauses, else_clause)?]
    } else if let Some(block) = node.else_block {
        let (name, def) = topic_param(&block.params)?;
        crate::parser::with_then_branch(&tmp_var, &tmp_var, &name, &def, block.body)?
    } else {
        Vec::new()
    };
    Ok(Stmt::If {
        cond,
        then_branch: with_body,
        else_branch,
        binding_var: None,
        is_statement_modifier: false,
        is_unless: false,
        with_kind: Some(if is_without {
            WithBlockKind::Without
        } else {
            WithBlockKind::With
        }),
    })
}

/// The `elsif` / `orwith` clauses and the `else` block, as the clause list and
/// trailing `else` that `lower_if_chain` folds. An `else` after an `orwith`
/// runs under the value that `orwith` tested.
// Cost: O(n), n = size of the clauses and the else block.
fn lower_chain(
    clauses: Vec<crate::ast::parsed::ConditionalClause>,
    else_block: Option<ParsedBlock>,
) -> Result<(Vec<IfChainClause>, Option<ElseClause>), RuntimeError> {
    let mut lowered = Vec::with_capacity(clauses.len());
    let mut last_orwith: Option<Expr> = None;
    for clause in clauses {
        match clause.kind {
            ClauseKind::Elsif => {
                let (params, body) = block_parts(clause.then)?;
                let (binding_var, then_branch) = match params {
                    Some(defs) => crate::parser::if_pointy_clause(defs, body)?,
                    None => (None, body),
                };
                lowered.push(IfChainClause {
                    cond: clause.condition,
                    then_branch,
                    binding_var,
                    with_kind: None,
                });
                last_orwith = None;
            }
            ClauseKind::Orwith => {
                let tmp = crate::with_desugar::next_tmp_name();
                let tmp_var = Expr::Var(tmp.clone());
                let (name, def) = topic_param(&clause.then.params)?;
                let then_branch = crate::parser::with_then_branch(
                    &clause.condition,
                    &tmp_var,
                    &name,
                    &def,
                    clause.then.body,
                )?;
                last_orwith = Some(tmp_var);
                lowered.push(IfChainClause {
                    cond: crate::with_desugar::defined_condition(false, &tmp, clause.condition),
                    then_branch,
                    binding_var: None,
                    with_kind: Some(WithBlockKind::Orwith),
                });
            }
        }
    }
    let Some(block) = else_block else {
        return Ok((lowered, None));
    };
    let (binding_params, mut body) = block_parts(block)?;
    let binding_params = match last_orwith {
        Some(topic) => {
            // The `else` of an `orwith` topicalizes the last tested value
            // through `given`, like the clause itself.
            let mut defs = binding_params.unwrap_or_default();
            if defs.len() > 1 {
                return Err(RuntimeError::new(
                    "an else after orwith takes a single pointy parameter",
                ));
            }
            let def = defs.pop();
            let name = def.as_ref().map(|def| def.name.clone());
            body = crate::parser::with_then_branch(&topic, &topic, &name, &def, body)?;
            None
        }
        None => binding_params,
    };
    Ok((
        lowered,
        Some(ElseClause {
            binding_params,
            body,
        }),
    ))
}

/// A block's parameter list (`None` for a plain block) and its body.
// Cost: O(1) (the body is moved).
fn block_parts(block: ParsedBlock) -> Result<(Option<Vec<ParamDef>>, Vec<Stmt>), RuntimeError> {
    let params = match block.params {
        BlockParams::None => None,
        BlockParams::List(defs) => Some(defs),
        BlockParams::Topic { def, .. } => Some(def.into_iter().map(|def| *def).collect()),
    };
    Ok((params, block.body))
}

/// The single topic parameter of a `with`-family block: its name and
/// declaration, both `None` for a plain block or `-> { }`.
// Cost: O(p), p = size of the parameter.
fn topic_param(params: &BlockParams) -> Result<(Option<String>, Option<ParamDef>), RuntimeError> {
    match params {
        BlockParams::None => Ok((None, None)),
        BlockParams::Topic { name, def } => Ok((name.clone(), def.as_deref().cloned())),
        BlockParams::List(defs) => match defs.as_slice() {
            [] => Ok((None, None)),
            [def] => Ok((Some(def.name.clone()), Some(def.clone()))),
            _ => Err(RuntimeError::new(
                "RakuAST: with-family pointy block requires one parameter",
            )),
        },
    }
}

/// `loop` keeps its header; `repeat` holds its pointy parameter in the loop's
/// `init` and runs `until` as `while` over a negated condition.
// Cost: O(1) (the parts are moved).
fn lower_loop(l: crate::ast::parsed::LoopStmt) -> Result<Stmt, RuntimeError> {
    use crate::ast::parsed::LoopKind;
    if l.kind == LoopKind::Loop {
        return Ok(Stmt::Loop {
            init: l.init,
            cond: l.condition,
            step: l.step,
            body: l.block.body,
            repeat: false,
            label: l.label,
            is_until: false,
            is_bare_term: l.bare_term,
        });
    }
    let until = l.kind == LoopKind::RepeatUntil;
    let param = match l.block.params {
        BlockParams::Topic { name, .. } => name,
        _ => None,
    };
    let (init, step) = crate::parser::repeat_param_clause(param);
    let cond = l.condition.map(|cond| {
        if until {
            Expr::Unary {
                op: crate::token_kind::TokenKind::Bang,
                expr: Box::new(cond),
                word: false,
            }
        } else {
            cond
        }
    });
    Ok(Stmt::Loop {
        init,
        cond,
        step,
        body: l.block.body,
        repeat: true,
        label: l.label,
        is_until: until,
        is_bare_term: l.bare_term,
    })
}

/// Lower a sigilless declaration. `is_value_term` says whether a bareword
/// names a declared sigilless value term: the parser asks its own scopes, the
/// RakuAST lowering the terms the unit declares.
// Cost: O(n), n = size of the initializer.
pub(crate) fn lower_sigilless(
    d: crate::ast::parsed::SigillessSource,
    is_value_term: fn(&str) -> bool,
) -> Result<Stmt, RuntimeError> {
    use crate::ast::parsed::SigillessForm;
    Ok(match d.form {
        SigillessForm::CallAssign => crate::ast::sigilless_decl::call_assign_expansion(
            d.name,
            d.expr,
            d.type_constraint,
            d.is_state,
            d.is_our,
        ),
        form => crate::parser::build_sigilless_bind_stmt(
            d.name,
            d.expr,
            d.type_constraint,
            d.is_state,
            d.is_our,
            form == SigillessForm::Assign,
            is_value_term,
        ),
    })
}
