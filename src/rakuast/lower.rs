//! RakuAST node tree → internal AST (`Stmt`/`Expr`) — the write direction that
//! backs `EVAL($rakuast)` (ADR-0011 Phase 5). The lowered AST is fed to the
//! existing compiler; there is no second execution engine.
//!
//! Slice 1 covers the literal cluster (and the `StatementList` /
//! `Statement::Expression` wrappers around it). Constructs outside that set
//! produce an explicit `RuntimeError` (the documented coverage boundary).

use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::{
    ContextKind, EnumVariantForm, Expr, GivenWithKind, ParamDef, Stmt, WithBlockKind,
};
use crate::regex_tree::{RegexNode, RegexTree};
use crate::value::{RegexAdverbs, RuntimeError, Value, ValueView};
use std::sync::Arc;

type LoweredEnumVariants = (Vec<(String, Option<Expr>)>, EnumVariantForm);

pub(super) fn unsupported(node: &RakuAstNode) -> RuntimeError {
    RuntimeError::new(format!(
        "RakuAST: EVAL does not yet support lowering `{}`",
        node.class.printed_name()
    ))
}

/// Lower a top-level RakuAST node to a statement list. A bare expression node
/// (e.g. `EVAL(RakuAST::IntLiteral.new(42))`) becomes a single expression
/// statement.
pub fn lower(node: &RakuAstNode) -> Result<Vec<Stmt>, RuntimeError> {
    let mut stmts = lower_stmts(node)?;
    // ADR-0033 Phase 3. A lowered tree carries `Expr::WhateverArg` leaves but no
    // priming *scopes*: those are planted by the parser at its own grammar
    // positions, and there is no parser here. Run the same scope authority the
    // parser's output goes through, in the mode that plants every scope rather
    // than only the thunk-barrier ones, so a `WhateverCode::Argument` tree —
    // hand-built or read back from `.AST` — becomes the same closure the
    // equivalent source does.
    crate::whatever_curry::with_all_scopes(|| {
        crate::whatever_curry::mark::mark_program(&mut stmts)
    });
    Ok(stmts)
}

pub(super) fn lower_stmts(node: &RakuAstNode) -> Result<Vec<Stmt>, RuntimeError> {
    match node.class {
        RakuAstClass::CompUnit => lower_stmts(named_child(node, "statement-list")?),
        RakuAstClass::StatementList => {
            let mut stmts = Vec::with_capacity(node.fields.len());
            for f in &node.fields {
                stmts.push(lower_stmt(child_node(&f.value)?)?);
            }
            Ok(stmts)
        }
        _ => Ok(vec![lower_stmt(node)?]),
    }
}

fn lower_stmt(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match node.class {
        // A declaration wrapped in Statement::Expression lowers to its own
        // statement (a `my $x = …` is a `Stmt::VarDecl`, not a `Stmt::Expr`).
        RakuAstClass::StatementAlso => super::role::lower_also(node),
        RakuAstClass::StatementExpression => {
            let statement = lower_stmt_inner(named_child(node, "expression")?)?;
            if let Some(modifier) = node.fields.iter().find(|f| f.name == Some("loop-modifier")) {
                let modifier = child_node(&modifier.value)?;
                if modifier.class == RakuAstClass::StatementModifierGiven {
                    return Ok(Stmt::Given {
                        topic: lower_expr(named_child_or_positional(modifier)?)?,
                        body: vec![statement],
                        is_statement_modifier: true,
                        with_kind: None,
                    });
                }
                return Err(unsupported(modifier));
            }
            // A postfix `if`/`unless`: raku hangs the condition off the modified
            // statement rather than wrapping it in a `Statement::If`. mutsu
            // models it as an `If` whose `is_statement_modifier` is set (so its
            // branch is not a block literal) — and, for `unless`, whose
            // condition carries the parser's `!`.
            if let Some(modifier) = node
                .fields
                .iter()
                .find(|f| f.name == Some("condition-modifier"))
            {
                let modifier = child_node(&modifier.value)?;
                // `with`/`without` are condition modifiers too, but they
                // topicalize: mutsu spells them as the `given` desugar the
                // parser builds, tagged with `with_kind` so the converter can
                // read the keyword back out.
                if let Some(kind) = match modifier.class {
                    RakuAstClass::StatementModifierWith => Some(GivenWithKind::With),
                    RakuAstClass::StatementModifierWithout => Some(GivenWithKind::Without),
                    _ => None,
                } {
                    return Ok(lower_with_modifier(
                        kind,
                        lower_expr(named_child_or_positional(modifier)?)?,
                        statement,
                    ));
                }
                let is_unless = match modifier.class {
                    RakuAstClass::StatementModifierIf => false,
                    RakuAstClass::StatementModifierUnless => true,
                    _ => return Err(unsupported(modifier)),
                };
                let cond = lower_expr(named_child_or_positional(modifier)?)?;
                return Ok(Stmt::If {
                    cond: negate_if(cond, is_unless),
                    then_branch: vec![statement],
                    else_branch: Vec::new(),
                    binding_var: None,
                    is_statement_modifier: true,
                    is_unless,
                    with_kind: None,
                });
            }
            Ok(statement)
        }
        _ => lower_stmt_inner(node),
    }
}

/// Rebuild the `given TOPIC { if $_.defined { STMT } }` shape that mutsu's
/// parser produces for `STMT with TOPIC` (condition negated for `without`),
/// carrying the `with_kind` tag the converter reads back. Keeping the lowered
/// form identical to the parsed one is what makes the round trip stable.
fn lower_with_modifier(kind: GivenWithKind, topic: Expr, statement: Stmt) -> Stmt {
    let defined = Expr::MethodCall {
        target: Box::new(Expr::Var("_".to_string())),
        name: crate::symbol::Symbol::intern("defined"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    };
    let cond = if matches!(kind, GivenWithKind::Without) {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Bang,
            expr: Box::new(defined),
        }
    } else {
        defined
    };
    Stmt::Given {
        topic,
        body: vec![Stmt::If {
            cond,
            then_branch: vec![statement],
            else_branch: Vec::new(),
            binding_var: None,
            is_statement_modifier: true,
            is_unless: false,
            with_kind: None,
        }],
        is_statement_modifier: true,
        with_kind: Some(kind),
    }
}

fn lower_stmt_inner(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match node.class {
        RakuAstClass::VarDeclarationSimple => lower_var_decl(node),
        RakuAstClass::VarDeclarationConstant => lower_constant(node),
        RakuAstClass::StatementIf => lower_if(node),
        // `with X { … }` / `without X { … }`. Both rebuild the conditional the
        // parser desugars them into, tagged so a round trip renders the same
        // node again. `without` names its block `body`, not `then`, and takes
        // no continuation clauses.
        RakuAstClass::StatementWith => lower_with_block(node, WithBlockKind::With),
        RakuAstClass::StatementWithout => lower_with_block(node, WithBlockKind::Without),
        // `unless C { … }`. mutsu stores it as a negated condition plus the
        // `is_unless` flag, which is what the converter reads back, so the
        // lowerer has to re-plant both. raku's node names the block `body`
        // (not `then`) and cannot carry `elsif`/`else`.
        RakuAstClass::StatementUnless => Ok(Stmt::If {
            cond: negate_if(lower_expr(named_child(node, "condition")?)?, true),
            then_branch: lower_block(named_child(node, "body")?)?,
            else_branch: Vec::new(),
            binding_var: None,
            is_statement_modifier: false,
            is_unless: true,
            with_kind: None,
        }),
        RakuAstClass::StatementLoopWhile | RakuAstClass::StatementLoopUntil => lower_while(node),
        RakuAstClass::StatementLoop => lower_cstyle_loop(node),
        // `repeat { … } while/until C` runs the body once before testing the
        // condition (`until` desugars to `while !C`, handled by the prefix `!`).
        // `repeat { … } while/until C`. mutsu stores an `until` as a negated
        // condition plus the flag, which is what the converter reads back, so
        // the lowerer has to re-plant both.
        RakuAstClass::StatementLoopRepeatWhile | RakuAstClass::StatementLoopRepeatUntil => {
            let is_until = node.class == RakuAstClass::StatementLoopRepeatUntil;
            let cond = lower_expr(named_child(node, "condition")?)?;
            Ok(Stmt::Loop {
                init: None,
                cond: Some(negate_if(cond, is_until)),
                step: None,
                body: lower_block(named_child(node, "body")?)?,
                repeat: true,
                label: None,
                is_until,
            })
        }
        RakuAstClass::StatementFor => lower_for(node),
        // `INIT { … }` / `LEAVE { … }` / … -> `Stmt::Phaser`, one class per kind.
        // `BEGIN` is absent deliberately — see `lower_phaser`.
        RakuAstClass::StatementPrefixPhaserBegin
        | RakuAstClass::StatementPrefixPhaserCheck
        | RakuAstClass::StatementPrefixPhaserInit
        | RakuAstClass::StatementPrefixPhaserEnd
        | RakuAstClass::StatementPrefixPhaserEnter
        | RakuAstClass::StatementPrefixPhaserLeave
        | RakuAstClass::StatementPrefixPhaserKeep
        | RakuAstClass::StatementPrefixPhaserUndo
        | RakuAstClass::StatementPrefixPhaserFirst
        | RakuAstClass::StatementPrefixPhaserNext
        | RakuAstClass::StatementPrefixPhaserLast
        | RakuAstClass::StatementPrefixPhaserQuit
        | RakuAstClass::StatementPrefixPhaserClose => lower_phaser(node),
        RakuAstClass::Class => lower_class(node),
        RakuAstClass::Grammar => lower_grammar(node),
        RakuAstClass::RegexDeclaration
        | RakuAstClass::TokenDeclaration
        | RakuAstClass::RuleDeclaration => lower_regex_declaration(node),
        RakuAstClass::Role => super::role::lower(node),
        RakuAstClass::Method | RakuAstClass::Submethod => lower_method(node),
        RakuAstClass::Module | RakuAstClass::Package => lower_package(node),
        RakuAstClass::TypeEnum => lower_enum(node),
        RakuAstClass::TypeSubset => lower_subset(node),
        // `CATCH { … }` — the `exception`/topic flags on its body block are
        // implied by the statement class, so only the block's statements are
        // read back.
        RakuAstClass::StatementCatch => Ok(Stmt::Catch(lower_block(named_child(node, "body")?)?)),
        RakuAstClass::VarDeclarationSignature => super::signature_decl::lower(node),
        // `use` / `no` statements (see `use_stmt`).
        RakuAstClass::Pragma => super::use_stmt::lower_pragma(node),
        RakuAstClass::StatementUse => super::use_stmt::lower_use(node),
        RakuAstClass::StatementLanguageVersion => super::use_stmt::lower_language_version(node),
        // A bare block in statement position runs once, here and now: it is
        // the parser's `Stmt::Block`, not a closure value. One that takes
        // arguments (placeholders, `@_`, `%_`) is a closure value even in
        // statement position, so it keeps the expression path.
        RakuAstClass::Block => {
            let body = lower_block(node)?;
            if crate::ast::collect_placeholders_shallow(&body).is_empty()
                && !crate::ast::body_reads_args_array(&body)
                && !crate::ast::body_reads_args_hash(&body)
            {
                Ok(Stmt::Block(body))
            } else {
                Ok(Stmt::Expr(lower_expr(node)?))
            }
        }
        // A named `sub f { … }` is a declaration; a nameless one (`sub ($x) { … }`,
        // `sub { … }`) is a closure *value*, so it lowers through the expression
        // path instead.
        RakuAstClass::Sub if node.fields.iter().any(|f| f.name == Some("name")) => lower_sub(node),
        // `given`/`when`/`default` — a `when`/`default` sits directly (not
        // Statement::Expression-wrapped) in the enclosing `given` block, so it
        // reaches this dispatch unwrapped.
        RakuAstClass::StatementGiven => Ok(Stmt::Given {
            topic: lower_expr(named_child(node, "source")?)?,
            body: lower_block(named_child(node, "body")?)?,
            is_statement_modifier: false,
            with_kind: None,
        }),
        RakuAstClass::StatementWhen => Ok(Stmt::When {
            cond: lower_expr(named_child(node, "condition")?)?,
            body: lower_block(named_child(node, "body")?)?,
            is_statement_modifier: false,
        }),
        RakuAstClass::StatementDefault => {
            Ok(Stmt::Default(lower_block(named_child(node, "body")?)?))
        }
        // `$x OP= EXPR` is represented by a `MetaInfix::Assign` child. Keep the
        // source-level marker while reusing the parser's existing execution
        // expansion.
        RakuAstClass::ApplyInfix if infix_is_compound_assignment(node) => {
            Ok(Stmt::Expr(lower_compound_assign_expr(node)?))
        }
        // `$x = EXPR` is an `ApplyInfix` whose infix is an `Assignment` node; it is
        // a `Stmt::Assign`, not a general binary expression.
        RakuAstClass::ApplyInfix if infix_is_assignment(node) => match subscript_assign(node)? {
            Some(assign) => Ok(Stmt::Expr(assign)),
            None => lower_assign(node),
        },
        // `$x := EXPR` is an `ApplyInfix` with a plain `:=` infix -- the
        // parser's `Stmt::Assign` with a `Bind` op.
        RakuAstClass::ApplyInfix if infix_is_bind_to_variable(node) => {
            let (name, expr) = lower_assign_parts(node)?;
            Ok(Stmt::Assign {
                name,
                expr,
                op: crate::ast::AssignOp::Bind,
                target_is_sigilless: false,
            })
        }
        // The listop I/O calls (`say`/`put`/`print`/`note`) are their own
        // statements in the internal AST.
        RakuAstClass::CallName if call_name_stash(node).is_some() => {
            Ok(Stmt::Expr(lower_expr(node)?))
        }
        RakuAstClass::CallName | RakuAstClass::CallNameWithoutParentheses => {
            let name = call_name_str(node)?;
            let args = arg_exprs(node)?;
            match name.as_str() {
                "say" => Ok(Stmt::Say(args)),
                "put" => Ok(Stmt::Put(args)),
                "print" => Ok(Stmt::Print(args)),
                "note" => Ok(Stmt::Note(args)),
                // `return`/`last`/`next` are modelled as bare calls in RakuAST but
                // are control-flow statements in the internal AST.
                "return" => Ok(Stmt::Return(
                    args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL)),
                )),
                "last" => Ok(Stmt::Last(None)),
                "next" => Ok(Stmt::Next(None)),
                "redo" => Ok(Stmt::Redo(None)),
                "die" => Ok(Stmt::Die(
                    args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL)),
                )),
                "fail" => Ok(Stmt::Fail(
                    args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL)),
                )),
                "take" => Ok(Stmt::Take(
                    args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL)),
                    false,
                )),
                _ => Ok(Stmt::Expr(lower_expr(node)?)),
            }
        }
        _ => Ok(Stmt::Expr(lower_expr(node)?)),
    }
}

/// Lower `if COND { … } elsif … { … } else { … }` to `Stmt::If`. Each `elsif`
/// clause becomes a nested `Stmt::If` in the enclosing `else` branch, folded
/// innermost-last so the source order is preserved.
fn lower_if(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let cond = lower_expr(named_child(node, "condition")?)?;
    let then_branch = lower_block(named_child(node, "then")?)?;
    Ok(Stmt::If {
        cond,
        then_branch,
        else_branch: lower_conditional_chain(node, None)?,
        binding_var: None,
        is_statement_modifier: false,
        is_unless: false,
        with_kind: None,
    })
}

/// Lower `with COND { … }` / `without COND { … }` to the conditional mutsu's
/// parser desugars them into (`with_desugar`), so execution reuses the existing
/// path and the converter renders the same node back.
fn lower_with_block(node: &RakuAstNode, kind: WithBlockKind) -> Result<Stmt, RuntimeError> {
    let cond_expr = lower_expr(named_child(node, "condition")?)?;
    let tmp_name = crate::with_desugar::next_tmp_name();
    let (body_field, else_branch) = if matches!(kind, WithBlockKind::Without) {
        // rakudo rejects `without … else` at compile time, so a node carrying
        // one is not a shape it could have produced.
        if node
            .fields
            .iter()
            .any(|f| f.name == Some("else") || f.name == Some("elsifs"))
        {
            return Err(unsupported(node));
        }
        ("body", Vec::new())
    } else {
        // The `else` of a `with` continues a topicalizing clause, so it runs
        // under the last tested value -- the hidden temp when no `orwith`
        // intervened.
        (
            "then",
            lower_conditional_chain(node, Some(Expr::Var(tmp_name.clone())))?,
        )
    };
    let body = lower_block(named_child(node, body_field)?)?;
    Ok(crate::with_desugar::with_conditional(
        kind,
        &tmp_name,
        cond_expr,
        body,
        else_branch,
    ))
}

/// The `else` branch of a conditional: its `elsifs` clauses folded
/// innermost-last into nested `if`s, with the `else` block at the bottom.
///
/// `topic` is the value the head clause tested when that clause topicalizes
/// (`with`), because a trailing `else` runs under the *last* tested value --
/// which an intervening `orwith` replaces and a plain `elsif` clears.
fn lower_conditional_chain(
    node: &RakuAstNode,
    topic: Option<Expr>,
) -> Result<Vec<Stmt>, RuntimeError> {
    let clauses = match node.fields.iter().find(|f| f.name == Some("elsifs")) {
        Some(field) => match &field.value {
            RakuAstFieldValue::List(items) => items.as_slice(),
            _ => return Err(unsupported(node)),
        },
        None => &[],
    };
    let mut lowered = Vec::with_capacity(clauses.len());
    let mut else_topic = topic;
    for item in clauses {
        let ValueView::RakuAst(clause) = item.view() else {
            return Err(unsupported(node));
        };
        let cond = lower_expr(named_child(clause, "condition")?)?;
        let body = lower_block(named_child(clause, "then")?)?;
        match clause.class {
            RakuAstClass::StatementElsif => else_topic = None,
            RakuAstClass::StatementOrwith => else_topic = Some(cond.clone()),
            _ => return Err(unsupported(clause)),
        }
        lowered.push((clause.class, cond, body));
    }

    let mut else_branch = match node.fields.iter().find(|f| f.name == Some("else")) {
        Some(_) => {
            let body = lower_block(named_child(node, "else")?)?;
            match else_topic {
                Some(topic) => vec![crate::with_desugar::topic_given(topic, body)],
                None => body,
            }
        }
        None => Vec::new(),
    };
    for (class, cond, body) in lowered.into_iter().rev() {
        else_branch = if class == RakuAstClass::StatementOrwith {
            vec![crate::with_desugar::orwith_conditional(
                cond,
                body,
                else_branch,
            )]
        } else {
            vec![Stmt::If {
                cond,
                then_branch: body,
                else_branch,
                binding_var: None,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: None,
            }]
        };
    }
    Ok(else_branch)
}

/// Lower `for SOURCE -> $x { … }` to `Stmt::For`. Only a single-parameter pointy
/// block is handled; the bare `for @x { … }` (`$_`) form and multi-parameter
/// blocks are the current coverage boundary.
fn lower_for(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let iterable = lower_expr(named_child(node, "source")?)?;
    let block = named_child(node, "body")?;
    // A pointy block (`-> $x { … }`) names the loop variable; a plain block
    // (`for @x { … $_ }`) has no explicit parameter and the body sees `$_`.
    let param = match block.class {
        RakuAstClass::PointyBlock => pointy_single_param(block)?,
        RakuAstClass::Block => None,
        _ => return Err(unsupported(node)),
    };
    // Both a Block and a PointyBlock wrap their statements in a `body` Blockoid.
    let body = lower_block(block)?;
    Ok(Stmt::For {
        iterable,
        param,
        param_def: Box::new(None),
        params: Vec::new(),
        params_def: Vec::new(),
        body,
        label: None,
        mode: crate::ast::ForMode::Normal,
        rw_block: false,
        explicit_zero_params: false,
        // RakuAST spells the modifier form as `StatementModifierFor`, which this
        // lowering does not cover; a `RakuAst::Statement::For` is the block form.
        is_statement_modifier: false,
        uses_block_magic: false,
    })
}

/// Lower `sub NAME (SIG) { … }` to `Stmt::SubDecl`. Only bare positional scalar
/// parameters are handled; typed/named/slurpy/defaulted parameters and anonymous
/// subs in expression position are the current coverage boundary.
fn lower_sub(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let (params, param_defs) = signature_positional_params(node)?;
    let mut is_traits = super::routine_traits::IsTraits::default();
    let (return_type, mut custom_traits) = routine_return_type(node, Some(&mut is_traits))?;
    let multi = multiness(node)?;
    match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => {}
        Some(_) => match leaf_str(node, "scope")?.as_str() {
            "our" => custom_traits.push((super::convert::OUR_SCOPED.to_string(), None)),
            "my" => {}
            _ => return Err(unsupported(node)),
        },
    }
    // A Sub's `body` is the Blockoid directly (not a Block wrapping one).
    let body = lower_stmts(named_child_or_positional(named_child(node, "body")?)?)?;
    Ok(Stmt::SubDecl {
        name: crate::symbol::Symbol::intern(&name),
        name_expr: None,
        params,
        param_defs,
        return_type,
        associativity: None,
        precedence_trait: None,
        signature_alternates: Vec::new(),
        body,
        multi,
        is_rw: is_traits.is_rw,
        is_raw: is_traits.is_raw,
        is_export: !is_traits.export_tags.is_empty(),
        export_tags: is_traits.export_tags,
        is_test_assertion: false,
        supersede: false,
        custom_traits,
    })
}

/// A class declaration's `traits` list, back into mutsu's three fields:
/// `Trait::Is(type => …)` is inheritance, `Trait::Does(…)` is role composition
/// (which mutsu records in BOTH `parents` and `does_parents`), and
/// `Trait::Is(name => "rw")` is the `rw` flag.
#[allow(clippy::type_complexity)]
fn class_traits(node: &RakuAstNode) -> Result<(Vec<String>, Vec<String>, bool), RuntimeError> {
    let Some(f) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok((Vec::new(), Vec::new(), false));
    };
    let RakuAstFieldValue::List(items) = &f.value else {
        return Err(unsupported(node));
    };
    let mut parents = Vec::new();
    let mut does_parents = Vec::new();
    let mut is_rw = false;
    for item in items {
        let ValueView::RakuAst(t) = item.view() else {
            return Err(unsupported(node));
        };
        match t.class {
            RakuAstClass::TraitIs => {
                if let Ok(type_node) = named_child(t, "type") {
                    parents.push(simple_type_name(node, type_node)?);
                } else if let Ok(name_node) = named_child(t, "name") {
                    match positional_leaf(name_node)?.view() {
                        ValueView::Str(s) if s.as_str() == "rw" => is_rw = true,
                        _ => return Err(unsupported(node)),
                    }
                } else {
                    return Err(unsupported(node));
                }
            }
            RakuAstClass::TraitDoes => {
                let role = simple_type_name(node, named_child_or_positional(t)?)?;
                // mutsu's dispatcher reads `parents`, so a composed role has to
                // appear there too — exactly what the parser records.
                parents.push(role.clone());
                does_parents.push(role);
            }
            _ => return Err(unsupported(node)),
        }
    }
    Ok((parents, does_parents, is_rw))
}

/// `constant X = 5` -> a `Stmt::VarDecl` carrying mutsu's `__constant` marker
/// pair. The package-scoped default spelling is `is_our`; `scope => "my"` is the
/// lexical one. Only the sigilless form round-trips, matching what the
/// converter renders.
fn lower_constant(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = leaf_str(node, "name")?;
    let is_our = match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => true,
        Some(_) => match leaf_str(node, "scope")?.as_str() {
            "my" => false,
            _ => return Err(unsupported(node)),
        },
    };
    let init = named_child(node, "initializer")?;
    if init.class != RakuAstClass::InitializerAssign {
        return Err(unsupported(node));
    }
    let expr = lower_expr(named_child_or_positional(init)?)?;
    Ok(Stmt::VarDecl {
        name,
        expr,
        type_constraint: None,
        is_state: false,
        is_our,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: vec![
            ("__constant".to_string(), None),
            ("__has_initializer".to_string(), None),
            (
                "__constant_sigil".to_string(),
                Some(Expr::Literal(Value::str_from(""))),
            ),
        ],
        where_constraint: None,
    })
}

/// A `StatementPrefix::Phaser::<Kind>` -> `Stmt::Phaser`.
///
/// One kind is deliberately absent: `PRE`/`POST` — rakudo wraps their block in
/// a call (the phaser's child is an `ApplyPostfix`, not a `Block`), and mutsu
/// also keeps a source-text condition for the `X::Phaser::PrePost` message, so
/// the converter refuses them and nothing lowered can be one.
///
/// `BEGIN` runs at *compile* time, which the re-entrant carrier this lowering
/// feeds did not do — it ran the phaser in statement position, so
/// `EVAL(Q{my $x = 0; BEGIN { $x = 1 }; $x}.AST)` answered 1 where raku and
/// mutsu's own direct execution both answer 0, and lowering it was refused
/// outright. Both EVAL carriers now run `run_toplevel_begin_phasers` — the same
/// compile-time pass the mainline pipeline uses — before
/// `reorder_phasers_for_eval` handles `CHECK`/`INIT`, so `BEGIN` lowers like any
/// other kind.
fn lower_phaser(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    use crate::ast::PhaserKind;
    let kind = match node.class {
        RakuAstClass::StatementPrefixPhaserBegin => PhaserKind::Begin,
        RakuAstClass::StatementPrefixPhaserCheck => PhaserKind::Check,
        RakuAstClass::StatementPrefixPhaserInit => PhaserKind::Init,
        RakuAstClass::StatementPrefixPhaserEnd => PhaserKind::End,
        RakuAstClass::StatementPrefixPhaserEnter => PhaserKind::Enter,
        RakuAstClass::StatementPrefixPhaserLeave => PhaserKind::Leave,
        RakuAstClass::StatementPrefixPhaserKeep => PhaserKind::Keep,
        RakuAstClass::StatementPrefixPhaserUndo => PhaserKind::Undo,
        RakuAstClass::StatementPrefixPhaserFirst => PhaserKind::First,
        RakuAstClass::StatementPrefixPhaserNext => PhaserKind::Next,
        RakuAstClass::StatementPrefixPhaserLast => PhaserKind::Last,
        RakuAstClass::StatementPrefixPhaserQuit => PhaserKind::Quit,
        RakuAstClass::StatementPrefixPhaserClose => PhaserKind::Close,
        _ => return Err(unsupported(node)),
    };
    Ok(Stmt::Phaser {
        kind,
        body: lower_block(named_child_or_positional(node)?)?,
        condition: None,
        // A RakuAST tree is lowered and run at run time, like an `EVAL`, so
        // its ENDs install where execution reaches them rather than at a
        // source position of the main compunit.
        end_index: None,
    })
}

/// A package body as the parser leaves it: the `method`s declared in its
/// nested blocks and routine bodies hoisted into it (the converter rendered
/// them where they were written, `parser::unhoist_nested_methods`).
pub(super) fn lower_package_body(mut body: Vec<Stmt>) -> Vec<Stmt> {
    crate::parser::hoist_nested_methods(&mut body);
    body
}

/// `class NAME { … }` -> `Stmt::ClassDecl`. The body is a `Block` whose
/// statements are the class body (methods, attributes, …), lowered by the same
/// statement dispatch. Only the plain form round-trips: the converter refuses
/// to *read* a class with inheritance, scope, a repr, or traits, so nothing
/// lowered here can carry them either.
fn lower_class(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let body = lower_package_body(lower_block(named_child(node, "body")?)?);
    let (parents, does_parents, class_is_rw) = class_traits(node)?;
    let repr = match node.fields.iter().find(|f| f.name == Some("repr")) {
        Some(_) => Some(leaf_str(node, "repr")?),
        None => None,
    };
    Ok(Stmt::ClassDecl {
        name: crate::symbol::Symbol::intern(&name),
        name_expr: None,
        parents,
        class_is_rw,
        is_hidden: false,
        is_lexical: package_is_lexical(node)?,
        hidden_parents: Vec::new(),
        does_parents,
        repr,
        body,
        language_version: crate::parser::current_language_version(),
        custom_traits: Vec::new(),
        is_unit: false,
        implicit_grammar_parent: false,
        is_grammar: false,
        // Lowering is the parser's counterpart, so the declaration gets its
        // own site id as a parsed one does: a lexical class is registered
        // under a name mangled with it, which is what keeps it lexical.
        decl_id: crate::ast::next_class_decl_id(),
        parent_args: Vec::new(),
        body_parents: Vec::new(),
    })
}

/// A package declaration's `scope`: `my` is lexical, `our` (the default,
/// rendered as no field) is not; any other scope stays the boundary.
fn package_is_lexical(node: &RakuAstNode) -> Result<bool, RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => Ok(false),
        Some(_) => match leaf_str(node, "scope")?.as_str() {
            "my" => Ok(true),
            "our" => Ok(false),
            _ => Err(unsupported(node)),
        },
    }
}

fn lower_grammar(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let body = lower_package_body(lower_block(named_child(node, "body")?)?);
    Ok(Stmt::ClassDecl {
        name: crate::symbol::Symbol::intern(&name),
        name_expr: None,
        parents: vec!["Grammar".to_string()],
        class_is_rw: false,
        is_hidden: false,
        is_lexical: package_is_lexical(node)?,
        hidden_parents: Vec::new(),
        does_parents: Vec::new(),
        repr: None,
        body,
        language_version: crate::parser::current_language_version(),
        custom_traits: Vec::new(),
        is_unit: false,
        implicit_grammar_parent: true,
        is_grammar: true,
        decl_id: crate::ast::next_class_decl_id(),
        parent_args: Vec::new(),
        body_parents: Vec::new(),
    })
}

fn lower_regex_declaration(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let body = named_child(node, "body")?;
    let declaration_kind = match node.class {
        RakuAstClass::RegexDeclaration => crate::regex_tree::RegexDeclKind::Regex,
        RakuAstClass::TokenDeclaration => crate::regex_tree::RegexDeclKind::Token,
        RakuAstClass::RuleDeclaration => crate::regex_tree::RegexDeclKind::Rule,
        _ => return Err(unsupported(node)),
    };
    let tree = RegexTree {
        body: lower_regex_node(body)?,
        match_immediately: false,
        adverbs: Vec::new(),
        declaration_kind: Some(declaration_kind),
    };
    let source = tree.to_source();
    let execution_pattern = match node.class {
        RakuAstClass::RegexDeclaration => source,
        RakuAstClass::TokenDeclaration => format!(":ratchet {source}"),
        RakuAstClass::RuleDeclaration => {
            let pattern = crate::parser::inject_implicit_rule_ws(&source);
            let pattern = crate::parser::inject_separator_ws(&pattern);
            format!(":ratchet {pattern}")
        }
        _ => unreachable!("declaration kind was checked above"),
    };
    let value = Value::regex(execution_pattern).with_regex_source_tree(tree.clone());
    let body = vec![Stmt::Expr(Expr::Literal(value))];
    let source_regex = Some(tree);
    match node.class {
        RakuAstClass::RegexDeclaration | RakuAstClass::TokenDeclaration => Ok(Stmt::TokenDecl {
            name: crate::symbol::Symbol::intern(&name),
            params: Vec::new(),
            param_defs: Vec::new(),
            body,
            source_regex,
            regex_kind: if node.class == RakuAstClass::RegexDeclaration {
                crate::regex_tree::RegexDeclKind::Regex
            } else {
                crate::regex_tree::RegexDeclKind::Token
            },
            multi: false,
            is_my: false,
            is_our: false,
            is_export: false,
            export_tags: Vec::new(),
        }),
        RakuAstClass::RuleDeclaration => Ok(Stmt::RuleDecl {
            name: crate::symbol::Symbol::intern(&name),
            params: Vec::new(),
            param_defs: Vec::new(),
            body,
            source_regex,
            multi: false,
            is_export: false,
            export_tags: Vec::new(),
        }),
        _ => Err(unsupported(node)),
    }
}

/// `role NAME { … }` -> `Stmt::RoleDecl`. A role's body is a `RoleBody` (not a
/// plain `Block`) wrapping the `Blockoid`, matching what the converter renders.
/// Parameterised roles, export, `is rw`, and traits are refused on the read
/// side, so nothing lowered here carries them.
/// `method NAME (…) { … }` -> `Stmt::MethodDecl`, the `Method` counterpart of
/// [`lower_sub`]. The return type comes back through the same
/// `signature.returns` / `Trait::Returns` / `Trait::Of` reading `lower_sub`
/// uses, so all three spellings the converter renders lower back.
/// `module M { … }` / `package P { … }` -> `Stmt::Package`. raku names the
/// declarator with the class, so the keyword comes back from `node.class`
/// rather than from a field. `grammar` is refused on the read side (its body
/// holds regex declarations this layer does not model), so nothing lowered
/// here is one.
fn lower_package(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let kind = match node.class {
        RakuAstClass::Module => crate::ast::PackageKind::Module,
        RakuAstClass::Package => crate::ast::PackageKind::Package,
        _ => return Err(unsupported(node)),
    };
    Ok(Stmt::Package {
        name: crate::symbol::Symbol::intern(&call_name_str(node)?),
        body: lower_package_body(lower_block(named_child(node, "body")?)?),
        kind,
        is_unit: false,
        is_my: false,
    })
}

/// `RakuAST::Type::Enum(name, term)` -> the existing enum declaration path.
/// The source form is recovered from the term node because the internal AST
/// stores only normalized variants for execution.
fn lower_enum(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let term = named_child(node, "term")?;
    let (variants, variant_form) = match term.class {
        RakuAstClass::QuotedString => lower_enum_word_term(term)?,
        RakuAstClass::CircumfixParentheses => lower_enum_pair_term(term)?,
        _ => return Err(unsupported(node)),
    };
    Ok(Stmt::EnumDecl {
        name: crate::symbol::Symbol::intern(&name),
        variants,
        variant_form,
        is_export: false,
        export_tags: Vec::new(),
        is_my: false,
        base_type: None,
        roles: Vec::new(),
        language_version: crate::parser::current_language_version(),
    })
}

fn lower_enum_word_term(term: &RakuAstNode) -> Result<LoweredEnumVariants, RuntimeError> {
    let processors = list_field(term, "processors")?;
    let processor_names = processors
        .iter()
        .map(|processor| match processor.view() {
            ValueView::Str(value) => Ok(value.to_string()),
            _ => Err(unsupported(term)),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let variant_form = match processor_names.as_slice() {
        [processor, val] if val == "val" && processor == "words" => EnumVariantForm::Words,
        [processor, val] if val == "val" && processor == "quotewords" => {
            EnumVariantForm::QuoteWords
        }
        _ => return Err(unsupported(term)),
    };
    let segments = list_field(term, "segments")?;
    let [segment] = segments else {
        return Err(unsupported(term));
    };
    let ValueView::RakuAst(segment) = segment.view() else {
        return Err(unsupported(term));
    };
    if segment.class != RakuAstClass::StrLiteral {
        return Err(unsupported(term));
    }
    let segment_value = positional_leaf(segment)?;
    let ValueView::Str(text) = segment_value.view() else {
        return Err(unsupported(term));
    };
    Ok((
        text.split_whitespace()
            .map(|name| (name.to_string(), None))
            .collect(),
        variant_form,
    ))
}

fn lower_enum_pair_term(term: &RakuAstNode) -> Result<LoweredEnumVariants, RuntimeError> {
    let semilist = named_child_or_positional(term)?;
    let statement = named_child_or_positional(semilist)?;
    let body = match lower_expr(statement)? {
        Expr::ArrayLiteral(items) => items,
        item => vec![item],
    };
    let mut variants = Vec::with_capacity(body.len());
    for item in body {
        let Expr::Binary { left, op, right } = item else {
            return Err(unsupported(term));
        };
        if op != crate::token_kind::TokenKind::FatArrow {
            return Err(unsupported(term));
        }
        let name = match *left {
            Expr::Literal(value) | Expr::LiteralSrc(value, _)
                if matches!(value.view(), ValueView::Str(_)) =>
            {
                value.to_string_value()
            }
            _ => return Err(unsupported(term)),
        };
        variants.push((name, Some(*right)));
    }
    Ok((variants, EnumVariantForm::PairList))
}

/// `subset S of T where P` -> `Stmt::SubsetDecl`. An explicit base type arrives
/// as the single `Trait::Of` entry of the `traits` list; a `subset` that writes
/// no `of` carries no `traits` field at all and takes the implied `Any`. Any
/// other trait is a shape the converter never produced.
fn lower_subset(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let predicate = match node.fields.iter().find(|f| f.name == Some("where")) {
        Some(f) => Some(lower_expr(child_node(&f.value)?)?),
        None => None,
    };
    let (base, base_is_explicit) = match node.fields.iter().find(|f| f.name == Some("traits")) {
        None => ("Any".to_string(), false),
        Some(_) => {
            let [trait_of] = list_field(node, "traits")? else {
                return Err(unsupported(node));
            };
            let ValueView::RakuAst(trait_of) = trait_of.view() else {
                return Err(unsupported(node));
            };
            if trait_of.class != RakuAstClass::TraitOf {
                return Err(unsupported(node));
            }
            (
                simple_type_name(node, named_child_or_positional(trait_of)?)?,
                true,
            )
        }
    };
    Ok(Stmt::SubsetDecl {
        name: crate::symbol::Symbol::intern(&name),
        base,
        base_is_explicit,
        predicate,
        version: crate::parser::current_language_version(),
        is_export: false,
        export_tags: Vec::new(),
        is_my: false,
        decl_id: crate::ast::next_class_decl_id(),
    })
}

/// `multiness => "multi"`. A `proto` carries a `{*}` body shape mutsu keeps in
/// a separate `Stmt::ProtoDecl`, so it stays the boundary.
fn multiness(node: &RakuAstNode) -> Result<bool, RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some("multiness")) {
        None => Ok(false),
        Some(_) => match leaf_str(node, "multiness")?.as_str() {
            "multi" => Ok(true),
            _ => Err(unsupported(node)),
        },
    }
}

fn lower_method(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let (params, param_defs) = signature_positional_params(node)?;
    let mut is_traits = super::routine_traits::IsTraits::default();
    let (return_type, custom_traits) = routine_return_type(node, Some(&mut is_traits))?;
    let body = lower_stmts(named_child_or_positional(named_child(node, "body")?)?)?;
    Ok(Stmt::MethodDecl {
        name: crate::symbol::Symbol::intern(&name),
        name_expr: None,
        params,
        param_defs,
        body,
        multi: multiness(node)?,
        is_rw: is_traits.is_rw,
        is_raw: is_traits.is_raw,
        is_private: bool_field(node, "private")?,
        is_our: false,
        is_my: false,
        // raku names the declarator with the class, so `submethod` comes back
        // from `node.class` rather than from a field.
        is_submethod: node.class == RakuAstClass::Submethod,
        our_variable_form: false,
        return_type,
        is_default_candidate: false,
        deprecated_message: None,
        handles: Vec::new(),
        custom_traits,
        is_export: !is_traits.export_tags.is_empty(),
        export_tags: is_traits.export_tags,
    })
}

/// A routine's return type, from either spelling: `signature.returns` (the
/// `-->` arrow) or a `traits => (Trait::Returns|Trait::Of(Type),)` entry. The
/// trait spelling round-trips through the same `__return_via_*` marker the
/// parser sets, so `EVAL` reproduces the source form the converter read.
/// Declaring both is refused rather than silently collapsed. A
/// `Trait::Is(name => rw|raw)` sets `is_traits` when the caller takes one
/// (a method) and is refused otherwise.
#[allow(clippy::type_complexity)]
fn routine_return_type(
    node: &RakuAstNode,
    mut is_traits: Option<&mut super::routine_traits::IsTraits>,
) -> Result<(Option<String>, Vec<(String, Option<Expr>)>), RuntimeError> {
    let arrow = match named_child(node, "signature") {
        Ok(sig) => match sig.fields.iter().find(|f| f.name == Some("returns")) {
            Some(f) => Some(simple_type_name(node, child_node(&f.value)?)?),
            None => None,
        },
        Err(_) => None,
    };
    let mut via_trait = None;
    if let Some(f) = node.fields.iter().find(|f| f.name == Some("traits")) {
        let RakuAstFieldValue::List(items) = &f.value else {
            return Err(unsupported(node));
        };
        for item in items {
            let ValueView::RakuAst(t) = item.view() else {
                return Err(unsupported(node));
            };
            let marker = match t.class {
                RakuAstClass::TraitReturns => "__return_via_trait",
                RakuAstClass::TraitOf => "__return_via_of",
                RakuAstClass::TraitIs => {
                    let read = match is_traits.as_deref_mut() {
                        Some(flags) => flags.read(t)?,
                        None => false,
                    };
                    if read {
                        continue;
                    }
                    return Err(unsupported(node));
                }
                _ => return Err(unsupported(node)),
            };
            if via_trait.is_some() {
                return Err(unsupported(node));
            }
            via_trait = Some((
                simple_type_name(node, named_child_or_positional(t)?)?,
                marker,
            ));
        }
    }
    match (arrow, via_trait) {
        (Some(t), None) => Ok((Some(t), Vec::new())),
        (None, Some((t, marker))) => Ok((Some(t), vec![(marker.to_string(), None)])),
        (None, None) => Ok((None, Vec::new())),
        (Some(_), Some(_)) => Err(unsupported(node)),
    }
}

/// The parser's type-constraint spelling of a type node (`Int`, `Str:D`,
/// `Int()`, `Array[Int]`) -- see `type_lower`.
fn simple_type_name(node: &RakuAstNode, type_node: &RakuAstNode) -> Result<String, RuntimeError> {
    super::type_lower::type_constraint(node, type_node)
}

/// The positional parameter names of a routine's `signature`, each with its
/// `$` sigil stripped. Nested `sub-signature` nodes are lowered recursively
/// into `ParamDef.sub_signature`.
#[allow(clippy::type_complexity)]
fn signature_positional_params(
    node: &RakuAstNode,
) -> Result<(Vec<String>, Vec<ParamDef>), RuntimeError> {
    let Ok(sig) = named_child(node, "signature") else {
        return Ok((Vec::new(), Vec::new()));
    };
    let defs = lower_signature_parameters(sig, node)?;
    let names = defs.iter().map(|def| def.name.clone()).collect();
    Ok((names, defs))
}

pub(super) fn lower_signature_parameters(
    signature: &RakuAstNode,
    owner: &RakuAstNode,
) -> Result<Vec<ParamDef>, RuntimeError> {
    if signature.class != RakuAstClass::Signature {
        return Err(unsupported(owner));
    }
    let params = match signature
        .fields
        .iter()
        .find(|f| f.name == Some("parameters"))
    {
        Some(f) => match &f.value {
            RakuAstFieldValue::List(items) => items,
            _ => return Err(unsupported(owner)),
        },
        None => return Ok(Vec::new()),
    };
    let mut defs = Vec::with_capacity(params.len());
    for v in params {
        let ValueView::RakuAst(p) = v.view() else {
            return Err(unsupported(owner));
        };
        defs.push(lower_parameter(p, owner)?);
    }
    Ok(defs)
}

fn lower_parameter(parameter: &RakuAstNode, owner: &RakuAstNode) -> Result<ParamDef, RuntimeError> {
    if parameter.class != RakuAstClass::Parameter {
        return Err(unsupported(owner));
    }
    let type_capture = if let Some(type_captures) = parameter
        .fields
        .iter()
        .find(|f| f.name == Some("type-captures"))
    {
        let RakuAstFieldValue::List(items) = &type_captures.value else {
            return Err(unsupported(owner));
        };
        let [type_capture] = items.as_slice() else {
            return Err(unsupported(owner));
        };
        let ValueView::RakuAst(type_capture) = type_capture.view() else {
            return Err(unsupported(owner));
        };
        if type_capture.class != RakuAstClass::TypeCapture {
            return Err(unsupported(owner));
        }
        let name_node = named_child_or_positional(type_capture)?;
        let name_value = positional_leaf(name_node)?;
        let ValueView::Str(name) = name_value.view() else {
            return Err(unsupported(owner));
        };
        Some(name.to_string())
    } else {
        None
    };
    let mut sigilless = false;
    let invocant = match parameter.fields.iter().find(|f| f.name == Some("invocant")) {
        Some(field) => match &field.value {
            RakuAstFieldValue::Node(value) => match value.view() {
                ValueView::Bool(b) => b,
                _ => return Err(unsupported(owner)),
            },
            _ => return Err(unsupported(owner)),
        },
        None => false,
    };
    let has_target = parameter.fields.iter().any(|f| f.name == Some("target"));
    let name = if let Some(target) = parameter.fields.iter().find(|f| f.name == Some("target")) {
        let target = child_node(&target.value)?;
        match target.class {
            RakuAstClass::ParameterTargetVar => {
                let raw = leaf_str(target, "name")?;
                raw.strip_prefix('$').map(str::to_string).unwrap_or(raw)
            }
            // `\x`: the term's name is the parameter's, with no sigil to strip.
            RakuAstClass::ParameterTargetTerm => {
                sigilless = true;
                match name_parts::name_shape(named_child_or_positional(target)?) {
                    Some(NameShape::Identifier(name)) => name,
                    _ => return Err(unsupported(owner)),
                }
            }
            _ => return Err(unsupported(owner)),
        }
    } else if invocant {
        // `Foo:D:` / `::?CLASS:U:`: the parser names a synthesized invocant
        // `self`.
        "self".to_string()
    } else if let Some(type_capture) = &type_capture {
        format!("__type_capture__{type_capture}")
    } else if parameter.fields.iter().any(|f| {
        f.name == Some("slurpy")
            && matches!(&f.value, RakuAstFieldValue::Node(v)
                if super::slurpy_marker_class(v) == Some(RakuAstClass::ParameterSlurpyCapture))
    }) {
        // An anonymous capture `|` binds under the parser's placeholder name.
        sigilless = true;
        ANONYMOUS_CAPTURE.to_string()
    } else {
        return Err(unsupported(owner));
    };
    let mut def = positional_param(&name);
    def.sigilless = sigilless;
    if invocant {
        def.is_invocant = true;
        def.traits.push("invocant".to_string());
        if !has_target {
            def.traits
                .push(crate::ast::IMPLICIT_INVOCANT_TRAIT.to_string());
        }
    }
    // A named parameter `:$x` carries a `names` list; it binds by name and is
    // optional by default. More than one name is an alias chain, rebuilt once
    // the parameter's own fields are read (`named_param::wrap_aliases`).
    let names = match parameter.fields.iter().find(|f| f.name == Some("names")) {
        Some(field) => {
            let RakuAstFieldValue::List(items) = &field.value else {
                return Err(unsupported(owner));
            };
            let mut names = Vec::with_capacity(items.len());
            for item in items {
                let ValueView::Str(name) = item.view() else {
                    return Err(unsupported(owner));
                };
                names.push(name.to_string());
            }
            def.named = true;
            def.required = false;
            Some(names)
        }
        None => None,
    };
    // `optional => True` makes a positional parameter optional. For named
    // parameters, an explicit False marks it required.
    if let Some(optional) = parameter.fields.iter().find(|f| f.name == Some("optional")) {
        let RakuAstFieldValue::Node(value) = &optional.value else {
            return Err(unsupported(owner));
        };
        let ValueView::Bool(is_optional) = value.view() else {
            return Err(unsupported(owner));
        };
        def.required = !is_optional;
        def.optional_marker = is_optional;
    }
    // `$x is copy` / `is rw` / `is raw` / `is readonly`: argument-less
    // `Trait::Is` nodes, kept by name as the parser keeps them.
    if let Some(traits) = parameter.fields.iter().find(|f| f.name == Some("traits")) {
        let RakuAstFieldValue::List(items) = &traits.value else {
            return Err(unsupported(owner));
        };
        for item in items {
            let ValueView::RakuAst(t) = item.view() else {
                return Err(unsupported(owner));
            };
            if t.class != RakuAstClass::TraitIs {
                return Err(unsupported(owner));
            }
            let name = positional_leaf(named_child(t, "name")?)?;
            let name = match name.view() {
                ValueView::Str(name) => name.to_string(),
                _ => return Err(unsupported(owner)),
            };
            if !matches!(name.as_str(), "copy" | "rw" | "raw" | "readonly") {
                return Err(unsupported(owner));
            }
            def.traits.push(name);
        }
    }
    // A slurpy parameter `*@a` / `**@a` carries a `slurpy` marker: the
    // `RakuAST::Parameter::Slurpy::*` type object, as rakudo stores it (a node
    // of the same class is accepted too -- see `slurpy_marker_class`).
    if let Some(s) = parameter.fields.iter().find(|f| f.name == Some("slurpy")) {
        let RakuAstFieldValue::Node(val) = &s.value else {
            return Err(unsupported(owner));
        };
        match super::slurpy_marker_class(val) {
            Some(RakuAstClass::ParameterSlurpyFlattened) => def.slurpy = true,
            Some(RakuAstClass::ParameterSlurpyUnflattened) => def.double_slurpy = true,
            // `+a` and `|c` are the parser's sigilless slurpies, told apart
            // by `onearg`.
            Some(RakuAstClass::ParameterSlurpySingleArgument) if def.sigilless => {
                def.slurpy = true;
                def.onearg = true;
            }
            Some(RakuAstClass::ParameterSlurpyCapture) if def.sigilless => def.slurpy = true,
            _ => return Err(unsupported(owner)),
        }
        def.required = false;
    }
    // `Int $x` -> a type constraint. `Type::Simple` (a plain type name) is
    // handled; the implicit `Type::Setting(Any)` on an untyped param is
    // ignored, and richer type forms (definite/coercion/parameterised) defer.
    if let Some(t) = parameter.fields.iter().find(|f| f.name == Some("type")) {
        if let RakuAstFieldValue::Node(val) = &t.value
            && let ValueView::RakuAst(type_node) = val.view()
        {
            if type_node.class != RakuAstClass::TypeSetting {
                // `Type::Setting(Any)` is the implicit type of an untyped param.
                def.type_constraint = Some(simple_type_name(owner, type_node)?);
            }
        } else {
            return Err(unsupported(owner));
        }
    }
    if let Some(type_capture) = type_capture {
        def.type_capture = Some(type_capture);
    }
    // `$y = EXPR` -> an optional positional with a default value.
    if let Some(d) = parameter.fields.iter().find(|f| f.name == Some("default")) {
        let default_node = match &d.value {
            RakuAstFieldValue::Node(val) => match val.view() {
                ValueView::RakuAst(child) => child,
                _ => return Err(unsupported(owner)),
            },
            _ => return Err(unsupported(owner)),
        };
        def.default = Some(lower_expr(default_node)?);
        def.required = false;
    }
    if let Some(w) = parameter.fields.iter().find(|f| f.name == Some("where")) {
        let where_node = match &w.value {
            RakuAstFieldValue::Node(val) => match val.view() {
                ValueView::RakuAst(child) => child,
                _ => return Err(unsupported(owner)),
            },
            _ => return Err(unsupported(owner)),
        };
        def.where_constraint = Some(Box::new(lower_expr(where_node)?));
    }
    if let Some(sub_signature) = parameter
        .fields
        .iter()
        .find(|f| f.name == Some("sub-signature"))
    {
        let sub_signature = child_node(&sub_signature.value)?;
        def.sub_signature = Some(lower_signature_parameters(sub_signature, owner)?);
    }
    match names {
        Some(names) => super::named_param::wrap_aliases(def, &names, owner),
        None => Ok(def),
    }
}

/// The name the parser gives an anonymous capture parameter (`|`).
pub(super) const ANONYMOUS_CAPTURE: &str = "_capture";

/// A default positional (required, non-slurpy, untyped) `ParamDef` for `name`.
fn positional_param(name: &str) -> ParamDef {
    ParamDef {
        type_capture: None,
        name: name.to_string(),
        default: None,
        multi_invocant: true,
        required: true,
        named: false,
        named_alias: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
        code: Default::default(),
        trait_args: Vec::new(),
    }
}

/// The single loop variable of a pointy block's signature (`-> $x`), with its
/// `$` sigil stripped, or `None` when the block takes no explicit parameter.
fn pointy_single_param(pointy: &RakuAstNode) -> Result<Option<String>, RuntimeError> {
    let Ok(sig) = named_child(pointy, "signature") else {
        return Ok(None);
    };
    let params = list_field(sig, "parameters")?;
    match params.len() {
        0 => Ok(None),
        1 => {
            let ValueView::RakuAst(p0) = params[0].view() else {
                return Err(unsupported(pointy));
            };
            let target = named_child(p0, "target")?;
            if target.class != RakuAstClass::ParameterTargetVar {
                return Err(unsupported(pointy));
            }
            let raw = leaf_str(target, "name")?;
            Ok(Some(
                raw.strip_prefix('$').map(str::to_string).unwrap_or(raw),
            ))
        }
        _ => Err(unsupported(pointy)),
    }
}

/// Lower `while COND { … }` to `Stmt::While`.
fn lower_while(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let is_until = node.class == RakuAstClass::StatementLoopUntil;
    let cond = lower_expr(named_child(node, "condition")?)?;
    let body = lower_block(named_child(node, "body")?)?;
    Ok(Stmt::While {
        cond: negate_if(cond, is_until),
        body,
        label: None,
        is_statement_modifier: false,
        is_until,
    })
}

/// mutsu stores an `until` loop as a `while` over a negated condition *plus* an
/// `is_until` flag; raku keeps the condition undecorated and puts the keyword in
/// the class name. Re-plant the negation the parser would have added.
fn negate_if(cond: Expr, is_until: bool) -> Expr {
    if is_until {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Bang,
            expr: Box::new(cond),
        }
    } else {
        cond
    }
}

/// Lower a C-style `loop (SETUP; COND; STEP) { … }` to `Stmt::Loop`. Each of the
/// three controls is optional (a bare `loop { … }` has none).
fn lower_cstyle_loop(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let init = match node.fields.iter().any(|f| f.name == Some("setup")) {
        true => Some(Box::new(lower_stmt_inner(named_child(node, "setup")?)?)),
        false => None,
    };
    let cond = match node.fields.iter().any(|f| f.name == Some("condition")) {
        true => Some(lower_expr(named_child(node, "condition")?)?),
        false => None,
    };
    let step = match node.fields.iter().any(|f| f.name == Some("increment")) {
        true => Some(lower_expr(named_child(node, "increment")?)?),
        false => None,
    };
    let body = lower_block(named_child(node, "body")?)?;
    Ok(Stmt::Loop {
        init,
        cond,
        step,
        body,
        repeat: false,
        label: None,
        is_until: false,
    })
}

/// Lower a `Block` (`body => Blockoid` wrapping a positional `StatementList`) to a
/// statement list.
pub(super) fn lower_block(block: &RakuAstNode) -> Result<Vec<Stmt>, RuntimeError> {
    let blockoid = named_child(block, "body")?;
    lower_stmts(named_child_or_positional(blockoid)?)
}

/// Whether an `ApplyInfix`'s `infix` child is an `Assignment` node (`$x = …`).
fn infix_is_assignment(node: &RakuAstNode) -> bool {
    named_child(node, "infix")
        .map(|c| c.class == RakuAstClass::Assignment)
        .unwrap_or(false)
}

/// Whether an `ApplyInfix` is `VAR := …`: a plain `:=` infix whose left side
/// is a variable. Any other bind target (a subscript, an attribute) stays the
/// boundary, as the converter never renders one.
fn infix_is_bind_to_variable(node: &RakuAstNode) -> bool {
    let is_bind = named_child(node, "infix").is_ok_and(|infix| {
        infix.class == RakuAstClass::Infix
            && positional_leaf(infix)
                .is_ok_and(|op| matches!(op.view(), ValueView::Str(s) if s.as_str() == ":="))
    });
    is_bind && named_child(node, "left").is_ok_and(|left| variable_spelling(left).is_ok())
}

/// Whether an `ApplyInfix` uses Raku's compound-assignment metaoperator.
fn infix_is_compound_assignment(node: &RakuAstNode) -> bool {
    named_child(node, "infix")
        .map(|child| child.class == RakuAstClass::MetaInfixAssign)
        .unwrap_or(false)
}

/// Lower `ApplyInfix(MetaInfix::Assign(Infix(OP)))` to the parser's existing
/// compound-assignment execution shape while retaining the source marker.
fn lower_compound_assign_expr(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let target = lower_expr(named_child(node, "left")?)?;
    let meta = named_child(node, "infix")?;
    let op = match positional_leaf(named_child_or_positional(meta)?)?.view() {
        ValueView::Str(value) => value.to_string(),
        _ => return Err(unsupported(node)),
    };
    let rhs = lower_expr(named_child(node, "right")?)?;
    let expanded = crate::parser::expand_compound_assign_expr(target.clone(), &op, rhs.clone())
        .map_err(RuntimeError::new)?;
    Ok(Expr::CompoundAssign {
        target: Box::new(target),
        op: format!("{op}="),
        rhs: Box::new(rhs),
        expanded: Box::new(expanded),
    })
}

/// The `(name, right)` of an `ApplyInfix(Assignment)`: the target variable name
/// (`$` sigil stripped to match the parser's naming; `@`/`%`/`&` kept) and the
/// lowered right-hand side.
fn lower_assign_parts(node: &RakuAstNode) -> Result<(String, Expr), RuntimeError> {
    let raw = variable_spelling(named_child(node, "left")?).map_err(|_| unsupported(node))?;
    let name = match raw.strip_prefix('$') {
        Some(bare) => bare.to_string(),
        None => raw,
    };
    let expr = lower_expr(named_child(node, "right")?)?;
    Ok((name, expr))
}

/// Lower `$x = EXPR` (statement position) to `Stmt::Assign`.
/// `ApplyInfix(left => <subscript>, Assignment, right)` -- the form rakudo
/// keeps for a `%h{…}` subscript -- as the parser's `IndexAssign`, or `None`
/// when the left side is not a subscript.
fn subscript_assign(node: &RakuAstNode) -> Result<Option<Expr>, RuntimeError> {
    let left = named_child(node, "left")?;
    if left.class != RakuAstClass::ApplyPostfix {
        return Ok(None);
    }
    let Expr::Index {
        target,
        index,
        is_positional,
    } = lower_expr(left)?
    else {
        return Ok(None);
    };
    Ok(Some(Expr::IndexAssign {
        target,
        index,
        value: Box::new(lower_expr(named_child(node, "right")?)?),
        is_positional,
    }))
}

fn lower_assign(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let (name, expr) = lower_assign_parts(node)?;
    Ok(Stmt::Assign {
        name,
        expr,
        op: crate::ast::AssignOp::Assign,
        target_is_sigilless: false,
    })
}

/// The source spelling of a variable node: `$x` for a `Var::Lexical`, and the
/// sigil plus the `::`-joined name for a `Var::Package` (`$Foo::v`), which is
/// how the parser names a package-qualified variable.
fn variable_spelling(node: &RakuAstNode) -> Result<String, RuntimeError> {
    match node.class {
        RakuAstClass::VarLexical => match positional_leaf(node)?.view() {
            ValueView::Str(s) => Ok(s.to_string()),
            _ => Err(unsupported(node)),
        },
        RakuAstClass::VarPackage => {
            let sigil = leaf_str(node, "sigil")?;
            if !matches!(sigil.as_str(), "$" | "@" | "%" | "&") {
                return Err(unsupported(node));
            }
            match name_parts::name_shape(named_child(node, "name")?) {
                Some(NameShape::Identifier(name)) => Ok(format!("{sigil}{name}")),
                _ => Err(unsupported(node)),
            }
        }
        _ => Err(unsupported(node)),
    }
}

/// The identifier string of a call node's `name` (a `Name`) child.
pub(super) fn call_name_str(node: &RakuAstNode) -> Result<String, RuntimeError> {
    match name_parts::name_shape(named_child(node, "name")?) {
        Some(NameShape::Identifier(name)) => Ok(name),
        _ => Err(unsupported(node)),
    }
}

/// The lowered positional arguments of a call node's `args` (`ArgList`) child, or
/// an empty vec when there are none.
fn arg_exprs(node: &RakuAstNode) -> Result<Vec<Expr>, RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some("args")) {
        Some(f) => {
            let arglist = child_node(&f.value)?;
            arglist
                .fields
                .iter()
                .map(|af| lower_expr(child_node(&af.value)?))
                .collect()
        }
        None => Ok(Vec::new()),
    }
}

/// Lower a plain `my $x = EXPR` declaration to `Stmt::VarDecl`. Scoped/typed/
/// attribute forms (which carry `scope`/`type`/`twigil`/`traits` fields) are the
/// coverage boundary.
fn lower_var_decl(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    // `scope => "has"` is an attribute declaration, not a variable one; it is
    // the only scope that lowers (the converter renders no other).
    if matches!(leaf_str(node, "scope").as_deref(), Ok("has")) {
        return lower_attribute(node);
    }
    // The remaining scopes the converter renders are `our` and `state`; `my` is
    // the default and carries no `scope` field at all. Anything else (a scope
    // raku has that the converter never emits) stays the boundary.
    let (is_our, is_state) = match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => (false, false),
        Some(_) => match leaf_str(node, "scope")?.as_str() {
            "our" => (true, false),
            "state" => (false, true),
            "my" => (false, false),
            _ => return Err(unsupported(node)),
        },
    };
    // `*` is a dynamic variable (`my $*x`); any other twigil on a non-`has`
    // declaration stays the boundary.
    let is_dynamic = match node.fields.iter().find(|f| f.name == Some("twigil")) {
        None => false,
        Some(_) if leaf_str(node, "twigil")? == "*" => true,
        Some(_) => return Err(unsupported(node)),
    };
    let type_constraint = match node.fields.iter().find(|f| f.name == Some("type")) {
        Some(f) => Some(simple_type_name(node, child_node(&f.value)?)?),
        None => None,
    };
    let mut custom_traits = super::decl_traits::lower(node)?;
    let sigil = leaf_str(node, "sigil")?;
    let desigil_node = named_child(node, "desigilname")?;
    let desigil = match positional_leaf(desigil_node)?.view() {
        ValueView::Str(s) => s.to_string(),
        _ => return Err(unsupported(node)),
    };
    let twigil = if is_dynamic { "*" } else { "" };
    let name = if sigil == "$" {
        format!("{twigil}{desigil}")
    } else {
        format!("{sigil}{twigil}{desigil}")
    };
    // The initializer field is present only for `= EXPR`; without it a plain
    // `my $x` declares an undefined value.
    let mut is_binding = false;
    let mut call_assign = None;
    let (expr, has_initializer) = match node.fields.iter().find(|f| f.name == Some("initializer")) {
        Some(_) => {
            let init = named_child(node, "initializer")?;
            is_binding = init.class == RakuAstClass::InitializerBind;
            if init.class == RakuAstClass::InitializerCallAssign {
                let call = named_child_or_positional(init)?;
                if call.class != RakuAstClass::CallMethod || dispatch_modifier(call)?.is_some() {
                    return Err(unsupported(node));
                }
                call_assign = Some((call_name_str(call)?, arg_exprs(call)?));
                (Expr::Literal(Value::NIL), false)
            } else {
                (lower_expr(named_child_or_positional(init)?)?, !is_binding)
            }
        }
        // The same sigil-aware default the parser gives an uninitialized
        // declaration: `my @a` is an empty Array and `my %h` an empty Hash,
        // not a container holding `Nil` (which read back as `[(Any)]`, #9568).
        None => (
            match sigil.as_str() {
                "@" => Expr::Literal(Value::real_array(Vec::new())),
                "%" => Expr::Hash(Vec::new(), crate::ast::HashSpelling::Composer),
                _ => Expr::Literal(Value::NIL),
            },
            false,
        ),
    };
    if has_initializer {
        custom_traits.push(("__has_initializer".to_string(), None));
    }
    if let Some((method, args)) = call_assign {
        return Ok(crate::ast::method_assign_decl::expand(
            crate::ast::method_assign_decl::MethodAssignDecl {
                name,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits,
                where_constraint: None,
                method: crate::symbol::Symbol::intern(&method),
                args,
                is_v6c: crate::parser::current_language_version_starts_with("6.c"),
            },
        ));
    }
    // `:=`: the same declaration the parser builds, expanded by the same
    // function (`ast::bind_decl`). A natively typed scalar cannot be bound;
    // the parser rejects it, and this path stays the boundary for it.
    if is_binding {
        if crate::ast::bind_decl::is_scalar_bind_name(&name) {
            if type_constraint
                .as_deref()
                .is_some_and(crate::native_types::is_native_array_element_type)
            {
                return Err(unsupported(node));
            }
            custom_traits.push((crate::ast::bind_decl::SCALAR_BIND.to_string(), None));
        } else if name.starts_with('&') {
            return Err(unsupported(node));
        }
        return Ok(crate::ast::bind_decl::expand(Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits,
            where_constraint: None,
        }));
    }
    Ok(Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits,
        where_constraint: None,
    })
}

/// `has [Type] $.x [is rw] [= EXPR]` -> `Stmt::HasDecl`. The converter renders
/// an attribute as a `VarDeclaration::Simple` with `scope => "has"` and a
/// `twigil` (`.` public / `!` private); its `Trait::Is` flags are read by
/// `rakuast::attribute`, and an `Initializer::Assign` is the default (the
/// implicit `WillBuild` beside it carries the same expression).
fn lower_attribute(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let traits = super::attribute::lower_traits(node)?;
    let initializer = match node.fields.iter().find(|f| f.name == Some("initializer")) {
        None => None,
        Some(f) => {
            let init = child_node(&f.value)?;
            if init.class != RakuAstClass::InitializerAssign {
                return Err(unsupported(node));
            }
            Some(lower_expr(named_child_or_positional(init)?)?)
        }
    };
    let sigil = leaf_str(node, "sigil")?;
    let mut chars = sigil.chars();
    let (Some(sigil_char), None) = (chars.next(), chars.next()) else {
        return Err(unsupported(node));
    };
    let is_public = match leaf_str(node, "twigil")?.as_str() {
        "." => true,
        "!" => false,
        _ => return Err(unsupported(node)),
    };
    let desigil = match positional_leaf(named_child(node, "desigilname")?)?.view() {
        ValueView::Str(s) => s.to_string(),
        _ => return Err(unsupported(node)),
    };
    let type_name = match node.fields.iter().find(|f| f.name == Some("type")) {
        Some(f) => Some(simple_type_name(node, child_node(&f.value)?)?),
        None => None,
    };
    // `Int:D` comes back as the parser's base type plus its smiley.
    let (type_constraint, type_smiley) = match type_name.as_deref() {
        Some(name) => match name.rsplit_once(':') {
            Some((base, smiley @ ("D" | "U"))) => {
                (Some(base.to_string()), Some(smiley.to_string()))
            }
            _ => (type_name.clone(), None),
        },
        None => (None, None),
    };
    // A scalar `is default(EXPR)` attribute without an initializer starts
    // with the trait's value, as the parser records it.
    let default_is_trait =
        initializer.is_none() && traits.is_default.is_some() && matches!(sigil_char, '$' | '&');
    let initializer = if default_is_trait {
        traits.is_default.clone()
    } else {
        initializer
    };
    Ok(Stmt::HasDecl {
        name: crate::symbol::Symbol::intern(&desigil),
        is_public,
        // A typed attribute carries an implicit default in the internal AST
        // (its type object, or a native type's zero); the parser plants it,
        // and the converter skips it on the way out, so re-plant the same one
        // here to keep the two sides symmetric.
        default_is_seed: initializer.is_none() && type_constraint.is_some(),
        default_is_trait,
        default: initializer.or_else(|| {
            type_constraint
                .as_deref()
                .map(crate::parser::auto_default_expr_for_type)
        }),
        handles: Vec::new(),
        is_rw: traits.is_rw,
        is_readonly: traits.is_readonly,
        type_constraint,
        type_smiley,
        is_required: traits.is_required.then_some(None),
        sigil: sigil_char,
        where_constraint: None,
        is_alias: false,
        is_embedded: false,
        is_our: false,
        is_my: false,
        is_default: traits.is_default,
        is_type: None,
        deprecated_message: None,
        is_built: traits.is_built,
        unknown_traits: Vec::new(),
        // RakuAST models an attribute's initializer as an assignment; rakudo
        // has no `:=` attribute-declaration node to lower from.
        default_is_bind: false,
    })
}

/// The value of a leaf-valued named field (e.g. `sigil => "$"`), as a `String`.
pub(super) fn leaf_str(node: &RakuAstNode, name: &str) -> Result<String, RuntimeError> {
    let field = node
        .fields
        .iter()
        .find(|f| f.name == Some(name))
        .ok_or_else(|| unsupported(node))?;
    match &field.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::Str(s) => Ok(s.to_string()),
            _ => Err(unsupported(node)),
        },
        _ => Err(unsupported(node)),
    }
}

/// The child node of an `Initializer::Assign` — its single positional child.
pub(super) fn named_child_or_positional(node: &RakuAstNode) -> Result<&RakuAstNode, RuntimeError> {
    match node.fields.first() {
        Some(f) if f.name.is_none() => child_node(&f.value),
        _ => Err(unsupported(node)),
    }
}

/// Lower a `RakuAST::Name` used as a term. Static names are the barewords that
/// the parser already uses for declared constants; a stash lookup (`Foo::`,
/// a trailing empty edge) is the parser's `PseudoStash`; a leading empty edge
/// followed by an expression part is the dynamic `::(...)` lookup retained by
/// `Expr::IndirectTypeLookup`.
/// A setting term named by a bare identifier, as the parser produces it:
/// `True`/`False` are the Bool literals the parser folds them to (a
/// `BareWord("False")` would evaluate to the string), anything else the
/// bareword. Shared by `Term::Enum` and `Term::Name`, which rakudo both
/// resolve against the setting.
// Cost: O(1).
fn term_identifier_expr(name: &str) -> Expr {
    match name {
        "True" => Expr::Literal(Value::truth(true)),
        "False" => Expr::Literal(Value::truth(false)),
        _ => Expr::BareWord(name.to_string()),
    }
}

fn lower_term_name(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match name_parts::name_shape(node).ok_or_else(|| unsupported(node))? {
        NameShape::Identifier(name) => Ok(term_identifier_expr(&name)),
        NameShape::Stash(stash) => Ok(Expr::PseudoStash(stash)),
        NameShape::Indirect {
            expr,
            tail,
            trailing,
        } => {
            let head = Box::new(lower_expr(expr)?);
            if tail.is_empty() && !trailing {
                return Ok(Expr::IndirectTypeLookup(head));
            }
            let tail = tail
                .iter()
                .flat_map(|seg| ["::", seg.as_str()])
                .collect::<String>();
            Ok(Expr::IndirectTypeLookupTail {
                head,
                tail: tail.into_boxed_str(),
                trailing,
            })
        }
    }
}

/// The stash a `Call::Name` names when it is the argument-less form Rakudo
/// gives an unresolved stash lookup (`Foo::`), or `None` for an ordinary call.
fn call_name_stash(node: &RakuAstNode) -> Option<String> {
    if node.fields.iter().any(|f| f.name == Some("args")) {
        return None;
    }
    match name_parts::name_shape(named_child(node, "name").ok()?)? {
        NameShape::Stash(stash) => Some(stash),
        _ => None,
    }
}

/// Lower a regex assertion name, preserving qualified name-part boundaries in
/// the execution spelling used by the existing matcher.
fn lower_regex_subrule_name(node: &RakuAstNode) -> Result<String, RuntimeError> {
    if node.class != RakuAstClass::Name {
        return Err(unsupported(node));
    }
    if let Some(field) = node.fields.iter().find(|field| field.name == Some("parts")) {
        let RakuAstFieldValue::List(parts) = &field.value else {
            return Err(unsupported(node));
        };
        if parts.is_empty() {
            return Err(unsupported(node));
        }
        let mut names = Vec::with_capacity(parts.len());
        for part in parts {
            let ValueView::RakuAst(part) = part.view() else {
                return Err(unsupported(node));
            };
            if part.class != RakuAstClass::NamePartSimple {
                return Err(unsupported(node));
            }
            let value = positional_leaf(part)?;
            let ValueView::Str(name) = value.view() else {
                return Err(unsupported(node));
            };
            if name.is_empty() {
                return Err(unsupported(node));
            }
            names.push(name.to_string());
        }
        return Ok(names.join("::"));
    }
    let value = positional_leaf(node)?;
    let ValueView::Str(name) = value.view() else {
        return Err(unsupported(node));
    };
    Ok(name.to_string())
}

fn lower_regex_subrule_args(
    node: &RakuAstNode,
) -> Result<crate::regex_tree::SubruleArgs, RuntimeError> {
    let Some(field) = node.fields.iter().find(|field| field.name == Some("args")) else {
        return Ok(crate::regex_tree::SubruleArgs {
            args: Vec::new(),
            source: None,
            literal_hash_indices: Vec::new(),
            colonpair_values: Vec::new(),
            colonpair_variables: Vec::new(),
            colonpair_trues: Vec::new(),
            colonpair_falses: Vec::new(),
        });
    };
    let args = child_node(&field.value)?;
    if args.class != RakuAstClass::ArgList {
        return Err(unsupported(node));
    }
    let mut lowered = Vec::with_capacity(args.fields.len());
    let mut sources = Vec::with_capacity(args.fields.len());
    let mut literal_hash_indices = Vec::with_capacity(args.fields.len());
    let mut colonpair_values = Vec::with_capacity(args.fields.len());
    let mut colonpair_variables = Vec::with_capacity(args.fields.len());
    let mut colonpair_trues = Vec::with_capacity(args.fields.len());
    let mut colonpair_falses = Vec::with_capacity(args.fields.len());
    for field in &args.fields {
        if field.name.is_some() {
            return Err(unsupported(node));
        }
        let argument = child_node(&field.value)?;
        literal_hash_indices.push(is_literal_hash_index(argument));
        colonpair_values.push(is_colonpair_value(argument));
        colonpair_variables.push(is_colonpair_variable(argument));
        colonpair_trues.push(is_colonpair_true(argument));
        colonpair_falses.push(is_colonpair_false(argument));
        sources.push(regex_subrule_argument_source(argument)?);
        lowered.push(lower_expr(argument)?);
    }
    Ok(crate::regex_tree::SubruleArgs {
        args: lowered,
        source: Some(sources.join(", ")),
        literal_hash_indices,
        colonpair_values,
        colonpair_variables,
        colonpair_trues,
        colonpair_falses,
    })
}

fn is_literal_hash_index(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ApplyPostfix
        && named_child(node, "postfix")
            .is_ok_and(|postfix| postfix.class == RakuAstClass::PostcircumfixLiteralHashIndex)
}

fn is_colonpair_value(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ColonPairValue
}

fn is_colonpair_variable(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ColonPairVariable
}

fn is_colonpair_true(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ColonPairTrue
}

fn is_colonpair_false(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ColonPairFalse
}

fn regex_subrule_argument_source(node: &RakuAstNode) -> Result<String, RuntimeError> {
    if is_literal_hash_index(node) {
        let operand = lower_expr(named_child(node, "operand")?)?;
        let postfix = named_child(node, "postfix")?;
        let index = lower_expr(named_child(postfix, "index")?)?;
        return Ok(format!(
            "{}<{}>",
            crate::regex_tree::expression_source(&operand).ok_or_else(|| unsupported(node))?,
            crate::regex_tree::expression_source(&index).ok_or_else(|| unsupported(node))?,
        ));
    }
    if is_colonpair_value(node) {
        let key = leaf_str(node, "key")?;
        let value = lower_expr(named_child(node, "value")?)?;
        let value = match value {
            Expr::Grouped(inner) => *inner,
            value => value,
        };
        let explicit_pointy_source = match &value {
            Expr::AnonSubParams {
                params,
                param_defs,
                body,
                is_rw: false,
                is_raw: false,
                is_whatever_code: false,
                return_type: None,
                declarator: crate::ast::RoutineDeclarator::Block,
                ..
            } if !params.is_empty() => pointy_block_source(param_defs, body),
            _ => None,
        };
        let block_body = match &value {
            Expr::AnonSub {
                is_block: true,
                body,
                ..
            } => Some(body),
            Expr::AnonSubParams { body, .. }
                if crate::regex_tree::is_scalar_placeholder_block(&value) =>
            {
                Some(body)
            }
            Expr::AnonSubParams { body, .. }
                if crate::regex_tree::is_array_slurpy_placeholder_block(&value) =>
            {
                Some(body)
            }
            Expr::AnonSubParams { body, .. }
                if crate::regex_tree::is_hash_slurpy_placeholder_block(&value) =>
            {
                Some(body)
            }
            _ => None,
        };
        let value = if let Some(source) = explicit_pointy_source {
            format!(":{key}({source})")
        } else if let Some(body) = block_body {
            let body = block_value_source(body).ok_or_else(|| unsupported(node))?;
            format!(":{key}{body}")
        } else if let Expr::Hash(pairs, crate::ast::HashSpelling::Composer) = &value {
            let body = hash_composer_source(pairs).ok_or_else(|| unsupported(node))?;
            format!(":{key}{body}")
        } else {
            let value = colonpair_value_source(&value).ok_or_else(|| unsupported(node))?;
            format!(":{key}({value})")
        };
        return Ok(value);
    }
    if is_colonpair_true(node) {
        let value = positional_leaf(node)?;
        let ValueView::Str(key) = value.view() else {
            return Err(unsupported(node));
        };
        return Ok(format!(":{}", *key));
    }
    if is_colonpair_false(node) {
        let value = positional_leaf(node)?;
        let ValueView::Str(key) = value.view() else {
            return Err(unsupported(node));
        };
        return Ok(format!(":!{}", *key));
    }
    if is_colonpair_variable(node) {
        let value = lower_expr(named_child(node, "value")?)?;
        let value =
            crate::regex_tree::expression_source(&value).ok_or_else(|| unsupported(node))?;
        return Ok(format!(":{value}"));
    }
    let expr = lower_expr(node)?;
    crate::regex_tree::expression_source(&expr).ok_or_else(|| unsupported(node))
}

fn colonpair_value_source(expr: &Expr) -> Option<String> {
    match expr {
        // `:name(a, b)` lowers to an ArrayLiteral internally, but its source
        // value is a parenthesized comma list rather than an Array composer.
        Expr::ArrayLiteral(items) => items
            .iter()
            .map(crate::regex_tree::expression_source)
            .collect::<Option<Vec<_>>>()
            .map(|parts| parts.join(", ")),
        expr => crate::regex_tree::expression_source(expr),
    }
}

/// Render the deliberately small explicit pointy-signature subset accepted by
/// the regex colonpair write direction. The source parser already supports all
/// closure signatures; this helper reconstructs ordinary multiple bare scalar
/// parameters, one named scalar parameter, one typed scalar parameter, and one
/// defaulted scalar parameter from a hand-built RakuAST tree. Slurpy and
/// trait-bearing parameters remain separate boundaries.
fn pointy_block_source(
    param_defs: &[crate::ast::ParamDef],
    body: &[crate::ast::Stmt],
) -> Option<String> {
    let ordinary_parameter = |param: &crate::ast::ParamDef| {
        !param.name.is_empty()
            && param
                .name
                .chars()
                .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
            && !param.slurpy
            && !param.double_slurpy
            && !param.onearg
            && !param.sigilless
            && param.type_capture.is_none()
            && param.literal_value.is_none()
            && param.sub_signature.is_none()
            && param.where_constraint.is_none()
            && param.traits.is_empty()
            && !param.optional_marker
            && param.outer_sub_signature.is_none()
            && param.code_signature.is_none()
            && !param.is_invocant
            && param.shape_constraints.is_none()
            && param.trait_args.is_empty()
    };

    let params = if let [param] = param_defs {
        if !ordinary_parameter(param) {
            return None;
        }
        let simple_type_name = |type_name: &str| {
            name_parts::identifier_segments(type_name).all(|part| {
                !part.is_empty()
                    && part
                        .chars()
                        .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
            })
        };
        if param.named {
            if param.type_constraint.is_none() && param.default.is_none() && !param.required {
                format!(":${}", param.name)
            } else {
                return None;
            }
        } else {
            match (
                param.type_constraint.as_deref(),
                param.default.as_ref(),
                param.required,
            ) {
                (Some(type_name), None, true) if simple_type_name(type_name) => {
                    format!("{type_name} ${}", param.name)
                }
                (Some(type_name), Some(default), false) if simple_type_name(type_name) => format!(
                    "{type_name} ${} = {}",
                    param.name,
                    crate::regex_tree::expression_source(default)?
                ),
                (None, Some(default), false) => format!(
                    "${} = {}",
                    param.name,
                    crate::regex_tree::expression_source(default)?
                ),
                _ => return None,
            }
        }
    } else {
        if param_defs.len() < 2
            || !param_defs.iter().all(|param| {
                ordinary_parameter(param)
                    && !param.named
                    && param.type_constraint.is_none()
                    && param.default.is_none()
                    && param.required
            })
        {
            return None;
        }
        param_defs
            .iter()
            .map(|param| format!("${}", param.name))
            .collect::<Vec<_>>()
            .join(", ")
    };

    if param_defs.is_empty() {
        return None;
    }
    Some(format!("-> {params} {}", block_value_source(body)?))
}

/// Render the small block subset used by a constructed regex's block-valued
/// colonpair. `SetLine` markers are compiler metadata and do not belong in the
/// reconstructed source. A comma list made entirely of fat-arrow pairs is
/// rendered without the internal `ArrayLiteral` brackets, because the source
/// parser intentionally reads `{ key => value, ... }` as a hash composer.
fn block_value_source(body: &[crate::ast::Stmt]) -> Option<String> {
    let mut expressions = Vec::new();
    for stmt in body {
        match stmt {
            crate::ast::Stmt::SetLine(_) => {}
            crate::ast::Stmt::Expr(Expr::ArrayLiteral(items)) => {
                if !items.iter().all(|item| {
                    matches!(
                        item,
                        Expr::Binary {
                            op: crate::token_kind::TokenKind::FatArrow,
                            ..
                        }
                    )
                }) {
                    return None;
                }
                expressions.push(
                    items
                        .iter()
                        .map(crate::regex_tree::expression_source)
                        .collect::<Option<Vec<_>>>()?
                        .join(", "),
                );
            }
            crate::ast::Stmt::Expr(expr) => {
                expressions.push(crate::regex_tree::expression_source(expr)?);
            }
            _ => return None,
        }
    }
    Some(format!("{{ {} }}", expressions.join("; ")))
}

/// The source of a `{a => 1, b => 2}` composer used as a colonpair value.
fn hash_composer_source(pairs: &[(String, Option<Expr>)]) -> Option<String> {
    let entries = pairs
        .iter()
        .map(|(key, value)| {
            let pair = Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(key.clone()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(value.clone()?),
            };
            crate::regex_tree::expression_source(&pair)
        })
        .collect::<Option<Vec<_>>>()?;
    Some(format!("{{ {} }}", entries.join(", ")))
}

fn lower_regex_node(node: &RakuAstNode) -> Result<RegexNode, RuntimeError> {
    match node.class {
        RakuAstClass::RegexLiteral => match positional_leaf(node)?.view() {
            ValueView::Str(text) => Ok(RegexNode::Literal(text.to_string())),
            _ => Err(unsupported(node)),
        },
        RakuAstClass::RegexQuote => {
            let quoted = named_child_or_positional(node)?;
            if quoted.class != RakuAstClass::QuotedString {
                return Err(unsupported(node));
            }
            let segments = list_field(quoted, "segments")?;
            let [segment] = segments else {
                return Err(unsupported(node));
            };
            let ValueView::RakuAst(segment) = segment.view() else {
                return Err(unsupported(node));
            };
            if segment.class != RakuAstClass::StrLiteral {
                return Err(unsupported(node));
            }
            match positional_leaf(segment)?.view() {
                ValueView::Str(text) => Ok(RegexNode::Quote(text.to_string())),
                _ => Err(unsupported(node)),
            }
        }
        RakuAstClass::RegexSequence
        | RakuAstClass::RegexAlternation
        | RakuAstClass::RegexSequentialAlternation => {
            let mut children = Vec::with_capacity(node.fields.len());
            for field in &node.fields {
                if field.name.is_some() {
                    return Err(unsupported(node));
                }
                children.push(lower_regex_node(child_node(&field.value)?)?);
            }
            match node.class {
                RakuAstClass::RegexSequence => Ok(RegexNode::Sequence(children)),
                RakuAstClass::RegexAlternation => Ok(RegexNode::Alternation(children)),
                RakuAstClass::RegexSequentialAlternation => {
                    Ok(RegexNode::SequentialAlternation(children))
                }
                _ => Err(unsupported(node)),
            }
        }
        RakuAstClass::RegexGroup => Ok(RegexNode::Group(Box::new(lower_regex_node(
            named_child_or_positional(node)?,
        )?))),
        RakuAstClass::RegexCapturingGroup => Ok(RegexNode::CapturingGroup(Box::new(
            lower_regex_node(named_child_or_positional(node)?)?,
        ))),
        RakuAstClass::RegexNamedCapture => Ok(RegexNode::NamedCapture {
            name: leaf_str(node, "name")?,
            array: bool_field(node, "array")?,
            regex: Box::new(lower_regex_node(named_child(node, "regex")?)?),
        }),
        RakuAstClass::RegexAssertionNamed => Ok(RegexNode::Subrule {
            name: lower_regex_subrule_name(named_child(node, "name")?)?,
            capturing: bool_field(node, "capturing")?,
            args: None,
        }),
        RakuAstClass::RegexAssertionNamedArgs => {
            let args = lower_regex_subrule_args(node)?;
            Ok(RegexNode::Subrule {
                name: lower_regex_subrule_name(named_child(node, "name")?)?,
                capturing: bool_field(node, "capturing")?,
                args: Some(Box::new(args)),
            })
        }
        RakuAstClass::RegexAssertionAlias => {
            let assertion = named_child(node, "assertion")?;
            let (name, capturing, args) = match assertion.class {
                RakuAstClass::RegexAssertionNamed => (
                    lower_regex_subrule_name(named_child(assertion, "name")?)?,
                    bool_field(assertion, "capturing")?,
                    None,
                ),
                RakuAstClass::RegexAssertionNamedArgs => {
                    let args = lower_regex_subrule_args(assertion)?;
                    (
                        lower_regex_subrule_name(named_child(assertion, "name")?)?,
                        bool_field(assertion, "capturing")?,
                        Some(Box::new(args)),
                    )
                }
                _ => return Err(unsupported(node)),
            };
            Ok(RegexNode::SubruleAlias {
                alias: leaf_str(node, "name")?,
                name,
                capturing,
                args,
            })
        }
        RakuAstClass::RegexAssertionLookahead => {
            let assertion = named_child(node, "assertion")?;
            match assertion.class {
                RakuAstClass::RegexAssertionNamedRegexArg => {
                    let RegexNode::NamedLookaround {
                        assertion: regex_arg,
                        is_behind,
                        ..
                    } = lower_regex_node(assertion)?
                    else {
                        return Err(unsupported(node));
                    };
                    Ok(RegexNode::Lookaround {
                        assertion: regex_arg,
                        negated: bool_field(node, "negated")?,
                        is_behind,
                    })
                }
                RakuAstClass::RegexAssertionInterpolatedVar => {
                    let RegexNode::RegexValueInterpolation { name, sigil, .. } =
                        lower_regex_node(assertion)?
                    else {
                        return Err(unsupported(node));
                    };
                    if sigil != '@' {
                        return Err(unsupported(node));
                    }
                    Ok(RegexNode::ArrayLookaround {
                        name,
                        negated: bool_field(node, "negated")?,
                    })
                }
                _ => Err(unsupported(node)),
            }
        }
        RakuAstClass::RegexAssertionInterpolatedVar => {
            let sequential = bool_field(node, "sequential")?;
            let var = named_child(node, "var")?;
            if var.class != RakuAstClass::VarLexical {
                return Err(unsupported(node));
            }
            let name_value = positional_leaf(var)?;
            let ValueView::Str(name) = name_value.view() else {
                return Err(unsupported(node));
            };
            let Some(sigil) = name.chars().next() else {
                return Err(unsupported(node));
            };
            if !matches!(sigil, '$' | '@' | '%') {
                return Err(unsupported(node));
            }
            let name = &name[sigil.len_utf8()..];
            if name.is_empty() || name.starts_with(['*', '?', '^', '.', '!']) {
                return Err(unsupported(node));
            }
            Ok(RegexNode::RegexValueInterpolation {
                name: name.to_string(),
                sequential,
                sigil,
            })
        }
        RakuAstClass::RegexAssertionCallable => {
            let callee = named_child(node, "callee")?;
            let callee = lower_expr(callee)?;
            let Expr::CodeVar(name) = callee else {
                return Err(unsupported(node));
            };
            let args = match node.fields.iter().find(|f| f.name == Some("args")) {
                Some(field) => {
                    let args = child_node(&field.value)?;
                    if args.class != RakuAstClass::ArgList {
                        return Err(unsupported(node));
                    }
                    let mut lowered = Vec::with_capacity(args.fields.len());
                    for field in &args.fields {
                        if field.name.is_some() {
                            return Err(unsupported(node));
                        }
                        lowered.push(lower_expr(child_node(&field.value)?)?);
                    }
                    lowered
                }
                None => Vec::new(),
            };
            let arg_source = if args.is_empty() {
                None
            } else {
                Some(
                    args.iter()
                        .map(crate::regex_tree::expression_source)
                        .collect::<Option<Vec<_>>>()
                        .ok_or_else(|| unsupported(node))?
                        .join(", "),
                )
            };
            Ok(RegexNode::Callable {
                name,
                args,
                arg_source,
            })
        }
        RakuAstClass::RegexAssertionPredicateBlock => Ok(RegexNode::CodeAssertion {
            code: String::new(),
            negated: bool_field(node, "negated")?,
            body: lower_block(named_child(node, "block")?)?,
        }),
        RakuAstClass::RegexBlock => Ok(RegexNode::CodeBlock {
            code: String::new(),
            body: lower_block(named_child_or_positional(node)?)?,
        }),
        RakuAstClass::RegexAssertionInterpolatedBlock => Ok(RegexNode::InterpolatedBlock {
            code: String::new(),
            body: lower_block(named_child(node, "block")?)?,
            sequential: bool_field(node, "sequential")?,
        }),
        RakuAstClass::RegexAssertionNamedRegexArg => {
            let name_node = named_child(node, "name")?;
            if name_node.class != RakuAstClass::Name {
                return Err(unsupported(node));
            }
            let name_value = positional_leaf(name_node)?;
            let ValueView::Str(name) = name_value.view() else {
                return Err(unsupported(node));
            };
            let is_behind = match name.as_str() {
                "before" => false,
                "after" => true,
                _ => return Err(unsupported(node)),
            };
            Ok(RegexNode::NamedLookaround {
                assertion: Box::new(lower_regex_node(named_child(node, "regex-arg")?)?),
                is_behind,
                capturing: bool_field(node, "capturing")?,
            })
        }
        RakuAstClass::RegexInterpolation => {
            let sequential = bool_field(node, "sequential")?;
            let var = named_child(node, "var")?;
            if var.class != RakuAstClass::VarLexical {
                return Err(unsupported(node));
            }
            let name_value = positional_leaf(var)?;
            let ValueView::Str(name) = name_value.view() else {
                return Err(unsupported(node));
            };
            let (array, name) = if let Some(name) = name.strip_prefix('$') {
                (false, name)
            } else if let Some(name) = name.strip_prefix('@') {
                (true, name)
            } else {
                return Err(unsupported(node));
            };
            if name.is_empty() || name.starts_with(['*', '?', '^', '.', '!']) {
                return Err(unsupported(node));
            }
            if array {
                Ok(RegexNode::ArrayInterpolation {
                    name: name.to_string(),
                    sequential,
                })
            } else {
                Ok(RegexNode::Interpolation {
                    name: name.to_string(),
                    sequential,
                })
            }
        }
        RakuAstClass::RegexWithWhitespace => Ok(RegexNode::WithWhitespace(Box::new(
            lower_regex_node(named_child_or_positional(node)?)?,
        ))),
        RakuAstClass::RegexQuantifiedAtom => {
            let atom = lower_regex_node(named_child(node, "atom")?)?;
            let mut quantifier =
                super::regex_quantifier::lower_quantifier(named_child(node, "quantifier")?)
                    .ok_or_else(|| unsupported(node))?;
            if node.fields.iter().any(|f| f.name == Some("separator")) {
                let separator = lower_regex_node(named_child(node, "separator")?)?;
                quantifier.separator = super::regex_quantifier::separator(
                    separator,
                    super::regex_quantifier::trailing_separator(node),
                );
            }
            Ok(RegexNode::Quantified {
                atom: Box::new(atom),
                quantifier,
            })
        }
        RakuAstClass::RegexAssertionCharClass => super::regex_enumeration::lower(node)
            .map(RegexNode::CharClassAssertion)
            .ok_or_else(|| unsupported(node)),
        RakuAstClass::RegexCharClass(kind) => super::regex_char_class::lower(kind, node)
            .map(RegexNode::CharClass)
            .ok_or_else(|| unsupported(node)),
        RakuAstClass::RegexInternalModifierIgnoreCase
        | RakuAstClass::RegexInternalModifierIgnoreMark
        | RakuAstClass::RegexInternalModifierSigspace
        | RakuAstClass::RegexInternalModifierRatchet => {
            use crate::regex_tree::RegexModifierKind;
            let kind = match node.class {
                RakuAstClass::RegexInternalModifierIgnoreCase => RegexModifierKind::IgnoreCase,
                RakuAstClass::RegexInternalModifierIgnoreMark => RegexModifierKind::IgnoreMark,
                RakuAstClass::RegexInternalModifierSigspace => RegexModifierKind::Sigspace,
                _ => RegexModifierKind::Ratchet,
            };
            let (short, long_name) = kind.spellings();
            let long = match node.fields.iter().find(|f| f.name == Some("modifier")) {
                None => false,
                Some(_) => match leaf_str(node, "modifier")?.as_str() {
                    s if s == short => false,
                    s if s == long_name => true,
                    _ => return Err(unsupported(node)),
                },
            };
            let negated = node.fields.iter().any(|f| {
                f.name == Some("negated")
                    && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
            });
            Ok(RegexNode::InternalModifier {
                kind,
                long,
                negated,
            })
        }
        RakuAstClass::RegexAnchorBeginningOfString => Ok(RegexNode::AnchorBeginningOfString),
        RakuAstClass::RegexAnchorBeginningOfLine => Ok(RegexNode::AnchorBeginningOfLine),
        RakuAstClass::RegexAnchorEndOfString => Ok(RegexNode::AnchorEndOfString),
        RakuAstClass::RegexAnchorEndOfLine => Ok(RegexNode::AnchorEndOfLine),
        RakuAstClass::RegexAnchorLeftWordBoundary => Ok(RegexNode::AnchorLeftWordBoundary),
        RakuAstClass::RegexMatchFrom => Ok(RegexNode::MatchFrom),
        RakuAstClass::RegexMatchTo => Ok(RegexNode::MatchTo),
        RakuAstClass::RegexAnchorRightWordBoundary => Ok(RegexNode::AnchorRightWordBoundary),
        _ => Err(unsupported(node)),
    }
}

fn lower_regex_adverb(node: &RakuAstNode) -> Result<crate::regex_tree::RegexAdverb, RuntimeError> {
    if node.class != RakuAstClass::ColonPairTrue {
        return Err(unsupported(node));
    }
    let value = positional_leaf(node)?;
    let ValueView::Str(name) = value.view() else {
        return Err(unsupported(node));
    };
    Ok(crate::regex_tree::RegexAdverb {
        name: name.to_string(),
        argument: None,
    })
}

/// Build the legacy execution value from source-level adverbs. This is a
/// compatibility bridge; the shared tree remains the source of truth for
/// RakuAST and the compiler still emits the existing match opcode.
fn regex_execution_value(tree: &RegexTree) -> Result<Value, RuntimeError> {
    if tree.adverbs.is_empty() {
        return Ok(Value::regex(tree.to_source()).with_regex_source_tree(tree.clone()));
    }
    let mut pattern = tree.to_source();
    let mut value = RegexAdverbs {
        pattern: Arc::new(String::new()),
        global: false,
        exhaustive: false,
        overlap: false,
        repeat: None,
        nth: None,
        pos: false,
        pos_value: None,
        continue_: false,
        continue_value: None,
        ignore_case: false,
        sigspace: false,
        samecase: false,
        samespace: false,
        source_adverbs: None,
        captured: None,
        topic: None,
        source_tree: None,
        id: Default::default(),
        name: Default::default(),
    };
    for adverb in &tree.adverbs {
        if adverb.argument.is_some() {
            return Err(RuntimeError::new(format!(
                "RakuAST: EVAL does not yet support regex adverb argument `{}`",
                adverb.name
            )));
        }
        match adverb.name.as_str() {
            "g" | "global" => value.global = true,
            "ex" | "exhaustive" => value.exhaustive = true,
            "ov" | "overlap" => value.overlap = true,
            "i" | "ignorecase" => {
                value.ignore_case = true;
                pattern = format!(":i {pattern}");
            }
            "ii" | "samecase" => {
                value.samecase = true;
                value.ignore_case = true;
                pattern = format!(":i {pattern}");
            }
            "s" | "sigspace" => {
                value.sigspace = true;
                pattern = format!(":s {pattern}");
            }
            "ss" | "samespace" => {
                value.samespace = true;
                value.sigspace = true;
                pattern = format!(":s {pattern}");
            }
            "r" | "ratchet" => pattern = format!(":ratchet {pattern}"),
            "m" | "ignoremark" => pattern = format!(":m {pattern}"),
            other => {
                return Err(RuntimeError::new(format!(
                    "RakuAST: EVAL does not yet support regex adverb `{other}`"
                )));
            }
        }
    }
    value.pattern = Arc::new(pattern);
    Ok(Value::regex_with_adverbs(value).with_regex_source_tree(tree.clone()))
}

pub(super) fn lower_expr(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match node.class {
        // A signature declaration in expression position (`if my ($a, $b) = …`)
        // is the parser's expansion wrapped in a `DoStmt`.
        RakuAstClass::VarDeclarationSignature => {
            Ok(Expr::DoStmt(Box::new(super::signature_decl::lower(node)?)))
        }
        RakuAstClass::IntLiteral
        | RakuAstClass::NumLiteral
        | RakuAstClass::RatLiteral
        | RakuAstClass::StrLiteral => Ok(Expr::Literal(positional_leaf(node)?)),
        // `"..."` parses to a QuotedString wrapping StrLiteral segments; a single
        // plain segment lowers to its string literal.
        RakuAstClass::QuotedString => {
            let segments = list_field(node, "segments")?;
            if segments.len() == 1
                && let ValueView::RakuAst(seg) = segments[0].view()
                && seg.class == RakuAstClass::StrLiteral
            {
                return Ok(Expr::Literal(positional_leaf(seg)?));
            }
            // A multi-segment (interpolated) string -> StringInterpolation of the
            // lowered segments (`StrLiteral` runs and interpolated terms alike).
            let mut parts = Vec::with_capacity(segments.len());
            for s in segments {
                let ValueView::RakuAst(seg) = s.view() else {
                    return Err(unsupported(node));
                };
                // An interpolated code block (`"a{ $x }b"`) is a `Block`
                // segment, but as a *segment* it is evaluated, not a closure
                // value. mutsu spells that `DoStmt(Block)`; lowering it through
                // the ordinary expression path would build a closure and
                // interpolate its stringification instead of its result.
                if seg.class == RakuAstClass::Block {
                    parts.push(Expr::DoStmt(Box::new(Stmt::Block(lower_block(seg)?))));
                    continue;
                }
                parts.push(lower_expr(seg)?);
            }
            Ok(Expr::StringInterpolation(parts))
        }
        RakuAstClass::QuotedRegex => {
            let body = named_child(node, "body")?;
            let match_immediately = bool_field(node, "match-immediately")?;
            let adverbs = match node
                .fields
                .iter()
                .find(|field| field.name == Some("adverbs"))
            {
                Some(field) => match &field.value {
                    RakuAstFieldValue::List(items) => items
                        .iter()
                        .map(|item| {
                            let ValueView::RakuAst(adverb) = item.view() else {
                                return Err(unsupported(node));
                            };
                            lower_regex_adverb(adverb)
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                    _ => return Err(unsupported(node)),
                },
                None => Vec::new(),
            };
            let tree = RegexTree {
                body: lower_regex_node(body)?,
                match_immediately,
                adverbs,
                declaration_kind: None,
            };
            let value = regex_execution_value(&tree)?;
            if tree.match_immediately {
                Ok(Expr::MatchRegexTree { value, tree })
            } else {
                Ok(Expr::RegexLiteral { value, tree })
            }
        }
        RakuAstClass::StatementExpression => lower_expr(named_child(node, "expression")?),
        // A `Block` in expression position (e.g. the `{ … }` argument to `.map`) is
        // a bare-block closure value. (raku itself EVALs a `Block` node to a
        // Callable, so a hash-shaped `{a => 1}` also lands here as a block.)
        // Placeholder declarations are represented in the execution AST as
        // caret-prefixed variables, so let the existing closure builder
        // recover the implicit signature when this is a constructed tree.
        RakuAstClass::Block => {
            let body = lower_block(node)?;
            if crate::ast::collect_placeholders_shallow(&body).is_empty() {
                if crate::ast::body_reads_args_array(&body)
                    || crate::ast::body_reads_args_hash(&body)
                {
                    if crate::ast::body_reads_args_hash(&body) {
                        Ok(crate::ast::make_rakuast_anon_sub(body))
                    } else {
                        Ok(crate::ast::make_anon_sub(body))
                    }
                } else {
                    Ok(Expr::AnonSub {
                        body,
                        is_rw: false,
                        is_raw: false,
                        is_block: true,
                        doc: Default::default(),
                    })
                }
            } else {
                Ok(crate::ast::make_anon_sub(body))
            }
        }
        // A nameless `RakuAST::Sub` in expression position is an anonymous
        // routine (`sub { … }`, `sub ($a, $b) { … }`). Unlike a pointy block it
        // keeps its `sub` spelling on the way back, so a re-read of the lowered
        // tree renders the same node.
        RakuAstClass::Sub if !node.fields.iter().any(|f| f.name == Some("name")) => {
            let (params, param_defs) = signature_positional_params(node)?;
            let (return_type, custom_traits) = routine_return_type(node, None)?;
            if !custom_traits.is_empty() {
                // Only the `-->` spelling survives an anonymous sub's internal
                // node (it keeps no `custom_traits`), so a `returns`/`of` trait
                // would be silently dropped. Refuse instead.
                return Err(unsupported(node));
            }
            // A Sub's `body` is the Blockoid directly (not a Block wrapping one).
            let body = lower_stmts(named_child_or_positional(named_child(node, "body")?)?)?;
            if params.is_empty() && return_type.is_none() {
                return Ok(Expr::AnonSub {
                    body,
                    is_rw: false,
                    is_raw: false,
                    is_block: false,
                    doc: Default::default(),
                });
            }
            Ok(Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw: false,
                is_raw: false,
                custom_traits: Default::default(),
                is_whatever_code: false,
                declarator: crate::ast::RoutineDeclarator::Sub,
            })
        }
        // A pointy block in expression position (`-> $x { … }`) is a closure. A
        // single plain parameter without a default lowers to `Expr::Lambda`;
        // zero or several to
        // `AnonSubParams` — which is exactly what the parser builds for
        // `-> { … }`, an arity-0 closure that (unlike a bare block) rejects
        // arguments. Keep a typed single parameter on `AnonSubParams`: Lambda
        // has no field for its type constraint, so collapsing it would make a
        // constructed `-> Int $x { … }` accept values that Rakudo rejects.
        RakuAstClass::PointyBlock => {
            let (params, param_defs) = signature_positional_params(node)?;
            let body = lower_block(node)?;
            match params.len() {
                // Only a plain parameter fits `Lambda`; an optional (`$p?`),
                // trait-carrying or destructuring (`-> [$a, $b]`) one keeps its
                // `ParamDef`, as the parser does.
                1 if param_defs.first().is_some_and(|param| {
                    !param.named
                        && param.type_constraint.is_none()
                        && param.default.is_none()
                        && !param.optional_marker
                        && param.traits.is_empty()
                        && param.sub_signature.is_none()
                }) =>
                {
                    Ok(Expr::Lambda {
                        param: params.into_iter().next().unwrap(),
                        body,
                        is_whatever_code: false,
                        param_sigilless: param_defs.first().is_some_and(|pd| pd.sigilless),
                    })
                }
                _ => Ok(Expr::AnonSubParams {
                    params,
                    param_defs,
                    return_type: None,
                    body,
                    is_rw: false,
                    is_raw: false,
                    custom_traits: Default::default(),
                    is_whatever_code: false,
                    declarator: crate::ast::RoutineDeclarator::Block,
                }),
            }
        }
        // `(EXPR)` -> its inner expression (parens are transparent for EVAL). The
        // node wraps a `SemiList` of `Statement::Expression`s; only the
        // single-statement form is handled.
        RakuAstClass::CircumfixParentheses => {
            let semilist = named_child_or_positional(node)?;
            let inner = named_child_or_positional(semilist)?;
            // The contents of `(...)` are a semilist of *statements*: a
            // declaration written there (`(my $x = 9) given 2`) is a statement,
            // and mutsu carries a statement in expression position as `DoStmt`.
            let declaration = if inner.class == RakuAstClass::StatementExpression {
                named_child(inner, "expression").ok()
            } else {
                Some(inner)
            };
            if let Some(declaration) = declaration
                && matches!(
                    declaration.class,
                    RakuAstClass::VarDeclarationSimple
                        | RakuAstClass::VarDeclarationConstant
                        | RakuAstClass::VarDeclarationSignature
                )
            {
                return Ok(Expr::DoStmt(Box::new(lower_stmt_inner(declaration)?)));
            }
            let lowered = lower_expr(inner)?;
            // A parenthesized bareword fat-arrow is a positional Pair at the
            // call site (`f((a => 1))`), even though RakuAST represents the
            // pair itself with the same `FatArrow` node as an unparenthesized
            // named argument. Preserve the parser's `PositionalPair` marker
            // across the RakuAST round trip so call-site namedness remains
            // observable to the compiler.
            if matches!(
                &lowered,
                Expr::Binary {
                    op: crate::token_kind::TokenKind::FatArrow,
                    ..
                }
            ) {
                return Ok(Expr::PositionalPair(Box::new(lowered)));
            }
            Ok(lowered)
        }
        RakuAstClass::CircumfixHashComposer => super::hash_literal::lower_composer(node),
        RakuAstClass::ContextualizerHash => super::contextualizer::lower(node, ContextKind::Hash),
        RakuAstClass::ContextualizerItem => super::contextualizer::lower(node, ContextKind::Item),
        RakuAstClass::ContextualizerList => super::contextualizer::lower(node, ContextKind::List),
        // `[1, 2, 3]` -> an array literal. The composer wraps a `SemiList` of a
        // single `Statement::Expression` (a comma list, or a lone element).
        RakuAstClass::CircumfixArrayComposer => {
            let semilist = named_child_or_positional(node)?;
            let inner = named_child_or_positional(semilist)?;
            let items = match lower_expr(inner)? {
                Expr::ArrayLiteral(items) => items,
                other => vec![other],
            };
            Ok(Expr::BracketArray(items, false))
        }
        // A bareword naming something the unit declared, or a dynamic
        // `::(...)` name. Both are represented by RakuAST::Term::Name; the
        // nested Name part tells the lowerer which internal expression to keep.
        RakuAstClass::TermName => lower_term_name(named_child_or_positional(node)?),
        // `.method(...)` on the topic: the same method call `$_.method(...)`
        // compiles to.
        RakuAstClass::TermTopicCall => {
            let call = named_child_or_positional(node)?;
            if call.class != RakuAstClass::CallMethod {
                return Err(unsupported(node));
            }
            Ok(Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name: crate::symbol::Symbol::intern(&call_name_str(call)?),
                args: arg_exprs(call)?,
                modifier: dispatch_modifier(call)?,
                quoted: false,
            })
        }
        // `[+] @a` / `[\\+] @a` -> a reduction over a single argument. mutsu's
        // `Expr::Reduction` keeps the triangle form in the operator string
        // itself (a leading backslash), which is how the converter reads it
        // back out into the `triangle` field.
        RakuAstClass::TermReduce => {
            let infix = named_child(node, "infix")?;
            if infix.class != RakuAstClass::Infix {
                return Err(unsupported(node));
            }
            let infix_value = positional_leaf(infix)?;
            let op = match infix_value.view() {
                ValueView::Str(s) => s.to_string(),
                _ => return Err(unsupported(node)),
            };
            let triangle = bool_field(node, "triangle")?;
            let args = named_child(node, "args")?;
            // A reduction takes exactly one argument expression (a list, an
            // array variable, ...); the converter never builds more.
            let [only] = args.fields.as_slice() else {
                return Err(unsupported(node));
            };
            Ok(Expr::Reduction {
                op: if triangle { format!("\\{op}") } else { op },
                expr: Box::new(lower_expr(child_node(&only.value)?)?),
            })
        }
        // The `*` whatever term — the *value* leaf (`1..*`, `@a[*]`).
        RakuAstClass::TermWhatever => Ok(Expr::Whatever),
        // The `*` priming-argument leaf (`* + 1`, `* > 3`). ADR-0033 splits the
        // two leaf roles; the enclosing priming *scope* is planted afterwards by
        // `whatever_curry`, not here — see `lower`'s entry point.
        RakuAstClass::WhateverCodeArgument => Ok(Expr::WhateverArg),
        // `do { … }` -> a do-block expression over the lowered block body.
        RakuAstClass::StatementPrefixDo => {
            let block = named_child_or_positional(node)?;
            Ok(Expr::DoBlock {
                body: lower_block(block)?,
                label: None,
                // The round-trip of a source `do { … }`, so it carries the same
                // block identity the parser gives that form (GH-7635).
                origin: crate::ast::DoBlockOrigin::SourceBlock,
            })
        }
        // `try { … }` -> a try expression over the lowered block body.
        RakuAstClass::StatementPrefixTry => {
            let block = named_child_or_positional(node)?;
            Ok(Expr::Try {
                body: lower_block(block)?,
                catch: None,
            })
        }
        // `gather { … }` -> a gather expression over the lowered block body.
        RakuAstClass::StatementPrefixGather => {
            let block = named_child_or_positional(node)?;
            Ok(Expr::Gather(lower_block(block)?))
        }
        // A `FatArrow` is raku's node for a BAREWORD key (`a => 1`), which is a
        // *named* argument -- mutsu spells that as a bare `Binary{FatArrow}`.
        // The quoted/computed spelling arrives as an `ApplyInfix` over `=>`
        // instead and lowers, below, to the `PositionalPair` that marks it
        // positional.
        RakuAstClass::ColonPairTrue | RakuAstClass::ColonPairFalse => {
            let value = positional_leaf(node)?;
            let ValueView::Str(key) = value.view() else {
                return Err(unsupported(node));
            };
            let value = if node.class == RakuAstClass::ColonPairTrue {
                Value::TRUE
            } else {
                Value::FALSE
            };
            Ok(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(key.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::Literal(value)),
            })
        }
        RakuAstClass::ColonPairVariable | RakuAstClass::ColonPairValue => {
            let key = leaf_str(node, "key")?;
            let value = lower_expr(named_child(node, "value")?)?;
            Ok(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(key))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(value),
            })
        }
        RakuAstClass::FatArrow => {
            let key = leaf_str(node, "key")?;
            let value = lower_expr(named_child(node, "value")?)?;
            Ok(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(key))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(value),
            })
        }
        // A bare type name `Int` (a `Type::Simple`) in expression position -> a
        // bareword term, which mutsu evaluates to the type object. `Nil` is
        // the parser's `Nil` literal instead (`term_literals`), which is what
        // `is default` restores on assignment.
        RakuAstClass::TypeSimple => match simple_type_name(node, node)?.as_str() {
            "Nil" => Ok(Expr::Literal(Value::NIL)),
            name => Ok(Expr::BareWord(name.to_string())),
        },
        // `self` -> the bareword the parser produces for it.
        RakuAstClass::TermSelf => Ok(Expr::BareWord("self".to_string())),
        // `True`/`False` -> the Bool literal; any other setting enum value
        // (`Less`, `Kept`) -> the bareword the parser produces for it.
        RakuAstClass::TermEnum => match positional_leaf(node)?.view() {
            ValueView::Str(s) if !s.is_empty() => Ok(term_identifier_expr(&s)),
            _ => Err(unsupported(node)),
        },
        // `$x` / `@a` / `%h` / `&f` -> the sigil-specific variable expression.
        RakuAstClass::VarLexical | RakuAstClass::VarPackage => {
            let name = variable_spelling(node)?;
            let (sigil, bare) = name.split_at(name.chars().next().map_or(0, char::len_utf8));
            Ok(match sigil {
                "@" => Expr::ArrayVar(bare.to_string()),
                "%" => Expr::HashVar(bare.to_string()),
                "&" => Expr::CodeVar(bare.to_string()),
                _ => Expr::Var(bare.to_string()),
            })
        }
        // RakuAST's implicit scalar placeholder declaration (`$^x`) lowers
        // back to the caret-prefixed lexical name used by the parser's
        // `make_anon_sub` path.
        RakuAstClass::VarDeclarationPlaceholderPositional => {
            let name = positional_leaf(node)?;
            let ValueView::Str(name) = name.view() else {
                return Err(unsupported(node));
            };
            let Some(name) = name.strip_prefix('$') else {
                return Err(unsupported(node));
            };
            if name.is_empty()
                || !name
                    .chars()
                    .all(|ch| ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-' | '\''))
            {
                return Err(unsupported(node));
            }
            Ok(Expr::Var(format!("^{name}")))
        }
        // RakuAST's implicit flattened array placeholder (`@_`) lowers back
        // to the legacy array variable used by `make_anon_sub`.
        RakuAstClass::VarDeclarationPlaceholderSlurpyArray => Ok(Expr::ArrayVar("_".to_string())),
        // RakuAST's implicit named hash placeholder (`%_`) lowers back to the
        // legacy hash variable used by `make_anon_sub`.
        RakuAstClass::VarDeclarationPlaceholderSlurpyHash => Ok(Expr::HashVar("_".to_string())),
        // `($x OP= EXPR)` in expression position -> a compound assignment.
        RakuAstClass::ApplyInfix if infix_is_compound_assignment(node) => {
            lower_compound_assign_expr(node)
        }
        // `($x = EXPR)` in expression position -> an assignment expression.
        RakuAstClass::ApplyInfix if infix_is_assignment(node) => {
            if let Some(assign) = subscript_assign(node)? {
                return Ok(assign);
            }
            let (name, expr) = lower_assign_parts(node)?;
            Ok(Expr::AssignExpr {
                name,
                expr: Box::new(expr),
                is_bind: false,
            })
        }
        // `($x := EXPR)` in expression position -> a binding expression.
        RakuAstClass::ApplyInfix if infix_is_bind_to_variable(node) => {
            let (name, expr) = lower_assign_parts(node)?;
            Ok(Expr::AssignExpr {
                name,
                expr: Box::new(expr),
                is_bind: true,
            })
        }
        // `@a >>+<< @b` -> the hyper operator, keeping both dwim flags.
        RakuAstClass::ApplyInfix
            if named_child(node, "infix")
                .is_ok_and(|i| i.class == RakuAstClass::MetaInfixHyper) =>
        {
            let hyper = named_child(node, "infix")?;
            let infix = named_child(hyper, "infix")?;
            let left = Box::new(lower_expr(named_child(node, "left")?)?);
            let right = Box::new(lower_expr(named_child(node, "right")?)?);
            let dwim_left = bool_field(hyper, "dwim-left")?;
            let dwim_right = bool_field(hyper, "dwim-right")?;
            if infix.class == RakuAstClass::FunctionInfix {
                let function = positional_leaf(infix)?;
                let ValueView::RakuAst(function) = function.view() else {
                    return Err(unsupported(node));
                };
                let Expr::CodeVar(func_name) = lower_expr(function)? else {
                    return Err(unsupported(node));
                };
                return Ok(Expr::HyperFuncOp {
                    func_name,
                    left,
                    right,
                    dwim_left,
                    dwim_right,
                });
            }
            let op_value = positional_leaf(infix)?;
            let ValueView::Str(op) = op_value.view() else {
                return Err(unsupported(node));
            };
            Ok(Expr::HyperOp {
                op: op.to_string(),
                left,
                right,
                dwim_left,
                dwim_right,
            })
        }
        RakuAstClass::ApplyInfix => {
            let left = lower_expr(named_child(node, "left")?)?;
            let right = lower_expr(named_child(node, "right")?)?;
            let op = infix_token(named_child(node, "infix")?)?;
            let binary = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            // `=>` as an ordinary infix is raku's node for a non-bareword key
            // (`"a" => 1`, `$k => 1`), which is a POSITIONAL pair rather than a
            // named argument. mutsu marks that with `PositionalPair`; the
            // bareword spelling is a `FatArrow` node and lowers bare, above.
            if matches!(
                &binary,
                Expr::Binary {
                    op: crate::token_kind::TokenKind::FatArrow,
                    ..
                }
            ) {
                return Ok(Expr::PositionalPair(Box::new(binary)));
            }
            Ok(binary)
        }
        RakuAstClass::ApplyPrefix => {
            let operand = lower_expr(named_child(node, "operand")?)?;
            let op = prefix_token(named_child(node, "prefix")?)?;
            Ok(Expr::Unary {
                op,
                expr: Box::new(operand),
            })
        }
        // `COND ?? THEN !! ELSE` -> the ternary expression.
        RakuAstClass::Ternary => Ok(Expr::Ternary {
            cond: Box::new(lower_expr(named_child(node, "condition")?)?),
            then_expr: Box::new(lower_expr(named_child(node, "then")?)?),
            else_expr: Box::new(lower_expr(named_child(node, "else")?)?),
        }),
        // A named call `f(1, 2)` -> Expr::Call (the listop I/O calls are handled
        // as statements in `lower_stmt_inner`; here they are ordinary calls too).
        // An argument-less `Call::Name` of a stash name is how Rakudo renders
        // an unresolved stash lookup `Foo::`; it is the same lookup.
        RakuAstClass::CallName if let Some(stash) = call_name_stash(node) => {
            Ok(Expr::PseudoStash(stash))
        }
        RakuAstClass::CallName | RakuAstClass::CallNameWithoutParentheses => Ok(Expr::Call {
            name: crate::symbol::Symbol::intern(&call_name_str(node)?),
            args: arg_exprs(node)?,
        }),
        // A comma list `1, 2, 3` (or parenthesised `(1, 2, 3)`) -> ApplyListInfix
        // with a `,` infix. `andthen` / `orelse` / `notandthen` are list infixes
        // in raku too, but mutsu keeps them as ordinary left-nested `Binary`
        // nodes, so they fold back into that shape. Other list infixes
        // (`Z`/`X`/...) are deferred.
        RakuAstClass::ApplyListInfix => {
            let infix = named_child(node, "infix")?;
            let infix_value = positional_leaf(infix)?;
            let ValueView::Str(op) = infix_value.view() else {
                return Err(unsupported(node));
            };
            let op = op.to_string();
            let mut items = Vec::new();
            for v in list_field(node, "operands")? {
                let ValueView::RakuAst(child) = v.view() else {
                    return Err(unsupported(node));
                };
                items.push(lower_expr(child)?);
            }
            if op == "," {
                return Ok(Expr::ArrayLiteral(items));
            }
            // Every other list infix mutsu renders — the `andthen` family, the
            // junction constructors, `min`/`max` — is an ordinary left-nested
            // `Expr::Binary` internally, so the operator name maps straight back
            // to its token.
            let Some(token) = crate::compiler::helpers_ops::op_name_to_token_kind(&op) else {
                return Err(unsupported(node));
            };
            // These chain left-associatively: `a andthen b andthen c` is
            // `(a andthen b) andthen c`, which is how the parser builds it.
            let mut items = items.into_iter();
            let Some(first) = items.next() else {
                return Err(unsupported(node));
            };
            Ok(items.fold(first, |left, right| Expr::Binary {
                left: Box::new(left),
                op: token.clone(),
                right: Box::new(right),
            }))
        }
        // A postfix method call (`$x.abs`, `$x.?abs`, `$x."abs"()`), a hyper
        // method call (`@a>>.abs`), a postfix operator (`$x++`), or a positional
        // subscript (`@a[1]`). Associative subscripts carry a different postfix
        // and are deferred.
        RakuAstClass::ApplyPostfix => {
            let operand = lower_expr(named_child(node, "operand")?)?;
            let postfix = named_child(node, "postfix")?;
            match postfix.class {
                RakuAstClass::CallMethod => Ok(Expr::MethodCall {
                    target: Box::new(operand),
                    name: crate::symbol::Symbol::intern(&call_name_str(postfix)?),
                    args: arg_exprs(postfix)?,
                    modifier: dispatch_modifier(postfix)?,
                    quoted: false,
                }),
                // `$x."name"()` -> Call::QuotedMethod, whose `name` is a
                // QuotedString rather than a Name. An interpolated name lowers
                // to the existing DynamicMethodCall execution path.
                RakuAstClass::CallQuotedMethod => {
                    let name_expr = lower_expr(named_child(postfix, "name")?)?;
                    match name_expr {
                        Expr::Literal(value) if matches!(value.view(), ValueView::Str(_)) => {
                            Ok(Expr::MethodCall {
                                target: Box::new(operand),
                                name: crate::symbol::Symbol::intern(&value.to_string_value()),
                                args: arg_exprs(postfix)?,
                                modifier: None,
                                quoted: true,
                            })
                        }
                        name_expr => Ok(Expr::DynamicMethodCall {
                            target: Box::new(operand),
                            name_expr: Box::new(name_expr),
                            args: arg_exprs(postfix)?,
                            modifier: None,
                            quoted: true,
                        }),
                    }
                }
                // `.^name` -> a metamethod call. Its `name` is a plain string,
                // not a `Name` node, and mutsu keeps the `^` in the same
                // `modifier` slot the dispatch modifiers use.
                RakuAstClass::CallMetaMethod => Ok(Expr::MethodCall {
                    target: Box::new(operand),
                    name: crate::symbol::Symbol::intern(&leaf_str(postfix, "name")?),
                    args: arg_exprs(postfix)?,
                    modifier: Some('^'),
                    quoted: false,
                }),
                // `@a>>.abs` -> MetaPostfix::Hyper wrapping the ordinary
                // method-call postfix.
                RakuAstClass::MetaPostfixHyper => {
                    let inner = named_child_or_positional(postfix)?;
                    let (name, quoted) = match inner.class {
                        RakuAstClass::CallMethod => (call_name_str(inner)?, false),
                        RakuAstClass::CallQuotedMethod => (quoted_method_name(inner)?, true),
                        _ => return Err(unsupported(node)),
                    };
                    Ok(Expr::HyperMethodCall {
                        target: Box::new(operand),
                        name: crate::symbol::Symbol::intern(&name),
                        args: arg_exprs(inner)?,
                        modifier: if quoted {
                            None
                        } else {
                            dispatch_modifier(inner)?
                        },
                        quoted,
                    })
                }
                // `$x++` / `$x--` -> Postfix(operator => "++").
                RakuAstClass::Postfix => Ok(Expr::PostfixOp {
                    op: postfix_token(postfix)?,
                    expr: Box::new(operand),
                }),
                // `$f(EXPR)` -> Call::Term(args) -> a call on the operand term.
                RakuAstClass::CallTerm => Ok(Expr::CallOn {
                    target: Box::new(operand),
                    args: arg_exprs(postfix)?,
                }),
                // `@a[EXPR]` / `%h{EXPR}` -> Postcircumfix::*Index(index =>
                // SemiList(Statement::Expression(EXPR))).
                RakuAstClass::PostcircumfixArrayIndex
                | RakuAstClass::PostcircumfixHashIndex
                | RakuAstClass::PostcircumfixLiteralHashIndex => {
                    let index_node = named_child(postfix, "index")?;
                    let index = if postfix.class == RakuAstClass::PostcircumfixLiteralHashIndex {
                        lower_expr(index_node)?
                    } else {
                        lower_expr(named_child_or_positional(index_node)?)?
                    };
                    let is_positional =
                        matches!(postfix.class, RakuAstClass::PostcircumfixArrayIndex);
                    // `@a[0] = 1`: rakudo folds an assignment to a subscript
                    // into the postcircumfix's `assignee`; the parser keeps it
                    // as `IndexAssign`.
                    if let Ok(assignee) = named_child(postfix, "assignee") {
                        if list_field(postfix, "colonpairs").is_ok_and(|c| !c.is_empty()) {
                            return Err(unsupported(postfix));
                        }
                        return Ok(Expr::IndexAssign {
                            target: Box::new(operand),
                            index: Box::new(index),
                            value: Box::new(lower_expr(assignee)?),
                            is_positional,
                        });
                    }
                    super::subscript_adverb::lower(
                        Expr::Index {
                            target: Box::new(operand),
                            index: Box::new(index),
                            is_positional,
                        },
                        postfix,
                    )
                }
                _ => Err(unsupported(node)),
            }
        }
        _ => Err(unsupported(node)),
    }
}

/// The `.?` / `.+` / `.*` dispatch modifier of a `Call::Method`, as the single
/// character mutsu's `MethodCall.modifier` keeps. The field is absent for a
/// plain `.method`.
fn dispatch_modifier(node: &RakuAstNode) -> Result<Option<char>, RuntimeError> {
    let Some(f) = node.fields.iter().find(|f| f.name == Some("dispatch")) else {
        return Ok(None);
    };
    let RakuAstFieldValue::Node(v) = &f.value else {
        return Err(unsupported(node));
    };
    let ValueView::Str(s) = v.view() else {
        return Err(unsupported(node));
    };
    // The rendered form is `.?` / `.+` / `.*`; mutsu keeps only the modifier.
    match s.strip_prefix('.').and_then(|rest| {
        let mut chars = rest.chars();
        match (chars.next(), chars.next()) {
            (Some(c), None) => Some(c),
            _ => None,
        }
    }) {
        Some(c) => Ok(Some(c)),
        None => Err(unsupported(node)),
    }
}

/// The name of a `Call::QuotedMethod`, whose `name` child is a `QuotedString`
/// rather than a `Name`. Only a single `StrLiteral` segment lowers; an
/// interpolated method name is a different internal node.
fn quoted_method_name(node: &RakuAstNode) -> Result<String, RuntimeError> {
    let quoted = named_child(node, "name")?;
    if quoted.class != RakuAstClass::QuotedString {
        return Err(unsupported(node));
    }
    let Some(f) = quoted.fields.iter().find(|f| f.name == Some("segments")) else {
        return Err(unsupported(node));
    };
    let RakuAstFieldValue::List(items) = &f.value else {
        return Err(unsupported(node));
    };
    let [only] = items.as_slice() else {
        return Err(unsupported(node));
    };
    let ValueView::RakuAst(seg) = only.view() else {
        return Err(unsupported(node));
    };
    if seg.class != RakuAstClass::StrLiteral {
        return Err(unsupported(node));
    }
    match positional_leaf(seg)?.view() {
        ValueView::Str(s) => Ok(s.to_string()),
        _ => Err(unsupported(node)),
    }
}

/// The `TokenKind` for a `Postfix` operator node. Unlike `Infix`/`Prefix`, a
/// `Postfix` carries its operator in a NAMED `operator` field.
fn postfix_token(node: &RakuAstNode) -> Result<crate::token_kind::TokenKind, RuntimeError> {
    let name = leaf_str(node, "operator")?;
    crate::compiler::helpers_ops::op_name_to_token_kind(&name).ok_or_else(|| unsupported(node))
}

/// The `TokenKind` for an `Infix`/`Prefix` operator node (its positional operator
/// string), or an error for an operator the lowerer doesn't handle yet.
fn infix_token(node: &RakuAstNode) -> Result<crate::token_kind::TokenKind, RuntimeError> {
    let name = positional_leaf(node)?;
    let ValueView::Str(s) = name.view() else {
        return Err(unsupported(node));
    };
    crate::compiler::helpers_ops::op_name_to_token_kind(&s).ok_or_else(|| unsupported(node))
}

/// Resolve a `Prefix` operator's positional spelling in prefix context.
fn prefix_token(node: &RakuAstNode) -> Result<crate::token_kind::TokenKind, RuntimeError> {
    let name = positional_leaf(node)?;
    let ValueView::Str(s) = name.view() else {
        return Err(unsupported(node));
    };
    crate::compiler::helpers_ops::prefix_op_name_to_token_kind(&s).ok_or_else(|| unsupported(node))
}

/// An optional boolean-valued named field (an omitted field is `False`, which
/// is exactly how raku's gist elides a false `dwim-left` / `dwim-right`).
fn bool_field(node: &RakuAstNode, name: &str) -> Result<bool, RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some(name)) {
        None => Ok(false),
        Some(f) => match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Bool(b) => Ok(b),
                _ => Err(unsupported(node)),
            },
            _ => Err(unsupported(node)),
        },
    }
}

/// The value of a node's single positional (name-less) leaf field.
pub(super) fn positional_leaf(node: &RakuAstNode) -> Result<Value, RuntimeError> {
    match node.fields.first() {
        Some(f) if f.name.is_none() => match &f.value {
            RakuAstFieldValue::Node(v) => Ok(v.clone()),
            _ => Err(unsupported(node)),
        },
        _ => Err(unsupported(node)),
    }
}

/// The child RakuAST node of a named field.
pub(super) fn named_child<'a>(
    node: &'a RakuAstNode,
    name: &str,
) -> Result<&'a RakuAstNode, RuntimeError> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => rakuast_node_of(v),
            _ => None,
        })
        .ok_or_else(|| unsupported(node))
}

/// The RakuAST node a field value holds, seeing through a role mixed in with
/// `but` (`RakuAST::Term::TopicCall.new(...) but Type('and')`): the mixin
/// only adds methods for the caller's own bookkeeping, and the node it
/// wraps is what lowers.
// Cost: O(d), d = depth of nested mixins (normally 0 or 1).
pub(super) fn rakuast_node_of(v: &Value) -> Option<&RakuAstNode> {
    match v.view() {
        ValueView::RakuAst(node) => Some(node),
        ValueView::Mixin(inner, _) => rakuast_node_of(inner),
        _ => None,
    }
}

/// The child RakuAST node wrapped in a `Node` field value.
fn child_node(fv: &RakuAstFieldValue) -> Result<&RakuAstNode, RuntimeError> {
    if let RakuAstFieldValue::Node(v) = fv
        && let Some(child) = rakuast_node_of(v)
    {
        return Ok(child);
    }
    Err(RuntimeError::new("RakuAST: EVAL expected a child node"))
}

pub(super) fn list_field<'a>(
    node: &'a RakuAstNode,
    name: &str,
) -> Result<&'a [Value], RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some(name)) {
        Some(f) => match &f.value {
            RakuAstFieldValue::List(items) => Ok(items),
            _ => Err(unsupported(node)),
        },
        None => Err(unsupported(node)),
    }
}
