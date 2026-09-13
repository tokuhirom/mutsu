//! Internal AST (`Stmt`/`Expr`) → RakuAST node tree (read direction, ADR-0011).
//!
//! Covered so far: the literal + say-call cluster (Phase 1); and variables,
//! plain `my` declarations, infix/prefix/postfix operators, `=` assignment, and
//! method calls (Phase 2). Constructs outside that set produce an explicit
//! `RuntimeError` (the documented coverage boundary) rather than a
//! silently-wrong node.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{
    AssignOp, EnumVariantForm, Expr, ForMode, GivenWithKind, ParamDef, Stmt, WithBlockKind,
};
use crate::compiler::helpers_ops::token_kind_to_op_name;
use crate::regex_tree::{RegexNode, RegexQuantifier, RegexTree};
use crate::runtime::utils::is_known_type_constraint;
use crate::value::{RuntimeError, Value, ValueView};

fn unsupported(what: &str) -> RuntimeError {
    RuntimeError::new(format!(
        "RakuAST: `.AST` does not yet support this construct: {what}"
    ))
}

fn node_field(name: Option<&'static str>, node: RakuAstNode) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
    }
}

fn leaf_field(name: Option<&'static str>, value: Value) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(value),
    }
}

/// Top-level: a parsed program becomes a `RakuAST::StatementList`.
pub(super) fn statement_list(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let _scope = DeclaredNames::collect(stmts);
    statement_list_inner(stmts)
}

fn statement_list_inner(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    for stmt in stmts {
        if let Some(node) = convert_stmt(stmt)? {
            fields.push(node_field(None, node));
        }
    }
    Ok(RakuAstNode {
        class: RakuAstClass::StatementList,
        fields,
    })
}

/// What a bareword naming something the same compilation unit declared means.
///
/// raku resolves such a name at parse time, so `class C { }; C.new` renders `C`
/// as a `Type::Simple` — exactly like a builtin type — and
/// `constant X = 5; X` renders `X` as a `Term::Name`. Both measured against
/// rakudo 2026.07. mutsu's parser leaves both as `Expr::BareWord`, so the
/// converter has to re-derive which is which from the unit's own declarations.
#[derive(Clone, Copy, PartialEq, Eq)]
enum DeclaredKind {
    /// A `class` / `role` / `grammar` / `enum` name.
    Type,
    /// A `constant` name.
    Constant,
}

thread_local! {
    /// The names the compilation unit currently being converted declares.
    /// Empty outside a conversion, so a nested/re-entrant conversion that never
    /// ran `statement_list` simply sees no declarations and keeps the old
    /// bareword boundary.
    static DECLARED_NAMES: std::cell::RefCell<std::collections::HashMap<String, DeclaredKind>> =
        std::cell::RefCell::new(std::collections::HashMap::new());
}

/// RAII guard installing the unit's declared names for the duration of a
/// conversion, restoring whatever was there before (so a nested conversion
/// cannot leak its names into the outer one).
struct DeclaredNames(std::collections::HashMap<String, DeclaredKind>);

impl DeclaredNames {
    fn collect(stmts: &[Stmt]) -> Self {
        let mut names = std::collections::HashMap::new();
        collect_declared_names(stmts, &mut names);
        Self(DECLARED_NAMES.with(|d| std::mem::replace(&mut *d.borrow_mut(), names)))
    }
}

impl Drop for DeclaredNames {
    fn drop(&mut self) {
        DECLARED_NAMES.with(|d| {
            *d.borrow_mut() = std::mem::take(&mut self.0);
        });
    }
}

fn declared_kind(name: &str) -> Option<DeclaredKind> {
    DECLARED_NAMES.with(|d| d.borrow().get(name).copied())
}

/// Walk a statement list for the names it declares. Nested blocks count: raku
/// resolves a name declared anywhere the reference can see it, and a bareword
/// that reaches conversion at all was already accepted by the parser.
fn collect_declared_names(
    stmts: &[Stmt],
    out: &mut std::collections::HashMap<String, DeclaredKind>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::ClassDecl { name, body, .. } => {
                out.insert(name.resolve(), DeclaredKind::Type);
                collect_declared_names(body, out);
            }
            Stmt::RoleDecl { name, body, .. } => {
                out.insert(name.resolve(), DeclaredKind::Type);
                collect_declared_names(body, out);
            }
            Stmt::EnumDecl { name, .. } | Stmt::SubsetDecl { name, .. } => {
                out.insert(name.resolve(), DeclaredKind::Type);
            }
            Stmt::VarDecl {
                name,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(n, _)| n == "__constant") => {
                out.insert(name.clone(), DeclaredKind::Constant);
            }
            // A `module`/`package`/`grammar` name resolves at parse time just
            // like a class one: raku renders a later bareword `M` as a
            // `Type::Simple` (measured on `module M { }; M.HOW`).
            Stmt::Package { name, body, .. } => {
                out.insert(name.resolve(), DeclaredKind::Type);
                collect_declared_names(body, out);
            }
            Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::SubDecl { body, .. }
            | Stmt::MethodDecl { body, .. } => collect_declared_names(body, out),
            _ => {}
        }
    }
}

/// Convert one statement. Returns `Ok(None)` for non-semantic bookkeeping
/// statements (e.g. `SetLine`) that carry no RakuAST representation.
fn convert_stmt(stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    match stmt {
        Stmt::SetLine(_) => Ok(None),
        // An expression statement modified by `with`/`without` is wrapped in a
        // `DoStmt` so it keeps expression semantics. The wrapper has no RakuAST
        // counterpart, so convert the `Given` it carries instead of rendering a
        // `Statement::Expression` around it.
        Stmt::Expr(Expr::DoStmt(inner))
            if matches!(
                inner.as_ref(),
                Stmt::Given {
                    with_kind: Some(_),
                    ..
                }
            ) =>
        {
            convert_stmt(inner)
        }
        Stmt::Expr(e) => Ok(Some(statement_expression(convert_expr(e)?))),
        Stmt::TokenDecl {
            name,
            params,
            param_defs,
            source_regex,
            regex_kind,
            multi,
            is_my,
            is_our,
            is_export,
            export_tags,
            ..
        } => {
            if *multi
                || *is_my
                || *is_our
                || *is_export
                || !export_tags.is_empty()
                || !params.is_empty()
                || !param_defs.is_empty()
            {
                return Err(unsupported(
                    "regex declaration with scope / params / traits",
                ));
            }
            let Some(tree) = source_regex else {
                return Err(unsupported("regex declaration without a source tree"));
            };
            let class = match regex_kind {
                crate::regex_tree::RegexDeclKind::Token => RakuAstClass::TokenDeclaration,
                crate::regex_tree::RegexDeclKind::Regex => RakuAstClass::RegexDeclaration,
                crate::regex_tree::RegexDeclKind::Rule => {
                    return Err(unsupported("rule declaration in TokenDecl"));
                }
            };
            Ok(Some(statement_expression(regex_declaration(
                class,
                &name.resolve(),
                tree,
            )?)))
        }
        Stmt::RuleDecl {
            name,
            params,
            param_defs,
            source_regex,
            multi,
            is_export,
            export_tags,
            ..
        } => {
            if *multi
                || *is_export
                || !export_tags.is_empty()
                || !params.is_empty()
                || !param_defs.is_empty()
            {
                return Err(unsupported("rule declaration with params / traits"));
            }
            let Some(tree) = source_regex else {
                return Err(unsupported("rule declaration without a source tree"));
            };
            Ok(Some(statement_expression(regex_declaration(
                RakuAstClass::RuleDeclaration,
                &name.resolve(),
                tree,
            )?)))
        }
        // Raku keeps an argument-less core pragma as a `Pragma` directly in
        // the statement list. Ordinary modules use `Statement::Use` instead;
        // do not reconstruct that distinction from every `Stmt::Use`.
        Stmt::Use {
            module,
            arg: None,
            tags,
            condition: None,
        } if tags.is_empty() && is_pragma_name(module) => Ok(Some(pragma_node(module))),
        // `say 42` / `put`/`print`/`note` as listops (no parens) parse to a
        // dedicated statement; raku models them as a call in WithoutParentheses
        // form.
        Stmt::Say(args) => Ok(Some(statement_expression(listop_call("say", args)?))),
        Stmt::Put(args) => Ok(Some(statement_expression(listop_call("put", args)?))),
        Stmt::Print(args) => Ok(Some(statement_expression(listop_call("print", args)?))),
        Stmt::Note(args) => Ok(Some(statement_expression(listop_call("note", args)?))),
        // `return`/`last`/`next` are control-flow statements in the internal AST;
        // raku models them as bare calls. A bare `return` (a `Nil` literal) and an
        // unlabelled `last`/`next` carry no arguments. Labelled `last LABEL` /
        // `next LABEL` are deferred.
        Stmt::Return(expr) => {
            let args: &[Expr] = match expr {
                Expr::Literal(v) if v.is_nil() => &[],
                other => std::slice::from_ref(other),
            };
            Ok(Some(statement_expression(control_call("return", args)?)))
        }
        Stmt::Last(None) => Ok(Some(statement_expression(control_call("last", &[])?))),
        Stmt::Next(None) => Ok(Some(statement_expression(control_call("next", &[])?))),
        Stmt::Redo(None) => Ok(Some(statement_expression(control_call("redo", &[])?))),
        Stmt::Last(Some(_)) | Stmt::Next(Some(_)) | Stmt::Redo(Some(_)) => {
            Err(unsupported("labelled last/next/redo"))
        }
        // `die`/`fail EXPR` are also modelled as bare calls.
        Stmt::Die(expr) => Ok(Some(statement_expression(control_call(
            "die",
            std::slice::from_ref(expr),
        )?))),
        Stmt::Fail(expr) => Ok(Some(statement_expression(control_call(
            "fail",
            std::slice::from_ref(expr),
        )?))),
        // `take EXPR` is a bare call too; `take-rw` stays the boundary.
        Stmt::Take(expr, is_rw) => {
            if *is_rw {
                return Err(unsupported("take-rw"));
            }
            Ok(Some(statement_expression(control_call(
                "take",
                std::slice::from_ref(expr),
            )?)))
        }
        Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic,
            custom_traits,
            where_constraint,
            ..
        } => {
            // `my`/`our`/`state` with an optional simple type. Dynamic (`$*x`),
            // `where` constraints, parameterised/definite/coercion types, and
            // real `is`/`does` traits carry richer shape, deferred.
            if *is_dynamic || where_constraint.is_some() {
                return Err(unsupported("dynamic / where-constrained declaration"));
            }
            // `constant X = 5` is a distinct raku node, not a scoped `my`.
            // mutsu marks it with a `__constant` pseudo-trait (plus a
            // `__constant_sigil` recording the declared sigil) and sets
            // `is_our` for the package-scoped default spelling.
            if custom_traits.iter().any(|(n, _)| n == "__constant") {
                return constant_declaration(name, expr, custom_traits, type_constraint, *is_our);
            }
            if custom_traits.iter().any(|(n, _)| n != "__has_initializer") {
                return Err(unsupported("declaration with traits"));
            }
            // build_type_node validates simple/definite and defers the rest.
            let type_name = type_constraint.as_deref();
            let scope = if *is_our {
                Some("our")
            } else if *is_state {
                Some("state")
            } else {
                None
            };
            let has_initializer = custom_traits
                .iter()
                .any(|(name, _)| name == "__has_initializer");
            let init = has_initializer.then_some(expr);
            Ok(Some(statement_expression(var_declaration(
                name, init, scope, type_name, None, None,
            )?)))
        }
        // A bare `{ ... }` block at statement level -> Statement::Expression(Block).
        Stmt::Block(body) => Ok(Some(statement_expression(block_node(body)?))),
        // `BEGIN { … }` / `INIT { … }` / `LEAVE { … }` / … -> a
        // `StatementPrefix::Phaser::<Kind>` wrapping the block positionally.
        // raku has one class per kind, which mutsu's single `PhaserKind` maps
        // onto 1:1. `PRE`/`POST` are the exception: rakudo desugars them into a
        // call around the block (an `ApplyPostfix` operand), and mutsu also
        // keeps a source-text `condition` for their exception message, so they
        // stay the boundary.
        Stmt::Phaser {
            kind,
            body,
            condition,
            ..
        } => {
            if condition.is_some() {
                return Err(unsupported("PRE/POST phaser condition"));
            }
            let class = match phaser_class(kind) {
                Some(c) => c,
                None => return Err(unsupported("PRE/POST phaser")),
            };
            Ok(Some(statement_expression(RakuAstNode {
                class,
                fields: vec![node_field(None, block_node(body)?)],
            })))
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var,
            is_statement_modifier,
            is_unless,
            with_kind,
        } => {
            if binding_var.is_some() {
                return Err(unsupported("`if EXPR -> $var` topic binding"));
            }
            // `with X { }` / `without X { }` reach here as the conditional the
            // parser desugared them into; `with_kind` records which keyword the
            // source wrote, because neither the shape nor the synthetic temp's
            // name can be trusted to say so (see `Stmt::If`'s `with_kind`).
            if let Some(kind) = with_kind {
                return with_block_node(*kind, cond, then_branch, else_branch).map(Some);
            }
            // mutsu stores `unless X` as `if !X` PLUS an `is_unless` flag, so the
            // source keyword is recoverable: raku has `Statement::Unless` (and
            // `StatementModifier::Unless`) and renders the *undecorated*
            // condition, so the `!` the parser added is stripped back off. Same
            // shape as `Stmt::While::is_until` below.
            let written_cond = if *is_unless {
                strip_negation(cond)?
            } else {
                cond
            };
            // A postfix `if`/`unless` introduces no block of its own: raku hangs
            // the condition off the modified statement as a `condition-modifier`
            // rather than building a `Statement::If` around it. Mirrors the
            // `given` modifier arm below.
            if *is_statement_modifier {
                let mut body = then_branch
                    .iter()
                    .filter(|s| !matches!(s, Stmt::SetLine(_)));
                let (Some(modified), None) = (body.next(), body.next()) else {
                    return Err(unsupported("multi-statement if/unless modifier body"));
                };
                if !else_branch.is_empty() {
                    return Err(unsupported("if/unless modifier with an else branch"));
                }
                let mut statement = convert_stmt(modified)?
                    .ok_or_else(|| unsupported("empty if/unless modifier body"))?;
                statement.fields.push(node_field(
                    Some("condition-modifier"),
                    RakuAstNode {
                        class: if *is_unless {
                            RakuAstClass::StatementModifierUnless
                        } else {
                            RakuAstClass::StatementModifierIf
                        },
                        fields: vec![node_field(None, convert_expr(written_cond)?)],
                    },
                ));
                return Ok(Some(statement));
            }
            // `unless` cannot carry `elsif`/`else` (rakudo rejects it at compile
            // time), so its node is just condition + body — note `body`, not
            // `then`.
            if *is_unless {
                return Ok(Some(RakuAstNode {
                    class: RakuAstClass::StatementUnless,
                    fields: vec![
                        node_field(Some("condition"), convert_expr(written_cond)?),
                        node_field(Some("body"), block_node(then_branch)?),
                    ],
                }));
            }
            let mut fields = vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("then"), block_node(then_branch)?),
            ];
            fields.extend(conditional_chain_fields(else_branch)?);
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementIf,
                fields,
            }))
        }
        Stmt::While {
            cond,
            body,
            label,
            is_until,
            ..
        } => {
            // mutsu stores `until X` as `while !X` PLUS an `is_until` flag, so
            // the source keyword is recoverable: raku has a `Loop::Until` class
            // and renders the *undecorated* condition, so the `!` the parser
            // added is stripped back off.
            let mut fields = label_fields(label);
            fields.push(node_field(
                Some("condition"),
                convert_expr(if *is_until {
                    strip_negation(cond)?
                } else {
                    cond
                })?,
            ));
            fields.push(node_field(Some("body"), block_node(body)?));
            Ok(Some(RakuAstNode {
                class: if *is_until {
                    RakuAstClass::StatementLoopUntil
                } else {
                    RakuAstClass::StatementLoopWhile
                },
                fields,
            }))
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat,
            label,
            is_until,
            ..
        } => {
            if *repeat {
                // `repeat { } while X` / `repeat { } until X`. As with the plain
                // `while`, mutsu negates the condition and keeps an `is_until`
                // flag, so both the class and the undecorated condition are
                // recoverable.
                let cond = cond
                    .as_ref()
                    .ok_or_else(|| unsupported("repeat loop without condition"))?;
                let mut fields = label_fields(label);
                fields.push(node_field(Some("body"), block_node(body)?));
                fields.push(node_field(
                    Some("condition"),
                    convert_expr(if *is_until {
                        strip_negation(cond)?
                    } else {
                        cond
                    })?,
                ));
                return Ok(Some(RakuAstNode {
                    class: if *is_until {
                        RakuAstClass::StatementLoopRepeatUntil
                    } else {
                        RakuAstClass::StatementLoopRepeatWhile
                    },
                    fields,
                }));
            }
            if init.is_none() && cond.is_none() && step.is_none() {
                // Bare `loop { }`.
                let mut fields = label_fields(label);
                fields.push(node_field(Some("body"), block_node(body)?));
                return Ok(Some(RakuAstNode {
                    class: RakuAstClass::StatementLoop,
                    fields,
                }));
            }
            // C-style `loop (init; cond; step) { }`. Each clause is optional and
            // present only when written.
            let mut fields = label_fields(label);
            if let Some(init) = init.as_deref() {
                fields.push(node_field(Some("setup"), loop_setup_node(init)?));
            }
            if let Some(cond) = cond {
                fields.push(node_field(Some("condition"), convert_expr(cond)?));
            }
            if let Some(step) = step {
                fields.push(node_field(Some("increment"), convert_expr(step)?));
            }
            fields.push(node_field(Some("body"), block_node(body)?));
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementLoop,
                fields,
            }))
        }
        Stmt::For {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body,
            label,
            mode,
            rw_block,
            explicit_zero_params,
            is_statement_modifier: _,
            uses_block_magic: _,
        } => {
            // Implicit-topic (`for SRC { ... $_ }`, slice 6) and explicit-signature
            // (`for @a -> $x`, slice 12) forms. Hyper/race/lazy modes, `<->` rw
            // blocks, and labels carry extra RakuAST shape, deferred.
            // The explicit param names live in `param_def` / `params_def`; the
            // sigil-stripped `param` / `params` string lists are unused here.
            let _ = (param, params);
            if *rw_block || *explicit_zero_params || !matches!(mode, ForMode::Normal) {
                return Err(unsupported("for loop with mode / rw"));
            }
            // A single explicit param lives in `param_def`, multiple in
            // `params_def`. With none, the body is an implicit-topic Block; with
            // an explicit signature, it is a PointyBlock (matching raku).
            let single = (**param_def).as_ref();
            let explicit_defs: &[ParamDef] = match single {
                Some(pd) => std::slice::from_ref(pd),
                None => params_def,
            };
            let body_node = if explicit_defs.is_empty() {
                topic_block_node(body)?
            } else {
                pointy_block(explicit_defs, body, None)?
            };
            // Field order matches raku: labels, mode, source, body.
            let mut fields = label_fields(label);
            fields.push(leaf_field(Some("mode"), Value::str("serial".to_string())));
            fields.push(node_field(Some("source"), convert_expr(iterable)?));
            fields.push(node_field(Some("body"), body_node));
            Ok(Some(RakuAstNode {
                class: RakuAstClass::StatementFor,
                fields,
            }))
        }
        // `given X { ... }` -> Statement::Given(source, body => topic Block).
        Stmt::Given {
            topic,
            body,
            is_statement_modifier,
            with_kind,
        } => {
            // `STMT with X` / `STMT without X` reach here as the `given` the
            // parser desugared them into; `with_kind` is what says which
            // keyword the source actually used (the desugared shape alone is
            // ambiguous with a hand-written `(STMT if $_.defined) given X`).
            // raku keeps them as a `condition-modifier`, like `if`/`unless`,
            // not as the `loop-modifier` a real `given` gets.
            match with_kind {
                Some(kind @ (GivenWithKind::With | GivenWithKind::Without)) => {
                    return with_modifier_node(*kind, topic, body).map(Some);
                }
                // The scaffold of a block form, reached on its own: the
                // conditional that owns it renders the parameterless spelling
                // itself, so arriving here means the block had a signature raku
                // would spell as a `PointyBlock`.
                Some(GivenWithKind::BlockTopic | GivenWithKind::BlockTopicPointy) => {
                    return Err(unsupported("with/without block with an explicit signature"));
                }
                None => {}
            }
            if *is_statement_modifier {
                let [modified] = body.as_slice() else {
                    return Err(unsupported("multi-statement given modifier body"));
                };
                let mut statement = convert_stmt(modified)?
                    .ok_or_else(|| unsupported("empty given modifier body"))?;
                statement.fields.push(node_field(
                    Some("loop-modifier"),
                    RakuAstNode {
                        class: RakuAstClass::StatementModifierGiven,
                        fields: vec![node_field(None, convert_expr(topic)?)],
                    },
                ));
                Ok(Some(statement))
            } else {
                Ok(Some(RakuAstNode {
                    class: RakuAstClass::StatementGiven,
                    fields: vec![
                        node_field(Some("source"), convert_expr(topic)?),
                        node_field(Some("body"), topic_block_node(body)?),
                    ],
                }))
            }
        }
        // `when Y { ... }` -> Statement::When(condition, body => plain Block).
        Stmt::When { cond, body, .. } => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementWhen,
            fields: vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("body"), block_node(body)?),
            ],
        })),
        // `default { ... }` -> Statement::Default(body => plain Block).
        Stmt::Default(body) => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementDefault,
            fields: vec![node_field(Some("body"), block_node(body)?)],
        })),
        // `CATCH { ... }` -> Statement::Catch(body => topic Block + exception).
        // The body topicalizes the exception, so raku marks the block both
        // `implicit-topic`/`required-topic` (like a `given` body) and
        // `exception => 1`, which is what distinguishes it from one.
        Stmt::Catch(body) => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementCatch,
            fields: vec![node_field(Some("body"), exception_block_node(body)?)],
        })),
        Stmt::SubDecl {
            name,
            name_expr,
            param_defs,
            return_type,
            associativity,
            precedence_trait,
            signature_alternates,
            body,
            multi,
            is_rw,
            is_raw,
            is_export,
            export_tags,
            is_test_assertion,
            supersede,
            custom_traits,
            ..
        } => {
            // Phase 2 slice 7 covers the plain named `sub NAME (params) { body }`
            // form; return types are covered too (`-->` via `Signature.returns`,
            // `returns`/`of` via `Trait::Returns`/`Trait::Of`). Other traits,
            // multi, export, operator subs, and alternate signatures carry extra
            // RakuAST shape, deferred.
            let spelling = return_type_spelling(custom_traits)?;
            if name_expr.is_some()
                || associativity.is_some()
                || precedence_trait.is_some()
                || !signature_alternates.is_empty()
                || *is_rw
                || *is_raw
                || *is_export
                || !export_tags.is_empty()
                || *is_test_assertion
                || *supersede
                || custom_traits
                    .iter()
                    .any(|(t, _)| !is_return_spelling_marker(t))
            {
                return Err(unsupported("sub with traits / multi / export"));
            }
            if return_type.is_none() && spelling != ReturnSpelling::Arrow {
                // A `__return_via_*` marker without a return type would be a
                // parser inconsistency; refuse rather than render a wrong node.
                return Err(unsupported("sub with a return trait but no return type"));
            }
            let mut node = routine_node(
                RakuAstClass::Sub,
                &name.resolve(),
                param_defs,
                body,
                return_type.as_deref().map(|t| (t, spelling)),
            )?;
            if *multi {
                // `multiness` precedes `name` in raku's field order.
                node.fields
                    .insert(0, leaf_field(Some("multiness"), Value::str_from("multi")));
            }
            Ok(Some(statement_expression(node)))
        }
        Stmt::MethodDecl {
            name,
            name_expr,
            param_defs,
            body,
            multi,
            is_rw,
            is_raw,
            is_private,
            is_our,
            is_my,
            is_submethod,
            our_variable_form,
            return_type,
            is_default_candidate,
            deprecated_message,
            handles,
            custom_traits,
            ..
        } => {
            // Plain `method NAME (params) { body }` and `submethod NAME (…) { … }`,
            // with return types in all three spellings (`-->` via
            // `Signature.returns`, `returns`/`of` via `Trait::Returns`/`Trait::Of`)
            // — a `RakuAST::Method` is a `RakuAST::Routine` just like
            // `RakuAST::Sub`, so it carries the same `signature` / `traits`
            // shape. raku spells `submethod` as its own class carrying exactly
            // that shape (measured: a `Submethod` and a `Method` of the same
            // signature differ only in the class name). Private/multi/our/my
            // forms, user traits, and delegation carry extra shape.
            let spelling = return_type_spelling(custom_traits)?;
            // `submethod_decl` marks every submethod `is_my` as its internal
            // "not inherited" flag, not because the source said `my` — so for a
            // submethod that flag carries no RakuAST shape of its own.
            let declared_my = *is_my && !*is_submethod;
            if name_expr.is_some()
                || *multi
                || *is_rw
                || *is_raw
                || *is_private
                || *is_our
                || declared_my
                || *our_variable_form
                || *is_default_candidate
                || deprecated_message.is_some()
                || !handles.is_empty()
                || custom_traits
                    .iter()
                    .any(|(t, _)| !is_return_spelling_marker(t))
            {
                return Err(unsupported(
                    "method with traits / private / multi / delegation",
                ));
            }
            if return_type.is_none() && spelling != ReturnSpelling::Arrow {
                // A `__return_via_*` marker without a return type would be a
                // parser inconsistency; refuse rather than render a wrong node.
                return Err(unsupported("method with a return trait but no return type"));
            }
            Ok(Some(statement_expression(routine_node(
                if *is_submethod {
                    RakuAstClass::Submethod
                } else {
                    RakuAstClass::Method
                },
                &name.resolve(),
                param_defs,
                body,
                return_type.as_deref().map(|t| (t, spelling)),
            )?)))
        }
        Stmt::ClassDecl {
            name,
            name_expr,
            parents,
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            repr,
            body,
            custom_traits,
            is_unit,
            implicit_grammar_parent,
            is_grammar,
            ..
        } => {
            if *is_grammar {
                if name_expr.is_some()
                    || *class_is_rw
                    || *is_hidden
                    || *is_lexical
                    || !hidden_parents.is_empty()
                    || !does_parents.is_empty()
                    || repr.is_some()
                    || !custom_traits.is_empty()
                    || *is_unit
                    || !*implicit_grammar_parent
                    || parents != &["Grammar".to_string()]
                {
                    return Err(unsupported("grammar with inheritance / scope / traits"));
                }
                return Ok(Some(statement_expression(RakuAstNode {
                    class: RakuAstClass::Grammar,
                    fields: vec![
                        node_field(Some("name"), name_from_identifier(&name.resolve())),
                        node_field(Some("body"), block_node(body)?),
                    ],
                })));
            }
            // `class NAME [is P] [does R] [is rw] [is repr(R)] { body }`.
            // Inheritance and `rw` are `traits`, the repr is its own leaf field.
            // `my`/unit scope, `hides`, computed names and user traits carry
            // extra RakuAST shape, deferred.
            if name_expr.is_some()
                || *is_hidden
                || *is_lexical
                || !hidden_parents.is_empty()
                || !custom_traits.is_empty()
                || *is_unit
            {
                return Err(unsupported(
                    "class with inheritance / scope / repr / traits",
                ));
            }
            let mut fields = vec![node_field(
                Some("name"),
                name_from_identifier(&name.resolve()),
            )];
            // Field order matches raku: scope, name, repr, traits, body.
            if let Some(r) = repr {
                fields.push(leaf_field(Some("repr"), Value::str(r.clone())));
            }
            let traits = class_traits(parents, does_parents, *class_is_rw)?;
            if !traits.is_empty() {
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(traits),
                });
            }
            fields.push(node_field(Some("body"), block_node(body)?));
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::Class,
                fields,
            })))
        }
        // `enum NAME <A B>` / `enum NAME <<A B>>` / `enum NAME (A => 1)`
        // -> `Type::Enum(name => Name, term => ...)`. The runtime only needs
        // normalized variants, but Rakudo preserves the source-level quoting
        // form in `term`, so the parser records it on `Stmt::EnumDecl`.
        Stmt::EnumDecl {
            name,
            variants,
            variant_form,
            is_export,
            export_tags,
            is_my,
            base_type,
            roles,
            ..
        } => {
            if *is_export
                || !export_tags.is_empty()
                || *is_my
                || base_type.is_some()
                || !roles.is_empty()
                || matches!(variant_form, EnumVariantForm::Computed)
                || variants.is_empty()
            {
                return Err(unsupported("enum with scope / traits / computed body"));
            }
            let term = match variant_form {
                EnumVariantForm::Words => enum_quoted_string(variants, "words")?,
                EnumVariantForm::QuoteWords => enum_quoted_string(variants, "quotewords")?,
                EnumVariantForm::PairList => enum_pair_list(variants)?,
                EnumVariantForm::Computed => unreachable!("checked above"),
            };
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::TypeEnum,
                fields: vec![
                    node_field(Some("name"), name_from_identifier(&name.resolve())),
                    node_field(Some("term"), term),
                ],
            })))
        }
        // `module M { }` / `package P { }` -> `RakuAST::Module` / `RakuAST::Package`.
        // raku gives each declarator keyword its own class (there is no shared
        // node with a `kind` field), and the body is a plain `Block` exactly as
        // for a class. `grammar` also parses to `Stmt::Package` here, but its
        // `RakuAST::Grammar` body holds regex declarations this layer does not
        // model yet, so it stays the boundary. `unit` and `my` scopes carry
        // extra RakuAST shape, deferred.
        Stmt::Package {
            name,
            body,
            kind,
            is_unit,
            is_my,
        } => {
            if *is_unit || *is_my {
                return Err(unsupported("unit / my package declaration"));
            }
            let class = match kind {
                crate::ast::PackageKind::Module => RakuAstClass::Module,
                crate::ast::PackageKind::Package => RakuAstClass::Package,
                crate::ast::PackageKind::Grammar => {
                    return Err(unsupported("grammar declaration"));
                }
            };
            Ok(Some(statement_expression(RakuAstNode {
                class,
                fields: vec![
                    node_field(Some("name"), name_from_identifier(&name.resolve())),
                    node_field(Some("body"), block_node(body)?),
                ],
            })))
        }
        // `subset S of T where P` -> `RakuAST::Type::Subset`. The `of T` base
        // type is a `Trait::Of` in the `traits` list (raku models it exactly as
        // a routine's `of` return type), and the `where` predicate is its own
        // named field. A subset that does not write a base type gets NO
        // `traits` field at all — measured: `subset S where *> 0` and
        // `subset S of Any where * > 0` are different nodes even though the
        // implied base *is* `Any`, which is what `base_is_explicit` preserves.
        // Export and `my` scope carry extra shape, deferred.
        Stmt::SubsetDecl {
            name,
            base,
            base_is_explicit,
            predicate,
            is_export,
            export_tags,
            is_my,
            ..
        } => {
            if *is_export || !export_tags.is_empty() || *is_my {
                return Err(unsupported("subset with export / my scope"));
            }
            let mut fields = vec![node_field(
                Some("name"),
                name_from_identifier(&name.resolve()),
            )];
            // Field order matches raku: name, where, traits.
            if let Some(pred) = predicate {
                fields.push(node_field(Some("where"), convert_expr(pred)?));
            }
            if *base_is_explicit {
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(RakuAstNode {
                        class: RakuAstClass::TraitOf,
                        fields: vec![node_field(None, build_type_node(base)?)],
                    }))]),
                });
            }
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::TypeSubset,
                fields,
            })))
        }
        Stmt::RoleDecl {
            name,
            type_params,
            is_export,
            export_tags,
            body,
            is_rw,
            custom_traits,
            ..
        } => {
            // Plain `role NAME { body }`. Parameterised roles (`role R[::T]`),
            // export, `rw`, and traits carry extra RakuAST shape, deferred. The
            // role body is a `RoleBody` (not a plain `Block`).
            if !type_params.is_empty()
                || *is_export
                || !export_tags.is_empty()
                || *is_rw
                || !custom_traits.is_empty()
            {
                return Err(unsupported("role with type params / export / traits"));
            }
            let role_body = RakuAstNode {
                class: RakuAstClass::RoleBody,
                fields: vec![node_field(Some("body"), blockoid(body)?)],
            };
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::Role,
                fields: vec![
                    node_field(Some("name"), name_from_identifier(&name.resolve())),
                    node_field(Some("body"), role_body),
                ],
            })))
        }
        Stmt::HasDecl {
            name,
            is_public,
            default,
            handles,
            is_rw,
            type_constraint,
            type_smiley,
            is_required,
            sigil,
            where_constraint,
            is_alias,
            is_our,
            is_my,
            is_default,
            is_type,
            deprecated_message,
            unknown_traits,
            ..
        } => {
            // A plain `has [Type] $.x` attribute -> a `VarDeclaration::Simple`
            // with `scope => "has"` and a `twigil` (`.` public accessor / `!`
            // private). An *explicit* attribute default (`has $.x = 5`) becomes a
            // `Trait::WillBuild` in raku (not an `initializer`), so it is
            // deferred; but a typed attribute (`has Int $.z`) carries an
            // *implicit* `BareWord(<TypeName>)` default that is NOT a will-build
            // and must be ignored. Traits, type smileys, `required`, `where`,
            // aliases, and `my`/`our` attributes are also deferred.
            // A typed attribute (`has Int $.z`) carries an *implicit*
            // `BareWord(<TypeName>)` default that is NOT a real default; an
            // *explicit* `= EXPR` default (slice 27) is a `Trait::WillBuild` and
            // an `initializer`.
            let explicit_default = match default {
                None => None,
                Some(Expr::BareWord(w)) if type_constraint.as_deref() == Some(w.as_str()) => None,
                Some(e) => Some(e),
            };
            if !handles.is_empty()
                || *is_rw
                || type_smiley.is_some()
                || is_required.is_some()
                || where_constraint.is_some()
                || *is_alias
                || *is_our
                || *is_my
                || is_default.is_some()
                || is_type.is_some()
                || deprecated_message.is_some()
                || !unknown_traits.is_empty()
            {
                return Err(unsupported("attribute with traits / smiley / scope"));
            }
            // Definite attributes carry `type_smiley` (guarded above), so the
            // type_constraint here is a bare type; build_type_node handles it.
            let type_name = type_constraint.as_deref();
            let twigil = if *is_public { "." } else { "!" };
            let full_name = if *sigil == '$' {
                name.resolve()
            } else {
                format!("{}{}", sigil, name.resolve())
            };
            Ok(Some(statement_expression(var_declaration(
                &full_name,
                explicit_default,
                Some("has"),
                type_name,
                Some(twigil),
                explicit_default,
            )?)))
        }
        Stmt::Assign { name, expr, op } => match op {
            // `$x = EXPR` — the special `Assignment` infix (slice 2). A compound
            // assignment keeps its source-level metaop marker inside the ordinary
            // assignment expansion used by the compiler.
            AssignOp::Assign => match expr {
                Expr::CompoundAssign {
                    target, op, rhs, ..
                } if !is_dotty_assign_op(op) && compound_target_matches_name(target, name) => {
                    Ok(Some(statement_expression(compound_assignment_infix(
                        target, op, rhs,
                    )?)))
                }
                _ => Ok(Some(statement_expression(assignment_infix(name, expr)?))),
            },
            // `$x := EXPR` — a plain `:=` infix (slice 9).
            AssignOp::Bind => Ok(Some(statement_expression(bind_infix(name, expr)?))),
            AssignOp::MatchAssign => Err(unsupported("`~~` match-assignment")),
        },
        // A `LABEL: STMT` wrapper (mutsu uses this for labelled `repeat`/C-style
        // loops, where the inner loop's own `label` field is None). Convert the
        // inner statement and prepend its `labels` field — raku renders labels
        // first, matching the inline-label loops of slice 17.
        Stmt::Label { name, stmt } => {
            let mut node =
                convert_stmt(stmt)?.ok_or_else(|| unsupported("labelled empty statement"))?;
            let mut fields = label_fields(&Some(name.clone()));
            fields.append(&mut node.fields);
            node.fields = fields;
            Ok(Some(node))
        }
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// The argument-less pragmas that Rakudo represents as `RakuAST::Pragma`.
/// Language-version pragmas and `use lib` have distinct parser contracts, and
/// ordinary modules remain `RakuAST::Statement::Use`, so neither belongs here.
fn is_pragma_name(name: &str) -> bool {
    matches!(
        name,
        "strict"
            | "fatal"
            | "nqp"
            | "soft"
            | "MONKEY"
            | "MONKEY-GUTS"
            | "MONKEY-TYPING"
            | "MONKEY-SEE-NO-EVAL"
            | "dynamic-scope"
            | "isms"
            | "precompilation"
            | "worries"
            | "trace"
            | "internals"
    )
}

fn pragma_node(name: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Pragma,
        fields: vec![leaf_field(Some("name"), Value::str(name.to_string()))],
    }
}

/// `$x = EXPR` -> `ApplyInfix(left => Var::Lexical, infix => Assignment, right)`.
/// The `Assignment` node carries `:item` for scalar (`$`) targets; the list form
/// (`@`/`%`) has no adverb.
fn assignment_infix(name: &str, rhs: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    let assignment = RakuAstNode {
        class: RakuAstClass::Assignment,
        fields: if sigil == "$" {
            vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Adverb("item"),
            }]
        } else {
            vec![]
        },
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), var_lexical(sigil, desigil)),
            node_field(Some("infix"), assignment),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

/// `$x := EXPR` -> `ApplyInfix(left => Var::Lexical, infix => Infix(":="), right)`.
/// Unlike `=`, binding uses a plain `Infix`, not the special `Assignment` node.
fn bind_infix(name: &str, rhs: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), var_lexical(sigil, desigil)),
            node_field(Some("infix"), plain_infix(":=")),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

/// A plain `Infix.new("<op>")` node from a literal operator string.
fn plain_infix(op: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Infix,
        fields: vec![leaf_field(None, Value::str(op.to_string()))],
    }
}

/// Whether a compound-assignment marker's `op` is the `.=` metaop, which rakudo
/// models as `ApplyDottyInfix`, not as `MetaInfix::Assign` over an infix.
fn is_dotty_assign_op(op: &str) -> bool {
    op == crate::parser::DOTTY_ASSIGN_OP
}

/// `$x OP= EXPR` -> `ApplyInfix(left, MetaInfix::Assign(Infix(OP)), right)`.
fn compound_assignment_infix(
    target: &Expr,
    op: &str,
    rhs: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let base_op = op.strip_suffix('=').unwrap_or(op);
    let meta_assign = RakuAstNode {
        class: RakuAstClass::MetaInfixAssign,
        fields: vec![node_field(None, plain_infix(base_op))],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), convert_expr(target)?),
            node_field(Some("infix"), meta_assign),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

/// A compound marker may be nested in the RHS of a separate assignment. Only
/// the marker that names the statement's own target replaces `Assignment` with
/// `MetaInfix::Assign`; nested markers stay under the outer assignment node.
fn compound_target_matches_name(target: &Expr, name: &str) -> bool {
    match target {
        Expr::Var(target_name) => target_name == name,
        Expr::ArrayVar(target_name) => name == format!("@{target_name}"),
        Expr::HashVar(target_name) => name == format!("%{target_name}"),
        Expr::CodeVar(target_name) => name == format!("&{target_name}"),
        _ => false,
    }
}

/// `my $x` / `my @a` / `my $x = EXPR` -> `VarDeclaration::Simple`. The sigil is
/// implicit (`$`) when mutsu already stripped it from the name; otherwise the
/// name carries its `@`/`%`/`&` sigil.
fn var_declaration(
    name: &str,
    init: Option<&Expr>,
    scope: Option<&'static str>,
    type_name: Option<&str>,
    twigil: Option<&str>,
    will_build: Option<&Expr>,
) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    // Field order matches raku: scope, type, sigil, twigil, desigilname, traits,
    // initializer — each omitted when absent (scope defaults to `my`; twigil and
    // traits appear only on attributes).
    let mut fields = Vec::new();
    if let Some(s) = scope {
        fields.push(leaf_field(Some("scope"), Value::str(s.to_string())));
    }
    if let Some(t) = type_name {
        fields.push(node_field(Some("type"), build_type_node(t)?));
    }
    fields.push(leaf_field(Some("sigil"), Value::str(sigil.to_string())));
    if let Some(tw) = twigil {
        fields.push(leaf_field(Some("twigil"), Value::str(tw.to_string())));
    }
    fields.push(node_field(
        Some("desigilname"),
        name_from_identifier(desigil),
    ));
    if let Some(wb) = will_build {
        // An attribute default (`has $.x = 5`) is a `Trait::WillBuild` (and also
        // an `initializer`, emitted below).
        let trait_node = RakuAstNode {
            class: RakuAstClass::TraitWillBuild,
            fields: vec![node_field(None, convert_expr(wb)?)],
        };
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(trait_node))]),
        });
    }
    if let Some(init_expr) = init {
        let assign = RakuAstNode {
            class: RakuAstClass::InitializerAssign,
            fields: vec![node_field(None, convert_expr(init_expr)?)],
        };
        fields.push(node_field(Some("initializer"), assign));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::VarDeclarationSimple,
        fields,
    })
}

/// A `Name.from-identifier("<s>")` node.
fn name_from_identifier(s: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(s.to_string()))],
    }
}

/// True when a type constraint is a plain (possibly `::`-qualified) identifier
/// (`Int`, `My::Type`) that maps to `Type::Simple`. Parameterised (`Array[Int]`)
/// and coercion (`Str()`) types carry richer RakuAST shape, deferred — so each
/// `::`-separated segment must be a bare identifier.
fn is_simple_type(t: &str) -> bool {
    !t.is_empty()
        && t.split("::").all(|seg| {
            !seg.is_empty() && seg.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        })
}

/// A bare simple type `Int` -> `Type::Simple(Name.from-identifier("Int"))`.
fn simple_type_node(t: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::TypeSimple,
        fields: vec![node_field(None, name_from_identifier(t))],
    }
}

/// Build the `type => ...` RakuAST node for a mutsu type-constraint string.
/// A plain identifier -> `Type::Simple`; a `:D`/`:U` definiteness smiley ->
/// `Type::Definedness`; a `Base[Arg, ...]` -> `Type::Parameterized`. Coercion
/// (`Str()`) and `:_` types defer.
fn build_type_node(t: &str) -> Result<RakuAstNode, RuntimeError> {
    if let Some(base) = t.strip_suffix(":D").or_else(|| t.strip_suffix(":U")) {
        if !is_simple_type(base) {
            return Err(unsupported("definite type over a non-simple base"));
        }
        let definite = t.ends_with(":D");
        return Ok(RakuAstNode {
            class: RakuAstClass::TypeDefinedness,
            fields: vec![
                node_field(Some("base-type"), simple_type_node(base)),
                RakuAstField {
                    name: Some("definite"),
                    value: RakuAstFieldValue::Node(Value::truth(definite)),
                },
            ],
        });
    }
    // `Int()` coercion -> Type::Coercion(base-type). A coercion with an explicit
    // target (`Str(Int)`) is deferred.
    if let Some(base) = t.strip_suffix("()") {
        if !is_simple_type(base) {
            return Err(unsupported("coercion type over a non-simple base"));
        }
        return Ok(RakuAstNode {
            class: RakuAstClass::TypeCoercion,
            fields: vec![node_field(Some("base-type"), simple_type_node(base))],
        });
    }
    // `Array[Int]` / `Hash[Str, Int]` -> Type::Parameterized(base-type, args).
    if let Some(open) = t.find('[') {
        let inner = t
            .strip_suffix(']')
            .ok_or_else(|| unsupported("malformed parameterised type"))?;
        let base = &t[..open];
        let args_str = &inner[open + 1..];
        if !is_simple_type(base) {
            return Err(unsupported("parameterised type over a non-simple base"));
        }
        let mut args = Vec::new();
        for a in args_str.split(',') {
            args.push(node_field(None, build_type_node(a.trim())?));
        }
        let arglist = RakuAstNode {
            class: RakuAstClass::ArgList,
            fields: args,
        };
        return Ok(RakuAstNode {
            class: RakuAstClass::TypeParameterized,
            fields: vec![
                node_field(Some("base-type"), simple_type_node(base)),
                node_field(Some("args"), arglist),
            ],
        });
    }
    if is_simple_type(t) {
        return Ok(simple_type_node(t));
    }
    Err(unsupported("coercion type"))
}

/// Split a declaration name into `(sigil, desigilname)`. mutsu keeps the sigil
/// on `@`/`%`/`&` declarations but strips it from `$` ones.
fn split_sigil(name: &str) -> (&str, &str) {
    match name.as_bytes().first() {
        Some(b'@') => ("@", &name[1..]),
        Some(b'%') => ("%", &name[1..]),
        Some(b'&') => ("&", &name[1..]),
        _ => ("$", name),
    }
}

fn statement_expression(expr: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::StatementExpression,
        fields: vec![node_field(Some("expression"), expr)],
    }
}

fn convert_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => convert_literal(v),
        Expr::RegexLiteral { tree, .. } | Expr::MatchRegexTree { tree, .. } => {
            quoted_regex_node(tree)
        }
        Expr::Call { name, args } | Expr::UserRoutineCall { name, args } => {
            if is_desugar_marker(name.as_str()) {
                return Err(desugared(name.as_str()));
            }
            Ok(call_name(name.as_str(), args, false)?)
        }
        Expr::Var(name) => {
            if is_desugar_marker(name) {
                return Err(desugared(name));
            }
            Ok(var_lexical("$", name))
        }
        // Calling a term `$f(1, 2)` -> ApplyPostfix(operand, Call::Term(args)).
        Expr::CallOn { target, args } => {
            let call_term = RakuAstNode {
                class: RakuAstClass::CallTerm,
                fields: vec![node_field(Some("args"), arg_list(args)?)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), call_term),
                ],
            })
        }
        // The `*` whatever term (a *value*, not a priming argument).
        Expr::Whatever => Ok(RakuAstNode {
            class: RakuAstClass::TermWhatever,
            fields: Vec::new(),
        }),
        // A `*` that participates in Whatever-priming (ADR-0033 Phase 2): the
        // left operand of `* + 1` is `WhateverCode::Argument`, not `Term::Whatever`.
        Expr::WhateverArg => Ok(RakuAstNode {
            class: RakuAstClass::WhateverCodeArgument,
            fields: Vec::new(),
        }),
        // `**` — read direction only; priming for `**` is out of scope (ADR-0033 §1).
        Expr::HyperWhatever => Ok(RakuAstNode {
            class: RakuAstClass::TermHyperWhatever,
            fields: Vec::new(),
        }),
        // A `WhateverCurry` marker carries no RakuAST node of its own — Rakudo's
        // tree has no priming-scope wrapper (ADR-0033 §5); the scope is derived
        // structurally at lowering. Convert straight through to the body.
        Expr::WhateverCurry(body) => convert_expr(body),
        // `do { … }` -> `StatementPrefix::Do(Block)`. A labelled do stays the boundary.
        Expr::DoBlock {
            body,
            label,
            origin,
        } => {
            if label.is_some() {
                return Err(unsupported("labelled do block"));
            }
            // `StatementPrefixDo` round-trips back through `lower.rs` as a
            // genuine source block. A desugar's node is not one, so converting
            // it would hand back a node with block semantics (`let`/`temp`
            // resolution) the original never had -- GH-7635.
            if origin != &crate::ast::DoBlockOrigin::SourceBlock {
                return Err(unsupported("desugared do-block"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::StatementPrefixDo,
                fields: vec![node_field(None, block_node(body)?)],
            })
        }
        // `try { … }` -> `StatementPrefix::Try(Block)`. A `CATCH` block stays the
        // boundary.
        Expr::Try { body, catch } => {
            if catch.is_some() {
                return Err(unsupported("try with a CATCH block"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::StatementPrefixTry,
                fields: vec![node_field(None, block_node(body)?)],
            })
        }
        // `gather { … }` -> `StatementPrefix::Gather(Block)`.
        Expr::Gather(body) => Ok(RakuAstNode {
            class: RakuAstClass::StatementPrefixGather,
            fields: vec![node_field(None, block_node(body)?)],
        }),
        // A pair the parser marked POSITIONAL: a non-bareword key (`"a" => 1`,
        // `$k => 1`), or a parenthesized one, which carries an inner `Grouped`.
        // The marker itself says nothing about the rendering — raku renders a
        // quoted/computed key as a plain `ApplyInfix` and parentheses as a
        // `Circumfix::Parentheses` — so unwrap and let those arms decide.
        //
        // Rendering keyed off this variant is what made mutsu emit the two
        // spellings the wrong way round: `PositionalPair` does not mean "quoted
        // key", it means "not a named argument", which a parenthesized BAREWORD
        // pair also is.
        Expr::PositionalPair(inner) => match &**inner {
            // A non-bareword key. Render the infix DIRECTLY rather than
            // recursing: the `FatArrow` arm below keys on the bare
            // `Binary{FatArrow}` shape, which is what this pair is once the
            // marker is peeled off, and it would claim a bareword key.
            Expr::Binary {
                left,
                op: op @ crate::token_kind::TokenKind::FatArrow,
                right,
            } => Ok(RakuAstNode {
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(Some("left"), convert_expr(left)?),
                    node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                    node_field(Some("right"), convert_expr(right)?),
                ],
            }),
            // Parenthesized (the paren parser's inner `Grouped` marker), or any
            // other shape: the parenthesization and the pair itself are what
            // raku renders, so let those arms decide.
            other => convert_expr(other),
        },
        // `self` -> `Term::Self`, a node with no fields. mutsu's parser leaves
        // it as a bareword, so it has to be picked off before the type-name and
        // declared-name arms below.
        Expr::BareWord(name) if name == "self" => Ok(RakuAstNode {
            class: RakuAstClass::TermSelf,
            fields: Vec::new(),
        }),
        // A bare type name used as a term (`Int`, `Str`) -> `Type::Simple`.
        Expr::BareWord(name) if is_known_type_constraint(name) => Ok(simple_type_node(name)),
        // A name the same compilation unit declared. raku resolves it at parse
        // time: a type name renders exactly like a builtin one, a constant
        // renders as a `Term::Name`. Any other bareword stays the boundary.
        Expr::BareWord(name) if declared_kind(name).is_some() => {
            match declared_kind(name).expect("just checked") {
                DeclaredKind::Type => Ok(simple_type_node(name)),
                DeclaredKind::Constant => Ok(RakuAstNode {
                    class: RakuAstClass::TermName,
                    fields: vec![node_field(None, name_from_identifier(name))],
                }),
            }
        }
        // `(EXPR)` -> `Circumfix::Parentheses(SemiList(Statement::Expression(...)))`.
        Expr::Grouped(inner) => {
            // The contents of `(...)` are a semilist of *statements*, so a
            // declaration written inside them (`(my $x = 9) given 2`) renders
            // as the statement it is rather than as an expression.
            let content = match inner.as_ref() {
                Expr::DoStmt(stmt) => convert_stmt(stmt)?.ok_or_else(|| {
                    RuntimeError::new(format!(
                        "RakuAST: `.AST` does not yet support this construct: {stmt:?}"
                    ))
                })?,
                other => statement_expression(convert_expr(other)?),
            };
            let semilist = RakuAstNode {
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, content)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::CircumfixParentheses,
                fields: vec![node_field(None, semilist)],
            })
        }
        // `($x = EXPR)` as an expression -> the same `ApplyInfix(Assignment)` as a
        // statement assignment. `:=` binding stays the boundary.
        Expr::AssignExpr {
            name,
            expr,
            is_bind,
        } => {
            if *is_bind {
                return Err(unsupported("`:=` binding expression"));
            }
            assignment_infix(name, expr)
        }
        Expr::CompoundAssign {
            target,
            op,
            rhs,
            expanded,
        } => {
            if is_dotty_assign_op(op) {
                // TODO: rakudo renders `$x .= meth` as
                // `ApplyDottyInfix(left, DottyInfix::CallAssign, Call::Method)`,
                // node classes this converter does not model yet. Until it does,
                // render the expansion -- exactly what the bare `AssignExpr`
                // produced before `.=` carried a marker.
                return convert_expr(expanded);
            }
            compound_assignment_infix(target, op, rhs)
        }
        Expr::ArrayVar(name) => {
            if is_desugar_marker(name) {
                return Err(desugared(name));
            }
            Ok(var_lexical("@", name))
        }
        Expr::HashVar(name) => {
            if is_desugar_marker(name) {
                return Err(desugared(name));
            }
            Ok(var_lexical("%", name))
        }
        Expr::CodeVar(name) => Ok(var_lexical("&", name)),
        // `todo/tickets/chained-compare-ast-node.md`: rakudo has no AST-level
        // `&&` for a chained comparison — `Q[1 < 2 < 3].AST` is a left-nested
        // `ApplyInfix(ApplyInfix(1, "<", 2), "<", 3)` with no wrapper, the
        // chaining semantics coming from the operator's chaining precedence at
        // rakudo's own codegen (measured). Render the same shape here instead
        // of the compiler's `&&`/`DoBlock` expansion, which is a compile-time
        // implementation detail (`crate::chain_compare::expand`) this
        // converter never sees.
        Expr::ChainedCompare { operands, ops } => convert_chained_compare(operands, ops),
        // A fat-arrow pair with a BAREWORD key -> `FatArrow(key => "a", value)`.
        // raku models the two key spellings as different nodes: a bareword key
        // is a `FatArrow` carrying the key as a plain string, while a quoted or
        // computed one is an ordinary `ApplyInfix` over `=>` (the arm below).
        // The parser draws exactly that line already -- a bareword key yields a
        // bare `Binary{FatArrow}` (it is a NAMED argument), and every other
        // spelling is wrapped in `PositionalPair` -- so the shape here is the
        // bareword one.
        //
        // Colonpairs (`:foo`, `:foo(1)`) share this shape and so render as a
        // `FatArrow` too, where raku has a distinct `ColonPair::*` family. That
        // is a separate divergence, unchanged in kind by this arm: before it,
        // they rendered as an `ApplyInfix` claiming a quoted-string key, which
        // they never had.
        Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
        } if matches!(&**left,
            Expr::Literal(v) | Expr::LiteralSrc(v, _) if matches!(v.view(), ValueView::Str(_))) =>
        {
            let (Expr::Literal(v) | Expr::LiteralSrc(v, _)) = &**left else {
                unreachable!("guarded above")
            };
            let ValueView::Str(key) = v.view() else {
                unreachable!("guarded above")
            };
            Ok(RakuAstNode {
                class: RakuAstClass::FatArrow,
                fields: vec![
                    leaf_field(Some("key"), Value::str(key.to_string())),
                    node_field(Some("value"), convert_expr(right)?),
                ],
            })
        }
        // List-associative infixes (`andthen`/`orelse`/`notandthen`) render as a
        // single flat `ApplyListInfix` in raku; mutsu nests them left-associatively,
        // so flatten a same-operator left chain into one operand list.
        Expr::Binary { left, op, right } if is_list_infix(op) => {
            let mut operands = Vec::new();
            flatten_list_infix(op, left, right, &mut operands);
            let mut nodes = Vec::with_capacity(operands.len());
            for e in operands {
                nodes.push(Value::rakuast(Box::new(convert_expr(e)?)));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyListInfix,
                fields: vec![
                    node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                    RakuAstField {
                        name: Some("operands"),
                        value: RakuAstFieldValue::List(nodes),
                    },
                ],
            })
        }
        Expr::Binary { left, op, right } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), convert_expr(left)?),
                node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                node_field(Some("right"), convert_expr(right)?),
            ],
        }),
        // A hyper infix `@a >>+<< @b` -> ApplyInfix(left, MetaInfix::Hyper(
        // [dwim-left,] infix, [dwim-right]), right). mutsu keeps the operator
        // text and both dwim flags on `Expr::HyperOp`, so this is a faithful
        // 1:1 mapping; raku omits a dwim field whose value is False.
        Expr::HyperOp {
            op,
            left,
            right,
            dwim_left,
            dwim_right,
        } => {
            let mut hyper_fields = Vec::with_capacity(3);
            if *dwim_left {
                hyper_fields.push(RakuAstField {
                    name: Some("dwim-left"),
                    value: RakuAstFieldValue::Node(Value::truth(true)),
                });
            }
            hyper_fields.push(node_field(
                Some("infix"),
                RakuAstNode {
                    class: RakuAstClass::Infix,
                    fields: vec![leaf_field(None, Value::str(op.clone()))],
                },
            ));
            if *dwim_right {
                hyper_fields.push(RakuAstField {
                    name: Some("dwim-right"),
                    value: RakuAstFieldValue::Node(Value::truth(true)),
                });
            }
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(Some("left"), convert_expr(left)?),
                    node_field(
                        Some("infix"),
                        RakuAstNode {
                            class: RakuAstClass::MetaInfixHyper,
                            fields: hyper_fields,
                        },
                    ),
                    node_field(Some("right"), convert_expr(right)?),
                ],
            })
        }
        // A hyper infix function `@a >>[&infix:<+>]<< @b` uses the same
        // MetaInfix::Hyper wrapper as an ordinary hyper operator, but its base
        // infix is a FunctionInfix containing the referenced code variable.
        Expr::HyperFuncOp {
            func_name,
            left,
            right,
            dwim_left,
            dwim_right,
        } => {
            let mut hyper_fields = Vec::with_capacity(3);
            if *dwim_left {
                hyper_fields.push(RakuAstField {
                    name: Some("dwim-left"),
                    value: RakuAstFieldValue::Node(Value::truth(true)),
                });
            }
            hyper_fields.push(node_field(
                Some("infix"),
                RakuAstNode {
                    class: RakuAstClass::FunctionInfix,
                    fields: vec![node_field(
                        None,
                        var_lexical("&", func_name.strip_prefix('&').unwrap_or(func_name)),
                    )],
                },
            ));
            if *dwim_right {
                hyper_fields.push(RakuAstField {
                    name: Some("dwim-right"),
                    value: RakuAstFieldValue::Node(Value::truth(true)),
                });
            }
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(Some("left"), convert_expr(left)?),
                    node_field(
                        Some("infix"),
                        RakuAstNode {
                            class: RakuAstClass::MetaInfixHyper,
                            fields: hyper_fields,
                        },
                    ),
                    node_field(Some("right"), convert_expr(right)?),
                ],
            })
        }
        Expr::Unary { op, expr } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(Some("prefix"), operator_node(RakuAstClass::Prefix, op)),
                node_field(Some("operand"), convert_expr(expr)?),
            ],
        }),
        Expr::PostfixOp { op, expr } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPostfix,
            fields: vec![
                node_field(Some("operand"), convert_expr(expr)?),
                node_field(Some("postfix"), postfix_node(op)),
            ],
        }),
        // `COND ?? THEN !! ELSE` -> Ternary(condition, then, else). Note raku
        // constant-folds a literal-condition ternary (`1 ?? 2 !! 3` -> IntLiteral(2));
        // mutsu does not, a documented divergence (the const-fold open question).
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Ok(RakuAstNode {
            class: RakuAstClass::Ternary,
            fields: vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("then"), convert_expr(then_expr)?),
                node_field(Some("else"), convert_expr(else_expr)?),
            ],
        }),
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => {
            let postfix = method_call_postfix(name.as_str(), args, *modifier, *quoted)?;
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), postfix),
                ],
            })
        }
        // Hyper method call `@a>>.abs` -> ApplyPostfix(operand,
        // postfix => MetaPostfix::Hyper(Call::Method(...))).
        Expr::HyperMethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => {
            let inner = method_call_postfix(name.as_str(), args, *modifier, *quoted)?;
            let hyper = RakuAstNode {
                class: RakuAstClass::MetaPostfixHyper,
                fields: vec![node_field(None, inner)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), hyper),
                ],
            })
        }
        // A bare comma list `1, 2, 3` -> ApplyListInfix(infix => ",", operands).
        Expr::ArrayLiteral(items) => comma_list_node(items),
        // A hash literal `{a => 1, b => 2}` -> a `Block` whose body is a comma
        // list of `FatArrow` pairs (raku models `{...}` as a block).
        Expr::Hash(pairs) => hash_literal_node(pairs),
        // An array-composer literal `[1, 2, 3]` ->
        // `Circumfix::ArrayComposer(SemiList(Statement::Expression(comma-list)))`.
        Expr::BracketArray(items, _) => {
            let inner = comma_list_node(items)?;
            let semilist = RakuAstNode {
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, statement_expression(inner))],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::CircumfixArrayComposer,
                fields: vec![node_field(None, semilist)],
            })
        }
        // Positional subscript `@x[EXPR]` -> ApplyPostfix(operand,
        // postfix => Postcircumfix::ArrayIndex(index => SemiList(...))).
        // Associative subscripts (`%h{...}` / `%h<...>`) are deferred: mutsu
        // cannot distinguish `<k>` (LiteralHashIndex) from `{"k"}` (HashIndex).
        // Reduction metaop `[+] @a` / triangle `[\+] @a` -> Term::Reduce.
        Expr::Reduction { op, expr } => {
            let (triangle, infix_op) = match op.strip_prefix('\\') {
                Some(stripped) => (true, stripped),
                None => (false, op.as_str()),
            };
            let arglist = RakuAstNode {
                class: RakuAstClass::ArgList,
                fields: vec![node_field(None, convert_expr(expr)?)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::TermReduce,
                fields: vec![
                    RakuAstField {
                        name: Some("triangle"),
                        value: RakuAstFieldValue::Node(Value::truth(triangle)),
                    },
                    node_field(Some("infix"), plain_infix(infix_op)),
                    node_field(Some("args"), arglist),
                ],
            })
        }
        Expr::Index {
            target,
            index,
            is_positional,
        } => {
            if !is_positional {
                return Err(unsupported("associative subscript"));
            }
            let semilist = RakuAstNode {
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, statement_expression(convert_expr(index)?))],
            };
            let array_index = RakuAstNode {
                class: RakuAstClass::PostcircumfixArrayIndex,
                fields: vec![node_field(Some("index"), semilist)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), convert_expr(target)?),
                    node_field(Some("postfix"), array_index),
                ],
            })
        }
        // A bare `{ ... }` block in expression position.
        Expr::Block(body) => block_node(body),
        Expr::AnonSub {
            body,
            is_rw,
            is_raw,
            is_block,
        } => {
            if *is_rw {
                return Err(unsupported("`is rw` block"));
            }
            if *is_raw {
                return Err(unsupported("`is raw` block"));
            }
            if *is_block {
                // A bare `{ ... }` block.
                block_node(body)
            } else {
                // An anonymous, parameter-less `sub { ... }`.
                Ok(RakuAstNode {
                    class: RakuAstClass::Sub,
                    fields: vec![node_field(Some("body"), blockoid(body)?)],
                })
            }
        }
        Expr::Lambda {
            param,
            body,
            is_whatever_code,
            ..
        } => {
            if *is_whatever_code {
                // ADR-0033 Phase 2 §2.5: reachable only from the still-eager
                // `* += 1` / `* -= 2` compound-assignment autoprime path (it
                // needs a `MetaInfix::Assign` class mutsu lacks; a separate,
                // operator-cluster-wide slice, not Whatever-specific).
                return Err(unsupported("Whatever-code closure (compound assignment)"));
            }
            pointy_block_from_lambda(param, body)
        }
        Expr::AnonSubParams {
            param_defs,
            body,
            is_rw,
            is_whatever_code,
            return_type,
            is_sub,
            ..
        } => {
            if *is_whatever_code {
                return Err(unsupported("Whatever-code closure (compound assignment)"));
            }
            if *is_rw {
                return Err(unsupported("`is rw` pointy block"));
            }
            if *is_sub {
                // `sub ($x) { }` — an anonymous *routine*, not a block. raku
                // renders it as a nameless `RakuAST::Sub` whose parameters
                // carry the implicit `type => Type::Setting(Any)` that every
                // sub/method signature has, where a pointy block's do not.
                return anon_routine_node(param_defs, body, return_type.as_deref());
            }
            pointy_block(param_defs, body, return_type.as_deref())
        }
        // An interpolated string `"a $x b"` -> QuotedString with a segment per
        // part (a literal run is a `StrLiteral`, an interpolated term keeps its
        // own node).
        Expr::StringInterpolation(parts) => {
            let mut segments = Vec::with_capacity(parts.len());
            for p in parts {
                segments.push(Value::rakuast(Box::new(interp_segment(p)?)));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::QuotedString,
                fields: vec![RakuAstField {
                    name: Some("segments"),
                    value: RakuAstFieldValue::List(segments),
                }],
            })
        }
        other => Err(unsupported(&format!("{other:?}"))),
    }
}

/// One segment of an interpolated string. A literal-string part is a bare
/// `StrLiteral` (not a nested `QuotedString`); any other part keeps its normal
/// converted node.
fn interp_segment(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) if matches!(v.view(), ValueView::Str(_)) => {
            Ok(RakuAstNode {
                class: RakuAstClass::StrLiteral,
                fields: vec![leaf_field(None, v.clone())],
            })
        }
        // An interpolated code block (`"a{ $x }b"`) is a segment like any other,
        // and raku renders it as a plain `Block`. mutsu wraps the block in a
        // `DoStmt` (that is how the parser makes it an expression), which has no
        // RakuAST counterpart, so unwrap it here rather than rendering the
        // wrapper. Measured against rakudo 2026.07.
        Expr::DoStmt(inner) => match inner.as_ref() {
            Stmt::Block(body) => block_node(body),
            _ => convert_expr(expr),
        },
        other => convert_expr(other),
    }
}

/// If `stmts` (ignoring `SetLine` bookkeeping) is exactly one `if` statement,
/// return it — this is how mutsu nests an `elsif`. Anything else (a real
/// `else` block, multiple statements, empty) returns `None`.
fn single_if_stmt(stmts: &[Stmt]) -> Option<&Stmt> {
    let mut real = stmts.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    let first = real.next()?;
    if real.next().is_some() {
        return None;
    }
    matches!(first, Stmt::If { .. }).then_some(first)
}

/// `STMT with X` / `STMT without X` -> the modified statement carrying a
/// `condition-modifier` of `StatementModifier::With` / `::Without`.
///
/// The parser desugars both into `given X { if $_.defined { STMT } }` (with the
/// condition negated for `without`), optionally wrapped in a `DoStmt` when the
/// modified statement is an expression statement. Everything but `STMT` and `X`
/// is scaffolding raku does not model, so this unwraps back to the two pieces
/// raku's node actually holds. Measured against rakudo 2026.07:
/// `Q["found" with "hello"].AST`.
fn with_modifier_node(
    kind: GivenWithKind,
    topic: &Expr,
    body: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let [wrapper] = body else {
        return Err(unsupported("multi-statement with/without modifier body"));
    };
    // The expression-statement spelling carries the `if` inside a `DoStmt`.
    let wrapper = match wrapper {
        Stmt::Expr(Expr::DoStmt(inner)) => inner.as_ref(),
        other => other,
    };
    let Stmt::If {
        then_branch,
        else_branch,
        ..
    } = wrapper
    else {
        return Err(unsupported(
            "with/without modifier body is not a conditional",
        ));
    };
    if !else_branch.is_empty() {
        return Err(unsupported("with/without modifier with an else branch"));
    }
    let mut real = then_branch
        .iter()
        .filter(|s| !matches!(s, Stmt::SetLine(_)));
    let (Some(modified), None) = (real.next(), real.next()) else {
        return Err(unsupported("multi-statement with/without modifier body"));
    };
    let mut statement =
        convert_stmt(modified)?.ok_or_else(|| unsupported("empty with/without modifier body"))?;
    statement.fields.push(node_field(
        Some("condition-modifier"),
        RakuAstNode {
            class: match kind {
                GivenWithKind::With => RakuAstClass::StatementModifierWith,
                GivenWithKind::Without => RakuAstClass::StatementModifierWithout,
                // The block scaffold never reaches the modifier path: the
                // conditional that owns it is handled by `with_block_node`, and
                // the `Given` arm rejects a stray one before calling this.
                GivenWithKind::BlockTopic | GivenWithKind::BlockTopicPointy => {
                    return Err(unsupported("with/without block scaffold"));
                }
            },
            fields: vec![node_field(None, convert_expr(topic)?)],
        },
    ));
    Ok(statement)
}

/// `with X { ... }` / `without X { ... }` -> `Statement::With` / `::Without`.
///
/// The parser desugars the block forms into
/// `if (my $tmp = X).defined { given X { ... } }` (condition negated for
/// `without`), so everything raku models is wrapped in scaffolding: the once-
/// evaluated temp around the condition, and the topicalizing `given` around the
/// body, which raku spells as the block's own `implicit-topic` flag. `kind`
/// says which keyword the source wrote -- see `Stmt::If`'s `with_kind`.
/// Measured against rakudo 2026.07: `Q[with 1 { say 2 }].AST`.
fn with_block_node(
    kind: WithBlockKind,
    cond: &Expr,
    then_branch: &[Stmt],
    else_branch: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let condition = node_field(
        Some("condition"),
        convert_expr(with_block_condition(kind, cond)?)?,
    );
    let Some(body) = topic_given_body(then_branch) else {
        // A pointy body (`with X -> $a { }`) binds its parameter inside the same
        // `given`, which raku spells as a `PointyBlock` rather than an
        // implicit-topic `Block`; report the boundary rather than drop it.
        return Err(unsupported("with/without block with an explicit signature"));
    };
    match kind {
        // `without` takes no `else`/`orwith`/`elsif` (rakudo rejects them at
        // compile time), so its node is condition + `body` -- the same naming
        // difference `Statement::Unless` has against `::If`.
        WithBlockKind::Without => {
            if !else_branch.is_empty() {
                return Err(unsupported("`without` with an else branch"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::StatementWithout,
                fields: vec![condition, node_field(Some("body"), topic_block_node(body)?)],
            })
        }
        WithBlockKind::With => {
            let mut fields = vec![condition, node_field(Some("then"), topic_block_node(body)?)];
            fields.extend(conditional_chain_fields(else_branch)?);
            Ok(RakuAstNode {
                class: RakuAstClass::StatementWith,
                fields,
            })
        }
        // An `orwith` clause is only ever reached through the chain walk of the
        // conditional it continues.
        WithBlockKind::Orwith => Err(unsupported("`orwith` outside a conditional chain")),
    }
}

/// One `orwith` clause -> `Statement::Orwith(condition, then => topic Block)`.
fn orwith_node(cond: &Expr, then_branch: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let Some(body) = topic_given_body(then_branch) else {
        return Err(unsupported("`orwith` block with an explicit signature"));
    };
    Ok(RakuAstNode {
        class: RakuAstClass::StatementOrwith,
        fields: vec![
            node_field(
                Some("condition"),
                convert_expr(with_block_condition(WithBlockKind::Orwith, cond)?)?,
            ),
            node_field(Some("then"), topic_block_node(body)?),
        ],
    })
}

/// The condition a `with`-family conditional was written with, recovered from
/// the `.defined` test the parser built around it.
fn with_block_condition(kind: WithBlockKind, cond: &Expr) -> Result<&Expr, RuntimeError> {
    let tested = match kind {
        WithBlockKind::Without => match cond {
            Expr::Unary {
                op: crate::token_kind::TokenKind::Bang,
                expr,
            } => expr.as_ref(),
            _ => return Err(unsupported("`without` condition")),
        },
        WithBlockKind::With | WithBlockKind::Orwith => cond,
    };
    let Expr::MethodCall { target, name, .. } = tested else {
        return Err(unsupported("with/without condition"));
    };
    if name.as_str() != "defined" {
        return Err(unsupported("with/without condition"));
    }
    Ok(match target.as_ref() {
        // The block forms evaluate the condition once into a hidden temp; an
        // `orwith` clause tests its expression directly.
        Expr::DoStmt(inner) => match inner.as_ref() {
            Stmt::VarDecl { expr, .. } => expr,
            _ => return Err(unsupported("with/without condition")),
        },
        other => other,
    })
}

/// The body of the topicalizing `given` the `with`-family block forms wrap a
/// `{ ... }` in, when `stmts` is exactly that scaffold. `None` for anything
/// else, including the untagged `given` a pointy body produces and a
/// hand-written `given` that happens to sit in the same position.
fn topic_given_body(stmts: &[Stmt]) -> Option<&[Stmt]> {
    let mut real = stmts.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    let (Some(first), None) = (real.next(), real.next()) else {
        return None;
    };
    match first {
        Stmt::Given {
            body,
            with_kind: Some(GivenWithKind::BlockTopic),
            ..
        } => Some(body),
        _ => None,
    }
}

/// The `elsifs` and `else` fields of a conditional chain. mutsu nests every
/// continuation clause as a single `if` inside the else branch; raku flattens
/// them into one `elsifs` list, with whatever remains as the `else` block.
/// Shared by `Statement::If` and `Statement::With`, both of which accept
/// `elsif` and `orwith` clauses.
fn conditional_chain_fields(else_branch: &[Stmt]) -> Result<Vec<RakuAstField>, RuntimeError> {
    let mut fields = Vec::new();
    let mut elsifs: Vec<Value> = Vec::new();
    let mut tail: &[Stmt] = else_branch;
    while let Some(Stmt::If {
        cond,
        then_branch,
        else_branch,
        binding_var,
        with_kind,
        ..
    }) = single_if_stmt(tail)
    {
        // A `with`/`without` BLOCK statement written inside an `else` is a
        // statement of its own, not a continuation clause: stop and let it be
        // converted as part of the else block.
        if matches!(
            with_kind,
            Some(WithBlockKind::With | WithBlockKind::Without)
        ) {
            break;
        }
        if binding_var.is_some() {
            return Err(unsupported("`elsif EXPR -> $var` topic binding"));
        }
        let node = match with_kind {
            Some(WithBlockKind::Orwith) => orwith_node(cond, then_branch)?,
            _ => elsif_node(cond, then_branch)?,
        };
        elsifs.push(Value::rakuast(Box::new(node)));
        tail = else_branch;
    }
    if !elsifs.is_empty() {
        fields.push(RakuAstField {
            name: Some("elsifs"),
            value: RakuAstFieldValue::List(elsifs),
        });
    }
    if tail.iter().any(|s| !matches!(s, Stmt::SetLine(_))) {
        // An `else` continuing a `with`/`orwith` topicalizes on the last tested
        // value, which raku records on the block itself.
        let block = match topic_given_body(tail) {
            Some(body) => topic_block_node(body)?,
            None => block_node(tail)?,
        };
        fields.push(node_field(Some("else"), block));
    }
    Ok(fields)
}

/// One `elsif` clause -> `Statement::Elsif(condition, then => Block)`.
fn elsif_node(cond: &Expr, then_branch: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::StatementElsif,
        fields: vec![
            node_field(Some("condition"), convert_expr(cond)?),
            node_field(Some("then"), block_node(then_branch)?),
        ],
    })
}

/// A `{ ... }` block body wraps its `StatementList` in a `Blockoid`.
fn blockoid(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Blockoid,
        fields: vec![node_field(None, statement_list(body)?)],
    })
}

/// A bare `{ ... }` block -> `Block(body => Blockoid)`.
fn block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![node_field(Some("body"), blockoid(body)?)],
    })
}

/// Convert the source-level regex tree to the corresponding RakuAST regex
/// node. The tree is deliberately separate from `RegexPattern`: the latter is
/// an execution plan and has already lost source-level declaration and
/// whitespace information by the time matching begins.
fn regex_node(node: &RegexNode) -> Result<RakuAstNode, RuntimeError> {
    let (class, fields) = match node {
        RegexNode::Literal(text) => (
            RakuAstClass::RegexLiteral,
            vec![leaf_field(None, Value::str(text.clone()))],
        ),
        RegexNode::Quote(text) => (
            RakuAstClass::RegexQuote,
            vec![node_field(None, quoted_string(Value::str(text.clone())))],
        ),
        RegexNode::Sequence(nodes) => (
            RakuAstClass::RegexSequence,
            nodes
                .iter()
                .map(|child| regex_node(child).map(|node| node_field(None, node)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        RegexNode::Alternation(nodes) => (
            RakuAstClass::RegexAlternation,
            nodes
                .iter()
                .map(|child| regex_node(child).map(|node| node_field(None, node)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        RegexNode::Group(child) => (
            RakuAstClass::RegexGroup,
            vec![node_field(None, regex_node(child)?)],
        ),
        RegexNode::CapturingGroup(child) => (
            RakuAstClass::RegexCapturingGroup,
            vec![node_field(None, regex_node(child)?)],
        ),
        RegexNode::NamedCapture { name, regex } => (
            RakuAstClass::RegexNamedCapture,
            vec![
                leaf_field(Some("name"), Value::str(name.clone())),
                node_field(Some("regex"), regex_node(regex)?),
            ],
        ),
        RegexNode::Interpolation { name, sequential } => (
            RakuAstClass::RegexInterpolation,
            vec![
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                node_field(Some("var"), var_lexical("$", name)),
            ],
        ),
        RegexNode::Quantified { atom, quantifier } => {
            let quantifier = match quantifier {
                RegexQuantifier::ZeroOrMore => RakuAstClass::RegexQuantifierZeroOrMore,
                RegexQuantifier::OneOrMore => RakuAstClass::RegexQuantifierOneOrMore,
                RegexQuantifier::ZeroOrOne => RakuAstClass::RegexQuantifierZeroOrOne,
            };
            (
                RakuAstClass::RegexQuantifiedAtom,
                vec![
                    node_field(Some("atom"), regex_node(atom)?),
                    node_field(
                        Some("quantifier"),
                        RakuAstNode {
                            class: quantifier,
                            fields: Vec::new(),
                        },
                    ),
                ],
            )
        }
        RegexNode::CharClassDigit => (RakuAstClass::RegexCharClassDigit, Vec::new()),
        RegexNode::WithWhitespace(child) => (
            RakuAstClass::RegexWithWhitespace,
            vec![node_field(None, regex_node(child)?)],
        ),
    };
    Ok(RakuAstNode { class, fields })
}

fn regex_declaration(
    class: RakuAstClass,
    name: &str,
    tree: &RegexTree,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class,
        fields: vec![
            node_field(Some("name"), name_from_identifier(name)),
            node_field(Some("body"), regex_node(&tree.body)?),
        ],
    })
}

fn quoted_regex_node(tree: &RegexTree) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    if tree.match_immediately {
        fields.push(leaf_field(Some("match-immediately"), Value::truth(true)));
    }
    fields.push(node_field(Some("body"), regex_node(&tree.body)?));
    if !tree.adverbs.is_empty() {
        let adverbs = tree
            .adverbs
            .iter()
            .map(regex_adverb_node)
            .collect::<Result<Vec<_>, _>>()?;
        fields.push(RakuAstField {
            name: Some("adverbs"),
            value: RakuAstFieldValue::List(
                adverbs
                    .into_iter()
                    .map(|node| Value::rakuast(Box::new(node)))
                    .collect(),
            ),
        });
    }
    Ok(RakuAstNode {
        class: RakuAstClass::QuotedRegex,
        fields,
    })
}

fn regex_adverb_node(adverb: &crate::regex_tree::RegexAdverb) -> Result<RakuAstNode, RuntimeError> {
    if adverb.argument.is_some() {
        return Err(unsupported("regex adverb with an argument"));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ColonPairTrue,
        fields: vec![leaf_field(None, Value::str(adverb.name.clone()))],
    })
}

/// The leading `labels => (Label(name => "..."),)` field for a labelled loop,
/// or an empty vec when unlabelled. raku always renders labels first.
fn label_fields(label: &Option<String>) -> Vec<RakuAstField> {
    match label {
        None => Vec::new(),
        Some(name) => {
            let label_node = RakuAstNode {
                class: RakuAstClass::Label,
                fields: vec![leaf_field(Some("name"), Value::str(name.clone()))],
            };
            vec![RakuAstField {
                name: Some("labels"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(label_node))]),
            }]
        }
    }
}

/// A C-style loop `setup` clause renders its node unwrapped — raku shows the
/// `VarDeclaration::Simple` / `ApplyInfix` directly, not inside a
/// `Statement::Expression`. mutsu's init is a full statement.
fn loop_setup_node(stmt: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    // A `my $i = 0` init is a special case: the loop-init parse path does NOT
    // set the `__has_initializer` trait the top-level VarDecl handler relies on,
    // so detect the initializer from a non-Nil `expr` instead.
    if let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        where_constraint,
        ..
    } = stmt
    {
        if type_constraint.is_some()
            || *is_state
            || *is_our
            || *is_dynamic
            || where_constraint.is_some()
        {
            return Err(unsupported("scoped/typed variable declaration"));
        }
        let init = (!expr_is_nil(expr)).then_some(expr);
        return var_declaration(name, init, None, None, None, None);
    }
    // Assignment / expression setups convert normally, then get unwrapped.
    let node = convert_stmt(stmt)?.ok_or_else(|| unsupported("empty loop setup clause"))?;
    if node.class != RakuAstClass::StatementExpression {
        return Ok(node);
    }
    if let Some(RakuAstField {
        value: RakuAstFieldValue::Node(v),
        ..
    }) = node.fields.into_iter().next()
        && let ValueView::RakuAst(inner) = v.view()
    {
        return Ok(inner.clone());
    }
    Err(unsupported("loop setup clause"))
}

/// True when `expr` is a `Nil` literal (mutsu's placeholder for a `my $x` with
/// no initializer in a loop-setup clause).
fn expr_is_nil(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(v) | Expr::LiteralSrc(v, _) if v.is_nil())
}

/// A topic-taking block body (the `{ ... }` of an implicit-topic `for`), which
/// raku marks with `implicit-topic => True` and `required-topic => 1` before
/// the `body` field.
fn topic_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![
            RakuAstField {
                name: Some("implicit-topic"),
                value: RakuAstFieldValue::Node(Value::truth(true)),
            },
            RakuAstField {
                name: Some("required-topic"),
                value: RakuAstFieldValue::Node(Value::int(1)),
            },
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// The body of a `CATCH` block: a topic block that also carries `exception => 1`.
fn exception_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut node = topic_block_node(body)?;
    // raku renders the fields in declaration order, with `exception` after the
    // two topic flags and before `body`.
    let body_field = node
        .fields
        .pop()
        .expect("topic_block_node pushes body last");
    node.fields.push(RakuAstField {
        name: Some("exception"),
        value: RakuAstFieldValue::Node(Value::int(1)),
    });
    node.fields.push(body_field);
    Ok(node)
}

/// A multi/zero-parameter pointy block (`-> $a, $b { }`, `-> { }`). An empty
/// parameter list with no `--> T` return type omits the `signature` field
/// entirely (matching raku).
fn pointy_block(
    param_defs: &[ParamDef],
    body: &[Stmt],
    returns: Option<&str>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    if !param_defs.is_empty() || returns.is_some() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, false, returns)?,
        ));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode {
        class: RakuAstClass::PointyBlock,
        fields,
    })
}

/// An anonymous routine (`sub ($x) { }`) — a nameless `RakuAST::Sub`. Same
/// shape as [`routine_node`] minus the `name` field, so its parameters carry
/// the implicit `type => Type::Setting(Any)` a sub signature has (a pointy
/// block's parameters do not). Only the `-->` return spelling can reach here:
/// an anonymous sub's node keeps no `custom_traits`, so there is no
/// `returns`/`of` marker to read.
fn anon_routine_node(
    param_defs: &[ParamDef],
    body: &[Stmt],
    returns: Option<&str>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    if !param_defs.is_empty() || returns.is_some() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, returns)?,
        ));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode {
        class: RakuAstClass::Sub,
        fields,
    })
}

/// A single-parameter pointy block (`-> $x { }`). mutsu's `Lambda` node strips
/// the sigil from its single param and does NOT preserve `@`/`%` for a single
/// non-scalar param (`-> @a` becomes `param: "a"`), so we assume `$` — a
/// documented divergence from raku, which shows the real sigil.
fn pointy_block_from_lambda(param: &str, body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let sig = RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(simple_parameter(
                "$", param, None, None, false, None,
            )?))]),
        }],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::PointyBlock,
        fields: vec![
            node_field(Some("signature"), sig),
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// `constant X = 5` -> `VarDeclaration::Constant(name => "X", initializer =>
/// Initializer::Assign(...))`. Measured against rakudo 2026.07: the `name` is a
/// plain string (not a `Name` node), the package-scoped default spelling emits
/// no `scope`, and `my constant Y = 7` emits `scope => "my"`.
///
/// A sigilled constant (`constant @a = 1, 2`) or a typed one carries shape this
/// does not model yet, so both stay a boundary.
fn constant_declaration(
    name: &str,
    expr: &Expr,
    custom_traits: &[(String, Option<Expr>)],
    type_constraint: &Option<String>,
    is_our: bool,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    if type_constraint.is_some() {
        return Err(unsupported("typed constant"));
    }
    // `__constant_sigil` carries the declared sigil; only the sigilless form
    // (a plain `constant X`) maps onto the measured node shape.
    let sigil = custom_traits.iter().find_map(|(n, arg)| {
        (n == "__constant_sigil").then(|| match arg {
            Some(Expr::Literal(v)) => match v.view() {
                ValueView::Str(s) => s.to_string(),
                _ => String::new(),
            },
            _ => String::new(),
        })
    });
    if sigil.is_some_and(|s| !s.is_empty()) {
        return Err(unsupported("sigilled constant"));
    }
    if custom_traits
        .iter()
        .any(|(n, _)| n != "__constant" && n != "__constant_sigil" && n != "__has_initializer")
    {
        return Err(unsupported("constant with traits"));
    }
    let mut fields = Vec::new();
    // The package-scoped default (`constant X`) prints no scope; `my constant`
    // does. mutsu records the default as `is_our`.
    if !is_our {
        fields.push(leaf_field(Some("scope"), Value::str_from("my")));
    }
    fields.push(leaf_field(Some("name"), Value::str(name.to_string())));
    fields.push(node_field(
        Some("initializer"),
        RakuAstNode {
            class: RakuAstClass::InitializerAssign,
            fields: vec![node_field(None, convert_expr(expr)?)],
        },
    ));
    Ok(Some(statement_expression(RakuAstNode {
        class: RakuAstClass::VarDeclarationConstant,
        fields,
    })))
}

/// Strip the `!` the parser adds when it stores `until X` as `while !X`.
///
/// The flag and the negation are planted together, so an `is_until` loop whose
/// condition is *not* a `!` would mean the two had drifted apart; refuse rather
/// than render a condition that is not the one in the source.
fn strip_negation(cond: &Expr) -> Result<&Expr, RuntimeError> {
    match cond {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Bang,
            expr,
        } => Ok(expr),
        _ => Err(unsupported(
            "`unless`/`until` without the parser's negation",
        )),
    }
}

/// A class declaration's `traits` list, in source order: `is P` inheritance
/// first, then `does R` role composition, then a bare `is rw`.
///
/// raku spells the three differently, all measured against rakudo 2026.07:
/// `is Int` is `Trait::Is(type => Type::Simple)` (a NAMED `type`), `does R` is
/// `Trait::Does(Type::Simple)` (POSITIONAL), and `is rw` is
/// `Trait::Is(name => Name)` — a trait *name*, not a type.
fn class_traits(
    parents: &[String],
    does_parents: &[String],
    is_rw: bool,
) -> Result<Vec<Value>, RuntimeError> {
    let mut traits = Vec::new();
    for parent in parents {
        // A `does R` role is recorded in BOTH lists (`parents` is the general
        // composed-type list the dispatcher reads), so skip the ones that are
        // really role composition or they would render twice.
        if does_parents.iter().any(|r| r == parent) {
            continue;
        }
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields: vec![node_field(Some("type"), build_type_node(parent)?)],
        })));
    }
    for role in does_parents {
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitDoes,
            fields: vec![node_field(None, build_type_node(role)?)],
        })));
    }
    if is_rw {
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields: vec![node_field(Some("name"), name_from_identifier("rw"))],
        })));
    }
    Ok(traits)
}

/// The `RakuAST::StatementPrefix::Phaser::<Kind>` class for a phaser kind.
/// `PRE`/`POST` have no plain mapping — rakudo wraps their block in a call —
/// so they answer `None` and stay a coverage boundary.
fn phaser_class(kind: &crate::ast::PhaserKind) -> Option<RakuAstClass> {
    use crate::ast::PhaserKind::*;
    Some(match kind {
        Begin => RakuAstClass::StatementPrefixPhaserBegin,
        Check => RakuAstClass::StatementPrefixPhaserCheck,
        Init => RakuAstClass::StatementPrefixPhaserInit,
        End => RakuAstClass::StatementPrefixPhaserEnd,
        Enter => RakuAstClass::StatementPrefixPhaserEnter,
        Leave => RakuAstClass::StatementPrefixPhaserLeave,
        Keep => RakuAstClass::StatementPrefixPhaserKeep,
        Undo => RakuAstClass::StatementPrefixPhaserUndo,
        First => RakuAstClass::StatementPrefixPhaserFirst,
        Next => RakuAstClass::StatementPrefixPhaserNext,
        Last => RakuAstClass::StatementPrefixPhaserLast,
        Quit => RakuAstClass::StatementPrefixPhaserQuit,
        Close => RakuAstClass::StatementPrefixPhaserClose,
        Pre | Post => return None,
    })
}

/// The parser markers that record a `returns` / `of` return-type trait. They
/// are internal (`__`-prefixed) bookkeeping, not user traits, so the converter
/// reads them for the spelling instead of treating them as a coverage boundary.
fn is_return_spelling_marker(trait_name: &str) -> bool {
    matches!(trait_name, "__return_via_trait" | "__return_via_of")
}

/// Which spelling a routine's return type used, read off the parser markers.
/// `returns X of Y` leaves both markers (mutsu folds them into one
/// `X[Y]` return type); raku models that as a single trait, so defer.
fn return_type_spelling(
    custom_traits: &[(String, Option<Expr>)],
) -> Result<ReturnSpelling, RuntimeError> {
    let returns = custom_traits.iter().any(|(t, _)| t == "__return_via_trait");
    let of = custom_traits.iter().any(|(t, _)| t == "__return_via_of");
    match (returns, of) {
        (false, false) => Ok(ReturnSpelling::Arrow),
        (true, false) => Ok(ReturnSpelling::ReturnsTrait),
        (false, true) => Ok(ReturnSpelling::OfTrait),
        (true, true) => Err(unsupported("`returns X of Y` combined return trait")),
    }
}

/// How a routine's return type was written in the source. raku models the two
/// spellings with different nodes, and mutsu's internal AST keeps them apart
/// (the `returns`/`of` forms leave a `__return_via_*` marker in `custom_traits`),
/// so the converter never has to guess.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ReturnSpelling {
    /// `sub f(--> Int)` — part of the signature (`Signature.returns`).
    Arrow,
    /// `sub f() returns Int` — a routine trait (`Trait::Returns`).
    ReturnsTrait,
    /// `sub f() of Int` — a routine trait (`Trait::Of`).
    OfTrait,
}

/// A named routine — `Sub` or `Method` — with an optional signature, optional
/// return type, and a body. A parameter-less routine with no `-->` return type
/// omits the `signature` field; parameters carry the implicit
/// `type => Type::Setting(Any)` (`type_setting = true`).
fn routine_node(
    class: RakuAstClass,
    name: &str,
    param_defs: &[ParamDef],
    body: &[Stmt],
    return_type: Option<(&str, ReturnSpelling)>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(Some("name"), name_from_identifier(name))];
    let arrow_returns = match return_type {
        Some((t, ReturnSpelling::Arrow)) => Some(t),
        _ => None,
    };
    if !param_defs.is_empty() || arrow_returns.is_some() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, arrow_returns)?,
        ));
    }
    if let Some((t, spelling)) = return_type
        && spelling != ReturnSpelling::Arrow
    {
        let trait_class = match spelling {
            ReturnSpelling::OfTrait => RakuAstClass::TraitOf,
            _ => RakuAstClass::TraitReturns,
        };
        let trait_node = RakuAstNode {
            class: trait_class,
            fields: vec![node_field(None, build_type_node(t)?)],
        };
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(trait_node))]),
        });
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode { class, fields })
}

/// `Signature(parameters => (Parameter, ...)[, returns => Type])`. `type_setting`
/// prepends the implicit `type => Type::Setting(Any)` on each parameter —
/// present in sub/method signatures, absent in pointy-block signatures.
fn signature(
    param_defs: &[ParamDef],
    type_setting: bool,
    returns: Option<&str>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut params = Vec::with_capacity(param_defs.len());
    for pd in param_defs {
        params.push(Value::rakuast(Box::new(parameter(pd, type_setting)?)));
    }
    let mut fields = vec![RakuAstField {
        name: Some("parameters"),
        value: RakuAstFieldValue::List(params),
    }];
    if let Some(t) = returns {
        fields.push(node_field(Some("returns"), build_type_node(t)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Signature,
        fields,
    })
}

/// One `Parameter`. Positional sub-signatures are represented recursively as
/// `sub-signature => Signature`, and a basic `::T` type capture becomes the
/// `type-captures` field. Richer capture forms remain the coverage boundary.
fn parameter(pd: &ParamDef, type_setting: bool) -> Result<RakuAstNode, RuntimeError> {
    if pd.onearg
        || pd.literal_value.is_some()
        || !pd.traits.is_empty()
        || pd.optional_marker
        || pd.is_invocant
        || pd.shape_constraints.is_some()
        || pd.code_signature.is_some()
        || pd.outer_sub_signature.is_some()
    {
        return Err(unsupported("non-trivial signature parameter"));
    }
    // Named aliases (`:s(:$sort)`) and capture parameters also use the
    // internal `sub_signature` slot, but RakuAST represents those with fields
    // other than `sub-signature`. Keep this slice to ordinary positional and
    // array-destructuring parameters whose target is preserved by mutsu.
    if pd.sub_signature.is_some()
        && (pd.named || pd.slurpy || pd.double_slurpy || pd.sigilless || pd.name.starts_with("__"))
    {
        return Err(unsupported("non-positional signature sub-signature"));
    }
    let type_capture = match pd.captured_type_name() {
        Some(name) => Some(type_capture_node(name)?),
        None => None,
    };
    // A bare `::T` is represented internally by a synthetic parameter name,
    // but RakuAST models it as a Parameter with no target. Keep the synthetic
    // name only in the internal AST used by the existing binder.
    if pd.name.starts_with("__type_capture__") {
        let Some(type_capture) = type_capture else {
            return Err(unsupported("type capture without a capture node"));
        };
        let mut fields = Vec::with_capacity(4);
        if type_setting {
            fields.push(node_field(Some("type"), type_setting_any()));
        }
        fields.push(type_captures_field(type_capture));
        match &pd.default {
            Some(default) => fields.push(node_field(Some("default"), convert_expr(default)?)),
            None => fields.push(RakuAstField {
                name: Some("optional"),
                value: RakuAstFieldValue::Node(Value::truth(false)),
            }),
        }
        return Ok(RakuAstNode {
            class: RakuAstClass::Parameter,
            fields,
        });
    }
    let ordinary_type_constraint = if type_capture.is_some() {
        None
    } else {
        pd.type_constraint.as_deref()
    };
    let (sigil, desigil) = split_sigil(&pd.name);
    let mut node = if pd.slurpy || pd.double_slurpy {
        // A typed or where-constrained slurpy carries richer shape; defer.
        if pd.type_constraint.is_some() || pd.where_constraint.is_some() {
            return Err(unsupported("typed slurpy parameter"));
        }
        slurpy_parameter(sigil, desigil, pd.double_slurpy)?
    } else if pd.named {
        // A typed/defaulted/where-constrained named param carries richer shape.
        if (pd.type_constraint.is_some() && type_capture.is_none())
            || pd.default.is_some()
            || pd.where_constraint.is_some()
        {
            return Err(unsupported("typed/defaulted named parameter"));
        }
        named_parameter(sigil, desigil, type_setting)?
    } else {
        simple_parameter(
            sigil,
            desigil,
            ordinary_type_constraint,
            pd.default.as_ref(),
            type_setting,
            pd.where_constraint.as_deref(),
        )?
    };
    if let Some(type_capture) = type_capture {
        let target_index = node
            .fields
            .iter()
            .position(|field| field.name == Some("target"))
            .ok_or_else(|| unsupported("type capture without a parameter target"))?;
        node.fields
            .insert(target_index, type_captures_field(type_capture));
    }
    if let Some(sub_params) = &pd.sub_signature {
        node.fields.push(node_field(
            Some("sub-signature"),
            signature(sub_params, type_setting, None)?,
        ));
    }
    Ok(node)
}

/// A basic `::T` capture is represented by `Parameter.type-captures` rather
/// than by the parameter's ordinary `type` node. Smiley-constrained and other
/// richer capture spellings need more internal metadata and remain deferred.
fn type_capture_node(name: &str) -> Result<RakuAstNode, RuntimeError> {
    if name.is_empty()
        || !name
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '-')
    {
        return Err(unsupported("complex type capture"));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::TypeCapture,
        fields: vec![node_field(None, name_from_identifier(name))],
    })
}

fn type_captures_field(type_capture: RakuAstNode) -> RakuAstField {
    RakuAstField {
        name: Some("type-captures"),
        value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(type_capture))]),
    }
}

/// A slurpy parameter `*@a` / `**@a` -> `Parameter(target => …, slurpy =>
/// RakuAST::Parameter::Slurpy::{Flattened,Unflattened})`. A slurpy carries no
/// `type`/`optional` field, and the marker is a type object rather than a node
/// (see `slurpy_marker_value`).
fn slurpy_parameter(sigil: &str, desigil: &str, double: bool) -> Result<RakuAstNode, RuntimeError> {
    let target = RakuAstNode {
        class: RakuAstClass::ParameterTargetVar,
        fields: vec![leaf_field(
            Some("name"),
            Value::str(format!("{sigil}{desigil}")),
        )],
    };
    // The marker is the `RakuAST::Parameter::Slurpy::*` TYPE OBJECT, as it is in
    // rakudo -- see `slurpy_marker_value`.
    let slurpy = super::slurpy_marker_value(if double {
        RakuAstClass::ParameterSlurpyUnflattened
    } else {
        RakuAstClass::ParameterSlurpyFlattened
    });
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields: vec![
            node_field(Some("target"), target),
            leaf_field(Some("slurpy"), slurpy),
        ],
    })
}

/// A named parameter `:$x` -> `Parameter(type => Type::Setting(Any),
/// names => ("x",), target => ParameterTarget::Var)`. Named params are optional
/// by default, so no `optional`/`default` field is emitted.
fn named_parameter(
    sigil: &str,
    desigil: &str,
    type_setting: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let target = RakuAstNode {
        class: RakuAstClass::ParameterTargetVar,
        fields: vec![leaf_field(
            Some("name"),
            Value::str(format!("{sigil}{desigil}")),
        )],
    };
    let mut fields = Vec::new();
    if type_setting {
        fields.push(node_field(Some("type"), type_setting_any()));
    }
    fields.push(RakuAstField {
        name: Some("names"),
        value: RakuAstFieldValue::List(vec![Value::str(desigil.to_string())]),
    });
    fields.push(node_field(Some("target"), target));
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    })
}

/// `Type::Setting.new(Name.from-identifier("Any"))` — the implicit default type
/// carried by every sub/method-signature parameter.
fn type_setting_any() -> RakuAstNode {
    let name = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str("Any".to_string()))],
    };
    RakuAstNode {
        class: RakuAstClass::TypeSetting,
        fields: vec![node_field(None, name)],
    }
}

/// Build a `Parameter(target => ParameterTarget::Var, ...)`. A param with a
/// default renders `default => <expr>`; a required one renders `optional => False`.
/// `type_setting` prepends `type => Type::Setting(Any)` (sub/method params).
fn simple_parameter(
    sigil: &str,
    desigil: &str,
    type_constraint: Option<&str>,
    default: Option<&Expr>,
    type_setting: bool,
    where_constraint: Option<&Expr>,
) -> Result<RakuAstNode, RuntimeError> {
    let target = RakuAstNode {
        class: RakuAstClass::ParameterTargetVar,
        fields: vec![leaf_field(
            Some("name"),
            Value::str(format!("{sigil}{desigil}")),
        )],
    };
    let mut fields = Vec::new();
    // An explicit type constraint (`Int $x`) replaces the implicit
    // `Type::Setting(Any)` that untyped sub/method params carry.
    match type_constraint {
        Some(tc) => fields.push(node_field(Some("type"), build_type_node(tc)?)),
        None if type_setting => fields.push(node_field(Some("type"), type_setting_any())),
        None => {}
    }
    fields.push(node_field(Some("target"), target));
    match default {
        Some(d) => fields.push(node_field(Some("default"), convert_expr(d)?)),
        None => fields.push(RakuAstField {
            name: Some("optional"),
            value: RakuAstFieldValue::Node(Value::truth(false)),
        }),
    }
    // `$x where EXPR` — the same `where` field `RakuAST::Parameter.new(:where)`
    // builds and `EVAL` already lowers and enforces. It follows `optional` /
    // `default` in the model's canonical accessor order.
    if let Some(w) = where_constraint {
        fields.push(node_field(Some("where"), convert_expr(w)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    })
}

/// The postfix `Call::Method` / `Call::QuotedMethod` node shared by plain and
/// hyper method calls: a quoted name -> `Call::QuotedMethod` (no modifier), an
/// unquoted name -> `Call::Method` (with an optional `.?`/`.+`/`.*` dispatch).
fn method_call_postfix(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
    quoted: bool,
) -> Result<RakuAstNode, RuntimeError> {
    if quoted {
        if modifier.is_some() {
            return Err(unsupported("quoted method name with a modifier"));
        }
        return call_quoted_method(name, args);
    }
    // `.^name` is a *metamethod* call, not an ordinary call with a dispatch
    // modifier: raku gives it its own `Call::MetaMethod` class whose `name` is a
    // plain string. Only `.?` / `.+` / `.*` are dispatch modifiers.
    if modifier == Some('^') {
        let mut fields = vec![leaf_field(Some("name"), Value::str(name.to_string()))];
        if !args.is_empty() {
            fields.push(node_field(Some("args"), arg_list(args)?));
        }
        return Ok(RakuAstNode {
            class: RakuAstClass::CallMetaMethod,
            fields,
        });
    }
    call_method(name, args, modifier)
}

/// `.method` / `.method(args)` -> `Call::Method(name => Name, [args => ArgList])`.
fn call_method(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    // Field order matches raku: name, args, dispatch.
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    if let Some(m) = modifier {
        // `.?` / `.+` / `.*` become a `dispatch` string.
        fields.push(leaf_field(Some("dispatch"), Value::str(format!(".{m}"))));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::CallMethod,
        fields,
    })
}

/// `."foo"` / `."foo"(args)` -> `Call::QuotedMethod(name => QuotedString,
/// [args => ArgList])`. Unlike `Call::Method`, the name is a QuotedString
/// (a string literal) rather than a `Name.from-identifier`.
fn call_quoted_method(name: &str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(
        Some("name"),
        quoted_string(Value::str(name.to_string())),
    )];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::CallQuotedMethod,
        fields,
    })
}

/// Whether a routine or variable name is one of mutsu's internal desugaring
/// markers (`__mutsu_hyper_prefix`, `__with_tmp_0`, `__destructure_tmp__`, …)
/// rather than something the source wrote. raku keeps these constructs as
/// dedicated nodes and never has such a name, so rendering one would emit a
/// node that cannot exist in a real RakuAST tree. Refusing is the same rule the
/// rest of the converter follows: an erased distinction is a boundary, never a
/// guess. The underlying constructs are tracked as read-direction gaps in
/// rakuast-remaining (#7564).
fn is_desugar_marker(name: &str) -> bool {
    name.starts_with("__") || name.starts_with("@__") || name.starts_with("%__")
}

/// The coverage-boundary error for a construct that reached conversion already
/// desugared into an internal marker.
fn desugared(name: &str) -> RuntimeError {
    unsupported(&format!("desugared construct (internal name `{name}`)"))
}

/// `$x` / `@a` / `%h` / `&f` usage -> `Var::Lexical("<sigil><name>")`.
fn var_lexical(sigil: &str, name: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::VarLexical,
        fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
    }
}

/// `Infix`/`Prefix` — a single positional operator string (e.g. `Infix.new("+")`).
fn operator_node(class: RakuAstClass, op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        class,
        fields: vec![leaf_field(None, Value::str(token_kind_to_op_name(op)))],
    }
}

/// Render a chained comparison as rakudo's left-nested `ApplyInfix`
/// (`todo/tickets/chained-compare-ast-node.md`): `ops[i]` links
/// `operands[i]` and `operands[i+1]`, folded left-to-right so `a < b < c`
/// becomes `ApplyInfix(ApplyInfix(a, "<", b), "<", c)`, matching
/// `Q[1 < 2 < 3].AST` measured against rakudo. A negated link (`!before`)
/// renders with the same `ApplyPrefix("!", ApplyInfix(...))` shape a
/// standalone negated comparison uses (the `Expr::Unary` arm above) —
/// matching rakudo's own `RakuAST::MetaInfix::Negate` is a separate,
/// pre-existing gap (`1 !before 2` already renders as `ApplyPrefix` today)
/// that this ticket does not close.
fn convert_chained_compare(
    operands: &[Expr],
    ops: &[(crate::token_kind::TokenKind, bool)],
) -> Result<RakuAstNode, RuntimeError> {
    let mut node = convert_expr(&operands[0])?;
    for (i, (op, negated)) in ops.iter().enumerate() {
        let infix = RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), node),
                node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                node_field(Some("right"), convert_expr(&operands[i + 1])?),
            ],
        };
        node = if *negated {
            RakuAstNode {
                class: RakuAstClass::ApplyPrefix,
                fields: vec![
                    node_field(
                        Some("prefix"),
                        operator_node(RakuAstClass::Prefix, &crate::token_kind::TokenKind::Bang),
                    ),
                    node_field(Some("operand"), infix),
                ],
            }
        } else {
            infix
        };
    }
    Ok(node)
}

/// True for the list-associative infixes raku renders as `ApplyListInfix`
/// (`andthen` / `orelse` / `notandthen`).
fn is_list_infix(op: &crate::token_kind::TokenKind) -> bool {
    use crate::token_kind::TokenKind;
    matches!(
        op,
        TokenKind::AndThen
            | TokenKind::OrElse
            | TokenKind::NotAndThen
            // The junction constructors and `min`/`max`. Measured against
            // rakudo 2026.07: each renders one flat `ApplyListInfix` with every
            // operand of the chain, where mutsu nests them left-associatively.
            | TokenKind::Pipe
            | TokenKind::Ampersand
            | TokenKind::Caret
    ) || matches!(op, TokenKind::Ident(name) if name == "min" || name == "max")
}

/// Flatten a left-nested same-operator chain (`a op b op c` parsed as
/// `(a op b) op c`) into a single operand list `[a, b, c]`.
fn flatten_list_infix<'a>(
    op: &crate::token_kind::TokenKind,
    left: &'a Expr,
    right: &'a Expr,
    out: &mut Vec<&'a Expr>,
) {
    if let Expr::Binary {
        left: ll,
        op: lop,
        right: lr,
    } = left
    {
        if lop == op {
            flatten_list_infix(op, ll, lr, out);
        } else {
            out.push(left);
        }
    } else {
        out.push(left);
    }
    out.push(right);
}

/// `Postfix` — a single NAMED `operator` string (e.g. `Postfix.new(operator => "++")`).
fn postfix_node(op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Postfix,
        fields: vec![leaf_field(
            Some("operator"),
            Value::str(token_kind_to_op_name(op)),
        )],
    }
}

fn convert_literal(v: &Value) -> Result<RakuAstNode, RuntimeError> {
    // `Nil` is a type object written as a bareword, not a literal value: raku
    // renders it `Type::Simple.new(Name.from-identifier("Nil"))`, exactly like
    // `Int`. mutsu's parser resolves the bareword to the value eagerly, so the
    // check has to come before the `view()` match — `Nil`'s view is not a
    // variant this function would otherwise recognize.
    if v.is_nil() {
        return Ok(simple_type_node("Nil"));
    }
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Ok(RakuAstNode {
            class: RakuAstClass::IntLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => Ok(RakuAstNode {
            class: RakuAstClass::RatLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Str(_) => Ok(quoted_string(v.clone())),
        // `True`/`False` are enum values: `Term::Enum.from-identifier('True')`.
        ValueView::Bool(b) => Ok(RakuAstNode {
            class: RakuAstClass::TermEnum,
            fields: vec![leaf_field(
                None,
                Value::str(if b { "True" } else { "False" }.to_string()),
            )],
        }),
        other => Err(unsupported(&format!("literal {other:?}"))),
    }
}

/// A string literal renders as `QuotedString.new(segments => (StrLiteral,))`.
fn quoted_string(str_value: Value) -> RakuAstNode {
    let seg = RakuAstNode {
        class: RakuAstClass::StrLiteral,
        fields: vec![leaf_field(None, str_value)],
    };
    RakuAstNode {
        class: RakuAstClass::QuotedString,
        fields: vec![RakuAstField {
            name: Some("segments"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(seg))]),
        }],
    }
}

/// The unevaluated word-list term of an enum declaration. `processors` is part
/// of the RakuAST contract: omitting it would turn `Red Green` into one enum
/// member named "Red Green" when the node is evaluated.
fn enum_quoted_string(
    variants: &[(String, Option<Expr>)],
    processor: &'static str,
) -> Result<RakuAstNode, RuntimeError> {
    if variants.iter().any(|(_, value)| value.is_some()) {
        return Err(unsupported("word-quoted enum with explicit values"));
    }
    let text = variants
        .iter()
        .map(|(name, _)| name.as_str())
        .collect::<Vec<_>>()
        .join(" ");
    let segment = RakuAstNode {
        class: RakuAstClass::StrLiteral,
        fields: vec![leaf_field(None, Value::str(text))],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(vec![
                    Value::str(processor.to_string()),
                    Value::str("val".to_string()),
                ]),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(segment))]),
            },
        ],
    })
}

/// The parenthesized pair-list term of an enum declaration.
fn enum_pair_list(variants: &[(String, Option<Expr>)]) -> Result<RakuAstNode, RuntimeError> {
    let operands = variants
        .iter()
        .map(|(name, value)| {
            let value = match value {
                Some(value) => convert_expr(value)?,
                None => RakuAstNode {
                    class: RakuAstClass::TermEnum,
                    fields: vec![leaf_field(None, Value::str("True".to_string()))],
                },
            };
            Ok(Value::rakuast(Box::new(RakuAstNode {
                class: RakuAstClass::FatArrow,
                fields: vec![
                    leaf_field(Some("key"), Value::str(name.clone())),
                    node_field(Some("value"), value),
                ],
            })))
        })
        .collect::<Result<Vec<_>, RuntimeError>>()?;
    let list = RakuAstNode {
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), plain_infix(",")),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(operands),
            },
        ],
    };
    let semilist = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(list))],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    })
}

/// A listop `say EXPR` — `Call::Name::WithoutParentheses`.
fn listop_call(name: &'static str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    call_name(name, args, true)
}

fn call_name(
    name: &str,
    args: &[Expr],
    without_parentheses: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    let arg_list = arg_list(args)?;
    let class = if without_parentheses {
        RakuAstClass::CallNameWithoutParentheses
    } else {
        RakuAstClass::CallName
    };
    let mut fields = vec![node_field(Some("name"), name_node)];
    // raku omits `args` entirely for an argument-less call, the same way
    // `control_call` below already does for a bare `return`/`last`/`next`. This
    // matters more than it looks: mutsu injects a `__mutsu_test_callsite_line`
    // named argument into listop calls, so filtering that out in `arg_list` can
    // leave an *empty* list where the source had no arguments at all.
    if !arg_list.fields.is_empty() {
        fields.push(node_field(Some("args"), arg_list));
    }
    Ok(RakuAstNode { class, fields })
}

/// A control-flow listop (`return`/`last`/`next`) — a `Call::Name` in
/// WithoutParentheses form. Unlike [`listop_call`], the `args` field is omitted
/// entirely when there are no arguments (matching raku's gist for a bare
/// `return`/`last`/`next`).
fn control_call(name: &'static str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::CallNameWithoutParentheses,
        fields,
    })
}

/// A hash literal `{a => 1, b => 2}` -> `Block(Blockoid(StatementList(
/// Statement::Expression(<pairs>))))`, where `<pairs>` is a single `FatArrow` for
/// one entry, or an `ApplyListInfix(",")` of `FatArrow`s for several. Value-less
/// keys (`{:a}`) are the boundary.
fn hash_literal_node(pairs: &[(String, Option<Expr>)]) -> Result<RakuAstNode, RuntimeError> {
    let mut fatarrows = Vec::with_capacity(pairs.len());
    for (k, v) in pairs {
        let value = v
            .as_ref()
            .ok_or_else(|| unsupported("value-less hash key"))?;
        fatarrows.push(RakuAstNode {
            class: RakuAstClass::FatArrow,
            fields: vec![
                leaf_field(Some("key"), Value::str(k.clone())),
                node_field(Some("value"), convert_expr(value)?),
            ],
        });
    }
    let inner = if fatarrows.len() == 1 {
        fatarrows.into_iter().next().unwrap()
    } else {
        let operands: Vec<Value> = fatarrows
            .into_iter()
            .map(|n| Value::rakuast(Box::new(n)))
            .collect();
        RakuAstNode {
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(Some("infix"), plain_infix(",")),
                RakuAstField {
                    name: Some("operands"),
                    value: RakuAstFieldValue::List(operands),
                },
            ],
        }
    };
    let stmt_list = RakuAstNode {
        class: RakuAstClass::StatementList,
        fields: vec![node_field(None, statement_expression(inner))],
    };
    let blockoid = RakuAstNode {
        class: RakuAstClass::Blockoid,
        fields: vec![node_field(None, stmt_list)],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![node_field(Some("body"), blockoid)],
    })
}

/// A comma list `1, 2, 3` -> `ApplyListInfix(infix => ",", operands)`.
fn comma_list_node(items: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut operands = Vec::with_capacity(items.len());
    for it in items {
        operands.push(Value::rakuast(Box::new(convert_expr(it)?)));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), plain_infix(",")),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(operands),
            },
        ],
    })
}

fn arg_list(args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::with_capacity(args.len());
    for a in args {
        // mutsu's parser attaches a `__mutsu_test_callsite_line => N` named
        // argument to every listop call so a failing `Test` assertion can report
        // the caller's line. It is instrumentation, not something the source
        // wrote, and raku's tree has no such argument — rendering it produced a
        // node that cannot exist upstream, on calls as ordinary as `f()`.
        if is_injected_named_arg(a) {
            continue;
        }
        fields.push(node_field(None, convert_expr(a)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ArgList,
        fields,
    })
}

/// Whether a call argument is one of mutsu's own injected named arguments
/// (`__`-prefixed key), rather than one the source wrote.
fn is_injected_named_arg(arg: &Expr) -> bool {
    let pair = match arg {
        Expr::PositionalPair(inner) => match inner.as_ref() {
            Expr::Grouped(g) => g.as_ref(),
            other => other,
        },
        other => other,
    };
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        ..
    } = pair
    else {
        return false;
    };
    match left.as_ref() {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => match v.view() {
            ValueView::Str(s) => is_desugar_marker(&s),
            _ => false,
        },
        _ => false,
    }
}
