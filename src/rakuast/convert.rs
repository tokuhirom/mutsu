//! Internal AST (`Stmt`/`Expr`) → RakuAST node tree (read direction, ADR-0011).
//!
//! Covered so far: the literal + say-call cluster (Phase 1); and variables,
//! plain `my` declarations, infix/prefix/postfix operators, `=` assignment, and
//! method calls (Phase 2). Constructs outside that set produce an explicit
//! `RuntimeError` (the documented coverage boundary) rather than a
//! silently-wrong node.

use super::bareword::simple_type_node;
use super::method_assign_decl::call_method;
use super::origin;
use super::placeholder::{is_placeholder_name, is_placeholder_param, placeholder_node};
use super::{
    RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, attribute, bareword, decl_traits,
    hash_literal, name_parts, routine_traits, subscript_adverb,
};
use crate::ast::{
    AssignOp, EnumVariantForm, Expr, ForMode, GivenWithKind, IMPLICIT_INVOCANT_TRAIT, ParamDef,
    Stmt, WithBlockKind,
};
use crate::compiler::helpers_ops::token_kind_to_op_name;
use crate::regex_tree::{RegexModifierKind, RegexNode, RegexTree};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) fn unsupported(what: &str) -> RuntimeError {
    RuntimeError::new(format!(
        "RakuAST: `.AST` does not yet support this construct: {what}"
    ))
}

pub(super) fn node_field(name: Option<&'static str>, node: RakuAstNode) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
    }
}

pub(super) fn leaf_field(name: Option<&'static str>, value: Value) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(value),
    }
}

/// Top-level: a parsed program becomes a `RakuAST::StatementList`.
pub(super) fn statement_list(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let _scope = bareword::DeclaredNames::collect(stmts);
    statement_list_inner(stmts)
}

fn statement_list_inner(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::new();
    // The line of the statement about to be converted: the `SetLine` marker
    // in front of it becomes the node's hidden origin (see `origin`).
    let mut line = None;
    for (at, stmt) in stmts.iter().enumerate() {
        if let Stmt::SetLine(n) = stmt {
            line = Some(*n);
            continue;
        }
        // `unit module M;` / `unit package P;`: rakudo holds the rest of the
        // unit in the declaration's body; the parser leaves it beside it.
        if let Some(unit) = unit_package_taking(stmt, &stmts[at + 1..]) {
            if let Some(mut node) = convert_stmt(&unit)? {
                if let Some(n) = line.take() {
                    node.fields.push(origin::field(n));
                }
                fields.push(node_field(None, node));
            }
            break;
        }
        // The `state` declaration the parser puts at the top of a block for
        // each bare `$` it contains: rakudo's node is the `$` itself.
        if crate::ast::anon_state::is_implicit_decl(stmt) {
            continue;
        }
        if let Some(mut node) = convert_stmt(stmt)? {
            if let Some(n) = line.take() {
                node.fields.push(origin::field(n));
            }
            fields.push(node_field(None, node));
        }
    }
    Ok(RakuAstNode {
        class: RakuAstClass::StatementList,
        fields,
    })
}

/// The `scope` a package declaration is written with: `my` for a lexical one,
/// `unit` for `unit class` / `unit module`; `our` is the default and renders
/// none.
// Cost: O(1).
pub(super) fn package_scope(is_lexical: bool, is_unit: bool) -> Option<&'static str> {
    if is_unit {
        Some("unit")
    } else if is_lexical {
        Some("my")
    } else {
        None
    }
}

/// The parser's `custom_traits` marker for an `our sub`.
pub(super) const OUR_SCOPED: &str = "__our_scoped";

/// The custom trait the parser gives an operator sub that declares its
/// precedence or associativity.
const OP_PREC_TRAIT: &str = "__prec";

/// The parser's `custom_traits` marker for a `my`-scoped declaration
/// (`my role R { }`).
pub(super) const MY_SCOPED: &str = "__my_scoped";

/// Whether `stmt` is a block-form loop or conditional the parser carries in an
/// expression (`do for ...`) as a `DoStmt`.
// Cost: O(1).
fn is_do_statement(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::For {
            is_statement_modifier,
            ..
        }
        | Stmt::If {
            is_statement_modifier,
            ..
        }
        | Stmt::Given {
            is_statement_modifier,
            ..
        } => !*is_statement_modifier,
        Stmt::While { .. } | Stmt::Loop { .. } => true,
        _ => false,
    }
}

/// Whether `stmt` is a statement carrying a statement modifier (`EXPR for LIST`,
/// `EXPR if COND`, `EXPR given TOPIC`), which a SemiList holds as a statement.
// Cost: O(1).
fn is_modifier_statement(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::For {
            is_statement_modifier: true,
            ..
        } | Stmt::If {
            is_statement_modifier: true,
            ..
        } | Stmt::Given {
            is_statement_modifier: true,
            ..
        }
    )
}

/// Convert one statement. Returns `Ok(None)` for non-semantic bookkeeping
/// statements (e.g. `SetLine`) that carry no RakuAST representation.
fn convert_stmt(stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    match stmt {
        // The `use trace` hook is bookkeeping too: rakudo models the trace as a
        // flag on the traced statement, not as a statement of its own.
        Stmt::SetLine(_) | Stmt::Trace { .. } => Ok(None),
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
            if *multi || *is_export || !export_tags.is_empty() {
                return Err(unsupported("multi / exported regex declaration"));
            }
            if params.len() != param_defs.len() {
                return Err(unsupported(
                    "regex declaration with an unnamed parameter list",
                ));
            }
            let scope = match (*is_my, *is_our) {
                (false, false) => None,
                (true, false) => Some("my"),
                (false, true) => Some("our"),
                (true, true) => return Err(unsupported("regex declaration both `my` and `our`")),
            };
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
                scope,
                param_defs,
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
            if *multi || *is_export || !export_tags.is_empty() {
                return Err(unsupported("multi / exported rule declaration"));
            }
            if params.len() != param_defs.len() {
                return Err(unsupported(
                    "rule declaration with an unnamed parameter list",
                ));
            }
            let Some(tree) = source_regex else {
                return Err(unsupported("rule declaration without a source tree"));
            };
            Ok(Some(statement_expression(regex_declaration(
                RakuAstClass::RuleDeclaration,
                &name.resolve(),
                tree,
                None,
                param_defs,
            )?)))
        }
        // A signature declaration (`my ($a, @b) = …`): the parser keeps its
        // source form as the expansion's first statement.
        Stmt::SyntheticBlock(_) if source_form(stmt).is_some() => match source_form(stmt) {
            Some(crate::ast::SourceForm::SignatureDecl(decl)) => Ok(Some(statement_expression(
                super::signature_decl::convert(decl)?,
            ))),
            Some(crate::ast::SourceForm::MethodAssignDecl(decl)) => Ok(Some(statement_expression(
                super::method_assign_decl::convert(decl)?,
            ))),
            // Only the on-demand lambda opens with a supply record.
            Some(crate::ast::SourceForm::SupplyBlock(_)) | None => Err(unsupported("source form")),
        },
        // `use` / `no` statements: `RakuAST::Pragma`, `Statement::Use` or
        // `Statement::LanguageVersion`. `:if(...)` (the `if` distribution's
        // adverb) is deferred.
        Stmt::Use {
            module,
            arg,
            tags,
            condition: None,
            ..
        } => Ok(Some(super::use_stmt::convert_use(
            module,
            arg.as_ref(),
            tags,
        )?)),
        // `need Module;` / `import Module :tag;`.
        Stmt::Need { module } => Ok(Some(super::use_stmt::convert_need(module))),
        Stmt::Import { module, tags } => Ok(Some(super::use_stmt::convert_import(module, tags))),
        Stmt::No { module, arg: None } if super::use_stmt::is_pragma_name(module) => {
            Ok(Some(super::use_stmt::convert_no(module)))
        }
        // `say 42` / `put`/`print`/`note` as listops (no parens) parse to a
        // dedicated statement; raku models them as a call in WithoutParentheses
        // form.
        // A call the parser resolved at statement level (an imported routine
        // such as `Test`'s `ok`) has `CallArg`s instead of argument
        // expressions; it is the same `Call::Name` as an expression call.
        Stmt::Call { name, args } => {
            if is_desugar_marker(name.as_str()) {
                let args = call_args_as_exprs(args)?;
                if let Some(stub) = stub_node(name.as_str(), &args) {
                    return Ok(Some(statement_expression(stub?)));
                }
                if let Some((call, value)) = method_lvalue_parts(name.as_str(), &args)
                    .or_else(|| call_lvalue_parts(name.as_str(), &args))
                {
                    return Ok(Some(statement_expression(method_lvalue_assignment(
                        convert_expr(&call)?,
                        convert_expr(value)?,
                    ))));
                }
                return Err(desugared(name.as_str()));
            }
            // `foo $obj: 1` is the method call `$obj.foo(1)`.
            if let Some(crate::ast::CallArg::Invocant(invocant)) = args.first() {
                let method = Expr::MethodCall {
                    target: Box::new(invocant.clone()),
                    name: *name,
                    args: call_args_as_exprs(&args[1..])?,
                    modifier: None,
                    quoted: false,
                };
                return Ok(Some(statement_expression(convert_expr(&method)?)));
            }
            let args = call_args_as_exprs(args)?;
            Ok(Some(statement_expression(call_name(
                name.as_str(),
                &args,
                false,
            )?)))
        }
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
        // `last FOO` / `next FOO` / `redo FOO`: the label is a `Term::Name`
        // argument of the call.
        Stmt::Last(Some(label)) => Ok(Some(statement_expression(labelled_control_call(
            "last", label,
        )))),
        Stmt::Next(Some(label)) => Ok(Some(statement_expression(labelled_control_call(
            "next", label,
        )))),
        Stmt::Redo(Some(label)) => Ok(Some(statement_expression(labelled_control_call(
            "redo", label,
        )))),
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
        // `my $x = 5 if COND`: the parser's split of the declaration and the
        // gated assignment is rakudo's one statement with a modifier.
        Stmt::SyntheticBlock(_)
            if crate::ast::decl_modifier::modified_declaration(stmt).is_some() =>
        {
            let modified =
                crate::ast::decl_modifier::modified_declaration(stmt).expect("just checked");
            convert_stmt(&modified)
        }
        // A package-like declaration with `:ver<..>` adverbs or `is export`.
        Stmt::SyntheticBlock(_) if crate::ast::package_header::unwrap(stmt).is_some() => {
            let (declaration, header) =
                crate::ast::package_header::unwrap(stmt).expect("just checked");
            let statement = convert_stmt(declaration)?
                .ok_or_else(|| unsupported("a declaration with a header"))?;
            let expression = expression_of(&statement)
                .ok_or_else(|| unsupported("a declaration with a header"))?;
            Ok(Some(statement_expression(super::package_header::apply(
                &expression,
                &header,
            )?)))
        }
        // `temp` / `let` over a variable, an element or a declaration.
        Stmt::Let { .. } => match super::temporize::convert(stmt) {
            Some(node) => Ok(Some(statement_expression(node?))),
            None => Err(unsupported("`temp`/`let` of this form")),
        },
        Stmt::SyntheticBlock(_) if crate::ast::temporize::recognize(stmt).is_some() => {
            let node = super::temporize::convert(stmt).expect("just checked")?;
            Ok(Some(statement_expression(node)))
        }
        // A sigilless declaration (`my \x = 5`, `my Int \x := $s`).
        Stmt::SyntheticBlock(_) if crate::ast::sigilless_decl::declaration(stmt).is_some() => {
            let decl = crate::ast::sigilless_decl::declaration(stmt).expect("just checked");
            Ok(Some(statement_expression(term_declaration(&decl)?)))
        }
        // A binding declaration (`my $x := …`, `my @a := …`, `my %h := …`):
        // the statement is exactly `ast::bind_decl::expand`'s form of the
        // declaration inside it.
        Stmt::VarDecl { .. } | Stmt::SyntheticBlock(_)
            if crate::ast::bind_decl::declaration(stmt).is_some() =>
        {
            let decl = crate::ast::bind_decl::declaration(stmt).expect("just checked");
            var_decl_statement(var_decl_parts(decl)?, true)
        }
        Stmt::VarDecl { .. } => var_decl_statement(var_decl_parts(stmt)?, false),
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
            // `if EXPR -> $v { }` binds the tested value; only a plain `if` chain
            // (not `with`, `unless` or a modifier) renders its pointy block.
            if binding_var.is_some()
                && (with_kind.is_some() || *is_unless || *is_statement_modifier)
            {
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
                node_field(Some("then"), clause_block_node(then_branch, binding_var)?),
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
            is_statement_modifier,
            uses_block_magic: _,
        } => {
            // `STMT for LIST` is the modified statement with a `loop-modifier`
            // of `StatementModifier::For`, not a `Statement::For` around a
            // block (measured on rakudo 2026.09). The parser's loop holds the
            // statement as its only body statement; one that was rewritten into
            // the loop's own signature stays the block form below.
            if *is_statement_modifier
                && label.is_none()
                && (**param_def).is_none()
                && params_def.is_empty()
                && !*rw_block
                && !*explicit_zero_params
                && matches!(mode, ForMode::Normal)
            {
                let mut real = body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
                if let (Some(modified), None) = (real.next(), real.next())
                    // The loop has no signature of its own beyond a bare
                    // block's placeholders (`parser::for_modifier_loop_params`).
                    && (param.clone(), params.clone())
                        == crate::parser::for_modifier_loop_params(modified)
                {
                    let mut statement = convert_stmt(modified)?
                        .ok_or_else(|| unsupported("empty for modifier body"))?;
                    statement.fields.push(node_field(
                        Some("loop-modifier"),
                        RakuAstNode {
                            class: RakuAstClass::StatementModifierFor,
                            fields: vec![node_field(None, convert_expr(iterable)?)],
                        },
                    ));
                    return Ok(Some(statement));
                }
            }
            // Implicit-topic (`for SRC { ... $_ }`, slice 6) and explicit-signature
            // (`for @a -> $x`, slice 12) forms. Hyper/race/lazy modes, `<->` rw
            // blocks, and labels carry extra RakuAST shape, deferred.
            // The explicit param names live in `param_def` / `params_def`; the
            // sigil-stripped `param` / `params` string lists are unused here.
            let _ = (param, params);
            // A single explicit param lives in `param_def`, multiple in
            // `params_def`. With none, the body is an implicit-topic Block; with
            // an explicit signature, it is a PointyBlock (matching raku).
            let single = (**param_def).as_ref();
            let explicit_defs: &[ParamDef] = match single {
                Some(pd) => std::slice::from_ref(pd),
                None => params_def,
            };
            let mut body_node = if explicit_defs.is_empty() && !*explicit_zero_params {
                topic_block_node(body)?
            } else {
                // `for @a -> { }`: a pointy block with no signature at all.
                pointy_block(explicit_defs, body, None)?
            };
            // `for @a <-> $x { }`: each parameter is a writable container
            // (`default-rw => True`, before its target).
            if *rw_block {
                mark_default_rw(&mut body_node)?;
            }
            let mode_name = match mode {
                ForMode::Normal => "serial",
                ForMode::Hyper => "hyper",
                ForMode::Race => "race",
                ForMode::Lazy => "lazy",
            };
            // Field order matches raku: labels, mode, source, body.
            let mut fields = label_fields(label);
            fields.push(leaf_field(Some("mode"), Value::str(mode_name.to_string())));
            fields.push(node_field(Some("source"), convert_expr(iterable)?));
            fields.push(node_field(Some("body"), body_node));
            let for_node = RakuAstNode {
                class: RakuAstClass::StatementFor,
                fields,
            };
            // `hyper for` / `race for` / `lazy for` are expressions: a statement
            // around the loop.
            if matches!(mode, ForMode::Normal) {
                Ok(Some(for_node))
            } else {
                Ok(Some(statement_expression(for_node)))
            }
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
        // `CONTROL { ... }` -> Statement::Control, the same exception block.
        Stmt::Control(body) => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementControl,
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
            let deferred = [
                (name_expr.is_some(), "sub with a computed name"),
                (
                    !signature_alternates.is_empty(),
                    "sub with alternate signatures",
                ),
                (
                    *is_export != !export_tags.is_empty(),
                    "sub with an untagged `is export`",
                ),
                (
                    *is_test_assertion && !custom_traits.iter().any(|(t, _)| t == "test-assertion"),
                    "sub with `is test-assertion`",
                ),
                (*supersede, "`supersede` sub"),
                (
                    custom_traits.iter().any(|(t, _)| {
                        t.starts_with("__")
                            && !is_return_spelling_marker(t)
                            && t != OUR_SCOPED
                            // The operator-precedence record the parser derives
                            // from `is assoc` / `is tighter`; lowering rebuilds it.
                            && t != OP_PREC_TRAIT
                            || crate::qualified::is_qualified_str(t)
                    }),
                    "sub with an internal or qualified trait",
                ),
            ];
            if let Some((_, what)) = deferred.iter().find(|(hit, _)| *hit) {
                return Err(unsupported(what));
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
            let flags = routine_traits::IsTraits {
                is_rw: *is_rw,
                is_raw: *is_raw,
                export_tags: export_tags.clone(),
                ..Default::default()
            }
            .with_precedence(associativity.as_ref(), precedence_trait.as_ref())?;
            routine_traits::add_flags(&mut node, *multi, false, &flags)?;
            routine_traits::add_custom(&mut node, custom_traits, !flags.nodes().is_empty())?;
            if custom_traits.iter().any(|(t, _)| t == OUR_SCOPED) {
                // `scope => "our"` leads the node; `my` is the default scope
                // and renders no field.
                node.fields
                    .insert(0, leaf_field(Some("scope"), Value::str_from("our")));
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
            is_export,
            export_tags,
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
            // forms, user traits, and delegation carry extra shape; `multi`,
            // `!private`, `is rw` and `is raw` are `rakuast::routine_traits`.
            let spelling = return_type_spelling(custom_traits)?;
            // `submethod_decl` marks every submethod `is_my` as its internal
            // "not inherited" flag, not because the source said `my` — so for a
            // submethod that flag carries no RakuAST shape of its own.
            let declared_my = *is_my && !*is_submethod;
            let deferred = [
                (name_expr.is_some(), "method with a computed name"),
                (
                    *is_export != !export_tags.is_empty(),
                    "method with a bare `is export`",
                ),
                (*is_our && *is_submethod, "`our` submethod"),
                (*our_variable_form, "`our &m = method` form"),
                (!handles.is_empty(), "method with `handles`"),
                (
                    custom_traits.iter().any(|(t, _)| {
                        t.starts_with("__") && !is_return_spelling_marker(t)
                            || crate::qualified::is_qualified_str(t)
                    }),
                    "method with an internal or qualified trait",
                ),
            ];
            if let Some((_, what)) = deferred.iter().find(|(hit, _)| *hit) {
                return Err(unsupported(what));
            }
            if return_type.is_none() && spelling != ReturnSpelling::Arrow {
                // A `__return_via_*` marker without a return type would be a
                // parser inconsistency; refuse rather than render a wrong node.
                return Err(unsupported("method with a return trait but no return type"));
            }
            let mut node = routine_node(
                if *is_submethod {
                    RakuAstClass::Submethod
                } else {
                    RakuAstClass::Method
                },
                &name.resolve(),
                param_defs,
                body,
                return_type.as_deref().map(|t| (t, spelling)),
            )?;
            let flags = routine_traits::IsTraits {
                is_rw: *is_rw,
                is_raw: *is_raw,
                export_tags: export_tags.clone(),
                ..Default::default()
            };
            routine_traits::add_flags(&mut node, *multi, *is_private, &flags)?;
            let custom = routine_traits::method_custom_traits(
                custom_traits,
                *is_default_candidate,
                deprecated_message.as_deref(),
            )?;
            routine_traits::add_custom(&mut node, &custom, !flags.nodes().is_empty())?;
            // `scope => "my"` / `"our"` leads the node, ahead of `multiness`.
            let scope = if *is_our {
                Some("our")
            } else if declared_my {
                Some("my")
            } else {
                None
            };
            if let Some(scope) = scope {
                node.fields
                    .insert(0, leaf_field(Some("scope"), Value::str_from(scope)));
            }
            Ok(Some(statement_expression(node)))
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
            parent_args,
            ..
        } => {
            if *is_grammar {
                if name_expr.is_some()
                    || *class_is_rw
                    || *is_hidden
                    || !hidden_parents.is_empty()
                    || repr.is_some()
                    || has_package_traits(custom_traits)
                {
                    return Err(unsupported(&format!(
                        "grammar with scope / traits (hidden {hidden_parents:?}, rw {class_is_rw}, \
                         traits {:?})",
                        custom_traits
                            .iter()
                            .map(|(t, _)| t.as_str())
                            .collect::<Vec<_>>()
                    )));
                }
                let mut fields = package_header_fields(
                    *name,
                    package_scope(*is_lexical, *is_unit),
                    is_colons_package(custom_traits),
                );
                // The implicit `Grammar` parent is not a written trait.
                let written: &[String] = if *implicit_grammar_parent {
                    parents.get(1..).unwrap_or(&[])
                } else {
                    parents
                };
                let traits = class_traits(written, does_parents, parent_args, false, false, &[])?;
                if !traits.is_empty() {
                    fields.push(RakuAstField {
                        name: Some("traits"),
                        value: RakuAstFieldValue::List(traits),
                    });
                }
                fields.push(node_field(
                    Some("body"),
                    block_node(&crate::parser::unhoist_nested_methods(body))?,
                ));
                let grammar = RakuAstNode {
                    class: RakuAstClass::Grammar,
                    fields,
                };
                let grammar = match super::package_header::lexical_export_tags(custom_traits) {
                    Some(tags) => super::package_header::apply(
                        &grammar,
                        &crate::ast::package_header::Header {
                            adverbs: Vec::new(),
                            export_tags: Some(tags),
                        },
                    )?,
                    None => grammar,
                };
                return Ok(Some(statement_expression(grammar)));
            }
            // `class NAME [is P] [does R] [is rw] [is repr(R)] { body }`.
            // Inheritance and `rw` are `traits`, the repr is its own leaf field.
            // A `my` class leads with `scope => "my"` (`our` is the default
            // and renders none). Unit scope, `hides`, computed names and user
            // traits carry extra RakuAST shape, deferred.
            if name_expr.is_some() || has_package_traits(custom_traits) {
                return Err(unsupported(&format!(
                    "class with inheritance / scope / repr / traits ({}{})",
                    if name_expr.is_some() {
                        "computed name "
                    } else {
                        ""
                    },
                    custom_traits
                        .iter()
                        .map(|(t, _)| t.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                )));
            }
            let mut fields = package_header_fields(
                *name,
                package_scope(*is_lexical, *is_unit),
                is_colons_package(custom_traits),
            );
            // Field order matches raku: scope, name, repr, traits, body.
            if let Some(r) = repr {
                fields.push(leaf_field(Some("repr"), Value::str(r.clone())));
            }
            let mut traits = class_traits(
                parents,
                does_parents,
                parent_args,
                *class_is_rw,
                *is_hidden,
                hidden_parents,
            )?;
            traits.extend(decl_traits::class_custom_traits(custom_traits)?);
            if !traits.is_empty() {
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(traits),
                });
            }
            fields.push(node_field(
                Some("body"),
                block_node(&crate::parser::unhoist_nested_methods(
                    without_composed_header(body, does_parents),
                ))?,
            ));
            let class = RakuAstNode {
                class: RakuAstClass::Class,
                fields,
            };
            // A lexical `my class ... is export` keeps its tags in a marker.
            let class = match super::package_header::lexical_export_tags(custom_traits) {
                Some(tags) => super::package_header::apply(
                    &class,
                    &crate::ast::package_header::Header {
                        adverbs: Vec::new(),
                        export_tags: Some(tags),
                    },
                )?,
                None => class,
            };
            Ok(Some(statement_expression(class)))
        }
        // `trusts B;` in a class body is a `Statement::Trusts` of its own, not an
        // expression statement.
        Stmt::TrustsDecl { name } => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementTrusts,
            fields: vec![node_field(Some("type"), build_type_node(&name.resolve())?)],
        })),
        // `augment class C { ... }` is a `Class` with `scope => "augment"`.
        Stmt::AugmentClass {
            name,
            body,
            does_roles,
            is_role: false,
        } => {
            let mut fields = vec![
                leaf_field(Some("scope"), Value::str_from("augment")),
                node_field(Some("name"), name_from_identifier(&name.resolve())),
            ];
            if !does_roles.is_empty() {
                let mut traits = Vec::new();
                for role in does_roles {
                    traits.push(Value::rakuast(Box::new(RakuAstNode {
                        class: RakuAstClass::TraitDoes,
                        fields: vec![node_field(None, build_type_node(&role.resolve())?)],
                    })));
                }
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(traits),
                });
            }
            fields.push(node_field(
                Some("body"),
                block_node(&crate::parser::unhoist_nested_methods(body))?,
            ));
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
            if base_type.is_some()
                || matches!(variant_form, EnumVariantForm::Computed)
                || variants.is_empty()
            {
                return Err(unsupported(&format!(
                    "enum with scope / traits / computed body (base {base_type:?}, roles {roles:?}, form {variant_form:?}, {} variants)",
                    variants.len()
                )));
            }
            let term = match variant_form {
                EnumVariantForm::Words => enum_quoted_string(variants, "words")?,
                EnumVariantForm::QuoteWords => enum_quoted_string(variants, "quotewords")?,
                EnumVariantForm::PairList => enum_pair_list(variants)?,
                EnumVariantForm::Computed => unreachable!("checked above"),
            };
            // Field order matches raku: scope, name, traits, term.
            let mut fields: Vec<RakuAstField> = super::package_header::my_scope_field(*is_my)
                .into_iter()
                .collect();
            fields.push(node_field(
                Some("name"),
                name_from_identifier(&name.resolve()),
            ));
            let mut traits = Vec::new();
            for role in roles {
                traits.push(Value::rakuast(Box::new(RakuAstNode {
                    class: RakuAstClass::TraitDoes,
                    fields: vec![node_field(None, build_type_node(role)?)],
                })));
            }
            traits.extend(super::package_header::export_trait_value(
                *is_export || !export_tags.is_empty(),
                export_tags,
            ));
            if !traits.is_empty() {
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(traits),
                });
            }
            fields.push(node_field(Some("term"), term));
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::TypeEnum,
                fields,
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
            let class = match kind {
                crate::ast::PackageKind::Module => RakuAstClass::Module,
                crate::ast::PackageKind::Package => RakuAstClass::Package,
                crate::ast::PackageKind::Grammar => {
                    return Err(unsupported("grammar declaration"));
                }
            };
            let mut fields: Vec<RakuAstField> = package_scope(*is_my, *is_unit)
                .map(|scope| leaf_field(Some("scope"), Value::str_from(scope)))
                .into_iter()
                .collect();
            fields.push(node_field(
                Some("name"),
                name_from_identifier(&name.resolve()),
            ));
            fields.push(node_field(
                Some("body"),
                block_node(&crate::parser::unhoist_nested_methods(body))?,
            ));
            Ok(Some(statement_expression(RakuAstNode { class, fields })))
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
            let mut fields: Vec<RakuAstField> = super::package_header::my_scope_field(*is_my)
                .into_iter()
                .collect();
            fields.push(node_field(
                Some("name"),
                name_from_identifier(&name.resolve()),
            ));
            // Field order matches raku: name, where, traits.
            if let Some(pred) = predicate {
                fields.push(node_field(Some("where"), convert_expr(pred)?));
            }
            // `is export` comes before the `of` base type in the traits.
            let mut traits: Vec<Value> = super::package_header::export_trait_value(
                *is_export || !export_tags.is_empty(),
                export_tags,
            )
            .into_iter()
            .collect();
            if *base_is_explicit {
                traits.push(Value::rakuast(Box::new(RakuAstNode {
                    class: RakuAstClass::TraitOf,
                    fields: vec![node_field(None, build_type_node(base)?)],
                })));
            }
            if !traits.is_empty() {
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(traits),
                });
            }
            Ok(Some(statement_expression(RakuAstNode {
                class: RakuAstClass::TypeSubset,
                fields,
            })))
        }
        Stmt::ProtoDecl {
            name,
            param_defs,
            return_type,
            body,
            is_export,
            export_tags,
            custom_traits,
            trait_args,
            is_method,
            is_our,
            ..
        } => Ok(Some(statement_expression(super::proto::convert(
            super::proto::ProtoDecl {
                name: *name,
                param_defs,
                return_type: return_type.as_deref(),
                body,
                is_export: *is_export,
                export_tags,
                has_traits: custom_traits.iter().any(|t| !is_return_spelling_marker(t))
                    || !trait_args.is_empty(),
                spelling: return_type_spelling(
                    &custom_traits
                        .iter()
                        .map(|t| (t.clone(), None))
                        .collect::<Vec<_>>(),
                )?,
                is_method: *is_method,
                is_our: *is_our,
            },
        )?))),
        Stmt::React { body, blorst } => Ok(Some(statement_expression(
            super::react::convert_react(body, *blorst)?,
        ))),
        Stmt::Whenever {
            supply,
            params,
            param_defs,
            body,
        } => Ok(Some(super::react::convert_whenever(
            supply, params, param_defs, body,
        )?)),
        Stmt::ReactDone => Ok(Some(statement_expression(super::react::convert_done()))),
        // `also does R;` in a package body.
        Stmt::DoesDecl {
            name,
            also: true,
            from_is: false,
            ..
        } => Ok(Some(super::role::also_statement(name.resolve().as_str())?)),
        Stmt::RoleDecl {
            name,
            type_params,
            type_param_defs,
            is_export,
            export_tags,
            body,
            is_rw,
            custom_traits,
            ..
        } => Ok(Some(statement_expression(super::role::convert(
            super::role::RoleDecl {
                name: *name,
                type_params,
                type_param_defs,
                is_export: *is_export,
                export_tags,
                body,
                is_rw: *is_rw,
                custom_traits,
            },
        )?))),
        Stmt::HasDecl {
            name,
            is_public,
            default,
            handles,
            is_rw,
            is_readonly,
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
            is_built,
            default_is_seed,
            default_is_trait,
            handles_terms,
            trait_order,
            ..
        } => {
            // A `has [Type] $.x` attribute -> a `VarDeclaration::Simple` with
            // `scope => "has"` and a `twigil` (`.` public accessor / `!`
            // private; none for the alias `has $x`). `our $.x` has the `our`
            // scope, `my $.x` none (the default). An explicit `= EXPR` default
            // is both the implicit `Trait::WillBuild` and the `initializer`; a
            // typed attribute (`has Int $.z`) carries an *implicit*
            // `BareWord(<TypeName>)` default that is no default at all. The
            // written traits come in written order (`rakuast::attribute`); a
            // `:D` / `:U` smiley is the type's `Type::Definedness`; a `where`
            // is the last field.
            let explicit_default = default
                .as_ref()
                .filter(|_| !*default_is_seed && !*default_is_trait);
            let smiley_type = match (type_constraint, type_smiley.as_deref()) {
                (_, None) => None,
                (Some(base), Some(smiley @ ("D" | "U"))) => Some(format!("{base}:{smiley}")),
                _ => return Err(unsupported("attribute with a `:_` smiley")),
            };
            if !handles.is_empty() && handles_terms.is_empty() {
                return Err(unsupported(
                    "attribute with a `handles` spelling not kept as a term",
                ));
            }
            let type_name = smiley_type.as_deref().or(type_constraint.as_deref());
            let twigil = match (*is_alias, *is_public) {
                (true, _) => None,
                (false, true) => Some("."),
                (false, false) => Some("!"),
            };
            let scope = match (*is_our, *is_my) {
                (true, _) => Some("our"),
                (false, true) => None,
                (false, false) => Some("has"),
            };
            let full_name = if *sigil == '$' {
                name.resolve()
            } else {
                format!("{}{}", sigil, name.resolve())
            };
            let mut decl = var_declaration(
                &full_name,
                explicit_default.map(Initializer::Assign),
                scope,
                type_name,
                twigil,
                explicit_default,
            )?;
            attribute::add_traits(
                &mut decl,
                &attribute::AttributeTraits {
                    order: trait_order.clone(),
                    is_rw: *is_rw,
                    is_readonly: *is_readonly,
                    is_required: is_required.clone(),
                    is_default: is_default.clone(),
                    is_built: *is_built,
                    deprecated_message: deprecated_message.clone(),
                    is_type: is_type.clone(),
                    unknown_traits: unknown_traits.clone(),
                    handles_terms: handles_terms.clone(),
                    handles: handles.clone(),
                },
            )?;
            if let Some(constraint) = where_constraint {
                decl.fields
                    .push(node_field(Some("where"), convert_expr(constraint)?));
            }
            Ok(Some(statement_expression(decl)))
        }
        // `v = EXPR` where `v` is a sigilless term: rakudo's left side is the
        // `Term::Name`, and the assignment is a list assignment (no `:item`).
        Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
            target_is_sigilless: true,
        } => Ok(Some(statement_expression(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(
                    Some("left"),
                    RakuAstNode {
                        class: RakuAstClass::TermName,
                        fields: vec![node_field(None, name_from_identifier(name))],
                    },
                ),
                node_field(
                    Some("infix"),
                    RakuAstNode {
                        class: RakuAstClass::Assignment,
                        fields: Vec::new(),
                    },
                ),
                node_field(Some("right"), convert_expr(expr)?),
            ],
        }))),
        Stmt::Assign { name, expr, op, .. } => match op {
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

/// `$x = EXPR` -> `ApplyInfix(left => Var::Lexical, infix => Assignment, right)`.
/// The `Assignment` node carries `:item` for scalar (`$`) targets; the list form
/// (`@`/`%`) has no adverb.
fn assignment_infix(name: &str, rhs: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    assignment_around(var_lexical(sigil, desigil), sigil == "$", rhs)
}

/// `LEFT = EXPR` over an already converted left side; `is_item` marks the
/// `Assignment` node `:item`, which rakudo does for a scalar target.
pub(super) fn assignment_around(
    left: RakuAstNode,
    is_item: bool,
    rhs: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let assignment = RakuAstNode {
        class: RakuAstClass::Assignment,
        fields: if is_item {
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
            node_field(Some("left"), left),
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

/// `$o.attr = EXPR` parses to the internal `__mutsu_assign_method_lvalue(target,
/// "attr", [args], value, var-name)` writeback call. Rakudo models it as a plain
/// assignment whose left side is the method call: this returns that method call
/// and the assigned value. `None` when `name` is another marker or the method
/// name is not a literal (a dynamic `$o."$n"() = v` stays the boundary).
fn method_lvalue_parts<'a>(name: &str, args: &'a [Expr]) -> Option<(Expr, &'a Expr)> {
    if name != "__mutsu_assign_method_lvalue" {
        return None;
    }
    // Only the five-argument record `parser::assign_to_target_expr` builds for
    // `CALL = value` renders: lowering the `ApplyInfix` hands the method call
    // back to that function, so a record it would not rebuild (the compound
    // forms' six-argument writeback, a topic write-back name) stays the
    // boundary.
    let [
        target,
        method,
        Expr::ArrayLiteral(method_args),
        value,
        write_back,
    ] = args
    else {
        return None;
    };
    let (Expr::Literal(method) | Expr::LiteralSrc(method, _)) = method else {
        return None;
    };
    let ValueView::Str(method) = method.view() else {
        return None;
    };
    let expected_write_back = crate::parser::method_lvalue_target_name(target);
    let write_back = match write_back {
        Expr::Literal(v) => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ if v.is_nil() => None,
            _ => return None,
        },
        _ => return None,
    };
    if write_back != expected_write_back {
        return None;
    }
    // `$(EXPR) = v` and `$o.AT-POS(i) = v` are assignments the parser routes
    // elsewhere; a record of them did not come from that function.
    let is_rerouted = (method.as_str() == "item" && method_args.is_empty())
        || (method.as_str() == "AT-POS"
            && method_args.len() == 1
            && !matches!(target, Expr::Var(n) | Expr::BareWord(n) if n == "self"));
    if is_rerouted {
        return None;
    }
    let (modifier, method) = match method.strip_prefix('!') {
        Some(private) => (Some('!'), private),
        None => (None, &method[..]),
    };
    let call = Expr::MethodCall {
        target: Box::new(target.clone()),
        name: crate::symbol::Symbol::intern(method),
        args: method_args.clone(),
        modifier,
        quoted: false,
    };
    Some((call, value))
}

/// A yada-yada stub (`...`, `!!!`, `???`) -> `Stub::Fail` / `Die` / `Warn`,
/// carrying an `args` list only when the source wrote a message; `None` for
/// any other call.
// Cost: O(n), n = size of the message.
fn stub_node(name: &str, args: &[Expr]) -> Option<Result<RakuAstNode, RuntimeError>> {
    use crate::ast::stub;
    let class = match name {
        stub::FAIL => RakuAstClass::StubFail,
        stub::DIE => RakuAstClass::StubDie,
        stub::WARN => RakuAstClass::StubWarn,
        _ => return None,
    };
    if args.is_empty() {
        return Some(Ok(RakuAstNode {
            class,
            fields: Vec::new(),
        }));
    }
    Some(arg_list(args).map(|list| RakuAstNode {
        class,
        fields: vec![node_field(Some("args"), list)],
    }))
}

/// The bound value of an indexed bind's `__mutsu_bind_index_value(rhs, meta)`
/// marker, or `None` for any other value. The marker's source metadata is
/// derived from `rhs`, so lowering rebuilds it.
// Cost: O(1).
fn index_bind_rhs(value: &Expr) -> Option<&Expr> {
    match value {
        Expr::Call { name, args } if name.as_str() == "__mutsu_bind_index_value" => {
            match args.as_slice() {
                [rhs, _] => Some(rhs),
                _ => None,
            }
        }
        _ => None,
    }
}

/// `ApplyInfix(left, Assignment, right)` over already converted operands.
/// `f(ARGS) = EXPR` and `$c(ARGS) = EXPR` parse to the internal
/// `__mutsu_assign_named_sub_lvalue("f", [ARGS], value)` /
/// `__mutsu_assign_callable_lvalue($c, [ARGS], value)` writeback calls. Rakudo
/// models both as a plain assignment whose left side is the call: this
/// returns that call and the assigned value. Lowering hands the call back to
/// `parser::assign_to_target_expr`, so only the record that function builds
/// renders: a plain routine name (`postcircumfix:<[ ]>` and its kin become
/// subscript assignments there), and an invocant variable for the callable
/// form (the compound forms' `do`-statement and internal-call targets stay
/// the boundary).
// Cost: O(1).
fn call_lvalue_parts<'a>(name: &str, args: &'a [Expr]) -> Option<(Expr, &'a Expr)> {
    let [target, Expr::ArrayLiteral(call_args), value] = args else {
        return None;
    };
    let call = match name {
        "__mutsu_assign_named_sub_lvalue" => {
            let (Expr::Literal(routine) | Expr::LiteralSrc(routine, _)) = target else {
                return None;
            };
            let ValueView::Str(routine) = routine.view() else {
                return None;
            };
            let plain = routine
                .chars()
                .next()
                .is_some_and(|c| c.is_alphabetic() || c == '_')
                && routine
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''));
            if !plain || is_desugar_marker(&routine) {
                return None;
            }
            Expr::Call {
                name: crate::symbol::Symbol::intern(&routine),
                args: call_args.clone(),
            }
        }
        // `(LVALUES) = rhs` assigns to a parenthesised list
        // (`parser::paren_list_assign_expr`).
        "__mutsu_assign_callable_lvalue"
            if call_args.is_empty() && matches!(target, Expr::ArrayLiteral(_)) =>
        {
            Expr::Grouped(Box::new(target.clone()))
        }
        "__mutsu_assign_callable_lvalue" => {
            if !matches!(target, Expr::Var(_) | Expr::CodeVar(_)) {
                return None;
            }
            Expr::CallOn {
                target: Box::new(target.clone()),
                args: call_args.clone(),
            }
        }
        _ => return None,
    };
    Some((call, value))
}

fn method_lvalue_assignment(left: RakuAstNode, right: RakuAstNode) -> RakuAstNode {
    let assignment = RakuAstNode {
        class: RakuAstClass::Assignment,
        fields: vec![],
    };
    RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), left),
            node_field(Some("infix"), assignment),
            node_field(Some("right"), right),
        ],
    }
}

/// A plain `Infix.new("<op>")` node from a literal operator string.
pub(super) fn plain_infix(op: &str) -> RakuAstNode {
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
    compound_assignment_with_left(convert_expr(target)?, op, rhs)
}

/// [`compound_assignment_infix`] over an already converted left side.
pub(super) fn compound_assignment_with_left(
    left: RakuAstNode,
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
            node_field(Some("left"), left),
            node_field(Some("infix"), meta_assign),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

/// `$x .= meth(args)` -> `ApplyDottyInfix(left, DottyInfix::CallAssign,
/// Call::Method)`. The marker's `rhs` is the method call applied to the target;
/// only its name, arguments and dispatch modifier are rendered.
// Cost: O(n), n = size of the target and the arguments.
fn dotty_assignment(target: &Expr, call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    dotty_assignment_with_left(convert_expr(target)?, call)
}

/// [`dotty_assignment`] over an already converted left side.
// Cost: O(n), n = size of the arguments.
pub(super) fn dotty_assignment_with_left(
    left: RakuAstNode,
    call: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let Some(crate::ast::dotty_assign::DottyCall {
        name,
        args,
        modifier,
        quoted,
    }) = crate::ast::dotty_assign::method_call(call)
    else {
        return Err(unsupported("`.=` with a call that is not a method call"));
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyDottyInfix,
        fields: vec![
            node_field(Some("left"), left),
            node_field(
                Some("infix"),
                RakuAstNode {
                    class: RakuAstClass::DottyInfixCallAssign,
                    fields: Vec::new(),
                },
            ),
            node_field(
                Some("right"),
                method_call_postfix(name, args, modifier, quoted)?,
            ),
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

/// A `my`/`our`/`state` declaration with an optional simple type, as a
/// `VarDeclaration::Simple` statement; `is_binding` renders its right-hand side
/// as an `Initializer::Bind` (`:=`) rather than an `Initializer::Assign`.
fn var_decl_statement(
    parts: VarDeclParts<'_>,
    is_binding: bool,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let VarDeclParts {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        custom_traits,
        where_constraint,
    } = parts;
    // Real `is`/`does` traits carry richer shape, deferred.
    // `constant X = 5` is a distinct raku node, not a scoped `my`.
    // mutsu marks it with a `__constant` pseudo-trait (plus a
    // `__constant_sigil` recording the declared sigil) and sets
    // `is_our` for the package-scoped default spelling.
    if !is_binding && custom_traits.iter().any(|(n, _)| n == "__constant") {
        return constant_declaration(name, expr, custom_traits, type_constraint, is_our);
    }
    let is_internal = |n: &str| {
        n == "__has_initializer"
            // The parser's own mark of a declaration whose initializer reads
            // the new binding; the lowering marks it again.
            || n == "__init_sees_self"
            || n == crate::ast::shaped_decl::SHAPED_DECL
            || n == crate::ast::keyed_hash::IMPLICIT_VALUE_TYPE
            || (is_binding && n == crate::ast::bind_decl::SCALAR_BIND)
    };
    let unrendered: Vec<&str> = custom_traits
        .iter()
        .filter(|(n, arg)| !is_internal(n) && !decl_traits::is_rendered(n, arg))
        .map(|(n, _)| n.as_str())
        .collect();
    if !unrendered.is_empty() {
        return Err(unsupported(&format!(
            "declaration with traits ({})",
            unrendered.join(", ")
        )));
    }
    // build_type_node validates simple/definite and defers the rest. A
    // key-typed hash (`my Int %h{Str}`) splits into its value `type` and a
    // `shape`.
    let keyed = super::keyed_hash::split(name, type_constraint.as_deref(), custom_traits);
    let type_name = match keyed {
        Some((value, _)) => value,
        None => type_constraint.as_deref(),
    };
    let scope = if is_our {
        Some("our")
    } else if is_state {
        Some("state")
    } else {
        None
    };
    // A shaped array (`my @a[2;3] = ...`): the parser's `Array.new(shape =>
    // ..., data => ...)` initializer is the node's `shape` and its initializer.
    let shaped = if name.starts_with('@') && !is_binding {
        crate::ast::shaped_decl::split(expr)
    } else {
        None
    };
    let expr = match &shaped {
        Some((_, Some(data))) => data,
        _ => expr,
    };
    // `my @a <== EXPR` is a declaration whose initializer is the parser's
    // feed helper call; without the `__has_initializer` mark it would read as
    // none and the feed would be dropped.
    let unmarked = !is_binding && !custom_traits.iter().any(|(n, _)| n == "__has_initializer");
    if unmarked
        && let Expr::Call { name: callee, .. } = expr
        && is_desugar_marker(callee.as_str())
    {
        return Err(desugared(callee.as_str()));
    }
    // The same for a closure: `my &a := { ... }` is bound, and the parser leaves
    // no mark of it on a `&` declaration.
    if unmarked
        && matches!(
            expr,
            Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Lambda { .. }
        )
    {
        return Err(unsupported("`&` declaration bound to a block"));
    }
    let init = if is_binding {
        Some(Initializer::Bind(expr))
    } else if shaped.as_ref().is_some_and(|(_, data)| data.is_none()) {
        None
    } else {
        custom_traits
            .iter()
            .any(|(name, _)| name == "__has_initializer")
            .then_some(Initializer::Assign(expr))
    };
    // A dynamic `my $*x` is named `*x` (`@*a` / `%*h` keep their sigil in
    // front); raku renders the `*` as the declaration's `twigil`.
    // `my $x is dynamic` keeps its plain name and renders the trait instead.
    let twigil_dynamic = is_dynamic && name.trim_start_matches(['@', '%', '&']).starts_with('*');
    let dynamic_name;
    let (name, twigil) = if twigil_dynamic {
        dynamic_name = name.replacen('*', "", 1);
        (dynamic_name.as_str(), Some("*"))
    } else {
        (name, None)
    };
    let mut decl = var_declaration(name, init, scope, type_name, twigil, None)?;
    if let Some((_, key)) = keyed {
        super::keyed_hash::insert_shape(&mut decl, key)?;
    }
    if let Some((dims, _)) = &shaped {
        super::keyed_hash::insert_dimensions(&mut decl, dims)?;
    }
    let mut traits = decl_traits::convert(custom_traits)?;
    if is_dynamic && !twigil_dynamic {
        // The parser keeps `is dynamic` as a flag, not in source order among
        // the other traits, so only a lone one renders faithfully.
        if !traits.is_empty() {
            return Err(unsupported("`is dynamic` beside other traits"));
        }
        traits.push(decl_traits::dynamic_trait());
    }
    decl_traits::insert(&mut decl, traits);
    // `where` is the declaration's last field, after the initializer.
    if let Some(constraint) = where_constraint {
        decl.fields
            .push(node_field(Some("where"), convert_expr(constraint)?));
    }
    Ok(Some(statement_expression(decl)))
}

/// The fields of a `Stmt::VarDecl` that a `VarDeclaration::Simple` renders.
/// Taken apart in one place so the rendering function is not one more step of
/// the converter's recursion over `Stmt` (`make check-ast-walkers`).
struct VarDeclParts<'a> {
    name: &'a str,
    expr: &'a Expr,
    type_constraint: &'a Option<String>,
    is_state: bool,
    is_our: bool,
    is_dynamic: bool,
    custom_traits: &'a [(String, Option<Expr>)],
    /// The declaration's `where` expression.
    where_constraint: Option<&'a Expr>,
}

fn var_decl_parts(stmt: &Stmt) -> Result<VarDeclParts<'_>, RuntimeError> {
    let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        custom_traits,
        where_constraint,
        ..
    } = stmt
    else {
        return Err(unsupported("variable declaration"));
    };
    Ok(VarDeclParts {
        name,
        expr,
        type_constraint,
        is_state: *is_state,
        is_our: *is_our,
        is_dynamic: *is_dynamic,
        custom_traits,
        where_constraint: where_constraint.as_deref(),
    })
}

/// A declaration's initializer: `= EXPR` or `:= EXPR`.
#[derive(Clone, Copy)]
pub(super) enum Initializer<'a> {
    Assign(&'a Expr),
    Bind(&'a Expr),
    CallAssign(&'a crate::ast::method_assign_decl::MethodAssignDecl),
}

/// `my $x` / `my @a` / `my $x = EXPR` -> `VarDeclaration::Simple`. The sigil is
/// implicit (`$`) when mutsu already stripped it from the name; otherwise the
/// name carries its `@`/`%`/`&` sigil.
pub(super) fn var_declaration(
    name: &str,
    init: Option<Initializer<'_>>,
    scope: Option<&'static str>,
    type_name: Option<&str>,
    twigil: Option<&str>,
    will_build: Option<&Expr>,
) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    // `state $ = 0`: the parser names the anonymous scalar `__ANON_STATE__`;
    // rakudo has no name.
    if sigil == "$"
        && (desigil == "__ANON_STATE__" || crate::ast::anon_state::is_scalar(desigil))
        && scope == Some("state")
        && type_name.is_none()
        && twigil.is_none()
        && will_build.is_none()
    {
        let initializer = match init {
            None => None,
            Some(Initializer::Assign(e)) => Some(RakuAstNode {
                class: RakuAstClass::InitializerAssign,
                fields: vec![node_field(None, convert_expr(e)?)],
            }),
            Some(_) => return Err(unsupported("anonymous state variable binding")),
        };
        return Ok(anonymous_declaration("$", initializer));
    }
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
    if let Some(init) = init {
        let initializer = match init {
            Initializer::Assign(e) => RakuAstNode {
                class: RakuAstClass::InitializerAssign,
                fields: vec![node_field(None, convert_expr(e)?)],
            },
            Initializer::Bind(e) => RakuAstNode {
                class: RakuAstClass::InitializerBind,
                fields: vec![node_field(None, convert_expr(e)?)],
            },
            Initializer::CallAssign(form) => RakuAstNode {
                class: RakuAstClass::InitializerCallAssign,
                fields: vec![node_field(
                    None,
                    call_method(&form.method.resolve(), &form.args, None)?,
                )],
            },
        };
        fields.push(node_field(Some("initializer"), initializer));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::VarDeclarationSimple,
        fields,
    })
}

/// The `Name` for an identifier. A `::`-qualified one (`A::B`) is stored as
/// simple name parts, which Rakudo renders as
/// `Name.from-identifier-parts("A","B")` — for a declaration's name, a type,
/// a call or a regex subrule alike (measured on 2026.09); retaining one opaque
/// `A::B` string would lose observable RakuAST structure. Anything else,
/// including an operator name that merely contains `::` (`infix:<::=>`),
/// stays one `Name.from-identifier("<s>")` string.
pub(super) fn name_from_identifier(s: &str) -> RakuAstNode {
    if name_parts::is_qualified_identifier(s) && !s.starts_with("::?") {
        return name_parts::qualified_name(s);
    }
    RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(s.to_string()))],
    }
}

/// True when a type constraint is a plain (possibly `::`-qualified) identifier
/// (`Int`, `My::Type`) that maps to `Type::Simple`. Parameterised (`Array[Int]`)
/// and coercion (`Str()`) types carry richer RakuAST shape, deferred — so each
/// `::`-separated segment must be a bare identifier.
pub(super) fn is_simple_type(t: &str) -> bool {
    is_pseudo_type(t)
        || !t.is_empty()
            && name_parts::identifier_segments(t).all(|seg| {
                !seg.is_empty() && seg.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
            })
}

/// Build the `type => ...` RakuAST node for a mutsu type-constraint string.
/// A plain identifier -> `Type::Simple`; a `:D`/`:U` definiteness smiley ->
/// `Type::Definedness`; a `Base[Arg, ...]` -> `Type::Parameterized`. Coercion
/// (`Str()`) and `:_` types defer.
pub(super) fn build_type_node(t: &str) -> Result<RakuAstNode, RuntimeError> {
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
    if t.find('[')
        .is_some_and(|bracket| t.find('(').is_none_or(|paren| bracket < paren))
    {
        return super::type_args::parameterized_type_node(t, None);
    }
    // `Int()` coercion -> Type::Coercion(base-type); `Int(Cool)` adds the
    // `constraint` the value is coerced from.
    if let Some(open) = t.find('(')
        && let Some(inner) = t[open + 1..].strip_suffix(')')
    {
        let base = &t[..open];
        if !is_simple_type(base) {
            return Err(unsupported("coercion type over a non-simple base"));
        }
        let mut fields = vec![node_field(Some("base-type"), simple_type_node(base))];
        if !inner.is_empty() {
            fields.push(node_field(Some("constraint"), build_type_node(inner)?));
        }
        return Ok(RakuAstNode {
            class: RakuAstClass::TypeCoercion,
            fields,
        });
    }
    if is_simple_type(t) {
        return Ok(simple_type_node(t));
    }
    Err(unsupported(&format!("type `{t}`")))
}

/// `my \x = 5` / `my Int \x := $s` -> `VarDeclaration::Term(type?, name,
/// initializer)`. A scoped one (`our \x`, `state \x`) stays the boundary.
// Cost: O(n), n = size of the initializer.
fn term_declaration(
    decl: &crate::ast::sigilless_decl::SigillessDecl<'_>,
) -> Result<RakuAstNode, RuntimeError> {
    if decl.is_our || decl.is_state {
        return Err(unsupported("scoped sigilless declaration"));
    }
    let mut fields = Vec::new();
    if let Some(type_name) = decl.type_constraint {
        fields.push(node_field(Some("type"), build_type_node(type_name)?));
    }
    fields.push(node_field(Some("name"), name_from_identifier(decl.name)));
    fields.push(node_field(
        Some("initializer"),
        RakuAstNode {
            class: if decl.assigned {
                RakuAstClass::InitializerAssign
            } else {
                RakuAstClass::InitializerBind
            },
            fields: vec![node_field(None, convert_expr(decl.expr)?)],
        },
    ));
    Ok(RakuAstNode {
        class: RakuAstClass::VarDeclarationTerm,
        fields,
    })
}

/// Split a declaration name into `(sigil, desigilname)`. mutsu keeps the sigil
/// on `@`/`%`/`&` declarations but strips it from `$` ones.
pub(super) fn split_sigil(name: &str) -> (&str, &str) {
    match name.as_bytes().first() {
        Some(b'@') => ("@", &name[1..]),
        Some(b'%') => ("%", &name[1..]),
        Some(b'&') => ("&", &name[1..]),
        _ => ("$", name),
    }
}

pub(super) fn statement_expression(expr: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::StatementExpression,
        fields: vec![node_field(Some("expression"), expr)],
    }
}

/// The leading fields of a package declaration node (`class`, `grammar`,
/// `role`): its `scope`, then its `name`.
///
/// A declaration with no source name (`class { }`) is registered under an
/// internal `__ANON_*__` name, which rakudo has no counterpart for: its node
/// simply has no `name`. The empty name `class :: { }` is told apart by the
/// parser's marker (`colons`); rakudo gives it an `anon` scope over the name
/// `::`.
// Cost: O(k), k = length of the name.
pub(super) fn package_header_fields(
    name: crate::symbol::Symbol,
    scope: Option<&'static str>,
    colons: bool,
) -> Vec<RakuAstField> {
    if colons {
        let mut fields = vec![leaf_field(Some("scope"), Value::str_from("anon"))];
        fields.extend(name_parts::stash_name("").map(|n| node_field(Some("name"), n)));
        return fields;
    }
    let mut fields: Vec<RakuAstField> = scope
        .map(|scope| leaf_field(Some("scope"), Value::str_from(scope)))
        .into_iter()
        .collect();
    let name = name.resolve();
    if !crate::value::is_internal_anon_type_name(&name) {
        fields.push(node_field(Some("name"), name_from_identifier(&name)));
    }
    fields
}

/// Whether a package declaration's `custom_traits` hold anything but the
/// parser's empty-name marker and its lexical `is export` marker, which the
/// node expresses itself.
pub(super) fn has_package_traits(custom_traits: &[(String, Option<Expr>)]) -> bool {
    custom_traits.iter().any(|(t, _)| {
        t != crate::parser::ANON_COLONS_TRAIT
            && t != crate::parser::EXPORT_TYPE_MARKER
            && !decl_traits::is_class_trait(t)
    })
}

/// The `expression` of a `Statement::Expression` node.
fn expression_of(statement: &RakuAstNode) -> Option<RakuAstNode> {
    statement
        .fields
        .iter()
        .find(|f| f.name == Some("expression"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(value) => match value.view() {
                ValueView::RakuAst(node) => Some(node.clone()),
                _ => None,
            },
            _ => None,
        })
}

/// Whether a package declaration was written with the empty name `::`.
pub(super) fn is_colons_package(custom_traits: &[(String, Option<Expr>)]) -> bool {
    custom_traits
        .iter()
        .any(|(t, _)| t == crate::parser::ANON_COLONS_TRAIT)
}

/// `TARGET[INDEX]` / `TARGET{INDEX}` as `ApplyPostfix(operand, Postcircumfix::*Index)`,
/// with the assigned value as the postcircumfix's `assignee` when there is one.
pub(super) fn subscript_node(
    target: &Expr,
    index: &Expr,
    is_positional: bool,
    assignee: Option<&Expr>,
    colonpairs: Vec<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    subscript_dims_node(
        target,
        std::slice::from_ref(index),
        is_positional,
        assignee,
        colonpairs,
    )
}

/// [`subscript_node`] over the dimensions of `@a[0;1]`: one statement of the
/// `SemiList` per dimension.
// Cost: O(n), n = nodes of the target and dimensions.
pub(super) fn subscript_dims_node(
    target: &Expr,
    dims: &[Expr],
    is_positional: bool,
    assignee: Option<&Expr>,
    colonpairs: Vec<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut statements = Vec::with_capacity(dims.len());
    for dim in dims {
        statements.push(node_field(None, statement_expression(convert_expr(dim)?)));
    }
    let semilist = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: statements,
    };
    let mut index_node = RakuAstNode {
        class: if is_positional {
            RakuAstClass::PostcircumfixArrayIndex
        } else {
            RakuAstClass::PostcircumfixHashIndex
        },
        fields: vec![node_field(Some("index"), semilist)],
    };
    if !colonpairs.is_empty() {
        index_node.fields.push(RakuAstField {
            name: Some("colonpairs"),
            value: RakuAstFieldValue::List(colonpairs),
        });
    }
    if let Some(value) = assignee {
        index_node
            .fields
            .push(node_field(Some("assignee"), convert_expr(value)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), convert_expr(target)?),
            node_field(Some("postfix"), index_node),
        ],
    })
}

/// The source-form record a parser expansion opens with (`ast::signature_decl`).
fn source_form(stmt: &Stmt) -> Option<&crate::ast::SourceForm> {
    match stmt {
        Stmt::SyntheticBlock(stmts) => match stmts.first() {
            Some(Stmt::SourceForm(form)) => Some(form),
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn convert_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    if let Some(node) = subscript_adverb::convert(expr) {
        return node;
    }
    // `supply { … }`: the parser's on-demand expansion opens its emitter
    // lambda with the written body.
    if let Expr::MethodCall { args, .. } = expr
        && let Some(body) = super::react::supply_record(args)
    {
        return super::react::convert_supply(body);
    }
    match expr {
        // `$<name>`: the named capture of the last match.
        Expr::CaptureVar(name) => super::match_vars::convert(name),
        // `pi` / `e` / `tau` are setting terms in raku; the parser folds them to
        // numeric literals, so recover the term from the source spelling kept
        // for a statement-level literal, or from the exact constant otherwise.
        Expr::LiteralSrc(v, src) if math_constant_spelling(v, Some(&**src)).is_some() => Ok(
            term_name_node(math_constant_spelling(v, Some(&**src)).unwrap_or_default()),
        ),
        Expr::Literal(v) if math_constant_spelling(v, None).is_some() => Ok(term_name_node(
            math_constant_spelling(v, None).unwrap_or_default(),
        )),
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => convert_literal(v),
        // `{*}` in a proto body.
        _ if expr.is_onlystar_dispatch() => Ok(RakuAstNode {
            class: RakuAstClass::OnlyStar,
            fields: Vec::new(),
        }),
        // `now` is `Term::Named`, not a call.
        Expr::Call { name, args } if args.is_empty() && name.as_str() == "now" => Ok(RakuAstNode {
            class: RakuAstClass::TermNamed,
            fields: vec![leaf_field(None, Value::str("now".to_string()))],
        }),
        Expr::Subst { .. } | Expr::NonDestructiveSubst { .. } | Expr::Transliterate { .. } => {
            super::substitution::convert(expr)
        }
        Expr::RegexLiteral { tree, .. } | Expr::MatchRegexTree { tree, .. } => {
            quoted_regex_node(tree)
        }
        Expr::Call { name, args } | Expr::UserRoutineCall { name, args } => {
            if let Some(stub) = stub_node(name.as_str(), args) {
                return stub;
            }
            if let Some(atomic) = super::atomic_op::convert(name.as_str(), args) {
                return atomic;
            }
            if is_desugar_marker(name.as_str()) {
                if let Some((call, value)) = method_lvalue_parts(name.as_str(), args)
                    .or_else(|| call_lvalue_parts(name.as_str(), args))
                {
                    return Ok(method_lvalue_assignment(
                        convert_expr(&call)?,
                        convert_expr(value)?,
                    ));
                }
                return Err(desugared(name.as_str()));
            }
            Ok(call_name(name.as_str(), args, false)?)
        }
        Expr::Var(name) => {
            if let Some(capture) = super::match_vars::convert_positional(name) {
                return Ok(capture);
            }
            if is_desugar_marker(name) && !crate::ast::anon_state::is_scalar(name) {
                return Err(desugared(name));
            }
            if let Some(name) = name.strip_prefix('^')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderPositional,
                    "$",
                    name,
                ));
            }
            if let Some(name) = name.strip_prefix(':')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderNamed,
                    "$",
                    name,
                ));
            }
            Ok(var_lexical("$", name))
        }
        // `$::($n)` / `@::($n)` -> `Var::Package` over a dynamic name.
        Expr::SymbolicDeref { sigil, expr } => super::symbolic_deref::convert(sigil, expr),
        Expr::SymbolicDerefAssign { sigil, expr, value } => {
            super::symbolic_deref::convert_assign(sigil, expr, value)
        }
        Expr::IndirectTypeLookupAssign { expr, value } => {
            super::symbolic_deref::convert_type_assign(expr, value)
        }
        // `::("x")` / `::($name)` ->
        // `Term::Name(Name(Part::Empty.new, Part::Expression(EXPR)))`, the
        // leading `::` being an empty name edge (measured on 2026.09). The
        // parser keeps this as an IndirectTypeLookup, so preserving the
        // expression part is necessary for `.AST` and for a later EVAL round
        // trip; rendering it as a static Name would change the lookup mode.
        Expr::IndirectTypeLookup(inner) => {
            let part = RakuAstNode {
                class: RakuAstClass::NamePartExpression,
                fields: vec![node_field(None, convert_expr(inner)?)],
            };
            let name = name_parts::name_from_parts(vec![
                name_parts::leading_empty(),
                Value::rakuast(Box::new(part)),
            ]);
            Ok(RakuAstNode {
                class: RakuAstClass::TermName,
                fields: vec![node_field(None, name)],
            })
        }
        // `::(EXPR)::A::B` / `::(EXPR)::` -> the indirect name above followed
        // by its static parts and, for a trailing `::`, the `Part::Empty` type
        // object (measured on 2026.09).
        Expr::IndirectTypeLookupTail {
            head,
            tail,
            trailing,
        } => {
            let part = RakuAstNode {
                class: RakuAstClass::NamePartExpression,
                fields: vec![node_field(None, convert_expr(head)?)],
            };
            let mut parts = vec![name_parts::leading_empty(), Value::rakuast(Box::new(part))];
            parts.extend(name_parts::tail_parts(tail));
            if *trailing {
                parts.push(name_parts::trailing_empty());
            }
            Ok(RakuAstNode {
                class: RakuAstClass::TermName,
                fields: vec![node_field(None, name_parts::name_from_parts(parts))],
            })
        }
        // A stash lookup `Foo::` / `MY::` / `::` -> a `Name` ending in the
        // `Part::Empty` type object. Rakudo wraps it in a `Term::Name` when the
        // package resolves at parse time and in an argument-less `Call::Name`
        // otherwise (measured: `class F {}; F::` vs. an undeclared `F::`).
        Expr::PseudoStash(stash) => {
            let stem = name_parts::stash_stem(stash)
                .ok_or_else(|| unsupported("stash lookup without its trailing `::`"))?;
            let name = name_parts::stash_name(stem)
                .ok_or_else(|| unsupported("stash lookup with an empty name segment"))?;
            if stem.is_empty() || bareword::package_resolves(stem) {
                Ok(RakuAstNode {
                    class: RakuAstClass::TermName,
                    fields: vec![node_field(None, name)],
                })
            } else {
                Ok(RakuAstNode {
                    class: RakuAstClass::CallName,
                    fields: vec![node_field(Some("name"), name)],
                })
            }
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
        // `eager EXPR` -> `StatementPrefix::Eager(Statement::Expression(EXPR))`.
        Expr::Eager(inner) => {
            let operand = convert_expr(inner)?;
            if operand.class == RakuAstClass::Block {
                return Err(unsupported("eager block"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::StatementPrefixEager,
                fields: vec![node_field(None, statement_expression(operand))],
            })
        }
        // `$@a` / `$%h` / `$[1, 2]` -> `Contextualizer::Item` over the term.
        Expr::Itemize(inner) => super::contextualizer::convert_itemize(inner),
        // `1 ==> foo()` -> `ApplyListInfix(Feed("==>"), operands)`.
        Expr::Feed {
            source,
            sink,
            append,
            left_is_source,
        } => super::feed_op::convert(source, sink, *append, *left_is_source),
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
        // A bareword the setting or the unit's own declarations resolve; any
        // other one stays the boundary (the catch-all arm below).
        Expr::BareWord(name) if bareword::convert(name).is_some() => {
            Ok(bareword::convert(name).expect("just checked"))
        }
        // `my $x = BEGIN { 1 }`: the phaser block as an expression is the same
        // `StatementPrefix::Phaser::*` node a phaser statement wraps.
        Expr::PhaserExpr { kind, body } => {
            let class =
                phaser_class(kind).ok_or_else(|| unsupported("PRE/POST phaser expression"))?;
            Ok(RakuAstNode {
                class,
                fields: vec![node_field(None, block_node(body)?)],
            })
        }
        // `once { ... }` -> `StatementPrefix::Once(Block)`.
        Expr::Once { body } => Ok(RakuAstNode {
            class: RakuAstClass::StatementPrefixOnce,
            fields: vec![node_field(None, block_node(body)?)],
        }),
        // A declaration in expression position (`class { }`, `role { }`,
        // `push my @u, 1`): the parser wraps the declaration in a `DoStmt`,
        // rakudo has the node itself.
        Expr::DoStmt(stmt)
            if matches!(
                stmt.as_ref(),
                Stmt::ClassDecl { .. }
                    | Stmt::RoleDecl { .. }
                    | Stmt::VarDecl { .. }
                    | Stmt::SubDecl { .. }
                    | Stmt::MethodDecl { .. }
                    | Stmt::EnumDecl { .. }
            ) || crate::ast::sigilless_decl::declaration(stmt).is_some()
                || crate::ast::bind_decl::declaration(stmt).is_some() =>
        {
            let statement = convert_stmt(stmt)?.ok_or_else(|| unsupported("declaration term"))?;
            statement
                .fields
                .iter()
                .find(|f| f.name == Some("expression"))
                .and_then(|f| match &f.value {
                    RakuAstFieldValue::Node(value) => match value.view() {
                        ValueView::RakuAst(node) => Some(node.clone()),
                        _ => None,
                    },
                    _ => None,
                })
                .ok_or_else(|| unsupported("declaration term"))
        }
        // `do for ... { }` / `do if ... { }` / `do given ... { }`: the loop or
        // conditional statement under `StatementPrefix::Do`.
        Expr::DoStmt(stmt) if is_do_statement(stmt) => {
            let statement =
                convert_stmt(stmt)?.ok_or_else(|| unsupported("empty `do` statement"))?;
            // `hyper for` / `race for` / `lazy for` are expressions of their own:
            // the loop, with no `do` prefix.
            if matches!(
                stmt.as_ref(),
                Stmt::For {
                    mode: ForMode::Hyper | ForMode::Race | ForMode::Lazy,
                    ..
                }
            ) {
                return statement
                    .fields
                    .iter()
                    .find(|f| f.name == Some("expression"))
                    .and_then(|f| match &f.value {
                        RakuAstFieldValue::Node(value) => match value.view() {
                            ValueView::RakuAst(node) => Some(node.clone()),
                            _ => None,
                        },
                        _ => None,
                    })
                    .ok_or_else(|| unsupported("hyper/race/lazy loop"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::StatementPrefixDo,
                fields: vec![node_field(None, statement)],
            })
        }
        // `(temp $x)` / `(let $x = 1)` in expression position.
        Expr::DoStmt(stmt) if super::temporize::convert(stmt).is_some() => {
            super::temporize::convert(stmt).expect("just checked")
        }
        // A signature declaration in expression position (`if my ($a, $b) = …`).
        Expr::DoStmt(stmt) if source_form(stmt).is_some() => match source_form(stmt) {
            Some(crate::ast::SourceForm::SignatureDecl(decl)) => {
                super::signature_decl::convert(decl)
            }
            Some(crate::ast::SourceForm::MethodAssignDecl(decl)) => {
                super::method_assign_decl::convert(decl)
            }
            // Only the on-demand lambda opens with a supply record.
            Some(crate::ast::SourceForm::SupplyBlock(_)) | None => Err(unsupported("source form")),
        },
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
        // `($x = EXPR)` / `($x := EXPR)` as an expression -> the same
        // `ApplyInfix` as the statement form.
        Expr::AssignExpr {
            name,
            expr,
            is_bind,
        } => {
            if *is_bind {
                return bind_infix(name, expr);
            }
            assignment_infix(name, expr)
        }
        Expr::CompoundAssign {
            target, op, rhs, ..
        } => {
            if is_dotty_assign_op(op) {
                return dotty_assignment(target, rhs);
            }
            compound_assignment_infix(target, op, rhs)
        }
        Expr::ArrayVar(name) => {
            if is_desugar_marker(name) && !crate::ast::anon_state::is_array(name) {
                return Err(desugared(name));
            }
            if name == "_" {
                return Ok(RakuAstNode {
                    class: RakuAstClass::VarDeclarationPlaceholderSlurpyArray,
                    fields: Vec::new(),
                });
            }
            if let Some(name) = name.strip_prefix('^')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderPositional,
                    "@",
                    name,
                ));
            }
            if let Some(name) = name.strip_prefix(':')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderNamed,
                    "@",
                    name,
                ));
            }
            Ok(var_lexical("@", name))
        }
        Expr::HashVar(name) => {
            if is_desugar_marker(name) && !crate::ast::anon_state::is_hash(name) {
                return Err(desugared(name));
            }
            if name == "_" {
                return Ok(RakuAstNode {
                    class: RakuAstClass::VarDeclarationPlaceholderSlurpyHash,
                    fields: Vec::new(),
                });
            }
            if let Some(name) = name.strip_prefix('^')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderPositional,
                    "%",
                    name,
                ));
            }
            if let Some(name) = name.strip_prefix(':')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderNamed,
                    "%",
                    name,
                ));
            }
            Ok(var_lexical("%", name))
        }
        Expr::CodeVar(name) => {
            if let Some(name) = name.strip_prefix('^')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderPositional,
                    "&",
                    name,
                ));
            }
            if let Some(name) = name.strip_prefix(':')
                && is_placeholder_name(name)
            {
                return Ok(placeholder_node(
                    RakuAstClass::VarDeclarationPlaceholderNamed,
                    "&",
                    name,
                ));
            }
            Ok(var_lexical("&", name))
        }
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
            let mut operands = left.flatten_binary_chain(op);
            operands.push(right);
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
        // A dynamically interpolated quoted method name (`$value."$name"()`)
        // keeps the name as a QuotedString expression.  It is distinct from an
        // unquoted dynamic dispatch: Rakudo still exposes Call::QuotedMethod,
        // whose name child contains the interpolation segments.
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier: None,
            quoted: true,
        } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPostfix,
            fields: vec![
                node_field(Some("operand"), convert_expr(target)?),
                node_field(
                    Some("postfix"),
                    call_quoted_method_expr(convert_expr(name_expr)?, args)?,
                ),
            ],
        }),
        // `$o.$name(1)` / `$o.&f(1)` -> `Call::TermAsMethod` / `Call::NameAsMethod`.
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier,
            quoted: false,
        } => super::dynamic_method::convert(target, name_expr, args, *modifier),
        Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            modifier,
        } => super::dynamic_method::convert_hyper(target, name_expr, args, *modifier),
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
        // `{a => 1}` / `%(a => 1)` -> `Circumfix::HashComposer` /
        // `Contextualizer::Hash`, by the spelling the parser recorded.
        Expr::Hash(pairs, spelling) => hash_literal::convert(pairs, *spelling),
        // `$(...)`, `@(...)`, `%(...)` -> `Contextualizer::Item/List/Hash`.
        Expr::Contextualizer { kind, inner } => super::contextualizer::convert(*kind, inner),
        // `$a minmax $b`, `$a foo $b` (a declared `infix:<foo>`), `$a ff $b`:
        // an ordinary application of an `Infix`.
        Expr::InfixFunc {
            name,
            left,
            right,
            modifier,
        } => super::infix_func::convert(name, left, right, modifier, expr),
        // `\(1, :a)` / `\$x` -> `Term::Capture`.
        Expr::CaptureLiteral(items, parenthesized) => {
            super::capture_term::convert(items, *parenthesized)
        }
        // `@a Z @b`, `@a X+ @b`, `@a R- @b`: `ApplyListInfix` / `ApplyInfix` over
        // `MetaInfix::Zip` / `Cross` / `Reverse`.
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } => super::meta_infix::convert(meta, op, left, right, expr),
        // An array-composer literal `[1, 2, 3]` ->
        // `Circumfix::ArrayComposer(SemiList(Statement::Expression(comma-list)))`.
        Expr::BracketArray(items, trailing_comma) => {
            // `[EXPR for LIST]`: the parser holds the modified statement as the
            // one element; rakudo has it as the composer's statement.
            let statement = match items.as_slice() {
                [Expr::DoStmt(stmt)] if is_modifier_statement(stmt) => convert_stmt(stmt)?
                    .ok_or_else(|| unsupported("empty statement in an array composer"))?,
                // `[$x]` holds the element itself; `[$x,]` a one-operand comma
                // list (which is why it does not flatten).
                [single] if !*trailing_comma => statement_expression(convert_expr(single)?),
                _ => statement_expression(comma_list_node(items)?),
            };
            let semilist = RakuAstNode {
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, statement)],
            };
            Ok(RakuAstNode {
                class: RakuAstClass::CircumfixArrayComposer,
                fields: vec![node_field(None, semilist)],
            })
        }
        // Positional subscript `@x[EXPR]` -> ApplyPostfix(operand,
        // postfix => Postcircumfix::ArrayIndex(index => SemiList(...))).
        // The parser retains the associative expression but not whether its
        // source delimiter was `{...}` or `<...>`.  The former is the bounded
        // HashIndex slice; LiteralHashIndex remains a provenance boundary.
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
        } => subscript_node(target, index, *is_positional, None, Vec::new()),
        // `@a[0;1]` / `%h{1;2}`: one `SemiList` statement per dimension.
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => subscript_dims_node(target, dimensions, *is_positional, None, Vec::new()),
        // `@a[]` / `%h{}`: a subscript with no dimension at all.
        Expr::ZenSlice(target) => subscript_dims_node(
            target,
            &[],
            !matches!(&**target, Expr::HashVar(_)),
            None,
            Vec::new(),
        ),
        // `@a[0;1] = 5` keeps an `Assignment` infix over the subscript.
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
            is_positional,
        } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(
                    Some("left"),
                    subscript_dims_node(target, dimensions, *is_positional, None, Vec::new())?,
                ),
                node_field(
                    Some("infix"),
                    RakuAstNode {
                        class: RakuAstClass::Assignment,
                        fields: Vec::new(),
                    },
                ),
                node_field(Some("right"), convert_expr(value)?),
            ],
        }),
        // Measured on 2026.09: rakudo folds an assignment to `@a[…]` or
        // `%h<…>` into the postcircumfix as its `assignee`, but keeps an
        // `Assignment` infix over a `%h{…}` subscript. mutsu does not tell
        // `%h<…>` from `%h{…}` yet (#10654) and renders both as `HashIndex`,
        // so an associative assignment takes the `HashIndex` form.
        // `@a[i] := v` / `%h<k> := v`: an `IndexAssign` whose value is the
        // parser's bind marker, rendered as rakudo does -- a plain `:=` infix
        // over the subscript. A slice or multi-dimensional index
        // (`@a[0,1] := …`, `@a[1;1] := …`) parses to the same flattened list,
        // so neither renders.
        Expr::IndexAssign {
            target,
            index,
            value,
            is_positional,
        } if index_bind_rhs(value).is_some() => {
            let rhs = index_bind_rhs(value).ok_or_else(|| unsupported("indexed bind"))?;
            if matches!(index.as_ref(), Expr::ArrayLiteral(_)) {
                return Err(unsupported("slice or multi-dimensional bind"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(
                        Some("left"),
                        subscript_node(target, index, *is_positional, None, Vec::new())?,
                    ),
                    node_field(Some("infix"), plain_infix(":=")),
                    node_field(Some("right"), convert_expr(rhs)?),
                ],
            })
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            is_positional: true,
        } => subscript_node(target, index, true, Some(value), Vec::new()),
        Expr::IndexAssign {
            target,
            index,
            value,
            is_positional: false,
        } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(
                    Some("left"),
                    subscript_node(target, index, false, None, Vec::new())?,
                ),
                node_field(
                    Some("infix"),
                    RakuAstNode {
                        class: RakuAstClass::Assignment,
                        fields: Vec::new(),
                    },
                ),
                node_field(Some("right"), convert_expr(value)?),
            ],
        }),
        // A bare `{ ... }` block in expression position.
        Expr::Block(body) => block_node(body),
        Expr::AnonSub {
            body,
            is_rw,
            is_raw,
            is_block,
            ..
        } => {
            if *is_block {
                if *is_rw {
                    return Err(unsupported("`is rw` block"));
                }
                if *is_raw {
                    return Err(unsupported("`is raw` block"));
                }
                // A bare `{ ... }` block.
                block_node(body)
            } else {
                // An anonymous, parameter-less `sub { ... }`, with the `is rw`
                // / `is raw` it was written with.
                let mut node = RakuAstNode {
                    class: RakuAstClass::Sub,
                    fields: vec![node_field(Some("body"), blockoid(body)?)],
                };
                routine_traits::add_flags(
                    &mut node,
                    false,
                    false,
                    &routine_traits::IsTraits {
                        is_rw: *is_rw,
                        is_raw: *is_raw,
                        ..Default::default()
                    },
                )?;
                Ok(node)
            }
        }
        Expr::Lambda {
            param,
            body,
            is_whatever_code,
            param_sigilless,
        } => {
            if *is_whatever_code {
                // ADR-0033 Phase 2 §2.5: reachable only from the still-eager
                // `* += 1` / `* -= 2` compound-assignment autoprime path (it
                // needs a `MetaInfix::Assign` class mutsu lacks; a separate,
                // operator-cluster-wide slice, not Whatever-specific).
                return Err(unsupported("Whatever-code closure (compound assignment)"));
            }
            pointy_block_from_lambda(param, *param_sigilless, body)
        }
        Expr::AnonSubParams {
            params,
            param_defs,
            body,
            is_rw,
            is_raw,
            is_whatever_code,
            return_type,
            declarator,
            ..
        } => {
            if *is_whatever_code {
                return Err(unsupported("Whatever-code closure (compound assignment)"));
            }
            if (*is_rw || *is_raw) && !declarator.is_routine() {
                return Err(unsupported("`is rw` pointy block"));
            }
            // A bare placeholder block is a Block whose body contains
            // placeholder declarations, not a PointyBlock with a signature.
            // The parser records the implicit parameters on AnonSubParams, so
            // recover the source-level node only for the ordinary scalar
            // placeholder subset handled by this regex boundary.
            if *declarator == crate::ast::RoutineDeclarator::Block
                && !params.is_empty()
                && (params.iter().all(|param| is_placeholder_param(param))
                    || crate::regex_tree::is_array_slurpy_placeholder_block(expr)
                    || crate::regex_tree::is_hash_slurpy_placeholder_block(expr))
            {
                return block_node(body);
            }
            if declarator.is_routine() {
                // `sub ($x) { }` / `method ($x) { }` — an anonymous *routine*,
                // not a block. raku renders it as a nameless `RakuAST::Sub`
                // whose parameters carry the implicit
                // `type => Type::Setting(Any)` that every sub/method signature
                // has, where a pointy block's do not.
                let mut node = match method_literal_class(*declarator) {
                    Some(class) => {
                        method_literal_node(class, param_defs, body, return_type.as_deref())?
                    }
                    None => anon_routine_node(param_defs, body, return_type.as_deref())?,
                };
                routine_traits::add_flags(
                    &mut node,
                    false,
                    false,
                    &routine_traits::IsTraits {
                        is_rw: *is_rw,
                        is_raw: *is_raw,
                        ..Default::default()
                    },
                )?;
                return Ok(node);
            }
            pointy_block(param_defs, body, return_type.as_deref())
        }
        // An interpolated string `"a $x b"` -> QuotedString with a segment per
        // part (a literal run is a `StrLiteral`, an interpolated term keeps its
        // own node).
        // A `qq:to/END/` body: the parser keeps the raw text for the compiler to
        // interpolate where the heredoc sits; the tree is that interpolation.
        // The terminator (rakudo's `Heredoc(stop => ...)`) is not kept, so it
        // renders as the quoted string it evaluates to. One that closes an
        // enclosing block on its own line is a scope diagnostic the
        // interpolation would lose.
        Expr::HeredocInterpolation(content, closes_block_same_line) => {
            if *closes_block_same_line {
                return Err(unsupported("heredoc closing an enclosing block"));
            }
            convert_expr(&crate::parser::interpolate_heredoc_content(content))
        }
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
        other => match super::match_vars::convert_interpolated(other) {
            Some(capture) => Ok(capture),
            None => convert_expr(other),
        },
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
        if binding_var.is_some() && with_kind.is_some() {
            return Err(unsupported("`orwith EXPR -> $var` topic binding"));
        }
        let node = match with_kind {
            Some(WithBlockKind::Orwith) => orwith_node(cond, then_branch)?,
            _ => elsif_node(cond, then_branch, binding_var)?,
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
fn elsif_node(
    cond: &Expr,
    then_branch: &[Stmt],
    binding_var: &Option<String>,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::StatementElsif,
        fields: vec![
            node_field(Some("condition"), convert_expr(cond)?),
            node_field(Some("then"), clause_block_node(then_branch, binding_var)?),
        ],
    })
}

/// The block of an `if` / `elsif` clause: a plain block, or the pointy block
/// `-> $v { }` when the clause binds the tested value (the parser's
/// `binding_var`, a bare scalar name; measured on rakudo 2026.09).
// Cost: O(n), n = size of the block.
fn clause_block_node(
    then_branch: &[Stmt],
    binding_var: &Option<String>,
) -> Result<RakuAstNode, RuntimeError> {
    match binding_var {
        None => block_node(then_branch),
        Some(name) if is_plain_scalar_name(name) => {
            pointy_block(&[super::lower::positional_param(name)], then_branch, None)
        }
        Some(_) => Err(unsupported(
            "`if EXPR -> $var` topic binding of another form",
        )),
    }
}

/// A bare scalar variable name: `v`, not `$v` / `@a` / an internal `__...`.
fn is_plain_scalar_name(name: &str) -> bool {
    !name.is_empty()
        && !name.starts_with("__")
        && name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
}

/// A `{ ... }` block body wraps its `StatementList` in a `Blockoid`.
///
/// The body keeps the enclosing unit's declared names: the unit-level scan
/// already entered every block, and re-collecting here would *replace* them
/// with the block's own, hiding `class C { }` from a closure `{ C.new }`.
pub(super) fn blockoid(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Blockoid,
        fields: vec![node_field(None, statement_list_inner(body)?)],
    })
}

/// A bare `{ ... }` block -> `Block(body => Blockoid)`.
pub(super) fn block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![node_field(Some("body"), blockoid(body)?)],
    })
}

/// Convert the source-level regex tree to the corresponding RakuAST regex
/// node. The tree is deliberately separate from `RegexPattern`: the latter is
/// an execution plan and has already lost source-level declaration and
/// whitespace information by the time matching begins.
pub(super) fn regex_node(node: &RegexNode) -> Result<RakuAstNode, RuntimeError> {
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
        RegexNode::SequentialAlternation(nodes) => (
            RakuAstClass::RegexSequentialAlternation,
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
        RegexNode::NamedCapture { name, array, regex } => {
            let mut fields = vec![leaf_field(Some("name"), Value::str(name.clone()))];
            if *array {
                fields.push(leaf_field(Some("array"), Value::truth(true)));
            }
            fields.push(node_field(Some("regex"), regex_node(regex)?));
            (RakuAstClass::RegexNamedCapture, fields)
        }
        RegexNode::Subrule {
            name,
            capturing,
            args,
            ..
        } => {
            let name_node = name_from_identifier(name);
            let mut fields = vec![node_field(Some("name"), name_node)];
            if let Some(args) = args
                && !args.args.is_empty()
            {
                fields.push(node_field(Some("args"), regex_arg_list(args)?));
            }
            if *capturing {
                fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            (
                if args.is_some() {
                    RakuAstClass::RegexAssertionNamedArgs
                } else {
                    RakuAstClass::RegexAssertionNamed
                },
                fields,
            )
        }
        RegexNode::SubruleAlias {
            alias,
            name,
            capturing,
            args,
        } => {
            let name_node = name_from_identifier(name);
            let mut assertion_fields = vec![node_field(Some("name"), name_node)];
            if let Some(args) = args
                && !args.args.is_empty()
            {
                assertion_fields.push(node_field(Some("args"), regex_arg_list(args)?));
            }
            if *capturing {
                assertion_fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            let assertion = RakuAstNode {
                class: if args.is_some() {
                    RakuAstClass::RegexAssertionNamedArgs
                } else {
                    RakuAstClass::RegexAssertionNamed
                },
                fields: assertion_fields,
            };
            (
                RakuAstClass::RegexAssertionAlias,
                vec![
                    leaf_field(Some("name"), Value::str(alias.clone())),
                    node_field(Some("assertion"), assertion),
                ],
            )
        }
        RegexNode::Lookaround {
            assertion,
            negated,
            is_behind,
        } => {
            let keyword = if *is_behind { "after" } else { "before" };
            let named_assertion = RakuAstNode {
                class: RakuAstClass::RegexAssertionNamedRegexArg,
                fields: vec![
                    node_field(Some("name"), name_from_identifier(keyword)),
                    node_field(Some("regex-arg"), regex_node(assertion)?),
                    leaf_field(Some("capturing"), Value::truth(true)),
                ],
            };
            let mut fields = Vec::new();
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            fields.push(node_field(Some("assertion"), named_assertion));
            (RakuAstClass::RegexAssertionLookahead, fields)
        }
        RegexNode::ArrayLookaround { name, negated } => {
            let interpolated = RakuAstNode {
                class: RakuAstClass::RegexAssertionInterpolatedVar,
                fields: vec![
                    leaf_field(Some("sequential"), Value::truth(false)),
                    node_field(Some("var"), var_lexical("@", name)),
                ],
            };
            let mut fields = Vec::new();
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            fields.push(node_field(Some("assertion"), interpolated));
            (RakuAstClass::RegexAssertionLookahead, fields)
        }
        RegexNode::Callable { name, args, .. } => {
            let mut fields = vec![node_field(Some("callee"), var_lexical("&", name))];
            if !args.is_empty() {
                fields.push(node_field(Some("args"), arg_list(args)?));
            }
            (RakuAstClass::RegexAssertionCallable, fields)
        }
        RegexNode::CodeAssertion {
            body,
            negated,
            code,
        } => {
            let mut fields = Vec::new();
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            fields.push(node_field(Some("block"), block_node(body)?));
            fields.push(super::regex_code::source_field(code));
            (RakuAstClass::RegexAssertionPredicateBlock, fields)
        }
        RegexNode::CodeBlock { body, code } => (
            RakuAstClass::RegexBlock,
            vec![
                node_field(None, block_node(body)?),
                super::regex_code::source_field(code),
            ],
        ),
        RegexNode::InterpolatedBlock {
            body,
            sequential,
            code,
        } => (
            RakuAstClass::RegexAssertionInterpolatedBlock,
            vec![
                node_field(Some("block"), block_node(body)?),
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                super::regex_code::source_field(code),
            ],
        ),
        RegexNode::NamedLookaround {
            assertion,
            is_behind,
            capturing,
        } => {
            let keyword = if *is_behind { "after" } else { "before" };
            (
                RakuAstClass::RegexAssertionNamedRegexArg,
                vec![
                    node_field(Some("name"), name_from_identifier(keyword)),
                    node_field(Some("regex-arg"), regex_node(assertion)?),
                    leaf_field(Some("capturing"), Value::truth(*capturing)),
                ],
            )
        }
        RegexNode::Interpolation { name, sequential } => (
            RakuAstClass::RegexInterpolation,
            vec![
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                node_field(Some("var"), var_lexical("$", name)),
            ],
        ),
        RegexNode::RegexValueInterpolation {
            name,
            sequential,
            sigil,
        } => (
            RakuAstClass::RegexAssertionInterpolatedVar,
            vec![
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                node_field(Some("var"), var_lexical(&sigil.to_string(), name)),
            ],
        ),
        RegexNode::ArrayInterpolation { name, sequential } => (
            RakuAstClass::RegexInterpolation,
            vec![
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                node_field(Some("var"), var_lexical("@", name)),
            ],
        ),
        RegexNode::Quantified { atom, quantifier } => {
            let separator = quantifier
                .separator
                .as_ref()
                .map(|separator| regex_node(&separator.node))
                .transpose()?;
            return Ok(super::regex_quantifier::convert(
                regex_node(atom)?,
                quantifier,
                separator,
            ));
        }
        RegexNode::AnchorBeginningOfString => {
            (RakuAstClass::RegexAnchorBeginningOfString, Vec::new())
        }
        RegexNode::AnchorBeginningOfLine => (RakuAstClass::RegexAnchorBeginningOfLine, Vec::new()),
        RegexNode::AnchorEndOfString => (RakuAstClass::RegexAnchorEndOfString, Vec::new()),
        RegexNode::AnchorEndOfLine => (RakuAstClass::RegexAnchorEndOfLine, Vec::new()),
        RegexNode::AnchorLeftWordBoundary => {
            (RakuAstClass::RegexAnchorLeftWordBoundary, Vec::new())
        }
        RegexNode::MatchFrom => (RakuAstClass::RegexMatchFrom, Vec::new()),
        RegexNode::AssertionPass => (RakuAstClass::RegexAssertionPass, Vec::new()),
        RegexNode::AssertionFail => (RakuAstClass::RegexAssertionFail, Vec::new()),
        RegexNode::MatchTo => (RakuAstClass::RegexMatchTo, Vec::new()),
        RegexNode::AnchorRightWordBoundary => {
            (RakuAstClass::RegexAnchorRightWordBoundary, Vec::new())
        }
        RegexNode::CharClass(atom) => return Ok(super::regex_char_class::convert(atom)),
        RegexNode::CharClassAssertion(elements) => {
            return Ok(super::regex_enumeration::convert(elements));
        }
        RegexNode::InternalModifier {
            kind,
            long,
            negated,
        } => {
            let class = match kind {
                RegexModifierKind::IgnoreCase => RakuAstClass::RegexInternalModifierIgnoreCase,
                RegexModifierKind::IgnoreMark => RakuAstClass::RegexInternalModifierIgnoreMark,
                RegexModifierKind::Sigspace => RakuAstClass::RegexInternalModifierSigspace,
                RegexModifierKind::Ratchet => RakuAstClass::RegexInternalModifierRatchet,
            };
            let mut fields = Vec::new();
            if *long {
                fields.push(leaf_field(
                    Some("modifier"),
                    Value::str(kind.spellings().1.to_string()),
                ));
            }
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            (class, fields)
        }
        RegexNode::WithWhitespace(child) => (
            RakuAstClass::RegexWithWhitespace,
            vec![node_field(None, regex_node(child)?)],
        ),
    };
    Ok(RakuAstNode { class, fields })
}

/// `[my|our] token NAME(SIG) { … }`: rakudo's `scope` leads the node (the
/// default `has` renders none), and a parameter list is the method-style
/// `signature` between `name` and `body` (measured on rakudo 2026.09).
// Cost: O(r + p), r = size of the regex, p = size of the parameters.
fn regex_declaration(
    class: RakuAstClass,
    name: &str,
    tree: &RegexTree,
    scope: Option<&str>,
    param_defs: &[ParamDef],
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::with_capacity(4);
    if let Some(scope) = scope {
        fields.push(leaf_field(Some("scope"), Value::str_from(scope)));
    }
    fields.push(node_field(Some("name"), name_from_identifier(name)));
    if !param_defs.is_empty() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, None)?,
        ));
    }
    fields.push(node_field(Some("body"), regex_node(&tree.body)?));
    Ok(RakuAstNode { class, fields })
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
            .map(super::substitution::adverb_node)
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
        let init = (!expr_is_nil(expr)).then_some(Initializer::Assign(expr));
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

/// `default-rw => True` on every parameter of a pointy block's signature, ahead
/// of its target: what `<->` makes of them (measured on rakudo 2026.09).
// Cost: O(p), p = parameters.
fn mark_default_rw(block: &mut RakuAstNode) -> Result<(), RuntimeError> {
    let Some(field) = block
        .fields
        .iter_mut()
        .find(|f| f.name == Some("signature"))
    else {
        return Ok(());
    };
    let RakuAstFieldValue::Node(signature) = &field.value else {
        return Err(unsupported("pointy block signature"));
    };
    let ValueView::RakuAst(signature) = signature.view() else {
        return Err(unsupported("pointy block signature"));
    };
    let mut signature = signature.clone();
    if let Some(parameters) = signature
        .fields
        .iter_mut()
        .find(|f| f.name == Some("parameters"))
        && let RakuAstFieldValue::List(items) = &mut parameters.value
    {
        for item in items.iter_mut() {
            let ValueView::RakuAst(parameter) = item.view() else {
                return Err(unsupported("pointy block parameter"));
            };
            let mut parameter = parameter.clone();
            let at = parameter
                .fields
                .iter()
                .position(|f| f.name == Some("target"))
                .unwrap_or(parameter.fields.len());
            parameter
                .fields
                .insert(at, leaf_field(Some("default-rw"), Value::truth(true)));
            *item = Value::rakuast(Box::new(parameter));
        }
    }
    *field = node_field(Some("signature"), signature);
    Ok(())
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

/// The RakuAST class of a method literal's declarator, `None` for a `sub`.
fn method_literal_class(declarator: crate::ast::RoutineDeclarator) -> Option<RakuAstClass> {
    match declarator {
        crate::ast::RoutineDeclarator::Method => Some(RakuAstClass::Method),
        crate::ast::RoutineDeclarator::Submethod => Some(RakuAstClass::Submethod),
        _ => None,
    }
}

/// `method ($a) { … }` -> a nameless `Method` (or `Submethod`) over the
/// written parameters. The parser prepends a synthetic receiver
/// (`parser::anon_method_expr`); it drops out unless the invocant was declared
/// (`method (Foo:D: $a)`, `method ($self: )`): the parser then folds the
/// declaration into the receiver's type and a `my $self := self` binding in the
/// body, which come back as rakudo's leading `invocant` parameter
/// (`parser::folded_invocant`).
// Cost: O(n), n = size of the literal.
fn method_literal_node(
    class: RakuAstClass,
    param_defs: &[ParamDef],
    body: &[Stmt],
    returns: Option<&str>,
) -> Result<RakuAstNode, RuntimeError> {
    let [receiver, rest @ ..] = param_defs else {
        return Err(unsupported("method literal without a receiver"));
    };
    let Some(folded) = crate::parser::folded_invocant(receiver, body) else {
        return Err(unsupported("method literal with an unrecognized receiver"));
    };
    let mut node = anon_routine_node(rest, folded.body, returns)?;
    node.class = class;
    if folded.type_constraint.is_some() || folded.alias.is_some() {
        let invocant = declared_invocant_parameter(folded.type_constraint, folded.alias.as_ref())?;
        add_leading_parameter(&mut node, invocant);
    }
    Ok(node)
}

/// `Parameter(type, invocant => True[, target], optional => False)`: the
/// invocant a method literal declared (measured on rakudo 2026.09). Without a
/// written type it is the `Any` of every routine parameter; without a name it
/// has no target (`method (Mu:D:)`).
// Cost: O(1).
fn declared_invocant_parameter(
    type_constraint: Option<&str>,
    alias: Option<&(String, bool)>,
) -> Result<RakuAstNode, RuntimeError> {
    let type_node = match type_constraint {
        Some(t) => build_type_node(t)?,
        None => type_setting_any(),
    };
    let mut fields = vec![
        node_field(Some("type"), type_node),
        RakuAstField {
            name: Some("invocant"),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        },
    ];
    if let Some((name, sigilless)) = alias {
        let target = if *sigilless {
            RakuAstNode {
                class: RakuAstClass::ParameterTargetTerm,
                fields: vec![node_field(None, name_from_identifier(name))],
            }
        } else {
            // The parser names the anonymous `$:` invocant like any anonymous
            // scalar parameter.
            let spelled = if name == ANONYMOUS_SCALAR_PARAM {
                "$".to_string()
            } else {
                format!("${name}")
            };
            RakuAstNode {
                class: RakuAstClass::ParameterTargetVar,
                fields: vec![leaf_field(Some("name"), Value::str(spelled))],
            }
        };
        fields.push(node_field(Some("target"), target));
    }
    fields.push(RakuAstField {
        name: Some("optional"),
        value: RakuAstFieldValue::Node(Value::truth(false)),
    });
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    })
}

/// `parameter` first among the parameters of `node`'s signature, creating the
/// signature when the routine had none.
// Cost: O(f), f = fields of `node`.
fn add_leading_parameter(node: &mut RakuAstNode, parameter: RakuAstNode) {
    let parameter = Value::rakuast(Box::new(parameter));
    if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("signature"))
        && let RakuAstFieldValue::Node(signature) = &field.value
        && let ValueView::RakuAst(signature) = signature.view()
    {
        let mut signature = signature.clone();
        if let Some(parameters) = signature
            .fields
            .iter_mut()
            .find(|f| f.name == Some("parameters"))
            && let RakuAstFieldValue::List(items) = &mut parameters.value
        {
            items.insert(0, parameter);
        }
        *field = node_field(Some("signature"), signature);
        return;
    }
    let signature = RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![parameter]),
        }],
    };
    node.fields
        .insert(0, node_field(Some("signature"), signature));
}

/// A single-parameter pointy block (`-> $x { }`). mutsu's `Lambda` node strips
/// the sigil from its single param and does NOT preserve `@`/`%` for a single
/// non-scalar param (`-> @a` becomes `param: "a"`), so we assume `$` — a
/// documented divergence from raku, which shows the real sigil.
fn pointy_block_from_lambda(
    param: &str,
    sigilless: bool,
    body: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let desigil = if param == ANONYMOUS_SCALAR_PARAM {
        ""
    } else {
        param
    };
    let mut parameter = simple_parameter("$", desigil, None, None, false, None)?;
    if sigilless {
        sigilless_target(&mut parameter, param);
    }
    let sig = RakuAstNode {
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(parameter))]),
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
/// A sigilled constant (`constant @a = 1, 2`) keeps its sigil in `name`. A
/// typed one (`our Mu constant X = 1`) has the `type` between `scope` and
/// `name`.
fn constant_declaration(
    name: &str,
    expr: &Expr,
    custom_traits: &[(String, Option<Expr>)],
    type_constraint: &Option<String>,
    is_our: bool,
) -> Result<Option<RakuAstNode>, RuntimeError> {
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
    // rakudo's `name` carries the sigil (`"@a"`, `"$x"`). The parser keeps
    // it in the name for `@` / `%` / `&` and strips a `$`.
    let name = match sigil.as_deref().unwrap_or("") {
        // `constant term:<$bar>`: a sigiled name without a sigil of its own
        // is a term, which rakudo names `term:<$bar>`.
        "" if name.starts_with(['$', '@', '%', '&']) => format!("term:<{name}>"),
        "" => name.to_string(),
        "$" => format!("${name}"),
        sigil @ ("@" | "%" | "&") if name.starts_with(sigil) => name.to_string(),
        _ => return Err(unsupported("sigilled constant")),
    };
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
    if let Some(type_name) = type_constraint {
        fields.push(node_field(Some("type"), build_type_node(type_name)?));
    }
    fields.push(leaf_field(Some("name"), Value::str(name)));
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
/// A class expression's body without the `does R` statements the parser puts
/// in front of it for each `does` clause of the header (`class :: does R { }`).
/// The clause is already in `does_parents`, where a class declaration keeps
/// it, and renders as a `Trait::Does`; the statement is its duplicate.
// Cost: O(d), d = number of leading statements.
fn without_composed_header<'a>(body: &'a [Stmt], does_parents: &[String]) -> &'a [Stmt] {
    let header = body
        .iter()
        .take_while(|stmt| {
            matches!(
                stmt,
                Stmt::DoesDecl { name, from_is: false, also: false, args: None }
                    if does_parents.iter().any(|r| r == name.resolve().as_str())
            )
        })
        .count();
    &body[header..]
}

fn class_traits(
    parents: &[String],
    does_parents: &[String],
    parent_args: &[(String, Vec<Expr>)],
    is_rw: bool,
    is_hidden: bool,
    hidden_parents: &[String],
) -> Result<Vec<Value>, RuntimeError> {
    let mut traits = Vec::new();
    for parent in parents {
        // A `does R` role is recorded in BOTH lists (`parents` is the general
        // composed-type list the dispatcher reads), so skip the ones that are
        // really role composition or they would render twice; a parent it
        // `hides` is in both too, and renders as a `Trait::Hides`.
        if does_parents.iter().any(|r| r == parent) || hidden_parents.iter().any(|h| h == parent) {
            continue;
        }
        // `is NAME` names a parent only when NAME is a type; the parser keeps
        // any other name (a `trait_mod:<is>` of the program's own) in
        // `parents` too, and rakudo renders it by name.
        let is_parent_type = parent_args.iter().any(|(p, _)| p == parent)
            || super::bareword::names_type(parent)
            || parent.contains(['[', ':']);
        let field = if is_parent_type {
            node_field(Some("type"), parent_type_node(parent, parent_args)?)
        } else {
            node_field(Some("name"), name_from_identifier(parent))
        };
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields: vec![field],
        })));
    }
    for role in does_parents {
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitDoes,
            fields: vec![node_field(None, parent_type_node(role, parent_args)?)],
        })));
    }
    if is_rw {
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields: vec![node_field(Some("name"), name_from_identifier("rw"))],
        })));
    }
    if is_hidden {
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields: vec![node_field(Some("name"), name_from_identifier("hidden"))],
        })));
    }
    for hidden in hidden_parents {
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitHides,
            fields: vec![node_field(None, parent_type_node(hidden, parent_args)?)],
        })));
    }
    Ok(traits)
}

fn parent_type_node(
    name: &str,
    parent_args: &[(String, Vec<Expr>)],
) -> Result<RakuAstNode, RuntimeError> {
    if let Some((_, args)) = parent_args.iter().find(|(parent, _)| parent == name) {
        return super::type_args::parameterized_type_node(name, Some(args));
    }
    build_type_node(name)
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
pub(super) fn routine_node(
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
pub(super) fn signature(
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
    // The parser records an invocant (`$self:`, `Foo:D:`) as `is_invocant`
    // plus an `invocant` trait, and a synthesized one (no variable) with the
    // `implicit-invocant` trait as well; RakuAST has the one `invocant` flag.
    let implicit_invocant = pd.traits.iter().any(|t| t == IMPLICIT_INVOCANT_TRAIT);
    let user_traits: Vec<&str> = pd
        .traits
        .iter()
        .map(String::as_str)
        .filter(|t| !matches!(*t, "invocant" | IMPLICIT_INVOCANT_TRAIT))
        .collect();
    let refusal = if pd.literal_value.is_some()
        && (pd.named
            || pd.slurpy
            || pd.onearg
            || pd.default.is_some()
            || pd.where_constraint.is_some()
            || !user_traits.is_empty())
    {
        Some("literal-value parameter with more than a value")
    } else if user_traits.iter().any(|t| !is_parameter_trait_name(t)) {
        Some("parameter with a trait the converter does not render")
    } else if pd.is_invocant != pd.traits.iter().any(|t| t == "invocant")
        || (implicit_invocant && !pd.is_invocant)
    {
        Some("invocant marker without an invocant")
    } else if pd.is_invocant && (pd.named || pd.slurpy || pd.double_slurpy) {
        Some("non-scalar invocant parameter")
    } else if pd.shape_constraints.is_some() {
        Some("shaped array parameter")
    } else if pd.code_signature.is_some() {
        Some("parameter with a code signature")
    } else if pd.outer_sub_signature.is_some() {
        Some("parameter with an outer sub-signature")
    } else {
        None
    };
    if let Some(what) = refusal {
        return Err(unsupported(what));
    }
    if let Some(node) = anonymous_destructuring(pd, type_setting)? {
        return Ok(node);
    }
    if let Some(value) = &pd.literal_value {
        return literal_parameter(pd, value);
    }
    // Capture parameters also use the internal `sub_signature` slot, but
    // RakuAST represents those with fields other than `sub-signature`. A named
    // alias's chain is read by `named_param`.
    // `|c ($x)` / `| ($a, $b)`: a capture parameter destructured through a
    // sub-signature, the anonymous one without a target.
    if let Some(sub_params) = pd.sub_signature.as_deref()
        && pd.sigilless
        && pd.slurpy
        && !pd.onearg
        && !pd.double_slurpy
        && !pd.named
        && pd.type_constraint.is_none()
        && pd.where_constraint.is_none()
        && pd.default.is_none()
        && pd.traits.is_empty()
    {
        let mut node = sigilless_slurpy_parameter(pd, type_setting);
        if pd.name == ANONYMOUS_SUBSIGNATURE {
            node.fields.retain(|f| f.name != Some("target"));
        }
        node.fields.push(node_field(
            Some("sub-signature"),
            signature(sub_params, type_setting, None)?,
        ));
        return Ok(node);
    }
    if pd.sub_signature.is_some()
        && (pd.slurpy || pd.double_slurpy || pd.sigilless || pd.name.starts_with("__"))
    {
        return Err(unsupported("non-positional signature sub-signature"));
    }
    let type_capture = match type_capture_name(pd) {
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
    // A `::T` capture spelled as the type constraint itself is not a nominal
    // type; `::T Foo:D:` keeps the capture apart from its nominal `Foo:D`.
    let constraint_is_capture = pd.type_capture.is_none() && type_capture.is_some();
    let ordinary_type_constraint = pd
        .type_constraint
        .as_deref()
        .filter(|_| !constraint_is_capture);
    // A sigilless parameter (`\x`) targets a term, not a variable. The
    // parser's sigilless slurpies are `+a` (`onearg`) and the capture `|c`.
    if pd.sigilless && (pd.double_slurpy || pd.named) {
        return Err(unsupported("sigilless named / double-slurpy parameter"));
    }
    if pd.sigilless && pd.slurpy {
        if pd.type_constraint.is_some() || pd.where_constraint.is_some() || pd.default.is_some() {
            return Err(unsupported("typed sigilless slurpy parameter"));
        }
        return Ok(sigilless_slurpy_parameter(pd, type_setting));
    }
    // The parser names an anonymous `$` / `@` / `%` parameter
    // `__ANON_STATE__` / `@__ANON_ARRAY__` / `%__ANON_HASH__`; rakudo's
    // target is the bare sigil.
    let (sigil, desigil) = match pd.name.as_str() {
        ANONYMOUS_SCALAR_PARAM => ("$", ""),
        ANONYMOUS_ARRAY_PARAM => ("@", ""),
        ANONYMOUS_HASH_PARAM => ("%", ""),
        name => split_sigil(name),
    };
    let mut node = if pd.slurpy || pd.double_slurpy {
        // A typed slurpy is an error in rakudo (`Int *@a`); `where` follows the
        // slurpy marker.
        if pd.type_constraint.is_some() {
            return Err(unsupported("typed slurpy parameter"));
        }
        let mut node = slurpy_parameter(
            sigil,
            desigil,
            if pd.double_slurpy {
                RakuAstClass::ParameterSlurpyUnflattened
            } else {
                RakuAstClass::ParameterSlurpyFlattened
            },
        )?;
        if let Some(w) = pd.where_constraint.as_deref() {
            node.fields
                .push(node_field(Some("where"), convert_expr(w)?));
        }
        node
    } else if pd.onearg {
        // `+@a` is a target and the marker; `+$a` / `+%a` are the plain
        // parameter with the marker after it (measured on rakudo 2026.09).
        if pd.type_constraint.is_some() {
            return Err(unsupported("typed single-argument slurpy parameter"));
        }
        let marker = RakuAstClass::ParameterSlurpySingleArgument;
        let mut node = if sigil == "@" {
            slurpy_parameter(sigil, desigil, marker)?
        } else {
            let mut node = simple_parameter(
                sigil,
                desigil,
                None,
                None,
                type_setting,
                pd.where_constraint.as_deref(),
            )?;
            node.fields.push(leaf_field(
                Some("slurpy"),
                super::slurpy_marker_value(marker),
            ));
            node
        };
        if sigil == "@"
            && let Some(w) = pd.where_constraint.as_deref()
        {
            node.fields
                .push(node_field(Some("where"), convert_expr(w)?));
        }
        node
    } else if pd.named {
        super::named_param::named_parameter(pd, type_setting)?
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
    if pd.sigilless {
        sigilless_target(&mut node, &pd.name);
    }
    // `named_parameter` places a named parameter's capture itself.
    if let Some(type_capture) = type_capture.filter(|_| !pd.named) {
        let target_index = node
            .fields
            .iter()
            .position(|field| field.name == Some("target"))
            .ok_or_else(|| unsupported("type capture without a parameter target"))?;
        node.fields
            .insert(target_index, type_captures_field(type_capture));
    }
    // `invocant => True` follows `type`/`type-captures`; a synthesized
    // invocant has no target (measured on rakudo 2026.09).
    if pd.is_invocant {
        let target_index = node
            .fields
            .iter()
            .position(|field| field.name == Some("target"))
            .ok_or_else(|| unsupported("invocant without a parameter target"))?;
        if implicit_invocant {
            node.fields.remove(target_index);
        }
        node.fields.insert(
            target_index,
            RakuAstField {
                name: Some("invocant"),
                value: RakuAstFieldValue::Node(Value::truth(true)),
            },
        );
    }
    // `$x?`: rakudo's `optional => True`, where a plain positional has False.
    // A defaulted `$x? = 3` has both, the flag first.
    if pd.optional_marker {
        let mut found = false;
        for field in &mut node.fields {
            if field.name == Some("optional") {
                field.value = RakuAstFieldValue::Node(Value::truth(true));
                found = true;
            }
        }
        if !found && let Some(at) = node.fields.iter().position(|f| f.name == Some("default")) {
            node.fields.insert(
                at,
                RakuAstField {
                    name: Some("optional"),
                    value: RakuAstFieldValue::Node(Value::truth(true)),
                },
            );
        }
    }
    if let Some(sub_params) = super::named_param::destructuring_sub_signature(pd) {
        node.fields.push(node_field(
            Some("sub-signature"),
            signature(sub_params, type_setting, None)?,
        ));
    }
    // `$x is copy` -> `traits => (Trait::Is(name => Name.from-identifier("copy")),)`,
    // after every other field (measured on 2026.09).
    if !user_traits.is_empty() {
        let traits = user_traits
            .iter()
            .map(|t| {
                let mut fields = vec![node_field(Some("name"), name_from_identifier(t))];
                if let Some((_, argument)) = pd.trait_args.iter().find(|(name, _)| name == t) {
                    fields.push(node_field(
                        Some("argument"),
                        parameter_trait_argument(argument)?,
                    ));
                }
                Ok(Value::rakuast(Box::new(RakuAstNode {
                    class: RakuAstClass::TraitIs,
                    fields,
                })))
            })
            .collect::<Result<Vec<_>, RuntimeError>>()?;
        node.fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        });
    }
    Ok(node)
}

/// The parser's names for an anonymous `$` / `@` / `%` parameter.
pub(super) const ANONYMOUS_SCALAR_PARAM: &str = "__ANON_STATE__";
pub(super) const ANONYMOUS_ARRAY_PARAM: &str = "@__ANON_ARRAY__";
pub(super) const ANONYMOUS_HASH_PARAM: &str = "%__ANON_HASH__";

/// The parser's name for an anonymous `[…]` destructuring parameter.
pub(super) const ANONYMOUS_ARRAY_SUBSIGNATURE: &str = "@";
/// The parser's name for an anonymous `(…)` destructuring parameter.
pub(super) const ANONYMOUS_SUBSIGNATURE: &str = "__subsig__";

/// Whether `name` is the parser's name for an anonymous destructuring
/// parameter, and if so whether it was the `[…]` form: `@` / `__subsig__` in
/// a signature, `__for_unpack[_array][_N]` in a `for` loop's.
fn anonymous_destructuring_form(name: &str) -> Option<bool> {
    use crate::parser::{FOR_UNPACK, FOR_UNPACK_ARRAY};
    let numbered = |base: &str| {
        name == base
            || name
                .strip_prefix(base)
                .and_then(|rest| rest.strip_prefix('_'))
                .is_some_and(|n| !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()))
    };
    match name {
        ANONYMOUS_ARRAY_SUBSIGNATURE => Some(true),
        ANONYMOUS_SUBSIGNATURE => Some(false),
        _ if numbered(FOR_UNPACK_ARRAY) => Some(true),
        _ if numbered(FOR_UNPACK) => Some(false),
        _ => None,
    }
}

/// An anonymous destructuring parameter, `[$a, $b]` or `($a, $b)`: rakudo's
/// `Parameter` with no target, holding the `sub-signature`; the bracket form
/// marks it `is-array`, and the parenthesised one carries the implicit `Any`
/// type where a sub signature does, and a written type (`Pair (…)`) is the
/// parameter's (measured on rakudo 2026.09). `None` for any other parameter;
/// one with a default, `where` or trait is refused.
// Cost: O(s), s = size of the sub-signature.
fn anonymous_destructuring(
    pd: &ParamDef,
    type_setting: bool,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let Some(is_array) = anonymous_destructuring_form(&pd.name) else {
        return Ok(None);
    };
    let Some(sub_params) = pd.sub_signature.as_deref() else {
        return Ok(None);
    };
    // A capture `| (…)` and the like are not this form.
    if pd.named || pd.slurpy || pd.double_slurpy || pd.sigilless {
        return Ok(None);
    }
    if pd.optional_marker
        || pd.default.is_some()
        || pd.type_capture.is_some()
        || pd.where_constraint.is_some()
        || !pd.traits.is_empty()
    {
        return Err(unsupported(
            "anonymous sub-signature with a default, `where` or trait",
        ));
    }
    let mut sub = signature(sub_params, type_setting, None)?;
    if is_array {
        sub.fields
            .push(leaf_field(Some("is-array"), Value::truth(true)));
    }
    let mut fields = Vec::with_capacity(3);
    // `Pair (…)` / `Positional […]` carry the written type.
    match pd.type_constraint.as_deref() {
        Some(t) => fields.push(node_field(Some("type"), build_type_node(t)?)),
        None if type_setting && !is_array => {
            fields.push(node_field(Some("type"), type_setting_any()));
        }
        None => {}
    }
    fields.push(leaf_field(Some("optional"), Value::truth(false)));
    fields.push(node_field(Some("sub-signature"), sub));
    Ok(Some(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    }))
}

/// The `::T` type capture `pd` declares. Unlike `ParamDef::captured_type_name`
/// (whose binder reading this does not change), a `::?CLASS` / `::?ROLE`
/// pseudo-type is a nominal type here, the way rakudo's `.AST` shows it.
pub(super) fn type_capture_name(pd: &ParamDef) -> Option<&str> {
    pd.captured_type_name().filter(|_| {
        !pd.type_constraint.as_deref().is_some_and(is_pseudo_type) || pd.type_capture.is_some()
    })
}

/// `::?CLASS`, `::?ROLE`, `::?PACKAGE`, with an optional smiley.
fn is_pseudo_type(t: &str) -> bool {
    let base = t
        .strip_suffix(":D")
        .or_else(|| t.strip_suffix(":U"))
        .unwrap_or(t);
    matches!(base, "::?CLASS" | "::?ROLE" | "::?PACKAGE")
}

/// The built-in parameter traits that take no argument.
fn is_parameter_is_trait(name: &str) -> bool {
    matches!(name, "copy" | "rw" | "raw" | "readonly")
}

/// Whether `name` is a trait the converter renders on a parameter: a builtin
/// one, or a plain user-level name (`is marked`, `is option<!>`); the parser's
/// internal markers (`__...`) and qualified names are not.
fn is_parameter_trait_name(name: &str) -> bool {
    is_parameter_is_trait(name)
        || (!name.starts_with("__")
            && !name.is_empty()
            && name
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-'))
}

/// The `(ARGS)` argument of a custom parameter trait, a list as one comma list.
fn parameter_trait_argument(argument: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match argument {
        Expr::Grouped(inner) if matches!(**inner, Expr::ArrayLiteral(_)) => {
            super::attribute::paren_argument(inner)
        }
        other => super::attribute::paren_argument(other),
    }
}

/// `sub f(1)` / `sub f("a")`: a `Parameter` with no target, the literal's type
/// and the literal as its `value` (measured on rakudo 2026.09). The parser
/// names it `__literal__`, gives it the literal's type unless one was written,
/// and keeps the value.
// Cost: O(1).
fn literal_parameter(pd: &ParamDef, value: &Value) -> Result<RakuAstNode, RuntimeError> {
    if pd.name != LITERAL_PARAM
        || !matches!(
            value.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Rat(..)
                | ValueView::Str(_)
        )
    {
        return Err(unsupported("literal-value parameter of another kind"));
    }
    // A sub's literal parameter carries its type (written or inferred); a pointy
    // block's does not, and rakudo infers it from the value.
    let inferred = match value.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => "Int",
        ValueView::Num(_) => "Num",
        ValueView::Rat(..) => "Rat",
        _ => "Str",
    };
    let type_name = pd.type_constraint.as_deref().unwrap_or(inferred);
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields: vec![
            node_field(Some("type"), build_type_node(type_name)?),
            RakuAstField {
                name: Some("optional"),
                value: RakuAstFieldValue::Node(Value::truth(false)),
            },
            leaf_field(Some("value"), value.clone()),
        ],
    })
}

/// The parser's name for a literal-value parameter.
pub(super) const LITERAL_PARAM: &str = "__literal__";

/// A basic `::T` capture is represented by `Parameter.type-captures` rather
/// than by the parameter's ordinary `type` node. Smiley-constrained and other
/// richer capture spellings need more internal metadata and remain deferred.
pub(super) fn type_capture_node(name: &str) -> Result<RakuAstNode, RuntimeError> {
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

pub(super) fn type_captures_field(type_capture: RakuAstNode) -> RakuAstField {
    RakuAstField {
        name: Some("type-captures"),
        value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(type_capture))]),
    }
}

/// A slurpy parameter `*@a` / `**@a` -> `Parameter(target => …, slurpy =>
/// RakuAST::Parameter::Slurpy::{Flattened,Unflattened})`. A slurpy carries no
/// `type`/`optional` field, and the marker is a type object rather than a node
/// (see `slurpy_marker_value`).
fn slurpy_parameter(
    sigil: &str,
    desigil: &str,
    marker: RakuAstClass,
) -> Result<RakuAstNode, RuntimeError> {
    let target = RakuAstNode {
        class: RakuAstClass::ParameterTargetVar,
        fields: vec![leaf_field(
            Some("name"),
            Value::str(format!("{sigil}{desigil}")),
        )],
    };
    // The marker is the `RakuAST::Parameter::Slurpy::*` TYPE OBJECT, as it is in
    // rakudo -- see `slurpy_marker_value`.
    let slurpy = super::slurpy_marker_value(marker);
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields: vec![
            node_field(Some("target"), target),
            leaf_field(Some("slurpy"), slurpy),
        ],
    })
}

/// `+a` -> `Parameter(target => ParameterTarget::Term, slurpy =>
/// Slurpy::SingleArgument)`; `|c` -> the same with `Slurpy::Capture`, and the
/// anonymous `|` has no target at all (measured on rakudo 2026.09).
fn sigilless_slurpy_parameter(pd: &ParamDef, type_setting: bool) -> RakuAstNode {
    let mut fields = Vec::with_capacity(3);
    if type_setting {
        fields.push(node_field(Some("type"), type_setting_any()));
    }
    if pd.name != super::lower::ANONYMOUS_CAPTURE {
        fields.push(node_field(
            Some("target"),
            RakuAstNode {
                class: RakuAstClass::ParameterTargetTerm,
                fields: vec![node_field(None, name_from_identifier(&pd.name))],
            },
        ));
    }
    let marker = if pd.onearg {
        RakuAstClass::ParameterSlurpySingleArgument
    } else {
        RakuAstClass::ParameterSlurpyCapture
    };
    fields.push(leaf_field(
        Some("slurpy"),
        super::slurpy_marker_value(marker),
    ));
    RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    }
}

/// Retarget a parameter at the term `name`: a sigilless `\x` binds
/// `ParameterTarget::Term(Name)`, not a variable (measured on rakudo 2026.09).
fn sigilless_target(parameter: &mut RakuAstNode, name: &str) {
    for field in &mut parameter.fields {
        if field.name == Some("target") {
            *field = node_field(
                Some("target"),
                RakuAstNode {
                    class: RakuAstClass::ParameterTargetTerm,
                    fields: vec![node_field(None, name_from_identifier(name))],
                },
            );
        }
    }
}

/// `Type::Setting.new(Name.from-identifier("Any"))` — the implicit default type
/// carried by every sub/method-signature parameter.
/// The type rakudo gives an untyped parameter: `Type::Setting(Any)` on a
/// scalar (or sigilless) routine parameter, nothing on an `@`/`%`/`&` one or
/// on a block's.
pub(super) fn implicit_parameter_type(sigil: &str, type_setting: bool) -> Option<RakuAstNode> {
    (type_setting && sigil == "$").then(type_setting_any)
}

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
        None => {
            if let Some(implicit) = implicit_parameter_type(sigil, type_setting) {
                fields.push(node_field(Some("type"), implicit));
            }
        }
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

/// `."foo"` / `."foo"(args)` -> `Call::QuotedMethod(name => QuotedString,
/// [args => ArgList])`. Unlike `Call::Method`, the name is a QuotedString
/// (a string literal) rather than a `Name.from-identifier`.
fn call_quoted_method(name: &str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    call_quoted_method_expr(quoted_string(Value::str(name.to_string())), args)
}

/// Construct `Call::QuotedMethod` from the quoted-string expression that names
/// it.  Dynamic quoted names retain their interpolation tree here rather than
/// being flattened into a static method-name string.
fn call_quoted_method_expr(name: RakuAstNode, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(Some("name"), name)];
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

/// `VarDeclaration::Anonymous(scope => "state", sigil, initializer?)`: a bare
/// `$` / `@` / `%`, which is a `state` variable of its block. Rakudo gives it
/// no name.
fn anonymous_declaration(sigil: &str, initializer: Option<RakuAstNode>) -> RakuAstNode {
    let mut fields = vec![
        leaf_field(Some("scope"), Value::str_from("state")),
        leaf_field(Some("sigil"), Value::str(sigil.to_string())),
    ];
    if let Some(initializer) = initializer {
        fields.push(node_field(Some("initializer"), initializer));
    }
    RakuAstNode {
        class: RakuAstClass::VarDeclarationAnonymous,
        fields,
    }
}

/// `$x` / `@a` / `%h` / `&f` usage -> `Var::Lexical("<sigil><name>")`.
/// A variable reference. A package-qualified one (`$Foo::v`, `@A::B::c`) is a
/// `Var::Package` carrying the segmented `Name` and the sigil, as Rakudo
/// 2026.09 renders it; a dynamic one (`$*x`, named `*x` by the parser) is a
/// `Var::Dynamic` of the whole spelling; anything else is a `Var::Lexical` of
/// the whole spelling.
fn var_lexical(sigil: &str, name: &str) -> RakuAstNode {
    // A bare `$` / `@` / `%` is the anonymous declaration itself.
    if (sigil == "$" && crate::ast::anon_state::is_scalar(name))
        || (sigil == "@" && crate::ast::anon_state::is_array(name))
        || (sigil == "%" && crate::ast::anon_state::is_hash(name))
    {
        return anonymous_declaration(sigil, None);
    }
    if name.len() > 1 && name.starts_with('*') {
        return RakuAstNode {
            class: RakuAstClass::VarDynamic,
            fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
        };
    }
    if name_parts::is_qualified_identifier(name) {
        return RakuAstNode {
            class: RakuAstClass::VarPackage,
            fields: vec![
                node_field(Some("name"), name_parts::qualified_name(name)),
                leaf_field(Some("sigil"), Value::str(sigil.to_string())),
            ],
        };
    }
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

/// The `Term::Name` spelling of a math constant (`pi`, `π`, `tau`, `τ`, `e`,
/// `𝑒`) the parser folded into a literal. `src` is the source a statement-level
/// literal kept; without it only the exact constant value identifies the term,
/// and the ASCII spelling is used.
// Cost: O(1).
fn math_constant_spelling(v: &Value, src: Option<&str>) -> Option<&'static str> {
    const NAMES: [&str; 6] = ["pi", "\u{3c0}", "tau", "\u{3c4}", "e", "\u{1D452}"];
    if let Some(src) = src {
        return NAMES.iter().copied().find(|n| *n == src);
    }
    let ValueView::Num(n) = v.view() else {
        return None;
    };
    [
        (std::f64::consts::PI, "pi"),
        (std::f64::consts::TAU, "tau"),
        (std::f64::consts::E, "e"),
    ]
    .into_iter()
    .find(|(c, _)| c.to_bits() == n.to_bits())
    .map(|(_, name)| name)
}

fn term_name_node(name: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::TermName,
        fields: vec![node_field(None, name_from_identifier(name))],
    }
}

/// `ApplyPostfix(<number>, Postfix("i"))` for an imaginary literal of
/// imaginary part `im`. The parser keeps only the value, so the number's
/// spelling is chosen from it: an integral part is an `IntLiteral` (`2i`), any
/// other finite one the `RatLiteral` of its shortest decimal spelling (`3.5i`).
/// A `3.5e0i` therefore comes back as `3.5i`, which denotes the same Complex.
// Cost: O(d), d = digits of the imaginary part.
fn imaginary_literal(im: f64) -> Result<RakuAstNode, RuntimeError> {
    if !im.is_finite() || im < 0.0 || (im == 0.0 && im.is_sign_negative()) {
        return Err(unsupported(
            "imaginary literal with a non-finite or negative part",
        ));
    }
    let number = if im.fract() == 0.0 && im < 9.007_199_254_740_992e15 {
        RakuAstNode {
            class: RakuAstClass::IntLiteral,
            fields: vec![leaf_field(None, Value::int(im as i64))],
        }
    } else {
        let spelling = format!("{im}");
        let rat = crate::parser::decimal_literal_value(&spelling)
            .ok_or_else(|| unsupported("imaginary literal without a decimal spelling"))?;
        RakuAstNode {
            class: RakuAstClass::RatLiteral,
            fields: vec![leaf_field(None, rat)],
        }
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), number),
            node_field(
                Some("postfix"),
                RakuAstNode {
                    class: RakuAstClass::Postfix,
                    fields: vec![leaf_field(Some("operator"), Value::str_from("i"))],
                },
            ),
        ],
    })
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
        ValueView::Num(_) => Ok(RakuAstNode {
            class: RakuAstClass::NumLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Str(_) => Ok(quoted_string(v.clone())),
        // A type the parser resolved to its type object (`Any`) is the same
        // `Type::Simple` an unresolved bareword type (`Int`) renders as.
        ValueView::Package(name) => Ok(simple_type_node(&name.resolve())),
        // The parser folds the term `Empty` to the empty Slip it denotes;
        // raku keeps the name.
        ValueView::Slip(items) if items.is_empty() => Ok(RakuAstNode {
            class: RakuAstClass::TermName,
            fields: vec![node_field(None, name_from_identifier("Empty"))],
        }),
        // `True`/`False` are enum values: `Term::Enum.from-identifier('True')`.
        ValueView::Bool(b) => Ok(RakuAstNode {
            class: RakuAstClass::TermEnum,
            fields: vec![leaf_field(
                None,
                Value::str(if b { "True" } else { "False" }.to_string()),
            )],
        }),
        // `2i` / `3.5i`: the parser folds the imaginary literal to the
        // Complex value; rakudo keeps the number under a `Postfix("i")`.
        ValueView::Complex(re, im) if re == 0.0 && !re.is_sign_negative() => imaginary_literal(im),
        // `<1+2i>`: a complex number written whole.
        ValueView::Complex(..) => Ok(RakuAstNode {
            class: RakuAstClass::ComplexLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        // `v6.d`, `v1.2.3+`.
        ValueView::Version { .. } => Ok(RakuAstNode {
            class: RakuAstClass::VersionLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Mixin(..) => match allomorph_word(v) {
            Some(word) => Ok(word_quote(word)),
            None => Err(unsupported("mixin literal")),
        },
        other => Err(unsupported(&format!("literal {other:?}"))),
    }
}

/// The word of an allomorph literal (`<42>` is `IntStr` 42 spelled "42"), or
/// `None` for any other mixin. Only a word `parser::angle_words_expr` reads
/// back as this one allomorph qualifies: one without whitespace or a
/// quote-word escape, and not the bracket content of a numeric literal term
/// (`<1/2>` is a `Rat`, not a `RatStr`).
// Cost: O(k), k = length of the word.
fn allomorph_word(v: &Value) -> Option<&str> {
    let ValueView::Mixin(inner, overrides) = v.view() else {
        return None;
    };
    let overrides = overrides.overrides();
    if overrides.len() != 1 {
        return None;
    }
    let word = overrides.get("Str")?.as_str()?;
    let numeric = matches!(
        inner.view(),
        ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::BigRat(..)
            | ValueView::Num(_)
            | ValueView::Complex(..)
    );
    let plain = !word.is_empty()
        && !word
            .chars()
            .any(|c| c.is_whitespace() || matches!(c, '\\' | '<' | '>' | '#'));
    (numeric && plain && !crate::parser::angle_word_is_numeric_literal(word)).then_some(word)
}

/// `<word>` -> `QuotedString(processors => <words val>, segments => (word,))`.
pub(super) fn word_quote(word: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(vec![
                    Value::str("words".to_string()),
                    Value::str("val".to_string()),
                ]),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(RakuAstNode {
                    class: RakuAstClass::StrLiteral,
                    fields: vec![leaf_field(None, Value::str(word.to_string()))],
                }))]),
            },
        ],
    }
}

/// A string literal renders as `QuotedString.new(segments => (StrLiteral,))`.
pub(super) fn quoted_string(str_value: Value) -> RakuAstNode {
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

/// `last LABEL` and friends: the bare call over the label as a `Term::Name`.
// Cost: O(|label|).
fn labelled_control_call(name: &'static str, label: &str) -> RakuAstNode {
    let term = RakuAstNode {
        class: RakuAstClass::TermName,
        fields: vec![node_field(None, name_from_identifier(label))],
    };
    RakuAstNode {
        class: RakuAstClass::CallNameWithoutParentheses,
        fields: vec![
            node_field(Some("name"), name_from_identifier(name)),
            node_field(
                Some("args"),
                RakuAstNode {
                    class: RakuAstClass::ArgList,
                    fields: vec![node_field(None, term)],
                },
            ),
        ],
    }
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

pub(super) fn arg_list(args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
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

fn regex_arg_list(args: &crate::regex_tree::SubruleArgs) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::ArgList,
        fields: args
            .args
            .iter()
            .enumerate()
            .map(|(index, argument)| {
                let node = if args.colonpair_falses.get(index).copied().unwrap_or(false) {
                    colonpair_false_expr(argument)?
                } else if args.colonpair_trues.get(index).copied().unwrap_or(false) {
                    colonpair_true_expr(argument)?
                } else if args
                    .colonpair_variables
                    .get(index)
                    .copied()
                    .unwrap_or(false)
                {
                    colonpair_variable_expr(argument)?
                } else if args.colonpair_values.get(index).copied().unwrap_or(false) {
                    colonpair_value_expr(argument)?
                } else if args
                    .literal_hash_indices
                    .get(index)
                    .copied()
                    .unwrap_or(false)
                {
                    literal_hash_index_expr(argument)?
                } else {
                    convert_expr(argument)?
                };
                Ok(node_field(None, node))
            })
            .collect::<Result<Vec<_>, RuntimeError>>()?,
    })
}

/// Convert the execution-level `key => True` shape back to Rakudo's
/// source-level `RakuAST::ColonPair::True` node. The ordinary expression AST
/// intentionally does not retain the leading colon.
fn colonpair_true_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
    } = expr
    else {
        return Err(unsupported("colonpair true provenance"));
    };
    let (Expr::Literal(value) | Expr::LiteralSrc(value, _)) = left.as_ref() else {
        return Err(unsupported("colonpair true key"));
    };
    let ValueView::Str(key) = value.view() else {
        return Err(unsupported("colonpair true key"));
    };
    if !matches!(right.as_ref(), Expr::Literal(value) if matches!(value.view(), ValueView::Bool(true)))
    {
        return Err(unsupported("colonpair true value"));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ColonPairTrue,
        fields: vec![leaf_field(None, Value::str(key.to_string()))],
    })
}

/// Convert the execution-level `key => False` shape back to Rakudo's
/// source-level `RakuAST::ColonPair::False` node. The ordinary expression AST
/// intentionally does not retain the leading `:!`.
fn colonpair_false_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
    } = expr
    else {
        return Err(unsupported("colonpair false provenance"));
    };
    let (Expr::Literal(value) | Expr::LiteralSrc(value, _)) = left.as_ref() else {
        return Err(unsupported("colonpair false key"));
    };
    let ValueView::Str(key) = value.view() else {
        return Err(unsupported("colonpair false key"));
    };
    if !matches!(right.as_ref(), Expr::Literal(value) if matches!(value.view(), ValueView::Bool(false)))
    {
        return Err(unsupported("colonpair false value"));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ColonPairFalse,
        fields: vec![leaf_field(None, Value::str(key.to_string()))],
    })
}

/// Convert the execution-level `key => variable` shape back to Rakudo's
/// source-level `RakuAST::ColonPair::Variable` node. The variable sigil and
/// leading colon are retained by `SubruleArgs` because the internal pair does
/// not distinguish this spelling from an ordinary named argument.
fn colonpair_variable_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
    } = expr
    else {
        return Err(unsupported("colonpair variable provenance"));
    };
    let (Expr::Literal(value) | Expr::LiteralSrc(value, _)) = left.as_ref() else {
        return Err(unsupported("colonpair variable key"));
    };
    let ValueView::Str(key) = value.view() else {
        return Err(unsupported("colonpair variable key"));
    };
    let value = convert_expr(right)?;
    if !value.class.is_simple_variable() {
        return Err(unsupported("colonpair variable value"));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ColonPairVariable,
        fields: vec![
            leaf_field(Some("key"), Value::str(key.to_string())),
            node_field(Some("value"), value),
        ],
    })
}

/// Convert the execution-level `key => value` shape back to Rakudo's
/// source-level `RakuAST::ColonPair::Value` node. Parenthesized values are part
/// of the measured read-direction shape, even though the internal AST stores
/// only the value expression. A bare block is the exception: Rakudo keeps it as
/// a direct `RakuAST::Block` value rather than wrapping it in parentheses.
pub(super) fn colonpair_value_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
    } = expr
    else {
        return Err(unsupported("colonpair value provenance"));
    };
    let (Expr::Literal(value) | Expr::LiteralSrc(value, _)) = left.as_ref() else {
        return Err(unsupported("colonpair value key"));
    };
    let ValueView::Str(key) = value.view() else {
        return Err(unsupported("colonpair value key"));
    };
    let value = convert_expr(right)?;
    let is_direct_block = matches!(
        right.as_ref(),
        Expr::AnonSub { is_block: true, .. }
            | Expr::Block(_)
            | Expr::Hash(_, crate::ast::HashSpelling::Composer)
    ) || crate::regex_tree::is_scalar_placeholder_block(right)
        || crate::regex_tree::is_array_slurpy_placeholder_block(right)
        || crate::regex_tree::is_hash_slurpy_placeholder_block(right);
    if is_direct_block {
        return Ok(RakuAstNode {
            class: RakuAstClass::ColonPairValue,
            fields: vec![
                leaf_field(Some("key"), Value::str(key.to_string())),
                node_field(Some("value"), value),
            ],
        });
    }
    let semilist = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(value))],
    };
    let parenthesized = RakuAstNode {
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ColonPairValue,
        fields: vec![
            leaf_field(Some("key"), Value::str(key.to_string())),
            node_field(Some("value"), parenthesized),
        ],
    })
}

fn literal_hash_index_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Index {
        target,
        index,
        is_positional: false,
    } = expr
    else {
        return Err(unsupported("literal hash index provenance"));
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), convert_expr(target)?),
            node_field(
                Some("postfix"),
                RakuAstNode {
                    class: RakuAstClass::PostcircumfixLiteralHashIndex,
                    fields: vec![node_field(Some("index"), convert_expr(index)?)],
                },
            ),
        ],
    })
}

/// Whether a call argument is one of mutsu's own injected named arguments
/// (`__`-prefixed key), rather than one the source wrote.
/// A statement call's `CallArg`s in the argument-expression form an
/// `Expr::Call` carries: a named argument is the `name => value` pair the
/// expression parser builds for it (a bare `:name` being `name => True`).
fn call_args_as_exprs(args: &[crate::ast::CallArg]) -> Result<Vec<Expr>, RuntimeError> {
    use crate::ast::CallArg;
    args.iter()
        .map(|arg| match arg {
            CallArg::Positional(expr) => Ok(expr.clone()),
            CallArg::Named { name, value } => Ok(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.clone()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(value.clone().unwrap_or(Expr::Literal(Value::TRUE))),
            }),
            // `foo |@a`: the slip is the tight prefix `|` over the term.
            CallArg::Slip(expr) => Ok(Expr::Unary {
                op: crate::token_kind::TokenKind::Pipe,
                expr: Box::new(expr.clone()),
            }),
            CallArg::Invocant(_) => {
                Err(unsupported("statement call with a later invocant argument"))
            }
        })
        .collect()
}

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

/// `unit module M;` / `unit package P;` followed by `rest`: the same
/// declaration with `rest` as its body (inside its header wrapper, if it has
/// one). `None` for any other statement.
// Cost: O(n), n = size of `rest` (it is cloned).
fn unit_package_taking(stmt: &Stmt, rest: &[Stmt]) -> Option<Stmt> {
    let (declaration, header) = match crate::ast::package_header::unwrap(stmt) {
        Some((declaration, header)) => (declaration, header),
        None => (stmt, crate::ast::package_header::Header::default()),
    };
    let Stmt::Package {
        name,
        kind,
        is_unit: true,
        is_my,
        body,
    } = declaration
    else {
        return None;
    };
    if !body.is_empty() {
        return None;
    }
    let package = Stmt::Package {
        name: *name,
        body: rest.to_vec(),
        kind: *kind,
        is_unit: true,
        is_my: *is_my,
    };
    Some(crate::ast::package_header::wrap(
        package,
        &name.resolve(),
        header,
    ))
}
