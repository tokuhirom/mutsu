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
use super::placeholder::{is_placeholder_name, placeholder_node};
use super::regex_declaration::{
    convert as regex_declaration, with_body as regex_declaration_with_body,
};
pub(super) use super::routine_source::routine_node;
use super::source_identifier::{named_routine_call, written_identifier, written_variable};
use super::{
    RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, bareword, decl_traits,
    hash_literal, name_parts, routine_traits, subscript_adverb,
};
use crate::ast::parsed::ParsedStmt;
use crate::ast::spelled::Spelling;
use crate::ast::{AssignOp, Expr, ForMode, ParamDef, ParamTarget, Stmt};
use crate::compiler::helpers_ops::token_kind_to_op_name;
use crate::regex_tree::{RegexExtension, RegexModifierKind, RegexNode, RegexTree};
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
    let _underscore_scope = super::underscore_bindings::Scope::reset();
    let _scope = bareword::DeclaredNames::collect(stmts);
    statement_list_inner(stmts)
}

/// The statements of `stmts`, each as its node.
// Cost: O(n), n = size of the statements.
pub(super) fn block_statements(stmts: &[Stmt]) -> Result<Vec<RakuAstNode>, RuntimeError> {
    Ok(statement_list_inner(stmts)?
        .fields
        .into_iter()
        .filter_map(|field| match field.value {
            RakuAstFieldValue::Node(value) => match value.view() {
                ValueView::RakuAst(node) => Some(node.clone()),
                _ => None,
            },
            _ => None,
        })
        .collect())
}

fn statement_list_inner(stmts: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let _statements = super::passthrough::Block::enter();
    let _underscore_scope = super::underscore_bindings::Scope::block(stmts);
    let _block = bareword::BlockScope::enter(stmts);
    let mut fields = Vec::new();
    // The line of the statement about to be converted: the `SetLine` marker
    // in front of it becomes the node's hidden origin (see `origin`).
    let mut line = None;
    let mut trace = None;
    for (at, stmt) in stmts.iter().enumerate() {
        if let Stmt::SetLine(n, span) = stmt {
            line = Some((*n, *span));
            continue;
        }
        if matches!(stmt, Stmt::Trace { .. }) {
            trace = Some(stmt);
            continue;
        }
        // `unit module M;` / `unit package P;`: rakudo holds the rest of the
        // unit in the declaration's body; the parser leaves it beside it.
        let _statement = super::expr_origins::InStatement::enter(line.map(|(_, span)| span));
        if let Some(unit) = unit_package_taking(stmt, &stmts[at + 1..]) {
            if let Some(mut node) = convert_stmt(&unit)? {
                super::routine_origins::package_origins(&mut node, &unit);
                if let Some(field) = trace.take().and_then(origin::trace_field) {
                    node.fields.push(field);
                }
                if let Some((n, span)) = line.take() {
                    origin::set(&mut node, n, span);
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
        if trace.is_none()
            && let Some(leaf) = super::passthrough::stmt_leaf(stmt, line.map(|(n, _)| n))
        {
            line = None;
            fields.push(node_field(None, leaf));
            continue;
        }
        if let Some(mut node) = convert_stmt(stmt)? {
            super::routine_origins::package_origins(&mut node, stmt);
            if let Some(field) = trace.take().and_then(origin::trace_field) {
                node.fields.push(field);
            }
            if let Some((n, span)) = line.take() {
                origin::set(&mut node, n, span);
            }
            fields.push(node_field(None, node));
        }
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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
        Stmt::Ast(_) => !is_modifier_statement(stmt),
        Stmt::For { .. }
        | Stmt::If { .. }
        | Stmt::Given { .. }
        | Stmt::While { .. }
        | Stmt::Loop { .. }
        | Stmt::Whenever { .. }
        | Stmt::When { .. }
        | Stmt::Default(_) => true,
        _ => false,
    }
}

/// Whether `stmt` is a statement carrying a statement modifier (`EXPR for LIST`,
/// `EXPR if COND`, `EXPR given TOPIC`), which a SemiList holds as a statement.
// Cost: O(1).
fn is_modifier_statement(stmt: &Stmt) -> bool {
    matches!(stmt, Stmt::Ast(node) if matches!(node.as_ref(), ParsedStmt::Modified(_)))
}

/// Convert one statement. Returns `Ok(None)` for non-semantic bookkeeping
/// statements (e.g. `SetLine`) that carry no RakuAST representation.
pub(super) fn convert_stmt(stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    let _guards = super::passthrough::stmt_guards(stmt);
    convert_stmt_node(stmt)
}

fn convert_stmt_node(stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    match stmt {
        // The `use trace` hook is bookkeeping too: rakudo models the trace as a
        // flag on the traced statement, not as a statement of its own.
        Stmt::SetLine(..) | Stmt::Trace { .. } => Ok(None),
        // The written traits are rendered by the routine they open.
        Stmt::Ast(_) if stmt.is_routine_traits() => Ok(None),
        // A construct the parser built in its RakuAST shape (ADR-10723
        // Stage 2): rendered, not reconstructed.
        Stmt::Ast(node) => super::parsed_view::parsed_node(node).map(Some),
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
        Stmt::Expr(e) => {
            let node = convert_expr(e)?;
            if matches!(
                node.class,
                RakuAstClass::StatementRequire
                    | RakuAstClass::StatementExpression
                    | RakuAstClass::StatementEmpty
            ) {
                Ok(Some(node))
            } else {
                Ok(Some(statement_expression(node)))
            }
        }
        Stmt::TokenDecl {
            name,
            params,
            param_defs,
            body,
            source_regex,
            regex_kind,
            multi,
            is_my,
            is_our,
            is_export,
            export_tags,
            ..
        } => {
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
            let mut declaration = regex_declaration(
                class,
                &name.resolve(),
                tree,
                scope,
                multi.then_some("multi"),
                param_defs,
                super::package_header::export_trait_value(
                    *is_export || !export_tags.is_empty(),
                    export_tags,
                ),
            )?;
            super::regex_declaration::retain_source(&mut declaration, body);
            Ok(Some(statement_expression(declaration)))
        }
        // `proto token t {*}`: the declaration with no regex, only the
        // dispatcher.
        Stmt::ProtoToken {
            name,
            param_defs,
            regex_kind,
            is_my,
            is_our,
        } => {
            let scope = match (*is_my, *is_our) {
                (false, false) => None,
                (true, false) => Some("my"),
                (false, true) => Some("our"),
                (true, true) => return Err(unsupported("proto regex both `my` and `our`")),
            };
            let class = match regex_kind {
                crate::regex_tree::RegexDeclKind::Token => RakuAstClass::TokenDeclaration,
                crate::regex_tree::RegexDeclKind::Regex => RakuAstClass::RegexDeclaration,
                crate::regex_tree::RegexDeclKind::Rule => RakuAstClass::RuleDeclaration,
            };
            Ok(Some(statement_expression(regex_declaration_with_body(
                class,
                Some(&name.resolve()),
                scope,
                Some("proto"),
                param_defs,
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::OnlyStar,
                    fields: Vec::new(),
                },
                Vec::new(),
            )?)))
        }
        Stmt::RuleDecl {
            name,
            params,
            param_defs,
            body,
            source_regex,
            multi,
            is_export,
            export_tags,
            ..
        } => {
            if params.len() != param_defs.len() {
                return Err(unsupported(
                    "rule declaration with an unnamed parameter list",
                ));
            }
            let Some(tree) = source_regex else {
                return Err(unsupported("rule declaration without a source tree"));
            };
            let mut declaration = regex_declaration(
                RakuAstClass::RuleDeclaration,
                &name.resolve(),
                tree,
                None,
                multi.then_some("multi"),
                param_defs,
                super::package_header::export_trait_value(
                    *is_export || !export_tags.is_empty(),
                    export_tags,
                ),
            )?;
            super::regex_declaration::retain_source(&mut declaration, body);
            Ok(Some(statement_expression(declaration)))
        }
        // `use` / `no` statements: `RakuAST::Pragma`, `Statement::Use` or
        // `Statement::LanguageVersion`. The `if` distribution's condition is
        // source provenance: the visible RakuAST use node omits it.
        Stmt::Use {
            module,
            arg,
            tags,
            condition,
            if_imports,
        } => Ok(Some(super::use_stmt::convert_use(
            module,
            arg.as_ref(),
            tags,
            condition.as_deref(),
            if_imports,
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
        Stmt::Call { name, args, listop } => {
            if is_desugar_marker(name.as_str()) {
                let args = call_args_as_exprs(args)?;
                if let Some(stub) = stub_node(name.as_str(), &args) {
                    return Ok(Some(statement_expression(stub?)));
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
                    sugar: false,
                };
                return Ok(Some(statement_expression(convert_expr(&method)?)));
            }
            let args = call_args_as_exprs(args)?;
            Ok(Some(statement_expression(call_name(
                name.as_str(),
                &args,
                *listop,
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
        // `goto LABEL` is represented as a bare call by RakuAST, with a
        // Term::Name argument for a bare label. Computed targets stay
        // ordinary expressions.
        Stmt::Goto(Expr::Literal(value) | Expr::LiteralSrc(value, _))
            if let ValueView::Str(label) = value.view() =>
        {
            Ok(Some(statement_expression(labelled_control_call(
                "goto", &label,
            ))))
        }
        Stmt::Goto(expr) => Ok(Some(statement_expression(control_call(
            "goto",
            std::slice::from_ref(expr),
        )?))),
        // `die`/`fail EXPR` are also modelled as bare calls.
        Stmt::Die(expr) => Ok(Some(statement_expression(control_call(
            "die",
            std::slice::from_ref(expr),
        )?))),
        Stmt::Fail(expr) => Ok(Some(statement_expression(control_call(
            "fail",
            std::slice::from_ref(expr),
        )?))),
        // `take EXPR` / `take-rw EXPR` are bare calls too.
        Stmt::Take(expr, is_rw) => Ok(Some(statement_expression(control_call(
            if *is_rw { "take-rw" } else { "take" },
            std::slice::from_ref(expr),
        )?))),
        // `proceed` / `succeed` without arguments are bare calls as well.
        Stmt::Proceed => Ok(Some(statement_expression(control_call("proceed", &[])?))),
        Stmt::Succeed => Ok(Some(statement_expression(control_call("succeed", &[])?))),
        Stmt::TempMethodAssign {
            var_name,
            method_name,
            method_args,
            value,
            is_attribute,
        } => Ok(Some(statement_expression(
            super::temporize::convert_method_assign(
                var_name,
                method_name,
                method_args,
                value,
                *is_attribute,
            )?,
        ))),
        Stmt::VarDecl { .. } => var_decl_statement(var_decl_parts(stmt)?, false),
        Stmt::DocPhaser(inner) => Ok(Some(super::doc_phaser::convert(inner)?)),
        // A bare `{ ... }` block at statement level -> Statement::Expression(Block).
        Stmt::Block(body) => Ok(Some(statement_expression(signature_block_node(body)?))),
        // `BEGIN { … }` / `INIT { … }` / `LEAVE { … }` / … -> a
        // `StatementPrefix::Phaser::<Kind>` wrapping the block positionally.
        // raku has one class per kind, which mutsu's single `PhaserKind` maps
        // onto 1:1. `PRE`/`POST` are the exception: rakudo desugars them into a
        // call around the block (an `ApplyPostfix` operand), and mutsu also
        // keeps a source-text `condition` for their exception message, so they
        // stay the boundary.
        Stmt::Phaser {
            spans,
            kind,
            body,
            condition,
            end_index,
        } => {
            if let Some(condition) = condition {
                return super::phaser_condition::convert(kind, body, condition)?
                    .map(Some)
                    .ok_or_else(|| unsupported("PRE/POST phaser condition"));
            }
            let class = match phaser_class(kind) {
                Some(c) => c,
                None => return Err(unsupported("PRE/POST phaser")),
            };
            let mut block = block_node(body)?;
            if let Some(spans) = spans.as_deref() {
                super::routine_origins::block_origins(&mut block, *spans);
            }
            let mut fields = vec![node_field(None, block)];
            // The source position of a main-compunit `END` (see `origin`).
            fields.extend(end_index.map(origin::end_index_field));
            Ok(Some(statement_expression(RakuAstNode {
                origin: Default::default(),
                class,
                fields,
            })))
        }
        // A conditional is a typed node (`ast::parsed::Conditional`), and a
        // statement modifier too (`ast::parsed::Modified`). The one desugared
        // conditional the conversion still reads is the guarded control
        // transfer `proceed |c` / `succeed |c` parses into
        // (`slipped_control_stmt`), which rakudo has as the plain call over the
        // slipped argument.
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            is_statement_modifier: true,
            ..
        } => match slipped_control_call(cond, then_branch, else_branch)? {
            Some(call) => Ok(Some(statement_expression(call))),
            None => Err(unsupported("desugared conditional")),
        },
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
            // A statement modifier is a typed node (`ast::parsed::Modified`).
            if *is_statement_modifier {
                return Err(unsupported("desugared for statement modifier"));
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
                origin: Default::default(),
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
        // `CATCH { ... }` -> Statement::Catch(body => topic Block + exception).
        // The body topicalizes the exception, so raku marks the block both
        // `implicit-topic`/`required-topic` (like a `given` body) and
        // `exception => 1`, which is what distinguishes it from one.
        Stmt::Catch(body) => Ok(Some(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementCatch,
            fields: vec![node_field(Some("body"), exception_block_node(body)?)],
        })),
        // `CONTROL { ... }` -> Statement::Control, the same exception block.
        Stmt::Control(body) => Ok(Some(RakuAstNode {
            origin: Default::default(),
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
            scope,
            custom_traits,
            spans,
            ..
        } => {
            // Phase 2 slice 7 covers the plain named `sub NAME (params) { body }`
            // form; return types are covered too (`-->` via `Signature.returns`,
            // `returns`/`of` via `Trait::Returns`/`Trait::Of`). Other traits,
            // Multi, export and operator subs carry their public RakuAST shape.
            // The S13 alternate-signature and supersede forms have no Rakudo
            // node shape; their execution data stays as hidden source metadata.
            let deferred = [
                (
                    *is_export != !export_tags.is_empty(),
                    "sub with an untagged `is export`",
                ),
                (
                    *is_test_assertion && !custom_traits.iter().any(|(t, _)| t == "test-assertion"),
                    "sub with `is test-assertion`",
                ),
                (
                    custom_traits.iter().any(|(t, _)| {
                        t.starts_with("__")
                            && !is_return_spelling_marker(t)
                            && t != "__hidden_from_backtrace"
                            // The operator-precedence record the parser derives
                            // from `is assoc` / `is tighter`; lowering rebuilds it.
                            && t != OP_PREC_TRAIT
                    }),
                    "sub with an internal trait",
                ),
            ];
            if let Some((_, what)) = deferred.iter().find(|(hit, _)| *hit) {
                return Err(unsupported(what));
            }
            let mut node = routine_node(
                RakuAstClass::Sub,
                &name.resolve(),
                param_defs,
                body,
                return_type.as_deref(),
            )?;
            super::routine_origins::apply(&mut node, spans.as_deref(), param_defs);
            super::routine_source::mark_legacy_sub_forms(
                &mut node,
                signature_alternates,
                *supersede,
            )?;
            super::symbolic_deref::replace_declarator_name(&mut node, name_expr.as_ref())?;
            // The traits are the ones the source wrote, in written order; the
            // execution fields the parser folded them into are not read back.
            let written = super::routine_source::apply(&mut node, body)?;
            if !written
                && (*is_rw
                    || *is_raw
                    || !export_tags.is_empty()
                    || associativity.is_some()
                    || precedence_trait.is_some()
                    || custom_traits.iter().any(|(t, _)| !t.starts_with("__")))
            {
                return Err(unsupported("sub with traits and no written-trait record"));
            }
            routine_traits::add_flags(&mut node, *multi, false);
            // `scope => "our"` / `"anon"` leads the node; `my` is the default
            // scope and renders no field.
            let written_scope = match scope {
                crate::ast::SubScope::My => None,
                crate::ast::SubScope::Our => Some("our"),
                crate::ast::SubScope::Anon => Some("anon"),
            };
            if let Some(scope) = written_scope {
                node.fields
                    .insert(0, leaf_field(Some("scope"), Value::str_from(scope)));
            }
            super::routine_origins::traits(&mut node, spans.as_deref());
            Ok(Some(statement_expression(node)))
        }
        Stmt::MethodDecl {
            value_prototype: _,
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
            return_type,
            is_default_candidate,
            deprecated_message,
            handles,
            custom_traits,
            is_export,
            export_tags,
            spans,
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
            let declared_my = *is_my;
            let deferred = [
                (
                    *is_export != !export_tags.is_empty(),
                    "method with a bare `is export`",
                ),
                (
                    !handles.is_empty() && !super::routine_source::keeps_handles(body),
                    "method with `handles`",
                ),
                (
                    custom_traits.iter().any(|(t, _)| {
                        t.starts_with("__")
                            && !is_return_spelling_marker(t)
                            && t != "__hidden_from_backtrace"
                    }),
                    "method with an internal trait",
                ),
            ];
            if let Some((_, what)) = deferred.iter().find(|(hit, _)| *hit) {
                return Err(unsupported(what));
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
                return_type.as_deref(),
            )?;
            super::routine_origins::apply(&mut node, spans.as_deref(), param_defs);
            super::symbolic_deref::replace_declarator_name(&mut node, name_expr.as_ref())?;
            // The traits are the ones the source wrote, in written order; the
            // execution fields the parser folded them into are not read back.
            let written = super::routine_source::apply(&mut node, body)?;
            if !written
                && (*is_rw
                    || *is_raw
                    || !export_tags.is_empty()
                    || *is_default_candidate
                    || deprecated_message.is_some()
                    || !handles.is_empty()
                    || custom_traits.iter().any(|(t, _)| !t.starts_with("__")))
            {
                return Err(unsupported(
                    "method with traits and no written-trait record",
                ));
            }
            routine_traits::add_flags(&mut node, *multi, *is_private);
            // `scope => "my"` / `"our"` leads the node, ahead of `multiness`.
            // Submethod's constructor rendering omits even a written scope,
            // but its accessor still distinguishes `my` and `our`. Keep the
            // value in the model and let the renderer apply that contract.
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
            super::routine_origins::traits(&mut node, spans.as_deref());
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
            body_parents,
            ..
        } => {
            let is_native = custom_traits
                .iter()
                .any(|(trait_name, _)| trait_name == "__mutsu_native_decl");
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
                    if is_anon_package(custom_traits) {
                        Some("anon")
                    } else {
                        package_scope(*is_lexical, *is_unit)
                    },
                    is_colons_package(custom_traits),
                );
                super::role_header::retain_name(&mut fields, *name);
                // The implicit `Grammar` parent is not a written trait.
                let written: &[String] = if *implicit_grammar_parent {
                    parents.get(1..).unwrap_or(&[])
                } else {
                    parents
                };
                // An `also is Parent;` lifted out of the body has no header
                // spelling; it renders as a trailing `is` trait, which lowers
                // back to the same parent.
                let mut header_parents = written.to_vec();
                let mut also_traits = Vec::new();
                for parent in body_parents {
                    if let Some(at) = header_parents.iter().rposition(|p| p == parent) {
                        header_parents.remove(at);
                    }
                    also_traits.push(Value::rakuast(Box::new(super::routine_source::trait_node(
                        &also_parent(parent, parent_args),
                    )?)));
                }
                let mut traits = header_trait_values(
                    body,
                    &HeaderFields {
                        parents: &header_parents,
                        does_parents,
                        hidden_parents,
                        is_rw: false,
                        is_hidden: false,
                        custom_traits: &[],
                    },
                )?;
                traits.extend(also_traits);
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
                    origin: Default::default(),
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
            let visible_custom_traits: Vec<_> = custom_traits
                .iter()
                .filter(|(trait_name, _)| trait_name != "__mutsu_native_decl")
                .cloned()
                .collect();
            if has_package_traits(&visible_custom_traits) && !is_native {
                return Err(unsupported(&format!(
                    "class with inheritance / scope / repr / traits ({})",
                    custom_traits
                        .iter()
                        .map(|(t, _)| t.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                )));
            }
            let mut fields = package_header_fields(
                *name,
                if is_anon_package(custom_traits) {
                    Some("anon")
                } else {
                    package_scope(*is_lexical, *is_unit)
                },
                is_colons_package(custom_traits),
            );
            super::role_header::retain_name(&mut fields, *name);
            if let Some(expr) = name_expr {
                let name = node_field(Some("name"), super::symbolic_deref::indirect_name(expr)?);
                match fields.iter_mut().find(|f| f.name == Some("name")) {
                    Some(slot) => *slot = name,
                    None => fields.push(name),
                }
            } else if has_leading_colons(custom_traits) {
                name_parts::with_leading_empty(&mut fields);
            }
            super::declare_how::add_field(&mut fields, custom_traits, "class");
            // Field order matches raku: scope, parsed-declarator, name, repr,
            // traits, body.
            if let Some(r) = repr {
                fields.push(leaf_field(Some("repr"), Value::str(r.clone())));
            }
            // `also is P;` in the body is a `Statement::Also`, not a trait.
            let mut header_parents = parents.clone();
            let mut also_statements = Vec::new();
            for parent in body_parents {
                if let Some(at) = header_parents.iter().rposition(|p| p == parent) {
                    header_parents.remove(at);
                }
                let node = super::routine_source::trait_node(&also_parent(parent, parent_args))?;
                also_statements.push(super::role::also_is_statement(Value::rakuast(Box::new(
                    node,
                ))));
            }
            let traits = header_trait_values(
                body,
                &HeaderFields {
                    parents: &header_parents,
                    does_parents,
                    hidden_parents,
                    is_rw: *class_is_rw,
                    is_hidden: *is_hidden,
                    custom_traits: &visible_custom_traits,
                },
            )?;
            if !traits.is_empty() {
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(traits),
                });
            }
            let class_body = block_node(&crate::parser::unhoist_nested_methods(
                without_composed_header(body, does_parents),
            ))?;
            fields.push(node_field(
                Some("body"),
                super::role::with_leading_statements(class_body, also_statements),
            ));
            let class = RakuAstNode {
                origin: Default::default(),
                class: if is_native {
                    RakuAstClass::Native
                } else {
                    RakuAstClass::Class
                },
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
            origin: Default::default(),
            class: RakuAstClass::StatementTrusts,
            fields: vec![node_field(Some("type"), build_type_node(&name.resolve())?)],
        })),
        // `augment class/role C { ... }` keeps its package kind and the
        // `scope => "augment"` declarator.
        Stmt::AugmentClass {
            name,
            body,
            does_roles,
            is_role,
        } => {
            let mut fields = vec![
                leaf_field(Some("scope"), Value::str_from("augment")),
                node_field(Some("name"), name_from_identifier(&name.resolve())),
            ];
            if !does_roles.is_empty() {
                let mut traits = Vec::new();
                for role in does_roles {
                    traits.push(Value::rakuast(Box::new(RakuAstNode {
                        origin: Default::default(),
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
                origin: Default::default(),
                class: if *is_role {
                    RakuAstClass::Role
                } else {
                    RakuAstClass::Class
                },
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
            if variants.is_empty()
                && !matches!(
                    variant_form,
                    crate::ast::EnumVariantForm::EmptyWords(_)
                        | crate::ast::EnumVariantForm::PairList
                )
            {
                return Err(unsupported("enum without a body"));
            }
            let term = super::enum_term::enum_term_node(variants, variant_form)?;
            // Field order matches raku: scope, name, of, traits, term.
            let mut fields: Vec<RakuAstField> = super::package_header::my_scope_field(*is_my)
                .into_iter()
                .collect();
            fields.push(node_field(
                Some("name"),
                name_from_identifier(&name.resolve()),
            ));
            if let Some(base) = base_type {
                fields.push(node_field(Some("of"), build_type_node(base)?));
            }
            let mut traits = Vec::new();
            for role in roles {
                traits.push(Value::rakuast(Box::new(RakuAstNode {
                    origin: Default::default(),
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
                origin: Default::default(),
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
            custom_traits,
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
            let mut node = RakuAstNode {
                origin: Default::default(),
                class,
                fields,
            };
            super::package_traits::apply(&mut node, custom_traits, body)?;
            Ok(Some(statement_expression(node)))
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
                    origin: Default::default(),
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
                origin: Default::default(),
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
                has_traits: custom_traits.iter().any(|t| !is_return_spelling_marker(t))
                    || !trait_args.is_empty(),
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
            args,
            also: true,
            from_is: false,
        } => Ok(Some(super::role::also_statement(
            name.resolve().as_str(),
            args.as_deref(),
        )?)),
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
        Stmt::HasDecl { .. } => super::attribute_decl::convert(stmt),
        // `v = EXPR` where `v` is a sigilless term: rakudo's left side is the
        // `Term::Name`, and the assignment is a list assignment (no `:item`).
        Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
            target_is_sigilless: true,
        } => Ok(Some(statement_expression(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(
                    Some("left"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::TermName,
                        fields: vec![node_field(None, name_from_identifier(name))],
                    },
                ),
                node_field(
                    Some("infix"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::Assignment,
                        fields: Vec::new(),
                    },
                ),
                node_field(Some("right"), convert_expr(expr)?),
            ],
        }))),
        // `$x := EXPR` — a plain `:=` infix (slice 9). `$x = EXPR` is the typed
        // assignment node a `.AST` parse keeps (ADR-10723 Stage 2).
        Stmt::Assign {
            name,
            expr,
            op: AssignOp::Bind,
            ..
        } => Ok(Some(statement_expression(bind_infix(name, expr)?))),
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

/// The op name of an `nqp::op(...)` call (`add_i` of `nqp::add_i`); `None` for
/// any other name, including a constant (`nqp::const::CCLASS_WORD`, which raku
/// has a node of its own for).
// Cost: O(k), k = length of `name`.
pub(super) fn nqp_op(name: &str) -> Option<&str> {
    let op = name.strip_prefix("nqp::")?;
    (!op.is_empty() && !crate::qualified::is_qualified_str(op)).then_some(op)
}

/// A hyper infix `LEFT >>OP<< RIGHT` -> `ApplyInfix(left, MetaInfix::Hyper(
/// [dwim-left,] infix, [dwim-right]), right)`; raku omits a dwim field whose
/// value is False, and the hyper assignment's infix is the `Assignment` node.
// Cost: O(n), n = nodes of the operands.
pub(super) fn hyper_infix(
    left: &Expr,
    op: &str,
    right: &Expr,
    dwim_left: bool,
    dwim_right: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let mut hyper_fields = Vec::with_capacity(3);
    if dwim_left {
        hyper_fields.push(RakuAstField {
            name: Some("dwim-left"),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        });
    }
    hyper_fields.push(node_field(
        Some("infix"),
        if op == "=" {
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::Assignment,
                fields: vec![RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Adverb("item"),
                }],
            }
        } else {
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::Infix,
                fields: vec![leaf_field(None, Value::str(op.to_string()))],
            }
        },
    ));
    if dwim_right {
        hyper_fields.push(RakuAstField {
            name: Some("dwim-right"),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        });
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), convert_expr(left)?),
            node_field(
                Some("infix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::MetaInfixHyper,
                    fields: hyper_fields,
                },
            ),
            node_field(Some("right"), convert_expr(right)?),
        ],
    })
}

/// `LEFT = EXPR` over an already converted left side; `is_item` marks the
/// `Assignment` node `:item`, which rakudo does for a scalar target.
pub(super) fn assignment_around(
    left: RakuAstNode,
    is_item: bool,
    rhs: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let assignment = RakuAstNode {
        origin: Default::default(),
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
        origin: Default::default(),
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
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), var_lexical(sigil, desigil)),
            node_field(Some("infix"), plain_infix(":=")),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
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
            origin: Default::default(),
            class,
            fields: Vec::new(),
        }));
    }
    Some(arg_list(args).map(|list| RakuAstNode {
        origin: Default::default(),
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
        Expr::Call { name, args, .. } if name.as_str() == "__mutsu_bind_index_value" => {
            match args.as_slice() {
                [rhs, _] => Some(rhs),
                _ => None,
            }
        }
        _ => None,
    }
}

/// A plain `Infix.new("<op>")` node from a literal operator string.
pub(super) fn plain_infix(op: &str) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Infix,
        fields: vec![leaf_field(None, Value::str(op.to_string()))],
    }
}

/// `$x OP= EXPR` -> `ApplyInfix(left, MetaInfix::Assign(Infix(OP)), right)`.
pub(super) fn compound_assignment_infix(
    target: &Expr,
    op: &str,
    rhs: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let left = match public_attribute_call(target) {
        Some(name) => var_lexical("$", &format!(".{name}")),
        None => convert_expr(target)?,
    };
    compound_assignment_with_left(left, op, rhs)
}

/// The attribute name of `$.name` as the statement parser spells an
/// `$.name OP= EXPR` target: a method call on `__ANON_STATE#`, the bare-`$`
/// carrier the VM resolves to `self`. Rakudo 2026.09 builds the same
/// `Var::Attribute::Public` it builds for `$.name` anywhere else.
// Cost: O(1).
fn public_attribute_call(target: &Expr) -> Option<&str> {
    match target {
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: None,
            quoted: false,
            ..
        } if args.is_empty() && matches!(target.as_ref(), Expr::Var(v) if v == "__ANON_STATE#") => {
            Some(name.as_str())
        }
        _ => None,
    }
}

/// `my $x OP= EXPR` (`ParsedStmt::DeclCompound`) -> `ApplyInfix(left =>
/// VarDeclaration, infix => MetaInfix::Assign(OP), right => EXPR)`, as
/// measured on rakudo 2026.09.
// Cost: O(n), n = size of the declaration and the right-hand side.
pub(super) fn declaration_compound_assign(
    decl: &Stmt,
    op: &str,
    rhs: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let statement = convert_stmt(decl)?.ok_or_else(|| unsupported("source form"))?;
    let decl = expression_of(&statement).ok_or_else(|| unsupported("source form"))?;
    // `my @a Z= EXPR`: the zip metaoperator over the assignment.
    if op == super::decl_compound_assign::ZIP_ASSIGN {
        return super::parsed_view::meta_assignment_eq_node(
            crate::ast::parsed::BracketedMeta::Zip,
            0,
            decl,
            convert_expr(rhs)?,
        );
    }
    compound_assignment_with_left(decl, &format!("{op}="), rhs)
}

/// [`compound_assignment_infix`] over an already converted left side.
pub(super) fn compound_assignment_with_left(
    left: RakuAstNode,
    op: &str,
    rhs: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let base_op = op.strip_suffix('=').unwrap_or(op);
    let meta_assign = match base_op.strip_prefix('R') {
        // `$x R-= $y` / `$x R= $y`: the reverse of the assignment infix.
        Some(inner) if !inner.is_empty() => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::MetaInfixReverse,
            fields: vec![node_field(
                None,
                if inner == "=" {
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::Assignment,
                        fields: vec![RakuAstField {
                            name: None,
                            value: RakuAstFieldValue::Adverb("item"),
                        }],
                    }
                } else {
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::MetaInfixAssign,
                        fields: vec![node_field(None, plain_infix(inner))],
                    }
                },
            )],
        },
        _ => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::MetaInfixAssign,
            fields: vec![node_field(None, plain_infix(base_op))],
        },
    };
    Ok(RakuAstNode {
        origin: Default::default(),
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
pub(super) fn dotty_assignment(target: &Expr, call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    dotty_assignment_with_left(convert_expr(target)?, call)
}

/// [`dotty_assignment`] over an already converted left side.
// Cost: O(n), n = size of the arguments.
pub(super) fn dotty_assignment_with_left(
    left: RakuAstNode,
    call: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    // `@a>>.=uc`: the hyper of the call, whose method carries the `.=`
    // dispatch (measured on 2026.09).
    if let Expr::HyperMethodCall {
        name,
        args,
        modifier,
        quoted,
        ..
    } = call
    {
        let mut method = method_call_postfix(name.as_str(), args, *modifier, *quoted)?;
        if modifier.is_none() {
            method
                .fields
                .push(leaf_field(Some("dispatch"), Value::str(".=".to_string())));
        }
        let right = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::MetaPostfixHyper,
            fields: vec![node_field(None, method)],
        };
        return Ok(dotty_node(left, right));
    }
    let right = match crate::ast::dotty_assign::method_call(call) {
        Some(crate::ast::dotty_assign::DottyCall {
            name,
            args,
            modifier,
            quoted,
        }) => method_call_postfix(name, args, modifier, quoted)?,
        // `$x .= &f` / `$x .= $m` / `$x .= "{...}"()`: a term as the method.
        None => super::dynamic_method::call_assign_postfix(call)?,
    };
    Ok(dotty_node(left, right))
}

/// `.= meth(args)` written with no target: `Term::TopicCall` over the method
/// call, which carries the `.=` dispatch (measured on 2026.09).
// Cost: O(n), n = size of the arguments.
pub(super) fn topic_dotty_assignment(call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let crate::ast::dotty_assign::DottyCall {
        name,
        args,
        modifier,
        quoted,
    } = crate::ast::dotty_assign::method_call(call).ok_or_else(|| unsupported("source form"))?;
    let mut method = method_call_postfix(name, args, modifier, quoted)?;
    method
        .fields
        .push(leaf_field(Some("dispatch"), Value::str(".=".to_string())));
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermTopicCall,
        fields: vec![node_field(None, method)],
    })
}

/// `ApplyDottyInfix(left, DottyInfix::CallAssign, right)`.
// Cost: O(1).
fn dotty_node(left: RakuAstNode, right: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyDottyInfix,
        fields: vec![
            node_field(Some("left"), left),
            node_field(
                Some("infix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::DottyInfixCallAssign,
                    fields: Vec::new(),
                },
            ),
            node_field(Some("right"), right),
        ],
    }
}

/// `TERM .meth` (`Spelling::DottyInfixCall`): the method call's postfix
/// application as rakudo's dotty infix,
/// `ApplyDottyInfix(left, DottyInfix::Call, Call::Method)`.
// Cost: O(n), n = size of the call.
fn dotty_infix_call(call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let _inspected = super::passthrough::Inspecting::enter();
    let mut node = convert_expr(call)?;
    if node.class != RakuAstClass::ApplyPostfix {
        return Ok(node);
    }
    let take = |node: &mut RakuAstNode, name: &str| {
        node.fields
            .iter()
            .position(|f| f.name == Some(name))
            .map(|i| node.fields.remove(i))
    };
    let (Some(mut left), Some(mut right)) =
        (take(&mut node, "operand"), take(&mut node, "postfix"))
    else {
        return Err(unsupported("a dotty infix call of another shape"));
    };
    left.name = Some("left");
    right.name = Some("right");
    Ok(RakuAstNode {
        origin: node.origin,
        class: RakuAstClass::ApplyDottyInfix,
        fields: vec![
            left,
            node_field(
                Some("infix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::DottyInfixCall,
                    fields: Vec::new(),
                },
            ),
            right,
        ],
    })
}

/// A `my`/`our`/`state` declaration with an optional simple type, as a
/// `VarDeclaration::Simple` statement; `is_binding` renders its right-hand side
/// as an `Initializer::Bind` (`:=`) rather than an `Initializer::Assign`.
pub(super) fn var_decl_statement(
    parts: VarDeclParts<'_>,
    is_binding: bool,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let VarDeclParts {
        name,
        anonymous,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        custom_traits,
        has_initializer,
        where_constraint,
        is_export,
        export_tags,
    } = parts;
    let export_trait = super::package_header::export_trait_value(is_export, export_tags);
    // Real `is`/`does` traits carry richer shape, deferred.
    // `constant X = 5` is a distinct raku node, not a scoped `my`.
    // mutsu marks it with a `__constant` pseudo-trait (plus a
    // `__constant_sigil` recording the declared sigil) and sets
    // `is_our` for the package-scoped default spelling.
    if !is_binding && custom_traits.iter().any(|(n, _)| n == "__constant") {
        return constant_declaration(
            name,
            expr,
            custom_traits,
            type_constraint,
            is_our,
            export_trait,
        );
    }
    let is_internal = |n: &str| {
        // The parser's own mark of a declaration whose initializer reads
        // the new binding; the lowering marks it again.
        n == "__init_sees_self"
            || n == crate::ast::method_assign_decl::METHOD_ASSIGN_DECL_TRAIT
            || n == crate::ast::shaped_decl::SHAPED_DECL
            || n == crate::ast::keyed_hash::IMPLICIT_VALUE_TYPE
            // The argument of `is dynamic(ARG)`, rendered with the trait.
            || n == crate::ast::DYNAMIC_TRAIT_ARGUMENT
            // The written colonpairs of the name, rendered in `desigilname`.
            || n == crate::ast::NAME_COLONPAIRS
            || (is_binding && n == crate::ast::bind_decl::SCALAR_BIND)
            || (is_binding && n == crate::ast::bind_decl::CODE_BIND)
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
    // feed helper call; without `has_initializer` it would read as none and
    // the feed would be dropped.
    let unmarked = !is_binding && !has_initializer;
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
    } else if let Expr::CompoundAssign { op, rhs, .. } = expr
        && op == crate::ast::method_assign_decl::DECL_INITIALIZER_OP
    {
        Some(Initializer::CallAssignMarker(rhs))
    } else {
        has_initializer.then_some(Initializer::Assign(expr))
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
    let mut decl = var_declaration(name, init, scope, type_name, twigil, None, anonymous)?;
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
        traits.push(decl_traits::dynamic_trait(custom_traits)?);
    }
    if let Some(export) = export_trait {
        // Like `is dynamic`, `is export` is a flag of the parsed declaration,
        // so only a lone one keeps the written trait order.
        if !traits.is_empty() {
            return Err(unsupported("`is export` beside other traits"));
        }
        traits.push(export);
    }
    decl_traits::insert(&mut decl, traits);
    super::name_colonpairs::apply(&mut decl, custom_traits)?;
    // `where` is the declaration's last field, after the initializer.
    if let Some(constraint) = where_constraint {
        decl.fields
            .push(node_field(Some("where"), convert_expr(constraint)?));
    }
    Ok(Some(statement_expression(decl)))
}

/// `my %h does R = EXPR`: the declaration with the role as a `Trait::Does`,
/// after its other traits and before the initializer. `decl` is the parser's
/// declaration without its initializer, `init` the assigned expression.
// Cost: O(n), n = size of the declaration and the role.
pub(super) fn does_declaration(
    decl: &Stmt,
    role: &Expr,
    init: Option<&Expr>,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let mut parts = var_decl_parts(decl)?;
    if let Some(init) = init {
        parts.has_initializer = true;
        parts.expr = init;
    }
    let Some(statement) = var_decl_statement(parts, false)? else {
        return Ok(None);
    };
    let Some(mut declaration) = expression_of(&statement) else {
        return Ok(None);
    };
    let does = Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TraitDoes,
        fields: vec![node_field(None, convert_expr(role)?)],
    }));
    match declaration
        .fields
        .iter_mut()
        .find(|f| f.name == Some("traits"))
    {
        Some(RakuAstField {
            value: RakuAstFieldValue::List(items),
            ..
        }) => items.push(does),
        _ => decl_traits::insert(&mut declaration, vec![does]),
    }
    Ok(Some(statement_expression(declaration)))
}

/// The fields of a `Stmt::VarDecl` that a `VarDeclaration::Simple` renders.
/// Taken apart in one place so the rendering function is not one more step of
/// the converter's recursion over `Stmt` (`make check-ast-walkers`).
pub(super) struct VarDeclParts<'a> {
    name: &'a str,
    anonymous: bool,
    expr: &'a Expr,
    type_constraint: &'a Option<String>,
    is_state: bool,
    is_our: bool,
    is_dynamic: bool,
    custom_traits: &'a [(String, Option<Expr>)],
    /// Whether the declaration has an explicit initializer.
    has_initializer: bool,
    /// The declaration's `where` expression.
    where_constraint: Option<&'a Expr>,
    /// Whether it is `is export`, and with which tags (`["DEFAULT"]` for the
    /// bare form): the parser keeps the trait as these two fields.
    is_export: bool,
    export_tags: &'a [String],
}

pub(super) fn var_decl_parts(stmt: &Stmt) -> Result<VarDeclParts<'_>, RuntimeError> {
    let Stmt::VarDecl {
        anonymous,
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        custom_traits,
        has_initializer,
        where_constraint,
        is_export,
        export_tags,
    } = stmt
    else {
        return Err(unsupported("variable declaration"));
    };
    Ok(VarDeclParts {
        name,
        anonymous: *anonymous,
        expr,
        type_constraint,
        is_state: *is_state,
        is_our: *is_our,
        is_dynamic: *is_dynamic,
        custom_traits,
        has_initializer: *has_initializer,
        where_constraint: where_constraint.as_deref(),
        is_export: *is_export,
        export_tags,
    })
}

/// A declaration's initializer: `= EXPR` or `:= EXPR`.
#[derive(Clone, Copy)]
pub(super) enum Initializer<'a> {
    Assign(&'a Expr),
    Bind(&'a Expr),
    CallAssign(&'a crate::ast::method_assign_decl::MethodAssignDecl),
    /// The call of a `.=` marker (`Expr::CompoundAssign` with op `.=`) that an
    /// expression-position declaration keeps as its initializer.
    CallAssignMarker(&'a Expr),
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
    anonymous: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, desigil) = split_sigil(name);
    // Provenance is carried by the declaration, never inferred from its name.
    // A `my` anonymous scalar keeps the default scope, which Rakudo omits.
    let is_anonymous = sigil == "$"
        && anonymous
        && (scope == Some("state") || scope.is_none())
        && twigil.is_none()
        && will_build.is_none();
    // Field order matches raku: scope, type, sigil, twigil, desigilname, traits,
    // initializer — each omitted when absent (scope defaults to `my`; twigil and
    // traits appear only on attributes).
    let mut fields = Vec::new();
    if let Some(s) = scope {
        fields.push(leaf_field(Some("scope"), Value::str(s.to_string())));
    }
    if let Some(t) = type_name.filter(|_| !is_anonymous) {
        fields.push(node_field(
            Some("type"),
            super::type_build::declaration_type_node(t)?,
        ));
    }
    fields.push(leaf_field(Some("sigil"), Value::str(sigil.to_string())));
    if let Some(t) = type_name.filter(|_| is_anonymous) {
        fields.push(node_field(
            Some("type"),
            super::type_build::declaration_type_node(t)?,
        ));
    }
    if let Some(tw) = twigil {
        fields.push(leaf_field(Some("twigil"), Value::str(tw.to_string())));
    }
    if !is_anonymous {
        fields.push(node_field(
            Some("desigilname"),
            name_from_identifier(desigil),
        ));
    }
    if let Some(wb) = will_build {
        // An attribute default (`has $.x = 5`) is a `Trait::WillBuild` (and also
        // an `initializer`, emitted below).
        let trait_node = RakuAstNode {
            origin: Default::default(),
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
                origin: Default::default(),
                class: RakuAstClass::InitializerAssign,
                fields: vec![node_field(None, convert_expr(e)?)],
            },
            Initializer::Bind(e) => RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::InitializerBind,
                fields: vec![node_field(None, convert_expr(e)?)],
            },
            Initializer::CallAssignMarker(call) => RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::InitializerCallAssign,
                fields: vec![node_field(
                    None,
                    match crate::ast::dotty_assign::method_call(call) {
                        Some(c) => call_method(c.name, c.args, None)?,
                        None => super::dynamic_method::call_assign_postfix(call)?,
                    },
                )],
            },
            Initializer::CallAssign(form) => RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::InitializerCallAssign,
                fields: vec![node_field(
                    None,
                    match &form.callee {
                        Some(callee) => super::dynamic_method::callee_postfix(callee, &form.args)?,
                        None => call_method(&form.method.resolve(), &form.args, None)?,
                    },
                )],
            },
        };
        fields.push(node_field(Some("initializer"), initializer));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: if is_anonymous {
            RakuAstClass::VarDeclarationAnonymous
        } else {
            RakuAstClass::VarDeclarationSimple
        },
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
    if let Some(name) = crate::qualified::type_capture_name(s)
        && !name.is_empty()
        && name_parts::identifier_segments(name).all(crate::parser::is_raku_identifier)
    {
        let mut parts = vec![name_parts::leading_empty()];
        parts.extend(name_parts::identifier_segments(name).map(name_parts::simple_part));
        return name_parts::name_from_parts(parts);
    }
    if name_parts::is_qualified_identifier(s) && !s.starts_with("::?") {
        return name_parts::qualified_name(s);
    }
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(s.to_string()))],
    }
}

pub(super) use super::type_build::{build_type_node, is_simple_type};

/// `my \x = 5` / `my Int \x := $s` -> `VarDeclaration::Term(type?, name,
/// initializer)`. A scoped one (`our \x`, `state \x`) stays the boundary.
// Cost: O(n), n = size of the initializer.
pub(super) fn term_declaration(
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
        match decl.call_assign {
            Some((method, args)) => RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::InitializerCallAssign,
                fields: vec![node_field(
                    None,
                    method_call_postfix(&method.resolve(), args, None, false)?,
                )],
            },
            None => RakuAstNode {
                origin: Default::default(),
                class: if decl.assigned {
                    RakuAstClass::InitializerAssign
                } else {
                    RakuAstClass::InitializerBind
                },
                fields: vec![node_field(None, term_initializer(decl)?)],
            },
        },
    ));
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarDeclarationTerm,
        fields,
    })
}

/// The initializer of a sigilless declaration. One bound to a declarator
/// (`my \x = my $y = 3`, `my \x = my $ = -3`) was split by the parser into the
/// inner declaration and a bind to its variable; rakudo renders the inner
/// declaration itself as the initializer. Its anonymous `$` gets back the name
/// the parser renamed it from.
// Cost: O(n), n = size of the initializer.
fn term_initializer(
    decl: &crate::ast::sigilless_decl::SigillessDecl<'_>,
) -> Result<RakuAstNode, RuntimeError> {
    convert_expr(decl.expr)
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
        origin: Default::default(),
        class: RakuAstClass::StatementExpression,
        fields: vec![node_field(Some("expression"), expr)],
    }
}

fn parenthesized_expression(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let semilist = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(convert_expr(expr)?))],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    })
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
        // A named `anon` package's registry name carries a site suffix.
        let name = name.split('\u{0}').next().unwrap_or_default();
        fields.push(node_field(Some("name"), name_from_identifier(name)));
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
            && t != crate::parser::LEADING_COLONS_TRAIT
            && t != crate::parser::DECLARE_HOW_TRAIT
            && t != crate::parser::ANON_DECL_TRAIT
            && !decl_traits::is_class_trait(t)
    })
}

/// The `expression` of a `Statement::Expression` node.
pub(super) fn expression_of(statement: &RakuAstNode) -> Option<RakuAstNode> {
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

/// Whether a package was declared `anon class Name` (the parser's
/// `ANON_DECL_TRAIT`; the registry name carries a site suffix).
// Cost: O(t), t = number of custom traits.
pub(super) fn is_anon_package(custom_traits: &[(String, Option<Expr>)]) -> bool {
    custom_traits
        .iter()
        .any(|(t, _)| t == crate::parser::ANON_DECL_TRAIT)
}

/// Whether a class was declared as `class ::Name`.
pub(super) fn has_leading_colons(custom_traits: &[(String, Option<Expr>)]) -> bool {
    custom_traits
        .iter()
        .any(|(t, _)| t == crate::parser::LEADING_COLONS_TRAIT)
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

/// The raw text of the keys of a `%h<a b>` subscript: the words joined by a
/// space, or `None` when the index is not a list of word literals.
// Cost: O(n), n = length of the keys.
pub(super) fn angle_key_text(index: &Expr) -> Option<String> {
    // `%h<<$k>>` / `%h«a b»`: the parser kept the raw text of the quote.
    if let Some(text) = spelled_angle_text(index) {
        return Some(text.to_string());
    }
    let word = |expr: &Expr| match expr {
        Expr::Literal(v) => Some(v.to_string_value()),
        _ => None,
    };
    match index {
        Expr::ArrayLiteral(items) => {
            let words = items.iter().map(word).collect::<Option<Vec<_>>>()?;
            Some(words.join(" "))
        }
        other => word(other),
    }
}

/// The raw text of an interpolating angle subscript (`%h<<$k>>`), when the
/// parser kept it.
fn spelled_angle_text(index: &Expr) -> Option<&str> {
    match index {
        Expr::Spelled(spelled) => match &spelled.spelling {
            Spelling::WordQuote {
                quotewords: true,
                text,
                ..
            }
            | Spelling::InterpolatingWords { text, .. } => Some(text),
            _ => None,
        },
        _ => None,
    }
}

/// `%h<a b>` as `ApplyPostfix(operand, Postcircumfix::LiteralHashIndex(index =>
/// QuotedString(processors => <words val>, segments => ("a b",))))`: rakudo
/// keeps the raw text of an angle subscript as one word-quote (measured on
/// 2026.09), where a `{...}` subscript is a `HashIndex` over a `SemiList`.
// Cost: O(n), n = nodes of the target plus the length of the keys.
pub(super) fn angle_subscript_node(
    target: &Expr,
    index: &Expr,
    assignee: Option<&Expr>,
    colonpairs: Vec<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    let quote = if spelled_angle_text(index).is_some() {
        convert_expr(index)?
    } else {
        let text = angle_key_text(index).ok_or_else(|| unsupported("angle subscript key"))?;
        word_quote(&text)
    };
    let mut index_node = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::PostcircumfixLiteralHashIndex,
        fields: vec![node_field(Some("index"), quote)],
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
    super::postfix_grouping::convert(target, index_node)
}

/// [`subscript_node`] over the dimensions of `@a[0;1]`: one statement of the
/// `SemiList` per dimension.
// Cost: O(n), n = nodes of the target and dimensions.
/// The dimensions of an `Expr::MultiDimIndex` as its subscript spells them.
/// The parser keeps `@a[|| @i]` (6.e: the list `@i` spread over the
/// dimensions) as the one dimension `@i`; rakudo 2026.09 spells it with the
/// `||` prefix, `SemiList(ApplyPrefix(Prefix::Multislice, @i))`. Several
/// dimensions are spelled as written.
// Cost: O(d), d = dimensions.
pub(super) fn splat_dimensions(dimensions: &[Expr]) -> std::borrow::Cow<'_, [Expr]> {
    match dimensions {
        [only] => std::borrow::Cow::Owned(vec![Expr::Unary {
            op: crate::token_kind::TokenKind::OrOr,
            expr: Box::new(only.clone()),
            word: false,
        }]),
        _ => std::borrow::Cow::Borrowed(dimensions),
    }
}

pub(super) fn subscript_dims_node(
    target: &Expr,
    dims: &[Expr],
    is_positional: bool,
    assignee: Option<&Expr>,
    colonpairs: Vec<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    // `Array[Int]` / `Hash[Str, Int]`: a subscript on a type name is a type
    // application, not a subscript (measured on 2026.09).
    if is_positional
        && assignee.is_none()
        && colonpairs.is_empty()
        && let [dim] = dims
        && target.as_bare_word().is_some()
        && let base = convert_expr(target)?
        && base.class == RakuAstClass::TypeSimple
    {
        let items = dim.comma_items();
        let mut args = Vec::with_capacity(items.len());
        for item in items {
            args.push(node_field(None, convert_expr(item)?));
        }
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TypeParameterized,
            fields: vec![
                node_field(Some("base-type"), base),
                node_field(
                    Some("args"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::ArgList,
                        fields: args,
                    },
                ),
            ],
        });
    }
    let mut statements = Vec::with_capacity(dims.len());
    for dim in dims {
        statements.push(node_field(None, statement_expression(convert_expr(dim)?)));
    }
    let semilist = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: statements,
    };
    let mut index_node = RakuAstNode {
        origin: Default::default(),
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
    super::postfix_grouping::convert(target, index_node)
}

pub(super) fn take_logical_tail_node(
    value: &Expr,
    op: &crate::token_kind::TokenKind,
    right: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(
                Some("left"),
                control_call("take", std::slice::from_ref(value))?,
            ),
            node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
            node_field(Some("right"), convert_expr(right)?),
        ],
    })
}

pub(super) fn signature_bind_node(
    signature: &Expr,
    rhs: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), convert_expr(signature)?),
            node_field(Some("infix"), plain_infix(":=")),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

pub(super) fn convert_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let mut node = convert_expr_at(expr)?;
    // A term the parser placed (ADR-10723 Stage 2).
    if let Some(span) = super::expr_origins::of(expr) {
        origin::set_span(&mut node, span);
        origin::quoted_text(&mut node, span);
        super::call_origins::postfix_call(&mut node, span);
    }
    super::infix_origins::derive(&mut node);
    Ok(node)
}

fn convert_expr_at(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    if let Some(leaf) = super::passthrough::expr_leaf(expr) {
        return Ok(leaf);
    }
    let _guards = super::passthrough::expr_guards(expr);
    convert_expr_node(expr)
}

fn convert_expr_node(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    if let Some(node) = super::deferred_name::convert(expr) {
        return node;
    }
    // The adverbs of a subscript lower from the class of the subscript.
    let adverbs = {
        let _inspected = super::passthrough::Inspecting::enter();
        subscript_adverb::convert(expr)
    };
    if let Some(node) = adverbs {
        return node;
    }
    match expr {
        // An expression the parser built in its RakuAST shape (ADR-10723
        // Stage 2): rendered, not reconstructed.
        Expr::Ast(node) => super::parsed_view::parsed_expr_node(node),
        // `$<name>`: the named capture of the last match.
        Expr::CaptureVar(name) => super::match_vars::convert(name),
        // `<a b  c>`: rakudo keeps the raw text of a word list, whitespace
        // included, as one word-quote (ADR-12199).
        Expr::Spelled(spelled) => match &spelled.spelling {
            // A term the parser placed (ADR-10723 Stage 2): its node with the
            // origin the parser recorded.
            Spelling::Origin(span) => {
                let mut node = convert_expr(&spelled.expr)?;
                origin::set_span(&mut node, *span);
                origin::quoted_text(&mut node, *span);
                super::call_origins::postfix_call(&mut node, *span);
                Ok(node)
            }
            Spelling::SourceIdentifier => written_identifier(&spelled.expr),
            Spelling::ArraySections(sections) => {
                let fields = sections
                    .iter()
                    .map(|items| {
                        if items.is_empty() {
                            return Ok(node_field(
                                None,
                                RakuAstNode {
                                    origin: Default::default(),
                                    class: RakuAstClass::StatementEmpty,
                                    fields: Vec::new(),
                                },
                            ));
                        }
                        let expression = match items.as_slice() {
                            [single] => convert_expr(single)?,
                            _ => comma_list_node(items)?,
                        };
                        let statement = if expression.class == RakuAstClass::StatementExpression {
                            expression
                        } else {
                            statement_expression(expression)
                        };
                        Ok(node_field(None, statement))
                    })
                    .collect::<Result<Vec<_>, RuntimeError>>()?;
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::CircumfixArrayComposer,
                    fields: vec![node_field(
                        None,
                        RakuAstNode {
                            origin: Default::default(),
                            class: RakuAstClass::SemiList,
                            fields,
                        },
                    )],
                })
            }
            Spelling::LabelledBlock => {
                let Expr::DoBlock { body, label, .. } = &spelled.expr else {
                    return Err(unsupported("labelled block spelling without a block"));
                };
                let mut fields = label_fields(label);
                fields.push(node_field(Some("expression"), block_node(body)?));
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::StatementExpression,
                    fields,
                })
            }
            Spelling::LabelTerm(name) => {
                let Expr::Literal(value) = &spelled.expr else {
                    return Err(unsupported("label spelling without a label value"));
                };
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TermName,
                    fields: vec![
                        node_field(None, name_from_identifier(name)),
                        leaf_field(Some("label-value"), value.clone()),
                    ],
                })
            }
            Spelling::ImportedTerm => {
                let Expr::BareWord(name) = &spelled.expr else {
                    return Err(unsupported("imported term spelling without a value lookup"));
                };
                Ok(super::term_name::imported(name))
            }
            Spelling::Words(text) => Ok(word_quote(text)),
            Spelling::ParenthesizedDeclaratorAdverb => parenthesized_expression(&spelled.expr),
            Spelling::BracketTraitArgument | Spelling::ParenSections(_) => {
                convert_expr(&spelled.expr)
            }
            Spelling::Heredoc { stop, body } => heredoc_node(&spelled.expr, stop, *body),
            Spelling::WordQuote {
                quotewords,
                val,
                text,
            } => Ok(word_quote_with(*quotewords, *val, text)),
            Spelling::InterpolatingWords {
                val,
                text,
                delimiter,
                interpolates,
            } => super::word_quote::convert(*val, text, *delimiter, *interpolates),
            Spelling::EmptyStatement => Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StatementEmpty,
                fields: Vec::new(),
            }),
            Spelling::PrefixCall => {
                let Expr::Unary { op, expr, .. } = &spelled.expr else {
                    return Err(unsupported("prefix call spelling without its prefix"));
                };
                let name = match op {
                    crate::token_kind::TokenKind::Bang => "not",
                    crate::token_kind::TokenKind::Question => "so",
                    _ => return Err(unsupported("prefix call spelling of an unknown prefix")),
                };
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::CallName,
                    fields: vec![
                        node_field(Some("name"), name_from_identifier(name)),
                        node_field(Some("args"), arg_list(std::slice::from_ref(&**expr))?),
                    ],
                })
            }
            Spelling::NegatedInfix => {
                let Expr::Unary {
                    op: crate::token_kind::TokenKind::Bang,
                    expr,
                    word: false,
                } = &spelled.expr
                else {
                    return Err(unsupported(
                        "negated infix spelling without a negated operator",
                    ));
                };
                let Expr::Binary {
                    left, op, right, ..
                } = expr.as_ref()
                else {
                    return Err(unsupported(
                        "negated infix spelling without a binary operator",
                    ));
                };
                super::meta_infix::convert(
                    "!",
                    &token_kind_to_op_name(op),
                    left,
                    right,
                    &spelled.expr,
                )
            }
            Spelling::NegatedSetInfix(written) => {
                let Some((left, _, right)) = negated_set_operands(&spelled.expr) else {
                    return Err(unsupported("negated set spelling without its relation"));
                };
                super::meta_infix::convert("!", written, left, right, &spelled.expr)
            }
            Spelling::NegatedSetGlyph(glyph) => {
                let Some((left, _, right)) = negated_set_operands(&spelled.expr) else {
                    return Err(unsupported("negated set glyph without its relation"));
                };
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::ApplyInfix,
                    fields: vec![
                        node_field(Some("left"), convert_expr(left)?),
                        node_field(Some("infix"), plain_infix(glyph)),
                        node_field(Some("right"), convert_expr(right)?),
                    ],
                })
            }
            Spelling::LeftExclusiveSequence(written) => {
                let Expr::MethodCall { target, .. } = &spelled.expr else {
                    return Err(unsupported("left-exclusive sequence without its skip"));
                };
                left_exclusive_sequence_node(written, target)
            }
            Spelling::LoopStepComma => {
                let Expr::DoBlock { body, .. } = &spelled.expr else {
                    return Err(unsupported("loop step comma without its execution block"));
                };
                let mut expressions = Vec::with_capacity(body.len());
                for statement in body {
                    let Stmt::Expr(expression) = statement else {
                        return Err(unsupported("non-expression loop step item"));
                    };
                    expressions.push(expression.clone());
                }
                comma_list_node(&expressions)
            }
            Spelling::BareStatement => super::bare_prefix::bare_statement_node(&spelled.expr),
            Spelling::PackageIndirect { package, key } => {
                super::symbolic_deref::convert_package_indirect(package, key)
            }
            Spelling::NamedSubscript { .. }
            | Spelling::ConflictingSubscript { .. }
            | Spelling::ConditionalMultidimDelete { .. } => {
                Err(unsupported("subscript adverb source form"))
            }
            Spelling::CompositionWhatever(source) => {
                super::composition_source::convert(source, &spelled.expr)
            }
            Spelling::DottyInfixCall => dotty_infix_call(&spelled.expr),
            Spelling::CompilerVar(written) => {
                super::compiler_var::convert_folded(written, &spelled.expr)
            }
        },
        // `pi` / `e` / `tau` are setting terms in raku; the parser folds them to
        // numeric literals, so recover the term from the source spelling kept
        // for a statement-level literal, or from the exact constant otherwise.
        Expr::LiteralSrc(v, src) if math_constant_spelling(v, Some(&**src)).is_some() => Ok(
            term_name_node(math_constant_spelling(v, Some(&**src)).unwrap_or_default()),
        ),
        Expr::Literal(v) if math_constant_spelling(v, None).is_some() => Ok(term_name_node(
            math_constant_spelling(v, None).unwrap_or_default(),
        )),
        // A constant colonpair statement keeps its source for the sink warning
        // (`:a(7)` folded to a `Pair`); its node is the written colonpair.
        Expr::LiteralSrc(v, src)
            if matches!(v.view(), ValueView::Pair(..))
                && let Some(pair) = crate::parser::colonpair_source_expr(src) =>
        {
            convert_expr(&pair)
        }
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => convert_literal(v),
        // A CORE term keyword the parser left shadowable (#9047) is the same
        // node as the plain keyword; lowering decides again whether it is.
        // `last` / `next` / `redo` in expression position (`COND or next`):
        // the same bare call as the statement form, over the label or value.
        Expr::ControlFlow {
            kind,
            label,
            value,
            take_value: false,
        } => {
            let name = match kind {
                crate::ast::ControlFlowKind::Last => "last",
                crate::ast::ControlFlowKind::Next => "next",
                crate::ast::ControlFlowKind::Redo => "redo",
            };
            match (label, value) {
                (Some(label), None) => Ok(labelled_control_call(name, label)),
                (None, Some(value)) => control_call(name, std::slice::from_ref(value.as_ref())),
                (None, None) => control_call(name, &[]),
                (Some(_), Some(_)) => Err(unsupported("a labelled loop control with a value")),
            }
        }
        // `{*}` in a proto body.
        _ if expr.is_onlystar_dispatch() => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::OnlyStar,
            fields: Vec::new(),
        }),
        // `now` is `Term::Named`, not a call.
        Expr::Call { name, args, .. } if args.is_empty() && name.as_str() == "now" => {
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::TermNamed,
                fields: vec![leaf_field(None, Value::str("now".to_string()))],
            })
        }
        Expr::Subst { .. } | Expr::NonDestructiveSubst { .. } | Expr::Transliterate { .. } => {
            super::substitution::convert(expr)
        }
        Expr::RegexLiteral { value, tree } => {
            let mut node = quoted_regex_node(tree)?;
            if matches!(value.view(), ValueView::RegexWithAdverbs(_)) {
                node.class = RakuAstClass::QuotedRegexRx;
            }
            Ok(node)
        }
        Expr::MatchRegexTree { tree, .. } => quoted_regex_node(tree),
        Expr::MatchRegexDynamicAdverbs { value, .. } => value
            .regex_source_tree()
            .ok_or_else(|| unsupported("dynamic match regex without a source tree"))
            .and_then(quoted_regex_node),
        Expr::Call { name, args, .. } | Expr::UserRoutineCall { name, args, .. } => {
            if name.as_str() == "__object_hash" {
                let mut fields = Vec::with_capacity(2);
                if args.len() == 1 {
                    fields.push(node_field(None, convert_expr(&args[0])?));
                } else if !args.is_empty() {
                    fields.push(node_field(None, comma_list_node(args)?));
                }
                fields.push(leaf_field(Some("object-hash"), Value::truth(true)));
                return Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::CircumfixHashComposer,
                    fields,
                });
            }
            if name.as_str() == "require"
                && let Some(require) = super::use_stmt::convert_require(args)?
            {
                return Ok(require);
            }
            let listop = matches!(
                expr,
                Expr::Call { listop: true, .. } | Expr::UserRoutineCall { listop: true, .. }
            );
            if let Some(prefix) = super::prefix_call::convert(name.as_str(), args) {
                return prefix;
            }
            if let Some(stub) = stub_node(name.as_str(), args) {
                return stub;
            }
            if let Some(atomic) = super::atomic_op::convert(name.as_str(), args) {
                return atomic;
            }
            if let Some(hyper) = super::hyper_prefix::convert(name.as_str(), args) {
                return hyper;
            }
            if name.as_str() == crate::ast::quote_processing::FORMAT {
                return super::quoted_string::convert_format(args);
            }
            if let Some(words) = super::word_quote::convert_words_atom(name.as_str(), args) {
                return words;
            }
            if name.as_str() == "__mutsu_unknown_backslash_escape"
                && let [Expr::Literal(escape)] = args.as_slice()
            {
                return Err(RuntimeError::typed_msg(
                    "X::Backslash::UnrecognizedSequence",
                    format!(
                        "Unrecognized backslash sequence: \\{}",
                        escape.to_string_value()
                    ),
                ));
            }
            if is_desugar_marker(name.as_str()) {
                return Err(desugared(name.as_str()));
            }
            if let Some(coercion) = super::type_call::convert(name.as_str(), args)? {
                return Ok(coercion);
            }
            if let Some(op) = nqp_op(name.as_str()) {
                let mut fields = vec![leaf_field(None, Value::str(op.to_string()))];
                for arg in args.iter().filter(|a| !is_injected_named_arg(a)) {
                    fields.push(node_field(None, convert_expr(arg)?));
                }
                return Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::Nqp,
                    fields,
                });
            }
            named_routine_call(expr, name.as_str(), args, listop)
        }
        Expr::Var(name) => {
            if let Some(capture) = super::match_vars::convert_positional(name) {
                return Ok(capture);
            }
            // The parser's re-read of an anonymous `my $` declaration in the
            // loose tail of `my $ = 5 andthen ...`; `decl_tail` swaps the
            // declaration itself in for it.
            if name == "__ANON_STATE#" {
                return Ok(anonymous_declaration(None, "$", None));
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
            if let Some(node) = super::compiler_var::convert_var(name) {
                return Ok(node);
            }
            Ok(var_lexical("$", name))
        }
        // `$::($n)` / `@::($n)` -> `Var::Package` over a dynamic name.
        Expr::SymbolicDeref { sigil, expr } => super::symbolic_deref::convert(sigil, expr),
        Expr::IndirectCodeLookup {
            package,
            name,
            caller,
        } => super::symbolic_deref::convert_indirect_code(package, name, *caller),
        // `::("x")` / `::($name)` ->
        // `Term::Name(Name(Part::Empty.new, Part::Expression(EXPR)))`, the
        // leading `::` being an empty name edge (measured on 2026.09). The
        // parser keeps this as an IndirectTypeLookup, so preserving the
        // expression part is necessary for `.AST` and for a later EVAL round
        // trip; rendering it as a static Name would change the lookup mode.
        Expr::IndirectTypeLookup(inner) => {
            let part = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::NamePartExpression,
                fields: vec![node_field(None, convert_expr(inner)?)],
            };
            let name = name_parts::name_from_parts(vec![
                name_parts::leading_empty(),
                Value::rakuast(Box::new(part)),
            ]);
            Ok(RakuAstNode {
                origin: Default::default(),
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
                origin: Default::default(),
                class: RakuAstClass::NamePartExpression,
                fields: vec![node_field(None, convert_expr(head)?)],
            };
            let mut parts = vec![name_parts::leading_empty(), Value::rakuast(Box::new(part))];
            parts.extend(name_parts::tail_parts(tail));
            if *trailing {
                parts.push(name_parts::trailing_empty());
            }
            Ok(RakuAstNode {
                origin: Default::default(),
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
                    origin: Default::default(),
                    class: RakuAstClass::TermName,
                    fields: vec![node_field(None, name)],
                })
            } else {
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::CallName,
                    fields: vec![node_field(Some("name"), name)],
                })
            }
        }
        // Calling a term `$f(1, 2)` -> ApplyPostfix(operand, Call::Term(args)).
        Expr::CallOn { target, args } => {
            // raku omits `args` for an argument-less `$f()` / `&f()` / `$f.()`.
            let mut call_term = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::CallTerm,
                fields: Vec::new(),
            };
            if !args.is_empty() {
                call_term
                    .fields
                    .push(node_field(Some("args"), arg_list(args)?));
            }
            super::postfix_grouping::convert(target, call_term)
        }
        // The `*` whatever term (a *value*, not a priming argument).
        Expr::Whatever => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TermWhatever,
            fields: Vec::new(),
        }),
        // A `*` that participates in Whatever-priming (ADR-0033 Phase 2): the
        // left operand of `* + 1` is `WhateverCode::Argument`, not `Term::Whatever`.
        Expr::WhateverArg => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::WhateverCodeArgument,
            fields: Vec::new(),
        }),
        // `**` — read direction only; priming for `**` is out of scope (ADR-0033 §1).
        Expr::HyperWhatever => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TermHyperWhatever,
            fields: Vec::new(),
        }),
        // A `WhateverCurry` marker carries no RakuAST node of its own — Rakudo's
        // tree has no priming-scope wrapper (ADR-0033 §5); the scope is derived
        // structurally at lowering. Convert straight through to the body.
        Expr::WhateverCurry(body) => Ok(super::thunk::mark(convert_expr(body)?)),
        // `do { … }` -> `StatementPrefix::Do(Block)`. A labelled do stays the boundary.
        Expr::DoBlock {
            body,
            label,
            origin,
        } => {
            // `StatementPrefixDo` round-trips back through `lower.rs` as a
            // genuine source block. A desugar's node is not one, so converting
            // it would hand back a node with block semantics (`let`/`temp`
            // resolution) the original never had -- GH-7635.
            if origin != &crate::ast::DoBlockOrigin::SourceBlock {
                // `120 = 3`: the written assignment to an immutable literal.
                if let Some((literal, rhs)) = crate::parser::literal_assign_ro_parts(expr) {
                    return assignment_around(convert_expr(literal)?, false, rhs);
                }
                return Err(unsupported("desugared do-block"));
            }
            let expression = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StatementPrefixDo,
                fields: vec![node_field(None, block_node(body)?)],
            };
            if label.is_some() {
                let mut fields = label_fields(label);
                fields.push(node_field(Some("expression"), expression));
                Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::StatementExpression,
                    fields,
                })
            } else {
                Ok(expression)
            }
        }
        // `try { … }` -> `StatementPrefix::Try(Block)`. A `CATCH` block stays the
        // boundary.
        Expr::Try { body, catch } => {
            if catch.is_some() {
                return Err(unsupported("try with a CATCH block"));
            }
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StatementPrefixTry,
                fields: vec![node_field(None, block_node(body)?)],
            })
        }
        // `gather { … }` -> `StatementPrefix::Gather(Block)`.
        Expr::Gather(body) => Ok(RakuAstNode {
            origin: Default::default(),
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
                origin: Default::default(),
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
                ..
            } => Ok(RakuAstNode {
                origin: Default::default(),
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
            let mut node = bareword::convert(name).expect("just checked");
            if matches!(
                node.class,
                RakuAstClass::CallName | RakuAstClass::CallNameWithoutParentheses
            ) {
                node.fields.push(super::declared_routines::bareword_field());
            }
            Ok(node)
        }
        // A qualified name that did not resolve in this parse scope is still
        // a valid Raku name term (`A::B`). Rakudo represents it as an
        // argument-less Call::Name, including when it is later used as a
        // method-call operand; the failed lookup remains catchable at runtime.
        Expr::BareWord(name) if name_parts::is_qualified_identifier(name) => {
            let mut node = call_name(name, &[], false)?;
            node.fields.push(super::declared_routines::bareword_field());
            Ok(node)
        }
        // Any other unresolved word: rakudo's `.AST` refuses it, and an
        // argument-less call renders the same as `foo()`, so a hidden field
        // keeps lowering from mistaking one for the other.
        Expr::BareWord(name)
            if !name.is_empty()
                && name
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''))
                && name.starts_with(|c: char| c.is_alphabetic() || c == '_') =>
        {
            let mut node = call_name(name, &[], false)?;
            node.fields.push(super::declared_routines::bareword_field());
            Ok(node)
        }
        // `my $x = BEGIN { 1 }`: the phaser block as an expression is the same
        // `StatementPrefix::Phaser::*` node a phaser statement wraps.
        Expr::PhaserExpr { kind, body } => {
            let class =
                phaser_class(kind).ok_or_else(|| unsupported("PRE/POST phaser expression"))?;
            Ok(RakuAstNode {
                origin: Default::default(),
                class,
                fields: vec![node_field(None, block_node(body)?)],
            })
        }
        // `once { ... }` -> `StatementPrefix::Once(Block)`.
        Expr::Once { body } => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementPrefixOnce,
            fields: vec![node_field(None, block_node(body)?)],
        }),
        // A declaration in expression position (`class { }`, `role { }`,
        // `push my @u, 1`): the parser wraps the declaration in a `DoStmt`,
        // rakudo has the node itself.
        Expr::DoStmt(stmt) if matches!(stmt.as_ref(), Stmt::Expr(_)) => {
            let Stmt::Expr(expr) = stmt.as_ref() else {
                unreachable!()
            };
            convert_expr(expr)
        }
        Expr::DoStmt(stmt)
            if matches!(
                stmt.as_ref(),
                Stmt::ClassDecl { .. }
                    | Stmt::Package { .. }
                    | Stmt::RoleDecl { .. }
                    | Stmt::VarDecl { .. }
                    | Stmt::SubDecl { .. }
                    | Stmt::ProtoDecl { .. }
                    | Stmt::MethodDecl { .. }
                    | Stmt::EnumDecl { .. }
            ) || matches!(
                stmt.as_ref(),
                Stmt::Ast(node)
                    if matches!(node.as_ref(), crate::ast::parsed::ParsedStmt::BindDecl(_) | crate::ast::parsed::ParsedStmt::SigillessDecl(_)
                        | crate::ast::parsed::ParsedStmt::AttributeGroup(_) | crate::ast::parsed::ParsedStmt::AttributeColonArglist(_)
                        | crate::ast::parsed::ParsedStmt::MethodAssignDecl(_) | crate::ast::parsed::ParsedStmt::SignatureBind { .. } | crate::ast::parsed::ParsedStmt::SignatureDecl(_) | crate::ast::parsed::ParsedStmt::Temporize(_))
            ) =>
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
        // `is my $k.defined, ...`: a declaration with a postfix (or any loose
        // tail) in expression position is the same split as at statement level.
        Expr::DoStmt(stmt)
            if matches!(stmt.as_ref(), Stmt::Ast(node)
                if matches!(node.as_ref(), crate::ast::parsed::ParsedStmt::DeclTail { .. })) =>
        {
            let Stmt::Ast(node) = stmt.as_ref() else {
                unreachable!()
            };
            let crate::ast::parsed::ParsedStmt::DeclTail { head, tail } = node.as_ref() else {
                unreachable!()
            };
            super::decl_tail::convert(head, tail)
        }
        // A comma list of declarations kept in its RakuAST shape.
        Expr::DoStmt(stmt)
            if matches!(stmt.as_ref(), Stmt::Ast(node)
                if matches!(node.as_ref(), crate::ast::parsed::ParsedStmt::CommaDecls(_))) =>
        {
            let Stmt::Ast(node) = stmt.as_ref() else {
                unreachable!()
            };
            let crate::ast::parsed::ParsedStmt::CommaDecls(declarations) = node.as_ref() else {
                unreachable!()
            };
            comma_declarations_node(declarations)
        }
        // `do for ... { }` / `do if ... { }` / `do given ... { }`: the loop or
        // conditional statement under `StatementPrefix::Do`.
        Expr::DoStmt(stmt) if is_do_statement(stmt) => {
            let statement =
                convert_stmt(stmt)?.ok_or_else(|| unsupported("empty `do` statement"))?;
            // `hyper for` / `race for` / `lazy for` are expressions of their own:
            // the loop, with no `do` prefix.
            let written = match stmt.as_ref() {
                Stmt::Ast(node) => node.inner_stmt().unwrap_or(stmt),
                other => other,
            };
            if matches!(
                written,
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
                origin: Default::default(),
                class: RakuAstClass::StatementPrefixDo,
                fields: vec![node_field(None, statement)],
            })
        }
        Expr::DoStmt(stmt) if is_modifier_statement(stmt) => {
            convert_stmt(stmt)?.ok_or_else(|| unsupported("empty modified expression"))
        }
        // `()` -> `Circumfix::Parentheses(SemiList.new)`: no statement inside.
        Expr::Grouped(inner) if matches!(inner.as_ref(), Expr::ArrayLiteral(items) if items.is_empty()) => {
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::CircumfixParentheses,
                fields: vec![node_field(
                    None,
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::SemiList,
                        fields: Vec::new(),
                    },
                )],
            })
        }
        // `(1; 2)`: one statement per section.
        Expr::Grouped(inner)
            if matches!(inner.as_ref(), Expr::Spelled(spelled)
                if matches!(spelled.spelling, Spelling::ParenSections(_))) =>
        {
            let Expr::Spelled(spelled) = inner.as_ref() else {
                unreachable!("guarded above");
            };
            let Spelling::ParenSections(sections) = &spelled.spelling else {
                unreachable!("guarded above");
            };
            let fields = sections
                .iter()
                .map(|items| {
                    let expression = match items.as_slice() {
                        [single] => convert_expr(single)?,
                        _ => comma_list_node(items)?,
                    };
                    let statement = if expression.class == RakuAstClass::StatementExpression {
                        expression
                    } else {
                        statement_expression(expression)
                    };
                    Ok(node_field(None, statement))
                })
                .collect::<Result<Vec<_>, RuntimeError>>()?;
            let semilist = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::SemiList,
                fields,
            };
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::CircumfixParentheses,
                fields: vec![node_field(None, semilist)],
            })
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
                other => {
                    let node = convert_expr(other)?;
                    if node.class == RakuAstClass::StatementRequire {
                        node
                    } else {
                        statement_expression(node)
                    }
                }
            };
            let semilist = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, content)],
            };
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::CircumfixParentheses,
                fields: vec![node_field(None, semilist)],
            })
        }
        // `($x := EXPR)`, a term's assignment, and a `*` lvalue as an expression
        // -> the same `ApplyInfix` as the statement form.
        Expr::AssignExpr {
            name,
            expr,
            is_bind,
        } => {
            // A bareword target is a term (spelled by its term key,
            // ADR-12441): rakudo's left side is the `Term::Name`, and `=` is
            // a list assignment (no `:item`).
            if let Some(term) = crate::runtime::term_names::term_spelling(name) {
                let left = RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TermName,
                    fields: vec![node_field(None, name_from_identifier(term))],
                };
                let infix = if *is_bind {
                    plain_infix(":=")
                } else {
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::Assignment,
                        fields: Vec::new(),
                    }
                };
                return Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::ApplyInfix,
                    fields: vec![
                        node_field(Some("left"), left),
                        node_field(Some("infix"), infix),
                        node_field(Some("right"), convert_expr(expr)?),
                    ],
                });
            }
            if *is_bind {
                return bind_infix(name, expr);
            }
            // `$x = EXPR` is the typed assignment node a `.AST` parse keeps
            // (ADR-10723 Stage 2).
            Err(unsupported("source form"))
        }
        Expr::ArrayVar(name) => {
            if is_desugar_marker(name) && !crate::ast::anon_state::is_array(name) {
                return Err(desugared(name));
            }
            if name == "_" && super::underscore_bindings::declares_placeholder(true) {
                return Ok(RakuAstNode {
                    origin: Default::default(),
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
            if name == "_" && super::underscore_bindings::declares_placeholder(false) {
                return Ok(RakuAstNode {
                    origin: Default::default(),
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
            if matches!(name.as_str(), "?BLOCK" | "?ROUTINE") {
                return Ok(RakuAstNode {
                    origin: Default::default(),
                    class: if name == "?BLOCK" {
                        RakuAstClass::VarCompilerBlock
                    } else {
                        RakuAstClass::VarCompilerRoutine
                    },
                    fields: Vec::new(),
                });
            }
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
            form,
        } if matches!(&**left,
            Expr::Literal(v) | Expr::LiteralSrc(v, _) if matches!(v.view(), ValueView::Str(_))) =>
        {
            // A colonpair (`:a(1)`, `:a`, `:!a`, `:$a`) is its own node class;
            // one the helpers cannot render keeps the `=>` form below.
            if let Some(pair) = colonpair_spelling(expr, *form) {
                return Ok(pair);
            }
            let (Expr::Literal(v) | Expr::LiteralSrc(v, _)) = &**left else {
                unreachable!("guarded above")
            };
            let ValueView::Str(key) = v.view() else {
                unreachable!("guarded above")
            };
            Ok(RakuAstNode {
                origin: Default::default(),
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
        Expr::Binary {
            left, op, right, ..
        } if is_list_infix(op) => {
            let mut operands = left.flatten_binary_chain(op);
            operands.push(right);
            let mut nodes = Vec::with_capacity(operands.len());
            for e in operands {
                nodes.push(Value::rakuast(Box::new(convert_expr(e)?)));
            }
            Ok(RakuAstNode {
                origin: Default::default(),
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
        // `^N` is the prefix spelling of `0 ..^ N`.
        Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::DotDotCaret,
            right,
            form: crate::ast::BinaryForm::CaretPrefix,
        } if matches!(&**left, Expr::Literal(v) if matches!(v.view(), ValueView::Int(0))) => {
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::ApplyPrefix,
                fields: vec![
                    node_field(
                        Some("prefix"),
                        operator_node(RakuAstClass::Prefix, &crate::token_kind::TokenKind::Caret),
                    ),
                    node_field(Some("operand"), convert_expr(right)?),
                ],
            })
        }
        Expr::Binary {
            left, op, right, ..
        } => {
            // `lower` reads the operands of an infix by their node class.
            let _inspected = super::passthrough::Inspecting::enter();
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(Some("left"), convert_expr(left)?),
                    node_field(
                        Some("infix"),
                        // `but` and `does` are `Mixin` infixes in raku.
                        operator_node(
                            if matches!(op, crate::token_kind::TokenKind::Ident(name) if name == "but" || name == "does")
                            {
                                RakuAstClass::Mixin
                            } else {
                                RakuAstClass::Infix
                            },
                            op,
                        ),
                    ),
                    node_field(Some("right"), convert_expr(right)?),
                ],
            })
        }
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
        } => hyper_infix(left, op, right, *dwim_left, *dwim_right),
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
                    origin: Default::default(),
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
                origin: Default::default(),
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(Some("left"), convert_expr(left)?),
                    node_field(
                        Some("infix"),
                        RakuAstNode {
                            origin: Default::default(),
                            class: RakuAstClass::MetaInfixHyper,
                            fields: hyper_fields,
                        },
                    ),
                    node_field(Some("right"), convert_expr(right)?),
                ],
            })
        }
        Expr::Unary { op, expr, word } => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(
                    Some("prefix"),
                    // `so EXPR` / `not EXPR` are the `?` / `!` the parser
                    // folds them into.
                    match (word, op) {
                        (true, crate::token_kind::TokenKind::Question) => RakuAstNode {
                            origin: Default::default(),
                            class: RakuAstClass::Prefix,
                            fields: vec![leaf_field(None, Value::str("so".to_string()))],
                        },
                        (true, crate::token_kind::TokenKind::Bang) => RakuAstNode {
                            origin: Default::default(),
                            class: RakuAstClass::Prefix,
                            fields: vec![leaf_field(None, Value::str("not".to_string()))],
                        },
                        // `||` only ever prefixes a subscript dimension
                        // (`splat_dimensions`).
                        (false, crate::token_kind::TokenKind::OrOr) => RakuAstNode {
                            origin: Default::default(),
                            class: RakuAstClass::PrefixMultislice,
                            fields: Vec::new(),
                        },
                        _ => operator_node(RakuAstClass::Prefix, op),
                    },
                ),
                node_field(Some("operand"), convert_expr(expr)?),
            ],
        }),
        Expr::PostfixOp { op, expr } => super::postfix_grouping::convert(expr, postfix_node(op)),
        // `COND ?? THEN !! ELSE` -> Ternary(condition, then, else). Note raku
        // constant-folds a literal-condition ternary (`1 ?? 2 !! 3` -> IntLiteral(2));
        // mutsu does not, a documented divergence (the const-fold open question).
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Ok(RakuAstNode {
            origin: Default::default(),
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
            sugar,
        } => {
            // `$(a; b)`: several statements in item context.
            if let Some(stmts) = crate::parser::item_statements_parts(expr) {
                return super::contextualizer::convert_statements(stmts);
            }
            // `lazy EXPR` / `hyper EXPR` / `race EXPR` and `$[1, 2]` / `${...}`:
            // sugar for the call, with a node of its own.
            if *sugar && args.is_empty() && modifier.is_none() {
                if let Some(prefix) = super::prefix_call::convert_method(name.as_str(), target) {
                    return prefix;
                }
                if name.as_str() == "item" && !matches!(&**target, Expr::Var(topic) if topic == "_")
                {
                    return super::contextualizer::convert_item_call(target);
                }
                if name.as_str() == "list" && !matches!(&**target, Expr::Var(topic) if topic == "_")
                {
                    return super::contextualizer::convert_list_call(target);
                }
            }
            let postfix = method_call_postfix(name.as_str(), args, *modifier, *quoted)?;
            // `.say` / `.foo(1)`: a call on the topic written without an
            // invocant is `Term::TopicCall` over the bare call.
            if *sugar && matches!(&**target, Expr::Var(topic) if topic == "_") {
                return Ok(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TermTopicCall,
                    fields: vec![node_field(None, postfix)],
                });
            }
            super::postfix_grouping::convert(target, postfix)
        }
        // A dynamically interpolated quoted method name (`$value."$name"()`)
        // keeps the name as a QuotedString expression.  It is distinct from an
        // unquoted dynamic dispatch: Rakudo still exposes Call::QuotedMethod,
        // whose name child contains the interpolation segments.
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier,
            quoted: true,
        } if matches!(modifier, None | Some('?' | '+' | '*' | '!')) => {
            super::postfix_grouping::convert(
                target,
                call_quoted_method_expr(convert_expr(name_expr)?, args, *modifier)?,
            )
        }
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
            let inner = if !quoted
                && modifier.is_none()
                && let Some(result) = super::hyper_subscript::convert(name.as_str(), args)
            {
                result?
            } else {
                method_call_postfix(name.as_str(), args, *modifier, *quoted)?
            };
            let hyper = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::MetaPostfixHyper,
                fields: vec![node_field(None, inner)],
            };
            super::postfix_grouping::convert(target, hyper)
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
        // `[]`: a composer of an empty semilist, with no statement at all.
        Expr::BracketArray(items, false) if items.is_empty() => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::CircumfixArrayComposer,
            fields: vec![node_field(
                None,
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::SemiList,
                    fields: Vec::new(),
                },
            )],
        }),
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
                origin: Default::default(),
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, statement)],
            };
            Ok(RakuAstNode {
                origin: Default::default(),
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
                origin: Default::default(),
                class: RakuAstClass::ArgList,
                fields: vec![node_field(None, convert_expr(expr)?)],
            };
            Ok(RakuAstNode {
                origin: Default::default(),
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
            is_positional: false,
            spelling: crate::ast::IndexSpelling::Angle,
        } if angle_key_text(index).is_some() => {
            angle_subscript_node(target, index, None, Vec::new())
        }
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => subscript_node(target, index, *is_positional, None, Vec::new()),
        // `@a[0;1]` / `%h{1;2}`: one `SemiList` statement per dimension.
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => subscript_dims_node(
            target,
            &splat_dimensions(dimensions),
            *is_positional,
            None,
            Vec::new(),
        ),
        // `@a[]` / `%h{}`: a subscript with no dimension at all.
        Expr::ZenSlice(target) => subscript_dims_node(
            target,
            &[],
            !matches!(&**target, Expr::HashVar(_)),
            None,
            Vec::new(),
        ),
        // `@a[i] = v` is the typed assignment node a `.AST` parse keeps
        // (ADR-10723 Stage 2).
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
            spelling,
        } if index_bind_rhs(value).is_some() => {
            let rhs = index_bind_rhs(value).ok_or_else(|| unsupported("indexed bind"))?;
            let left = if *spelling == crate::ast::IndexSpelling::SemicolonDimensions {
                let Expr::ArrayLiteral(dimensions) = index.as_ref() else {
                    return Err(unsupported("multi-dimensional bind source form"));
                };
                subscript_dims_node(target, dimensions, *is_positional, None, Vec::new())?
            } else if *spelling == crate::ast::IndexSpelling::Angle
                && angle_key_text(index).is_some()
            {
                angle_subscript_node(target, index, None, Vec::new())?
            } else {
                subscript_node(target, index, *is_positional, None, Vec::new())?
            };
            Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(Some("left"), left),
                    node_field(Some("infix"), plain_infix(":=")),
                    node_field(Some("right"), convert_expr(rhs)?),
                ],
            })
        }
        // A bare `{ ... }` block in expression position.
        Expr::Block(body) => signature_block_node(body),
        Expr::AnonSub {
            body,
            is_rw,
            is_raw,
            is_block,
            doc,
        } => {
            if *is_block {
                if *is_rw {
                    return Err(unsupported("`is rw` block"));
                }
                if *is_raw {
                    return Err(unsupported("`is raw` block"));
                }
                // A bare `{ ... }` block.
                let mut node = signature_block_node(body)?;
                super::declarator_docs::attach(&mut node, doc.get());
                Ok(node)
            } else {
                // An anonymous, parameter-less `sub { ... }`, with the `is rw`
                // / `is raw` it was written with.
                let mut node = RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::Sub,
                    fields: vec![node_field(Some("body"), blockoid(body)?)],
                };
                super::declarator_docs::attach(&mut node, doc.get());
                if super::routine_source::apply(&mut node, body)? {
                    return Ok(node);
                }
                if *is_rw || *is_raw {
                    return Err(unsupported(
                        "anonymous routine with traits and no written-trait record",
                    ));
                }
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
            param_defs,
            body,
            is_rw,
            is_raw,
            is_whatever_code,
            return_type,
            declarator,
            custom_traits,
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
            // The declarator and inferred parameters retain this distinction,
            // including blocks that combine different placeholder kinds.
            if crate::ast::placeholders::is_implicit_placeholder_block(expr) {
                return signature_block_node(body);
            }
            if declarator.is_routine() {
                // A placeholder in the body of a routine with a signature of its
                // own (`sub () { $^x }`) is a compile-time error in rakudo, which
                // raises it while building the tree.
                // (`%_` / `@_` are implicit in a method, never a placeholder.)
                if matches!(declarator, crate::ast::RoutineDeclarator::Sub)
                    && let Some(exception) =
                        crate::parser::placeholder_signature_exception(body, param_defs)
                {
                    return Err(RuntimeError::from_exception_value(exception));
                }
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
                super::declarator_docs::attach(&mut node, custom_traits.doc());
                if super::routine_source::apply(&mut node, body)? {
                    return Ok(node);
                }
                if *is_rw || *is_raw {
                    return Err(unsupported(
                        "anonymous routine with traits and no written-trait record",
                    ));
                }
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
                return super::quoted_string::convert_closed_block_heredoc(content);
            }
            convert_expr(&crate::parser::interpolate_heredoc_content(content))
        }
        Expr::StringInterpolation(parts) => {
            let mut segments = Vec::with_capacity(parts.len());
            for p in parts {
                segments.push(Value::rakuast(Box::new(interp_segment(p)?)));
            }
            Ok(RakuAstNode {
                origin: Default::default(),
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
pub(super) fn interp_segment(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) if matches!(v.view(), ValueView::Str(_)) => {
            Ok(RakuAstNode {
                origin: Default::default(),
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
            // `"a$(1 + 2)b"`: the item contextualizer around the contents.
            Stmt::Expr(contents) => super::contextualizer::convert_segment(contents),
            _ => convert_expr(expr),
        },
        // `"a$(my $x = 1; $x)b"`: several statements in the item contextualizer.
        Expr::DoBlock {
            body,
            label: None,
            origin: crate::ast::DoBlockOrigin::Desugar,
        } if body.len() > 1 => super::contextualizer::convert_statements(body),
        other => match super::match_vars::convert_interpolated(other) {
            Some(capture) => Ok(capture),
            None => convert_expr(other),
        },
    }
}

/// A `{ ... }` block body wraps its `StatementList` in a `Blockoid`.
///
/// The body keeps the enclosing unit's declared names: the unit-level scan
/// already entered every block, and re-collecting here would *replace* them
/// with the block's own, hiding `class C { }` from a closure `{ C.new }`.
pub(super) fn blockoid(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Blockoid,
        fields: vec![node_field(None, statement_list_inner(body)?)],
    })
}

/// The operand of an `ApplyPostfix` (`(1 + 2).abs`, `($x)[0]`, `($x)++`): the
/// expression without the one level of parentheses raku drops
/// ([`Expr::postfix_operand`]).
// Cost: O(n), n = size of the operand.
pub(super) fn postfix_operand(operand: &Expr) -> Result<RakuAstNode, RuntimeError> {
    convert_expr(operand.postfix_operand())
}

/// A `Block` that raku builds without a signature of its own: the body of a
/// `loop`, `try`, `do`, `gather`, `start`, a phaser, a class or a `default`
/// (`Block(body => Blockoid)`).
pub(super) fn block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Block,
        fields: vec![node_field(Some("body"), blockoid(body)?)],
    })
}

/// A `Block` that may take a signature (placeholders, `@_`, `%_`): the bare
/// `{ ... }` statement or term, a block argument, and the bodies of `if` /
/// `unless` / `else`, `while` / `until`, `repeat` and `when`. raku marks it
/// `may-have-signature => True` ahead of the body (measured on 2026.09).
pub(super) fn signature_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Block,
        fields: vec![
            true_field("may-have-signature"),
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// A named field holding `True`: raku's boolean block flags.
pub(super) fn true_field(name: &'static str) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(Value::truth(true)),
    }
}

/// Convert the source-level regex tree to the corresponding RakuAST regex
/// node. The tree is deliberately separate from `RegexPattern`: the latter is
/// an execution plan and has already lost source-level declaration and
/// whitespace information by the time matching begins.
pub(super) fn regex_node(node: &RegexNode) -> Result<RakuAstNode, RuntimeError> {
    let (class, fields) = match node {
        RegexNode::Extension(extension) => return super::regex_extension::convert(extension),
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
        RegexNode::NamedCapture {
            name,
            array,
            hash,
            regex,
        } => {
            let mut fields = vec![leaf_field(Some("name"), Value::str(name.clone()))];
            if *array {
                fields.push(leaf_field(Some("array"), Value::truth(true)));
            }
            if *hash {
                fields.push(leaf_field(Some("hash"), Value::truth(true)));
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
            let mut name_node = name_from_identifier(name);
            // `<value:sym<number>>`: the adverb belongs to the name.
            let sym = args.as_deref().and_then(super::regex_sym::name_colonpairs);
            if let Some(colonpairs) = sym.clone() {
                name_node.fields.push(colonpairs);
            }
            let mut fields = vec![node_field(Some("name"), name_node)];
            if let Some(args) = args
                && !args.args.is_empty()
                && sym.is_none()
            {
                fields.push(node_field(Some("args"), regex_arg_list(args)?));
            }
            if *capturing {
                fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            (
                if args.is_some() && sym.is_none() {
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
            let mut name_node = name_from_identifier(name);
            let sym = args.as_deref().and_then(super::regex_sym::name_colonpairs);
            if let Some(colonpairs) = sym.clone() {
                name_node.fields.push(colonpairs);
            }
            let mut assertion_fields = vec![node_field(Some("name"), name_node)];
            if let Some(args) = args
                && !args.args.is_empty()
                && sym.is_none()
            {
                assertion_fields.push(node_field(Some("args"), regex_arg_list(args)?));
            }
            if *capturing {
                assertion_fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            let assertion = RakuAstNode {
                origin: Default::default(),
                class: if args.is_some() && sym.is_none() {
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
                origin: Default::default(),
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
                origin: Default::default(),
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
        // `<::(EXPR)>` -> `Assertion::Named` over `Name(Part::Empty,
        // Part::Expression(EXPR))` (measured on 2026.09).
        RegexNode::SymbolicSubrule {
            expr, capturing, ..
        } => {
            let part = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::NamePartExpression,
                fields: vec![node_field(None, convert_expr(expr)?)],
            };
            let name = name_parts::name_from_parts(vec![
                name_parts::leading_empty(),
                Value::rakuast(Box::new(part)),
            ]);
            let mut fields = vec![node_field(Some("name"), name)];
            if *capturing {
                fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            (RakuAstClass::RegexAssertionNamed, fields)
        }
        RegexNode::Callable { name, args, .. } => {
            // `<&$var>`: rakudo item-contextualizes the scalar.
            let callee = match name.strip_prefix('$') {
                Some(name) => RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::ContextualizerItem,
                    fields: vec![node_field(None, var_lexical("$", name))],
                },
                None => var_lexical("&", name),
            };
            let mut fields = vec![node_field(Some("callee"), callee)];
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
            return super::regex_quantifier::convert(regex_node(atom)?, quantifier, separator);
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
            return super::regex_enumeration::convert(elements);
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
        RegexNode::WithWhitespace(child)
            if matches!(
                **child,
                RegexNode::Extension(RegexExtension::VariableAlias { .. })
            ) =>
        {
            return super::regex_variable_alias::convert_spaced(child);
        }
        RegexNode::WithWhitespace(child) => (
            RakuAstClass::RegexWithWhitespace,
            vec![node_field(None, regex_node(child)?)],
        ),
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
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
        origin: Default::default(),
        class: RakuAstClass::QuotedRegex,
        fields,
    })
}

/// The leading `labels => (Label("..."),)` field for a labelled loop, or an
/// empty vec when unlabelled. raku always renders labels first, and a `Label`
/// holds its name as a positional (`Label.new("L")`, measured on 2026.09).
pub(super) fn label_fields(label: &Option<String>) -> Vec<RakuAstField> {
    match label {
        None => Vec::new(),
        Some(name) => {
            let label_node = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::Label,
                fields: vec![leaf_field(None, Value::str(name.clone()))],
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
pub(super) fn loop_setup_node(stmt: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    // Declarations and expression setups share ordinary statement conversion.
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

/// A topic-taking block body (the `{ ... }` of an implicit-topic `for`, a
/// `given`, a `with`), which raku marks `implicit-topic`, `required-topic` and
/// `may-have-signature`, all `True`, before the `body` field.
pub(super) fn topic_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Block,
        fields: vec![
            true_field("implicit-topic"),
            true_field("required-topic"),
            true_field("may-have-signature"),
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// The body of a `CATCH` / `CONTROL` block: it topicalizes the exception, so
/// raku marks it `implicit-topic`, `required-topic` and `exception`, all
/// `True`, and gives it no `may-have-signature`.
fn exception_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Block,
        fields: vec![
            true_field("implicit-topic"),
            true_field("required-topic"),
            true_field("exception"),
            node_field(Some("body"), blockoid(body)?),
        ],
    })
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
pub(super) fn pointy_block(
    param_defs: &[ParamDef],
    body: &[Stmt],
    returns: Option<&str>,
) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::parameters(param_defs);
    let mut fields = Vec::new();
    if !param_defs.is_empty() || returns.is_some() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, false, returns)?,
        ));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode {
        origin: Default::default(),
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
    let _underscore_scope = super::underscore_bindings::Scope::parameters(param_defs);
    let mut fields = Vec::new();
    if !param_defs.is_empty() || returns.is_some() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, returns)?,
        ));
    }
    fields.push(node_field(Some("body"), blockoid(body)?));
    Ok(RakuAstNode {
        origin: Default::default(),
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
    let _underscore_scope = super::underscore_bindings::Scope::parameters(param_defs);
    let [receiver, rest @ ..] = param_defs else {
        return Err(unsupported("method literal without a receiver"));
    };
    let Some(folded) = crate::parser::folded_invocant(receiver, body) else {
        return Err(unsupported("method literal with an unrecognized receiver"));
    };
    let mut node = anon_routine_node(rest, folded.body, returns)?;
    node.class = class;
    if folded.type_constraint.is_some() || folded.alias.is_some() || folded.default.is_some() {
        let invocant = declared_invocant_parameter(
            folded.type_constraint,
            folded.alias.as_ref(),
            folded.default,
        )?;
        add_leading_parameter(&mut node, invocant);
    }
    Ok(node)
}

/// `Parameter(type, invocant => True[, target], optional => False)`: the
/// invocant a method literal declared (measured on rakudo 2026.09). Without a
/// written type it is the `Any` of every routine parameter; without a name it
/// has no target (`method (Mu:D:)`).
// Cost: O(t + d), t = type-name bytes, d = default-initializer AST nodes.
fn declared_invocant_parameter(
    type_constraint: Option<&str>,
    alias: Option<&(String, bool)>,
    default: Option<&Expr>,
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
                origin: Default::default(),
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
                origin: Default::default(),
                class: RakuAstClass::ParameterTargetVar,
                fields: vec![leaf_field(Some("name"), Value::str(spelled))],
            }
        };
        fields.push(node_field(Some("target"), target));
    }
    if let Some(default) = default {
        fields.push(node_field(Some("default"), convert_expr(default)?));
    } else {
        fields.push(RakuAstField {
            name: Some("optional"),
            value: RakuAstFieldValue::Node(Value::truth(false)),
        });
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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
        origin: Default::default(),
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
        origin: Default::default(),
        class: RakuAstClass::Signature,
        fields: vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(parameter))]),
        }],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
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
    export_trait: Option<Value>,
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
        .any(|(n, _)| n != "__constant" && n != "__constant_sigil")
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
    // `constant X is export = …`: the trait sits between the name and the
    // initializer (measured on rakudo 2026.09).
    if let Some(export) = export_trait {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![export]),
        });
    }
    fields.push(node_field(
        Some("initializer"),
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::InitializerAssign,
            fields: vec![node_field(None, convert_expr(expr)?)],
        },
    ));
    Ok(Some(statement_expression(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarDeclarationConstant,
        fields,
    })))
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
            stmt.is_routine_traits()
                || matches!(
                    stmt,
                    Stmt::DoesDecl { name, from_is: false, also: false, args: None }
                        if does_parents.iter().any(|r| r == name.resolve().as_str())
                )
        })
        .count();
    &body[header..]
}

/// The `is` clause an `also is Parent;` statement of a class or grammar body
/// stands for.
// Cost: O(p), p = parents with arguments.
fn also_parent(
    parent: &str,
    parent_args: &[(String, Vec<Expr>)],
) -> crate::ast::routine_trait::RoutineTrait {
    crate::ast::routine_trait::RoutineTrait::Parent {
        kind: crate::ast::routine_trait::ParentKind::Is,
        name: parent.to_string(),
        args: parent_args
            .iter()
            .find(|(name, _)| name == parent)
            .map(|(_, args)| args.clone()),
    }
}

/// What the parser folded a class or grammar header into, to check against the
/// record of what was written.
struct HeaderFields<'a> {
    parents: &'a [String],
    does_parents: &'a [String],
    hidden_parents: &'a [String],
    is_rw: bool,
    is_hidden: bool,
    custom_traits: &'a [(String, Option<Expr>)],
}

/// The `traits` of a class or grammar header: the nodes of the traits the
/// source wrote, in written order. The fields the parser folds them into are
/// not read back; a header whose fields name a trait that no record says was
/// written was not built by a parse that keeps spellings, and is refused.
// Cost: O(t + a), t = written traits, a = size of their arguments.
fn header_trait_values(
    body: &[Stmt],
    fields: &HeaderFields<'_>,
) -> Result<Vec<Value>, RuntimeError> {
    let written = crate::ast::routine_trait::get(body);
    if written.is_none()
        && (fields.is_hidden
            || !fields.parents.is_empty()
            || !fields.does_parents.is_empty()
            || !fields.hidden_parents.is_empty()
            || fields
                .custom_traits
                .iter()
                .any(|(name, _)| decl_traits::is_class_trait(name)))
    {
        return Err(unsupported("class traits without a written-trait record"));
    }
    let mut traits = super::routine_source::header_traits(body)?;
    // `also is rw;` in the body sets the flag without a header spelling; it
    // renders as the trailing `is rw` trait, which lowers back to the flag.
    if fields.is_rw && !written.is_some_and(|w| has_is(w, "rw")) {
        traits.push(Value::rakuast(Box::new(routine_traits::trait_is(
            "rw", None,
        ))));
    }
    Ok(traits)
}

/// Whether the written traits include `is NAME`.
// Cost: O(t), t = written traits.
pub(super) fn has_is(written: &[crate::ast::routine_trait::RoutineTrait], wanted: &str) -> bool {
    written.iter().any(
        |t| matches!(t, crate::ast::routine_trait::RoutineTrait::Is { name, .. } if name == wanted),
    )
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
        let mut node = parameter(pd, type_setting)?;
        if !pd.multi_invocant {
            node.fields
                .push(leaf_field(Some("multi-invocant"), Value::truth(false)));
        }
        params.push(Value::rakuast(Box::new(node)));
    }
    let mut fields = vec![RakuAstField {
        name: Some("parameters"),
        value: RakuAstFieldValue::List(params),
    }];
    if let Some(t) = returns {
        fields.push(node_field(
            Some("returns"),
            super::type_build::return_constraint_node(t)?,
        ));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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
    let implicit_invocant = pd.implicit_invocant;
    let user_traits: Vec<&str> = pd.traits.iter().map(String::as_str).collect();
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
    } else if implicit_invocant && !pd.is_invocant {
        Some("invocant marker without an invocant")
    } else if pd.is_invocant && (pd.named || pd.slurpy || pd.double_slurpy) {
        Some("non-scalar invocant parameter")
    } else if pd.outer_sub_signature.is_some() && !(pd.named && pd.named_alias) {
        Some("parameter with an outer sub-signature")
    } else {
        None
    };
    if let Some(what) = refusal {
        return Err(unsupported(what));
    }
    if let Some(node) = super::parameter_destructure::anonymous_destructuring(pd, type_setting)? {
        return Ok(node);
    }
    if let Some(value) = &pd.literal_value {
        return super::literal_parameter::convert(pd, value);
    }
    if let Some(num) = super::literal_parameter::special_num(pd) {
        let literal = crate::ast::ParamDef {
            name: super::literal_parameter::NAME.to_string(),
            type_constraint: None,
            ..pd.clone()
        };
        return super::literal_parameter::convert(&literal, &Value::num(num));
    }
    let type_capture = match type_capture_name(pd) {
        Some(name) => Some(type_capture_node(name)?),
        None => None,
    };
    // A bare `::T` is represented internally by a synthetic parameter name,
    // but RakuAST models it as a Parameter with no target. Keep the synthetic
    // name only in the internal AST used by the existing binder.
    if pd.target == ParamTarget::TypeCapture {
        let Some(type_capture) = type_capture else {
            return Err(unsupported("type capture without a capture node"));
        };
        let mut fields = Vec::with_capacity(4);
        if type_setting {
            fields.push(node_field(Some("type"), type_setting_any()));
        }
        fields.push(type_captures_field(type_capture));
        if pd.is_invocant {
            // Rakudo omits the invocant flag of a bare capture from .raku.
            // Retain it as metadata so lowering does not add an argument.
            fields.push(leaf_field(Some("capture-invocant"), Value::truth(true)));
        }
        match &pd.default {
            Some(default) => fields.push(node_field(Some("default"), convert_expr(default)?)),
            None => fields.push(RakuAstField {
                name: Some("optional"),
                value: RakuAstFieldValue::Node(Value::truth(false)),
            }),
        }
        return Ok(RakuAstNode {
            origin: Default::default(),
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
    // The parser names an anonymous `$` / `@` / `%` parameter internally;
    // the model's target keeps only the bare sigil.
    let (sigil, desigil) = match pd.target {
        ParamTarget::Anonymous('$') => ("$", ""),
        ParamTarget::Anonymous('@') => ("@", ""),
        ParamTarget::Anonymous(_) => ("%", ""),
        _ => split_sigil(&pd.name),
    };
    let mut node = if pd.sigilless && pd.slurpy {
        let mut node = sigilless_slurpy_parameter(pd, type_setting);
        if let Some(t) = ordinary_type_constraint {
            node.fields.retain(|field| field.name != Some("type"));
            node.fields
                .insert(0, node_field(Some("type"), build_type_node(t)?));
        }
        if let Some(w) = pd.where_constraint.as_deref() {
            node.fields
                .push(node_field(Some("where"), convert_expr(w)?));
        }
        if let Some(d) = pd.default.as_ref() {
            node.fields
                .push(node_field(Some("default"), convert_expr(d)?));
        }
        node
    } else if pd.slurpy || pd.double_slurpy {
        // Rakudo accepts a typed scalar slurpy (`Code *$block`) but rejects
        // typed positional array/hash slurpies (`Int *@a`).
        if ordinary_type_constraint.is_some() && sigil != "$" {
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
        if let Some(t) = ordinary_type_constraint {
            node.fields
                .insert(0, node_field(Some("type"), build_type_node(t)?));
        }
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
                name: Some(if implicit_invocant && type_capture_name(pd).is_some() {
                    "capture-invocant"
                } else {
                    "invocant"
                }),
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
    // `sub f(Int)`: the parser names a parameter written as a bare type, which
    // rakudo gives no target.
    if pd.target == ParamTarget::TypeOnly && !pd.is_invocant {
        node.fields.retain(|field| field.name != Some("target"));
    }
    // `:x($r) ($a, $b)`: the trailing signature is the parameter's own
    // `sub-signature`, next to the alias chain's `names` and `target`.
    if let Some(sub_params) = pd
        .outer_sub_signature
        .as_deref()
        .or_else(|| super::named_param::destructuring_sub_signature(pd))
    {
        node.fields.push(node_field(
            Some("sub-signature"),
            signature(
                sub_params,
                type_setting,
                pd.code_signature
                    .as_ref()
                    .and_then(|(_, ret)| ret.as_deref()),
            )?,
        ));
    }
    super::parameter_signature::attach_constraint(&mut node, pd, type_setting)?;
    if let Some(shape) = super::parameter_shape::convert(pd)? {
        node.fields.push(node_field(Some("array-shape"), shape));
    }
    // `$x is copy` -> `traits => (Trait::Is(name => Name.from-identifier("copy")),)`,
    // after every other field (measured on 2026.09).
    if let Some(traits) = parameter_traits(pd)? {
        node.fields.push(traits);
    }
    Ok(node)
}

/// Render the `traits` field shared by ordinary and anonymous destructuring
/// parameters.
pub(super) fn parameter_traits(pd: &ParamDef) -> Result<Option<RakuAstField>, RuntimeError> {
    let traits: Vec<&str> = pd.traits.iter().map(String::as_str).collect();
    if traits.is_empty() {
        return Ok(None);
    }
    let values = traits
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
                origin: Default::default(),
                class: RakuAstClass::TraitIs,
                fields,
            })))
        })
        .collect::<Result<Vec<_>, RuntimeError>>()?;
    Ok(Some(RakuAstField {
        name: Some("traits"),
        value: RakuAstFieldValue::List(values),
    }))
}

/// The parser's names for an anonymous `$` / `@` / `%` parameter.
pub(super) const ANONYMOUS_SCALAR_PARAM: &str = crate::ast::ANON_SCALAR_PARAM;
pub(super) const ANONYMOUS_ARRAY_PARAM: &str = crate::ast::ANON_ARRAY_PARAM;
pub(super) const ANONYMOUS_HASH_PARAM: &str = crate::ast::ANON_HASH_PARAM;

/// The `::T` type capture `pd` declares. Unlike `ParamDef::captured_type_name`
/// (whose binder reading this does not change), a `::?CLASS` / `::?ROLE`
/// pseudo-type is a nominal type here, the way rakudo's `.AST` shows it.
pub(super) fn type_capture_name(pd: &ParamDef) -> Option<&str> {
    pd.captured_type_name().filter(|_| {
        !pd.type_constraint.as_deref().is_some_and(|t| {
            // `::?CLASS() $x` is a coercion on the pseudo-type, not a capture.
            is_pseudo_type(t.strip_suffix("()").unwrap_or(t))
        }) || pd.type_capture.is_some()
    })
}

/// `::?CLASS`, `::?ROLE`, `::?PACKAGE`, with an optional smiley.
pub(super) fn is_pseudo_type(t: &str) -> bool {
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

/// A basic `::T` capture is represented by `Parameter.type-captures` rather
/// than by the parameter's ordinary `type` node. Smiley-constrained and other
/// richer capture spellings need more internal metadata and remain deferred.
pub(super) fn type_capture_node(name: &str) -> Result<RakuAstNode, RuntimeError> {
    if super::type_capture_indirect::is_indirect(name) {
        return super::type_capture_indirect::capture_node(name);
    }
    if name.is_empty()
        || !name
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '-')
    {
        return Err(unsupported("complex type capture"));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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
        origin: Default::default(),
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
        origin: Default::default(),
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
    if pd.name != super::lower::ANONYMOUS_CAPTURE && pd.target != ParamTarget::Parens {
        fields.push(node_field(
            Some("target"),
            RakuAstNode {
                origin: Default::default(),
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
        origin: Default::default(),
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
                    origin: Default::default(),
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

pub(super) fn type_setting_any() -> RakuAstNode {
    let name = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str("Any".to_string()))],
    };
    RakuAstNode {
        origin: Default::default(),
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
        origin: Default::default(),
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
        origin: Default::default(),
        class: RakuAstClass::Parameter,
        fields,
    })
}

/// The postfix `Call::Method` / `Call::QuotedMethod` node shared by plain and
/// hyper method calls: a quoted name -> `Call::QuotedMethod` (no modifier), an
/// unquoted name -> `Call::Method` (with an optional `.?`/`.+`/`.*` dispatch).
pub(super) fn method_call_postfix(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
    quoted: bool,
) -> Result<RakuAstNode, RuntimeError> {
    if quoted {
        if !matches!(modifier, None | Some('?' | '+' | '*' | '!')) {
            return Err(unsupported("quoted method name with a modifier"));
        }
        return call_quoted_method(name, args, modifier);
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
            origin: Default::default(),
            class: RakuAstClass::CallMetaMethod,
            fields,
        });
    }
    call_method(name, args, modifier)
}

/// `."foo"` / `."foo"(args)` -> `Call::QuotedMethod(name => QuotedString,
/// [args => ArgList])`. Unlike `Call::Method`, the name is a QuotedString
/// (a string literal) rather than a `Name.from-identifier`.
fn call_quoted_method(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    call_quoted_method_expr(quoted_string(Value::str(name.to_string())), args, modifier)
}

/// Construct `Call::QuotedMethod` from the quoted-string expression that names
/// it.  Dynamic quoted names retain their interpolation tree here rather than
/// being flattened into a static method-name string.
pub(super) fn call_quoted_method_expr(
    name: RakuAstNode,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(Some("name"), name)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    // `.?"name"` / `.+"name"` / `.*"name"` keep the dispatch string; the
    // private call `self!"name"` has the bare `!`.
    if let Some(m) = modifier {
        let dispatch = if m == '!' {
            "!".to_string()
        } else {
            format!(".{m}")
        };
        fields.push(leaf_field(Some("dispatch"), Value::str(dispatch)));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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

/// `VarDeclaration::Anonymous(scope?, sigil, initializer?)`: a bare `$` / `@`
/// / `%`, which is a `state` variable of its block, or a declared `my $`,
/// whose default scope rakudo omits. Rakudo gives it no name.
pub(super) fn anonymous_declaration(
    scope: Option<&'static str>,
    sigil: &str,
    initializer: Option<RakuAstNode>,
) -> RakuAstNode {
    let mut fields = Vec::new();
    if let Some(scope) = scope {
        fields.push(leaf_field(Some("scope"), Value::str_from(scope)));
    }
    fields.push(leaf_field(Some("sigil"), Value::str(sigil.to_string())));
    if let Some(initializer) = initializer {
        fields.push(node_field(Some("initializer"), initializer));
    }
    RakuAstNode {
        origin: Default::default(),
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
pub(super) fn var_lexical(sigil: &str, name: &str) -> RakuAstNode {
    // A bare `$` / `@` / `%` is the anonymous declaration itself.
    if (sigil == "$" && crate::ast::anon_state::is_scalar(name))
        || (sigil == "@" && crate::ast::anon_state::is_array(name))
        || (sigil == "%" && crate::ast::anon_state::is_hash(name))
    {
        return anonymous_declaration(Some("state"), sigil, None);
    }
    written_variable(sigil, name)
}

/// `Infix`/`Prefix` — a single positional operator string (e.g. `Infix.new("+")`).
fn operator_node(class: RakuAstClass, op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
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
        let mut infix = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), node),
                node_field(Some("infix"), operator_node(RakuAstClass::Infix, op)),
                node_field(Some("right"), convert_expr(&operands[i + 1])?),
            ],
        };
        super::infix_origins::derive(&mut infix);
        node = if *negated {
            RakuAstNode {
                origin: Default::default(),
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

/// The operands and positive relation under a negated set operator's `!`.
fn negated_set_operands(expr: &Expr) -> Option<(&Expr, &crate::token_kind::TokenKind, &Expr)> {
    let Expr::Unary { expr, .. } = expr else {
        return None;
    };
    match expr.as_ref() {
        Expr::Binary {
            left, op, right, ..
        } => Some((left, op, right)),
        _ => None,
    }
}

/// `a ^... b ^... c` -> one `ApplyListInfix(Infix("^..."), (a, b, c))`: the
/// parser nests the chain as `.skip(1)` over the sequence, left to right.
// Cost: O(n), n = operands of the chain.
fn left_exclusive_sequence_node(
    written: &str,
    sequence: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let mut operands: Vec<&Expr> = Vec::new();
    let mut current = sequence;
    loop {
        let Expr::Binary { left, right, .. } = current else {
            return Err(unsupported("left-exclusive sequence without its operator"));
        };
        operands.push(right);
        match left.as_ref() {
            Expr::Spelled(inner) if matches!(&inner.spelling, Spelling::LeftExclusiveSequence(w) if **w == *written) =>
            {
                let Expr::MethodCall { target, .. } = &inner.expr else {
                    return Err(unsupported("left-exclusive sequence without its skip"));
                };
                current = target;
            }
            first => {
                operands.push(first);
                break;
            }
        }
    }
    operands.reverse();
    let mut nodes = Vec::with_capacity(operands.len());
    for operand in operands {
        nodes.push(Value::rakuast(Box::new(convert_expr(operand)?)));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), plain_infix(written)),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(nodes),
            },
        ],
    })
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
            // The sequence operators, `^^` and the set operators are list
            // associative too (measured on rakudo 2026.09).
            | TokenKind::DotDotDot
            | TokenKind::DotDotDotCaret
            | TokenKind::XorXor
            | TokenKind::SetUnion
            | TokenKind::SetAddition
            | TokenKind::SetIntersect
            | TokenKind::SetMultiply
            | TokenKind::SetDiff
            | TokenKind::SetSymDiff
    ) || matches!(op, TokenKind::Ident(name) if matches!(name.as_str(), "min" | "max" | "∪" | "∩" | "⊎" | "⊍" | "∖" | "⊖"))
}

/// `Postfix` — a single NAMED `operator` string (e.g. `Postfix.new(operator => "++")`).
fn postfix_node(op: &crate::token_kind::TokenKind) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
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
        origin: Default::default(),
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
            origin: Default::default(),
            class: RakuAstClass::IntLiteral,
            fields: vec![leaf_field(None, Value::int(im as i64))],
        }
    } else {
        let spelling = format!("{im}");
        let rat = crate::parser::decimal_literal_value(&spelling)
            .ok_or_else(|| unsupported("imaginary literal without a decimal spelling"))?;
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::RatLiteral,
            fields: vec![leaf_field(None, rat)],
        }
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), number),
            node_field(
                Some("postfix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::Postfix,
                    fields: vec![leaf_field(Some("operator"), Value::str_from("i"))],
                },
            ),
        ],
    })
}

pub(super) fn convert_literal(v: &Value) -> Result<RakuAstNode, RuntimeError> {
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
            origin: Default::default(),
            class: RakuAstClass::IntLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::RatLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Num(_) => Ok(RakuAstNode {
            origin: Default::default(),
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
            origin: Default::default(),
            class: RakuAstClass::TermName,
            fields: vec![node_field(None, name_from_identifier("Empty"))],
        }),
        // `True`/`False` are enum values: `Term::Enum.from-identifier('True')`.
        ValueView::Bool(b) => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TermEnum,
            fields: vec![leaf_field(
                None,
                Value::str(if b { "True" } else { "False" }.to_string()),
            )],
        }),
        // `2i` / `3.5i`: the parser folds the imaginary literal to the
        // Complex value; rakudo keeps the number under a `Postfix("i")`.
        ValueView::Complex(re, im)
            if re == 0.0
                && !re.is_sign_negative()
                && im.is_finite()
                && im >= 0.0
                && !im.is_sign_negative() =>
        {
            imaginary_literal(im)
        }
        // `<1+2i>`: a complex number written whole.
        ValueView::Complex(..) => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ComplexLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        // `q:nfc'…'` and its siblings: see `nf_quote`.
        ValueView::Uni(uni) => super::nf_quote::convert(uni),
        // `v6.d`, `v1.2.3+`.
        ValueView::Version { .. } => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::VersionLiteral,
            fields: vec![leaf_field(None, v.clone())],
        }),
        ValueView::Mixin(..) => match allomorph_word(v) {
            Some(word) => Ok(word_quote(word)),
            None => Err(unsupported("mixin literal")),
        },
        // `:(Int $x)`: the parser folds a signature literal to a `Signature`
        // value, which keeps the declared parameters it was built from;
        // rakudo wraps them in a `FakeSignature`.
        ValueView::Instance { class_name, .. } if class_name == "Signature" => signature_literal(v),
        // The parser defers a Perl 5 construct's `X::Obsolete` into the unit as
        // `die(<instance>)`; rakudo throws it while compiling, so it is the
        // unit's compile error, not a conversion gap.
        ValueView::Instance { class_name, .. } if class_name == "X::Obsolete" => {
            Err(RuntimeError::from_exception_value(v.clone()))
        }
        // `regex { }` / `token { }` / `rule { }` as a term: the parser keeps
        // the declarator's tree on the value.
        ValueView::Regex(_) => match v.regex_source_tree() {
            Some(tree) if tree.declaration_kind.is_some() => anon_regex_declaration(v, tree),
            _ => Err(unsupported("literal Regex without a declarator tree")),
        },
        other => Err(unsupported(&format!("literal {other:?}"))),
    }
}

/// An anonymous `regex`/`token`/`rule` term: the named declaration's node
/// without its `name`.
// Cost: O(n), n = size of the regex tree and signature.
fn anon_regex_declaration(v: &Value, tree: &RegexTree) -> Result<RakuAstNode, RuntimeError> {
    use crate::regex_tree::RegexDeclKind;
    let class = match tree.declaration_kind {
        Some(RegexDeclKind::Token) => RakuAstClass::TokenDeclaration,
        Some(RegexDeclKind::Rule) => RakuAstClass::RuleDeclaration,
        _ => RakuAstClass::RegexDeclaration,
    };
    let params = v
        .regex_signature()
        .map(|p| (*p).clone())
        .unwrap_or_default();
    let mut declaration = regex_declaration_with_body(
        class,
        None,
        None,
        None,
        &params,
        regex_node(&tree.body)?,
        Vec::new(),
    )?;
    super::regex_declaration::retain_value_source(&mut declaration, v);
    Ok(declaration)
}

/// `FakeSignature.new(Signature(parameters => (...)[, returns => Type]))` for a
/// `:(...)` literal. A signature without its declared parameters (the permissive
/// fallback parse) stays the boundary.
// Cost: O(n), n = size of the parameter list.
fn signature_literal(v: &Value) -> Result<RakuAstNode, RuntimeError> {
    let info = crate::value::signature::extract_sig_info(v)
        .ok_or_else(|| unsupported("signature literal"))?;
    let Some(defs) = info.param_defs.as_deref() else {
        return Err(unsupported("signature literal without declared parameters"));
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::FakeSignature,
        fields: vec![node_field(
            None,
            signature(defs, false, info.return_type.as_deref())?,
        )],
    })
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
    word_quote_with(false, true, word)
}

/// A word quote over `word` with the `words` / `quotewords` processor, then
/// `val` when the words are allomorphs.
pub(super) fn word_quote_with(quotewords: bool, val: bool, word: &str) -> RakuAstNode {
    let mut processors = vec![Value::str(
        if quotewords { "quotewords" } else { "words" }.to_string(),
    )];
    if val {
        processors.push(Value::str("val".to_string()));
    }
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(processors),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::StrLiteral,
                    fields: vec![leaf_field(None, Value::str(word.to_string()))],
                }))]),
            },
        ],
    }
}

/// `q:to/END/`: the heredoc's text as the quoted string it evaluates to,
/// plus the terminator line rakudo keeps as `stop`. A body that is one piece
/// of text has the origin of the lines it was written on, as in Rakudo.
fn heredoc_node(
    body: &Expr,
    stop: &str,
    written: crate::ast::SrcSpan,
) -> Result<RakuAstNode, RuntimeError> {
    let mut node = {
        let _inspected = super::passthrough::Inspecting::enter();
        convert_expr(body)?
    };
    if node.class != RakuAstClass::QuotedString {
        return Err(unsupported("a heredoc that is not quoted text"));
    }
    if let Some(segments) = node.fields.iter_mut().find(|f| f.name == Some("segments"))
        && let RakuAstFieldValue::List(items) = &mut segments.value
        && let [only] = items.as_mut_slice()
        && let ValueView::RakuAst(text) = only.view()
        && text.class == RakuAstClass::StrLiteral
    {
        let mut text = RakuAstNode::clone(text);
        origin::set_span(&mut text, written);
        *only = Value::rakuast(Box::new(text));
    }
    node.class = RakuAstClass::Heredoc;
    node.fields
        .push(leaf_field(Some("stop"), Value::str(stop.to_string())));
    Ok(node)
}

/// A string literal renders as `QuotedString.new(segments => (StrLiteral,))`.
pub(super) fn quoted_string(str_value: Value) -> RakuAstNode {
    let seg = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StrLiteral,
        fields: vec![leaf_field(None, str_value)],
    };
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::QuotedString,
        fields: vec![RakuAstField {
            name: Some("segments"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(seg))]),
        }],
    }
}

/// A listop `say EXPR` — `Call::Name::WithoutParentheses`.
fn listop_call(name: &'static str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    call_name(name, args, true)
}

pub(super) fn call_name(
    name: &str,
    args: &[Expr],
    without_parentheses: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = name_from_identifier(name);
    let arg_list = arg_list(args)?;
    // A qualified name (`M::foo`) is a `Call::Name` however it is spelled:
    // raku has the `WithoutParentheses` form for a plain identifier only
    // (measured on 2026.09 for `M::foo`, `M::foo 1` and `M::foo.bar`).
    let class = if without_parentheses && !crate::qualified::is_qualified_str(name) {
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
    Ok(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })
}

/// A control-flow listop (`return`/`last`/`next`) — a `Call::Name` in
/// WithoutParentheses form. Unlike [`listop_call`], the `args` field is omitted
/// entirely when there are no arguments (matching raku's gist for a bare
/// `return`/`last`/`next`).
/// The call a `proceed |c` / `succeed |c` guard stands for, or `None` when the
/// `if` is not that parser expansion.
// Cost: O(|slip|).
fn slipped_control_call(
    cond: &Expr,
    then_branch: &[Stmt],
    else_branch: &[Stmt],
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let name = match else_branch {
        [Stmt::Proceed] => "proceed",
        [Stmt::Succeed] => "succeed",
        _ => return Ok(None),
    };
    let Expr::MethodCall {
        target, name: m, ..
    } = cond
    else {
        return Ok(None);
    };
    let (Expr::ArrayLiteral(items), "elems") = (&**target, m.as_str()) else {
        return Ok(None);
    };
    let [slip @ Expr::Unary { .. }] = items.as_slice() else {
        return Ok(None);
    };
    if !matches!(then_branch, [Stmt::Die(_)]) {
        return Ok(None);
    }
    control_call(name, std::slice::from_ref(slip)).map(Some)
}

pub(super) fn control_call(name: &'static str, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallNameWithoutParentheses,
        fields,
    })
}

/// `last LABEL` and friends: the bare call over the label as a `Term::Name`.
// Cost: O(|label|).
fn labelled_control_call(name: &'static str, label: &str) -> RakuAstNode {
    let term = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermName,
        fields: vec![node_field(None, name_from_identifier(label))],
    };
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallNameWithoutParentheses,
        fields: vec![
            node_field(Some("name"), name_from_identifier(name)),
            node_field(
                Some("args"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::ArgList,
                    fields: vec![node_field(None, term)],
                },
            ),
        ],
    }
}

/// A comma list `1, 2, 3` -> `ApplyListInfix(infix => ",", operands)`.
/// The source declarations of `my $x = 1, my $y = 2` as one comma application.
// Cost: O(n), n = declarations and their nested nodes.
pub(super) fn comma_declarations_node(declarations: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let mut operands = Vec::with_capacity(declarations.len());
    for declaration in declarations {
        let statement = convert_stmt(declaration)?
            .ok_or_else(|| unsupported("empty declaration in comma list"))?;
        if statement.class != RakuAstClass::StatementExpression {
            return Err(unsupported("non-expression declaration in comma list"));
        }
        let expression = statement
            .fields
            .iter()
            .find(|field| field.name == Some("expression"))
            .and_then(|field| match &field.value {
                RakuAstFieldValue::Node(value) => match value.view() {
                    ValueView::RakuAst(node) => Some(node.clone()),
                    _ => None,
                },
                _ => None,
            })
            .ok_or_else(|| unsupported("declaration expression in comma list"))?;
        operands.push(Value::rakuast(Box::new(expression)));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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

fn comma_list_node(items: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let mut operands = Vec::with_capacity(items.len());
    for it in items {
        operands.push(Value::rakuast(Box::new(convert_expr(it)?)));
    }
    let mut node = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), plain_infix(",")),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(operands),
            },
        ],
    };
    super::infix_origins::derive(&mut node);
    Ok(node)
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
        if let Some(leaf) = super::passthrough::arg_leaf(a) {
            fields.push(node_field(None, leaf));
            continue;
        }
        fields.push(node_field(None, convert_expr(a)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ArgList,
        fields,
    })
}

fn regex_arg_list(args: &crate::regex_tree::SubruleArgs) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
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
/// The colonpair node `form` says `expr` was written as, or `None` for a pair
/// the dedicated helpers cannot render (the `=>` rendering then applies).
// Cost: O(n), n = size of the pair.
fn colonpair_spelling(expr: &Expr, form: crate::ast::BinaryForm) -> Option<RakuAstNode> {
    use crate::ast::BinaryForm;
    match form {
        BinaryForm::ColonPairTrue => colonpair_true_expr(expr),
        BinaryForm::ColonPairFalse => colonpair_false_expr(expr),
        BinaryForm::ColonPairVariable => colonpair_variable_expr(expr),
        BinaryForm::ColonPairValue => colonpair_value_expr(expr),
        BinaryForm::ColonPairBracketed => colonpair_bracketed_expr(expr),
        BinaryForm::ColonPairNumber => colonpair_number_expr(expr),
        BinaryForm::Infix | BinaryForm::CaretPrefix => return None,
    }
    .ok()
}

/// `:7a` -> `ColonPair::Number(key => "a", value => IntLiteral(7))`.
fn colonpair_number_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
        ..
    } = expr
    else {
        return Err(unsupported("colonpair number provenance"));
    };
    let (Expr::Literal(key), Expr::Literal(value)) = (left.as_ref(), right.as_ref()) else {
        return Err(unsupported("colonpair number parts"));
    };
    let ValueView::Str(key) = key.view() else {
        return Err(unsupported("colonpair number key"));
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairNumber,
        fields: vec![
            leaf_field(Some("key"), Value::str(key.to_string())),
            node_field(Some("value"), convert_literal(value)?),
        ],
    })
}

fn colonpair_true_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
        ..
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
        origin: Default::default(),
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
        ..
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
        origin: Default::default(),
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
        ..
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
    let value = {
        let _inspected = super::passthrough::Inspecting::enter();
        convert_expr(right)?
    };
    if !value.class.is_simple_variable() {
        return Err(unsupported("colonpair variable value"));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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
    colonpair_value_node(expr, true)
}

/// `:a<x y>` / `:a[1, 2]`: the colonpair over the bracketed value itself, a
/// word-quote or an array composer, with no parentheses around it.
// Cost: O(n), n = size of the pair.
fn colonpair_bracketed_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    colonpair_value_node(expr, false)
}

// Cost: O(n), n = size of the pair.
fn colonpair_value_node(expr: &Expr, parenthesize: bool) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
        ..
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
    let value = match right.as_ref() {
        // `:a<x y>` keeps the raw word text as one word-quote.
        Expr::Literal(_) | Expr::ArrayLiteral(_) if !parenthesize => match angle_key_text(right) {
            Some(text) => word_quote(&text),
            None => convert_expr(right)?,
        },
        _ => convert_expr(right)?,
    };
    let is_direct_block = parenthesize
        && matches!(
            right.as_ref(),
            Expr::AnonSub { is_block: true, .. }
                | Expr::Block(_)
                | Expr::Hash(_, crate::ast::HashSpelling::Composer)
        )
        || crate::ast::placeholders::is_implicit_placeholder_block(right);
    if is_direct_block || !parenthesize {
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ColonPairValue,
            fields: vec![
                leaf_field(Some("key"), Value::str(key.to_string())),
                node_field(Some("value"), value),
            ],
        });
    }
    let semilist = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: vec![node_field(
            None,
            if value.class == RakuAstClass::StatementExpression {
                value
            } else {
                statement_expression(value)
            },
        )],
    };
    let parenthesized = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
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
        ..
    } = expr
    else {
        return Err(unsupported("literal hash index provenance"));
    };
    super::postfix_grouping::convert(
        target,
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::PostcircumfixLiteralHashIndex,
            fields: vec![node_field(Some("index"), convert_expr(index)?)],
        },
    )
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
            CallArg::Named { name, value, form } => Ok(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.clone()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(value.clone().unwrap_or(Expr::Literal(Value::TRUE))),
                form: *form,
            }),
            // `foo |@a`: the slip is the tight prefix `|` over the term.
            CallArg::Slip(expr) => Ok(Expr::Unary {
                op: crate::token_kind::TokenKind::Pipe,
                expr: Box::new(expr.clone()),
                word: false,
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
    let (declaration, header) = match stmt {
        Stmt::Ast(node) => match node.as_ref() {
            crate::ast::parsed::ParsedStmt::PackageHeader { decl, header, .. } => {
                (decl.as_ref(), header.clone())
            }
            _ => return None,
        },
        other => (other, crate::ast::package_header::Header::default()),
    };
    let Stmt::Package {
        name,
        kind,
        is_unit: true,
        is_my,
        body,
        custom_traits,
    } = declaration
    else {
        return None;
    };
    if body.iter().any(|stmt| !stmt.is_marker()) {
        return None;
    }
    let package = Stmt::Package {
        name: *name,
        body: body.iter().cloned().chain(rest.iter().cloned()).collect(),
        custom_traits: custom_traits.clone(),
        kind: *kind,
        is_unit: true,
        is_my: *is_my,
    };
    if header.adverbs.is_empty() && header.export_tags.is_none() {
        return Some(package);
    }
    Some(Stmt::Ast(Box::new(
        crate::ast::parsed::ParsedStmt::PackageHeader {
            decl: Box::new(package),
            name: name.resolve(),
            header,
        },
    )))
}
