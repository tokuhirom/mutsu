//! Internal AST (`Stmt`/`Expr`) → RakuAST node tree (read direction, ADR-0011).
//!
//! Covered so far: the literal + say-call cluster (Phase 1); and variables,
//! plain `my` declarations, infix/prefix/postfix operators, `=` assignment, and
//! method calls (Phase 2). Constructs outside that set produce an explicit
//! `RuntimeError` (the documented coverage boundary) rather than a
//! silently-wrong node.

#[path = "convert_conditional.rs"]
mod conditional;
use conditional::*;

use super::bareword::simple_type_node;
use super::method_assign_decl::call_method;
use super::origin;
use super::placeholder::{is_placeholder_name, placeholder_node};
use super::routine_source::return_type_spelling;
pub(super) use super::routine_source::routine_node;
use super::source_identifier::{named_routine_call, written_identifier, written_variable};
use super::{
    RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, bareword, decl_traits,
    hash_literal, name_parts, routine_traits, subscript_adverb,
};
use crate::ast::spelled::Spelling;
use crate::ast::{
    AssignOp, Expr, ForMode, GivenWithKind, IMPLICIT_INVOCANT_TRAIT, ParamDef, Stmt, WithBlockKind,
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
    let _underscore_scope = super::underscore_bindings::Scope::block(stmts);
    let _block = bareword::BlockScope::enter(stmts);
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
        if let Some(expansion) = done_value_expansion(stmt) {
            for part in expansion {
                if let Some(mut node) = convert_stmt(part)? {
                    if let Some(n) = line.take() {
                        node.fields.push(origin::field(n));
                    }
                    fields.push(node_field(None, node));
                }
            }
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

/// `done VALUE` expands to `emit VALUE; done`, which Rakudo keeps as two
/// statements in a supply block.
// Cost: O(1), the expansion has exactly two statements.
fn done_value_expansion(stmt: &Stmt) -> Option<&[Stmt]> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    let [
        Stmt::Call {
            name,
            args,
            listop: false,
        },
        Stmt::ReactDone,
    ] = stmts.as_slice()
    else {
        return None;
    };
    (name.resolve() == "emit" && matches!(args.as_slice(), [crate::ast::CallArg::Positional(_)]))
        .then_some(stmts)
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
    if super::control_signature::source(stmt).is_some() {
        return true;
    }
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
        Stmt::While { .. }
        | Stmt::Loop { .. }
        | Stmt::Whenever { .. }
        | Stmt::When { .. }
        | Stmt::Default(_) => true,
        _ => false,
    }
}

/// The condition and the modified statement of `STMT when COND`, which the
/// parser spells `given $_ { when COND { STMT } }` with the `When` flagged as a
/// modifier. `None` for any other `given` body.
// Cost: O(s), s = statements in `body` (SetLine markers skipped).
fn when_modifier_parts<'a>(topic: &Expr, body: &'a [Stmt]) -> Option<(&'a Expr, &'a Stmt)> {
    if !matches!(topic, Expr::Var(name) if name == "_") {
        return None;
    }
    let mut real = body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    let (
        Some(Stmt::When {
            cond,
            body,
            is_statement_modifier: true,
        }),
        None,
    ) = (real.next(), real.next())
    else {
        return None;
    };
    let mut inner = body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    let (Some(modified), None) = (inner.next(), inner.next()) else {
        return None;
    };
    Some((cond, modified))
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
pub(super) fn convert_stmt(stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    if let Some(form) = super::control_signature::source(stmt) {
        return super::control_signature::convert(form).map(Some);
    }
    if let Some(zip) = super::zip_assign::convert_zip_assign(stmt) {
        return zip.map(|node| Some(statement_expression(node)));
    }
    match stmt {
        // The `use trace` hook is bookkeeping too: rakudo models the trace as a
        // flag on the traced statement, not as a statement of its own.
        Stmt::SetLine(_) | Stmt::Trace { .. } => Ok(None),
        Stmt::SourceForm(form)
            if matches!(form.as_ref(), crate::ast::SourceForm::RoutineTraits(_)) =>
        {
            Ok(None)
        }
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
                RakuAstClass::StatementRequire | RakuAstClass::StatementExpression
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
            Ok(Some(statement_expression(regex_declaration(
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
            )?)))
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
            Ok(Some(statement_expression(regex_declaration(
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
            )?)))
        }
        // A signature declaration (`my ($a, @b) = …`): the parser keeps its
        // source form as the expansion's first statement.
        Stmt::SyntheticBlock(stmts)
            if matches!(
                stmts.as_slice(),
                [
                    Stmt::SubDecl { .. },
                    Stmt::Given {
                        is_statement_modifier: true,
                        ..
                    }
                ]
            ) =>
        {
            let [declaration, Stmt::Given { topic, .. }] = stmts.as_slice() else {
                unreachable!("guarded declaration modifier shape")
            };
            let mut statement = convert_stmt(declaration)?
                .ok_or_else(|| unsupported("empty declaration modifier"))?;
            statement.fields.push(node_field(
                Some("loop-modifier"),
                RakuAstNode {
                    class: RakuAstClass::StatementModifierGiven,
                    fields: vec![node_field(None, convert_expr(topic)?)],
                },
            ));
            Ok(Some(statement))
        }
        Stmt::SyntheticBlock(_) if source_form(stmt).is_some() => match source_form(stmt) {
            Some(crate::ast::SourceForm::DeclarationModifier {
                declaration,
                condition,
                kind,
            }) => declaration_modifier_node(declaration, condition, *kind).map(Some),
            Some(crate::ast::SourceForm::SignatureDecl(decl)) => Ok(Some(statement_expression(
                super::signature_decl::convert(decl)?,
            ))),
            Some(crate::ast::SourceForm::AttributeGroup(attributes)) => Ok(Some(
                statement_expression(super::attribute_group::to_signature(attributes)?),
            )),
            Some(crate::ast::SourceForm::AttributeColonArglist(attributes)) => Ok(Some(
                statement_expression(super::attribute_group::convert_colon(attributes)?),
            )),
            Some(crate::ast::SourceForm::MethodAssignDecl(decl)) => Ok(Some(statement_expression(
                super::method_assign_decl::convert(decl)?,
            ))),
            // `my $x OP= EXPR`: the declaration as the left operand.
            Some(crate::ast::SourceForm::DeclarationCompoundAssign) => {
                declaration_compound_assign_statement(stmt).map(Some)
            }
            Some(crate::ast::SourceForm::TakeLogicalTail { value, op, right }) => {
                take_logical_tail_node(value, op, right)
                    .map(|node| Some(statement_expression(node)))
            }
            Some(crate::ast::SourceForm::SignatureBind { signature, rhs }) => {
                signature_bind_node(signature, rhs).map(|node| Some(statement_expression(node)))
            }
            // `sub f { … }(ARGS)`: the routine as the operand of its call.
            Some(crate::ast::SourceForm::ImmediateCall) => super::immediate_call::convert(stmt)
                .ok_or_else(|| unsupported("source form"))?
                .map(|node| Some(statement_expression(node))),
            // `BEGIN say 1`: the phaser (second statement) over its bare statement.
            Some(crate::ast::SourceForm::BarePhaser) => match stmt {
                Stmt::SyntheticBlock(stmts) if stmts.len() == 2 => {
                    super::bare_prefix::bare_phaser_statement(&stmts[1])
                }
                _ => Err(unsupported("source form")),
            },
            // Only the on-demand lambda opens with a supply record.
            Some(
                crate::ast::SourceForm::SupplyBlock(_)
                | crate::ast::SourceForm::ControlPointy { .. }
                | crate::ast::SourceForm::WithPointy { .. }
                | crate::ast::SourceForm::GivenPointy { .. }
                | crate::ast::SourceForm::IfPointy { .. }
                | crate::ast::SourceForm::HyperAssign { .. }
                | crate::ast::SourceForm::InlineSubsetTerm(_)
                | crate::ast::SourceForm::RoutineTraits(_)
                | crate::ast::SourceForm::StartBlock,
            )
            | None => Err(unsupported("source form")),
        },
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
                if name.as_str() == "__mutsu_assign_callable_lvalue"
                    && let [Expr::DoStmt(stmt), Expr::ArrayLiteral(call_args), value] =
                        args.as_slice()
                    && call_args.is_empty()
                    && let Stmt::VarDecl { .. } = stmt.as_ref()
                {
                    let left = convert_expr(&Expr::Grouped(Box::new(Expr::DoStmt(stmt.clone()))))?;
                    return Ok(Some(statement_expression(method_lvalue_assignment(
                        left,
                        convert_expr(value)?,
                    ))));
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
        Stmt::SyntheticBlock(_) if crate::ast::temporize::recognize(stmt).is_some() => {
            let node = super::temporize::convert(stmt).expect("just checked")?;
            Ok(Some(statement_expression(node)))
        }
        // A sigilless declaration (`my \x = 5`, `my Int \x := $s`).
        Stmt::SyntheticBlock(_) if crate::ast::sigilless_decl::declaration(stmt).is_some() => {
            let decl = crate::ast::sigilless_decl::declaration(stmt).expect("just checked");
            Ok(Some(statement_expression(term_declaration(&decl)?)))
        }
        // `my $x will begin { ... }`: the declaration with `Trait::Will`s.
        Stmt::SyntheticBlock(_) if crate::ast::var_will::recognize(stmt).is_some() => {
            super::var_will::convert(stmt).map(Some)
        }
        // `my %h does R = EXPR`: one declaration with a `Trait::Does`.
        Stmt::SyntheticBlock(_) if crate::ast::var_does::recognize(stmt).is_some() => {
            let (decl, role, init) = crate::ast::var_does::recognize(stmt).expect("just checked");
            does_declaration(decl, role, init)
        }
        // `my $x = 1 and 2` / `my $x = 1, 2, 3`: the declaration and the tail the
        // parser re-attaches to it are one expression.
        Stmt::SyntheticBlock(_) if crate::ast::decl_tail::recognize(stmt).is_some() => {
            match super::decl_tail::convert(stmt) {
                Some(node) => Ok(Some(statement_expression(node?))),
                None => Err(unsupported("a declaration with a loose tail")),
            }
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
        Stmt::Block(body) => Ok(Some(statement_expression(signature_block_node(body)?))),
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
            let mut fields = vec![node_field(None, block_node(body)?)];
            // The source position of a main-compunit `END` (see `origin`).
            fields.extend(end_index.map(origin::end_index_field));
            Ok(Some(statement_expression(RakuAstNode { class, fields })))
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
                    // `proceed |c` / `succeed |c` is parsed into a guarded
                    // control transfer (`slipped_control_stmt`); rakudo has the
                    // plain call over the slipped argument.
                    if let Some(call) = slipped_control_call(cond, then_branch, else_branch)? {
                        return Ok(Some(statement_expression(call)));
                    }
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
                        node_field(Some("body"), signature_block_node(then_branch)?),
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
            is_statement_modifier,
            ..
        } => {
            // mutsu stores `until X` as `while !X` PLUS an `is_until` flag, so
            // the source keyword is recoverable: raku has a `Loop::Until` class
            // and renders the *undecorated* condition, so the `!` the parser
            // added is stripped back off.
            if *is_statement_modifier && label.is_none() {
                let mut real = body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
                if let (Some(modified), None) = (real.next(), real.next()) {
                    let mut statement = convert_stmt(modified)?
                        .ok_or_else(|| unsupported("empty while/until modifier body"))?;
                    statement.fields.push(node_field(
                        Some("loop-modifier"),
                        RakuAstNode {
                            class: if *is_until {
                                RakuAstClass::StatementModifierUntil
                            } else {
                                RakuAstClass::StatementModifierWhile
                            },
                            fields: vec![node_field(
                                None,
                                convert_expr(if *is_until {
                                    strip_negation(cond)?
                                } else {
                                    cond
                                })?,
                            )],
                        },
                    ));
                    return Ok(Some(statement));
                }
            }
            let mut fields = label_fields(label);
            fields.push(node_field(
                Some("condition"),
                convert_expr(if *is_until {
                    strip_negation(cond)?
                } else {
                    cond
                })?,
            ));
            fields.push(node_field(Some("body"), signature_block_node(body)?));
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
                // `repeat while C -> $p { }`: the parser keeps the parameter as
                // the loop's `init` declaration (`parser::repeat_param_clause`);
                // raku spells it as the body's signature.
                let body_node = match init.as_deref() {
                    Some(Stmt::VarDecl { name, .. }) => {
                        pointy_block(&[ParamDef::plain_positional(name)], body, None)?
                    }
                    Some(_) => return Err(unsupported("repeat loop with a setup statement")),
                    None => signature_block_node(body)?,
                };
                let mut fields = label_fields(label);
                fields.push(node_field(Some("body"), body_node));
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
            // `STMT when COND`: the parser's synthetic `given $_` over a
            // modifier `When` (see `Stmt::When::is_statement_modifier`).
            if let Some((cond, modified)) = when_modifier_parts(topic, body) {
                let mut statement = convert_stmt(modified)?
                    .ok_or_else(|| unsupported("empty when modifier body"))?;
                statement.fields.push(node_field(
                    Some("condition-modifier"),
                    RakuAstNode {
                        class: RakuAstClass::StatementModifierWhen,
                        fields: vec![node_field(None, convert_expr(cond)?)],
                    },
                ));
                return Ok(Some(statement));
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
                        node_field(Some("body"), given_body_node(body)?),
                    ],
                }))
            }
        }
        // `when Y { ... }` -> Statement::When(condition, body => plain Block).
        Stmt::When { cond, body, .. } => Ok(Some(RakuAstNode {
            class: RakuAstClass::StatementWhen,
            fields: vec![
                node_field(Some("condition"), convert_expr(cond)?),
                node_field(Some("body"), signature_block_node(body)?),
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
                            && t != crate::parser::ANON_DECL_TRAIT
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
            super::symbolic_deref::replace_declarator_name(&mut node, name_expr.as_ref())?;
            if super::routine_source::apply(&mut node, body)? {
                routine_traits::add_flags(&mut node, *multi, false, &Default::default())?;
                if let Some(scope) = sub_scope(custom_traits) {
                    node.fields
                        .insert(0, leaf_field(Some("scope"), Value::str_from(scope)));
                }
                return Ok(Some(statement_expression(node)));
            }
            let flags = routine_traits::IsTraits {
                is_rw: *is_rw,
                is_raw: *is_raw,
                export_tags: export_tags.clone(),
                ..Default::default()
            }
            .with_precedence(associativity.as_ref(), precedence_trait.as_ref())?;
            routine_traits::add_flags(&mut node, *multi, false, &flags)?;
            routine_traits::add_custom(&mut node, custom_traits, !flags.nodes().is_empty())?;
            if let Some(scope) = sub_scope(custom_traits) {
                // `scope => "our"` / `"anon"` leads the node; `my` is the
                // default scope and renders no field.
                node.fields
                    .insert(0, leaf_field(Some("scope"), Value::str_from(scope)));
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
                (
                    *is_export != !export_tags.is_empty(),
                    "method with a bare `is export`",
                ),
                (*our_variable_form, "`our &m = method` form"),
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
            super::symbolic_deref::replace_declarator_name(&mut node, name_expr.as_ref())?;
            let source_traits = super::routine_source::apply(&mut node, body)?;
            let flags = routine_traits::IsTraits {
                is_rw: *is_rw,
                is_raw: *is_raw,
                export_tags: export_tags.clone(),
                ..Default::default()
            };
            if source_traits {
                routine_traits::add_flags(&mut node, *multi, *is_private, &Default::default())?;
            } else {
                routine_traits::add_flags(&mut node, *multi, *is_private, &flags)?;
                let custom = routine_traits::method_custom_traits(
                    custom_traits,
                    *is_default_candidate,
                    deprecated_message.as_deref(),
                )?;
                routine_traits::add_custom(&mut node, &custom, !flags.nodes().is_empty())?;
            }
            // `scope => "my"` / `"our"` leads the node, ahead of `multiness`.
            // Rakudo's RakuAST does not retain a scope field on Submethod:
            // `my`, `our`, and unscoped forms all have the same model shape.
            let scope = if *is_submethod {
                None
            } else if *is_our {
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
            if let Some(expr) = name_expr {
                let name = node_field(Some("name"), super::symbolic_deref::indirect_name(expr)?);
                match fields.iter_mut().find(|f| f.name == Some("name")) {
                    Some(slot) => *slot = name,
                    None => fields.push(name),
                }
            } else if has_leading_colons(custom_traits) {
                name_parts::with_leading_empty(&mut fields);
            }
            super::declare_how::add_field(&mut fields, custom_traits);
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
                for t in class_traits(
                    std::slice::from_ref(parent),
                    &[],
                    parent_args,
                    false,
                    false,
                    &[],
                )? {
                    also_statements.push(super::role::also_is_statement(t));
                }
            }
            let mut traits = class_traits(
                &header_parents,
                does_parents,
                parent_args,
                *class_is_rw,
                *is_hidden,
                hidden_parents,
            )?;
            traits.extend(decl_traits::class_custom_traits(&visible_custom_traits)?);
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
            if variants.is_empty() {
                return Err(unsupported("enum without a body"));
            }
            let term = super::enum_term::enum_term_node(variants, *variant_form)?;
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
            let mut node = RakuAstNode { class, fields };
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
                    Ok(Some(statement_expression(super::compound_stmt::mark(
                        compound_assignment_infix(target, op, rhs)?,
                        name,
                    ))))
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
fn hyper_infix(
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
                class: RakuAstClass::Assignment,
                fields: vec![RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Adverb("item"),
                }],
            }
        } else {
            RakuAstNode {
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
    // The five-argument record comes from `assign_to_target_expr`; the
    // six-argument record is the method-lvalue compound-assignment expansion.
    // Both retain the method call and final value needed to rebuild through
    // the parser's assignment path. A non-true sixth argument is not a source
    // form this conversion recognizes.
    let (target, method, method_args, value, write_back) = match args {
        [
            target,
            method,
            Expr::ArrayLiteral(method_args),
            value,
            write_back,
        ] => (target, method, method_args, value, write_back),
        [
            target,
            method,
            Expr::ArrayLiteral(method_args),
            value,
            write_back,
            Expr::Literal(flag),
        ] if matches!(flag.view(), ValueView::Bool(true)) => {
            (target, method, method_args, value, write_back)
        }
        _ => {
            return None;
        }
    };
    let (method, dynamic_name) = match method {
        Expr::Literal(method) | Expr::LiteralSrc(method, _) => {
            let ValueView::Str(method) = method.view() else {
                return None;
            };
            (Some(method.to_string()), None)
        }
        name_expr => (None, Some(name_expr)),
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
    let is_rerouted = (method.as_deref() == Some("item") && method_args.is_empty())
        || (method.as_deref() == Some("AT-POS")
            && method_args.len() == 1
            && !matches!(target, Expr::Var(n) | Expr::BareWord(n) if n == "self"));
    if is_rerouted {
        return None;
    }
    let call = if let Some(name_expr) = dynamic_name {
        Expr::DynamicMethodCall {
            target: Box::new(target.clone()),
            name_expr: Box::new(name_expr.clone()),
            args: method_args.clone(),
            modifier: None,
            quoted: matches!(name_expr, Expr::StringInterpolation(_)),
        }
    } else {
        let method = method.as_deref()?;
        let (modifier, method) = match method.strip_prefix('!') {
            Some(private) => (Some('!'), private),
            None => (None, method),
        };
        Expr::MethodCall {
            target: Box::new(target.clone()),
            name: crate::symbol::Symbol::intern(method),
            args: method_args.clone(),
            modifier,
            quoted: false,
            sugar: false,
        }
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
        Expr::Call { name, args, .. } if name.as_str() == "__mutsu_bind_index_value" => {
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
/// `parser::assign_to_target_expr`. Callable targets retain their full source
/// expression, including method objects, indexed values and adverb-bearing
/// subscripts; compound writeback records stay the boundary.
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
                listop: false,
            }
        }
        // `(LVALUES) = rhs` assigns to a parenthesised list
        // (`parser::paren_list_assign_expr`).
        "__mutsu_assign_callable_lvalue"
            if call_args.is_empty() && matches!(target, Expr::ArrayLiteral(_)) =>
        {
            Expr::Grouped(Box::new(target.clone()))
        }
        // `(COND ?? $a !! $b) = v`, `($a || $b) = v`, `nqp::op(..) = v`: an
        // lvalue expression with no call arguments is the parenthesised
        // left side of a plain assignment (`parser::ternary` /
        // `parser::precedence::logic`). A `do` declaration or an internal
        // call target belongs to the compound-assignment writebacks, whose
        // value is not the written right side, so those stay the boundary.
        "__mutsu_assign_callable_lvalue"
            if call_args.is_empty()
                && matches!(
                    target,
                    Expr::Ternary { .. } | Expr::Binary { .. } | Expr::Call { .. }
                )
                && !matches!(target, Expr::Call { name, .. } if is_desugar_marker(name.as_str())) =>
        {
            Expr::Grouped(Box::new(target.clone()))
        }
        "__mutsu_assign_callable_lvalue" => Expr::CallOn {
            target: Box::new(target.clone()),
            args: call_args.clone(),
        },
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

/// `my $x OP= EXPR` (`ast::decl_compound_assign`) -> `ApplyInfix(left =>
/// VarDeclaration, infix => MetaInfix::Assign(OP), right => EXPR)`, as
/// measured on rakudo 2026.09.
// Cost: O(n), n = size of the declaration and the right-hand side.
fn declaration_compound_assign(stmt: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    let (decl, op, rhs) =
        crate::ast::decl_compound_assign::parts(stmt).ok_or_else(|| unsupported("source form"))?;
    let statement = convert_stmt(decl)?.ok_or_else(|| unsupported("source form"))?;
    let decl = expression_of(&statement).ok_or_else(|| unsupported("source form"))?;
    compound_assignment_with_left(decl, op, rhs)
}

/// [`declaration_compound_assign`] as a statement, including the postfix `for`
/// the parser hoisted the declaration out of: the loop renders as the
/// assignment's statement with its `loop-modifier`, whose assignment then
/// takes the declaration as its left operand.
// Cost: O(n), n = size of the declaration, the loop and the right-hand side.
fn declaration_compound_assign_statement(stmt: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    let Some((decl, loop_stmt)) = crate::ast::decl_compound_assign::for_modifier_parts(stmt) else {
        return Ok(statement_expression(declaration_compound_assign(stmt)?));
    };
    let mut statement = convert_stmt(loop_stmt)?.ok_or_else(|| unsupported("source form"))?;
    let decl_statement = convert_stmt(decl)?.ok_or_else(|| unsupported("source form"))?;
    let decl = expression_of(&decl_statement).ok_or_else(|| unsupported("source form"))?;
    let field = statement
        .fields
        .iter_mut()
        .find(|f| f.name == Some("expression"))
        .ok_or_else(|| unsupported("source form"))?;
    let RakuAstFieldValue::Node(value) = &field.value else {
        return Err(unsupported("source form"));
    };
    let ValueView::RakuAst(assignment) = value.view() else {
        return Err(unsupported("source form"));
    };
    let mut assignment = (*assignment).clone();
    if assignment.class != RakuAstClass::ApplyInfix {
        return Err(unsupported("source form"));
    }
    // The statement-assignment mark names the variable the loop body
    // assigns; the declaration statement it now sits in is not one.
    assignment
        .fields
        .retain(|f| !super::compound_stmt::is_source_field(f));
    for f in assignment.fields.iter_mut() {
        if f.name == Some("left") {
            *f = node_field(Some("left"), decl.clone());
        }
    }
    *field = node_field(Some("expression"), assignment);
    Ok(statement)
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
            class: RakuAstClass::MetaInfixReverse,
            fields: vec![node_field(
                None,
                if inner == "=" {
                    RakuAstNode {
                        class: RakuAstClass::Assignment,
                        fields: vec![RakuAstField {
                            name: None,
                            value: RakuAstFieldValue::Adverb("item"),
                        }],
                    }
                } else {
                    RakuAstNode {
                        class: RakuAstClass::MetaInfixAssign,
                        fields: vec![node_field(None, plain_infix(inner))],
                    }
                },
            )],
        },
        _ => RakuAstNode {
            class: RakuAstClass::MetaInfixAssign,
            fields: vec![node_field(None, plain_infix(base_op))],
        },
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
            node_field(Some("right"), right),
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
        anonymous,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        custom_traits,
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
        n == "__has_initializer"
            // The parser's own mark of a declaration whose initializer reads
            // the new binding; the lowering marks it again.
            || n == "__init_sees_self"
            || n == crate::ast::shaped_decl::SHAPED_DECL
            || n == crate::ast::keyed_hash::IMPLICIT_VALUE_TYPE
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
        traits.push(decl_traits::dynamic_trait());
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
    let mut traits = parts.custom_traits.to_vec();
    if let Some(init) = init {
        traits.push(("__has_initializer".to_string(), None));
        parts.expr = init;
    }
    parts.custom_traits = &traits;
    let Some(statement) = var_decl_statement(parts, false)? else {
        return Ok(None);
    };
    let Some(mut declaration) = expression_of(&statement) else {
        return Ok(None);
    };
    let does = Value::rakuast(Box::new(RakuAstNode {
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
struct VarDeclParts<'a> {
    name: &'a str,
    anonymous: bool,
    expr: &'a Expr,
    type_constraint: &'a Option<String>,
    is_state: bool,
    is_our: bool,
    is_dynamic: bool,
    custom_traits: &'a [(String, Option<Expr>)],
    /// The declaration's `where` expression.
    where_constraint: Option<&'a Expr>,
    /// Whether it is `is export`, and with which tags (`["DEFAULT"]` for the
    /// bare form): the parser keeps the trait as these two fields.
    is_export: bool,
    export_tags: &'a [String],
}

fn var_decl_parts(stmt: &Stmt) -> Result<VarDeclParts<'_>, RuntimeError> {
    let Stmt::VarDecl {
        anonymous,
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        custom_traits,
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
        fields.push(node_field(Some("type"), build_type_node(t)?));
    }
    fields.push(leaf_field(Some("sigil"), Value::str(sigil.to_string())));
    if let Some(t) = type_name.filter(|_| is_anonymous) {
        fields.push(node_field(Some("type"), build_type_node(t)?));
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
    if name_parts::is_qualified_identifier(s) && !s.starts_with("::?") {
        return name_parts::qualified_name(s);
    }
    RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(s.to_string()))],
    }
}

pub(super) use super::type_build::{build_type_node, is_simple_type};

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
        match decl.call_assign {
            Some((method, args)) => RakuAstNode {
                class: RakuAstClass::InitializerCallAssign,
                fields: vec![node_field(
                    None,
                    method_call_postfix(&method.resolve(), args, None, false)?,
                )],
            },
            None => RakuAstNode {
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
    let Some(declarator) = decl.declarator else {
        return convert_expr(decl.expr);
    };
    let mut declarator = declarator.clone();
    if let Stmt::VarDecl { name, .. } = &mut declarator
        && crate::parser::is_anon_sigilless_source(name)
    {
        *name = "__ANON_STATE#".to_string();
    }
    convert_expr(&Expr::DoStmt(Box::new(declarator)))
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

fn parenthesized_expression(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let semilist = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(convert_expr(expr)?))],
    };
    Ok(RakuAstNode {
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

/// The scope a named sub's `custom_traits` mark it with, when it is not the
/// default `my`: `our sub f`, or `anon sub g` (built but not installed).
// Cost: O(t), t = number of custom traits.
fn sub_scope(custom_traits: &[(String, Option<Expr>)]) -> Option<&'static str> {
    if custom_traits.iter().any(|(t, _)| t == OUR_SCOPED) {
        Some("our")
    } else if custom_traits
        .iter()
        .any(|(t, _)| t == crate::parser::ANON_DECL_TRAIT)
    {
        Some("anon")
    } else {
        None
    }
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
            class: RakuAstClass::TypeParameterized,
            fields: vec![
                node_field(Some("base-type"), base),
                node_field(
                    Some("args"),
                    RakuAstNode {
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
    super::postfix_grouping::convert(target, index_node)
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

fn take_logical_tail_node(
    value: &Expr,
    op: &crate::token_kind::TokenKind,
    right: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
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

fn signature_bind_node(signature: &Expr, rhs: &Expr) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), convert_expr(signature)?),
            node_field(Some("infix"), plain_infix(":=")),
            node_field(Some("right"), convert_expr(rhs)?),
        ],
    })
}

pub(super) fn convert_expr(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    if let Some(node) = super::deferred_name::convert(expr) {
        return node;
    }
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
        // `<a b  c>`: rakudo keeps the raw text of a word list, whitespace
        // included, as one word-quote (ADR-12199).
        Expr::Spelled(spelled) => match &spelled.spelling {
            Spelling::SourceIdentifier => written_identifier(&spelled.expr),
            Spelling::ArraySections(sections) => {
                let fields = sections
                    .iter()
                    .map(|items| {
                        if items.is_empty() {
                            return Ok(node_field(
                                None,
                                RakuAstNode {
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
                    class: RakuAstClass::CircumfixArrayComposer,
                    fields: vec![node_field(
                        None,
                        RakuAstNode {
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
                    class: RakuAstClass::StatementExpression,
                    fields,
                })
            }
            Spelling::LabelTerm(name) => {
                let Expr::Literal(value) = &spelled.expr else {
                    return Err(unsupported("label spelling without a label value"));
                };
                Ok(RakuAstNode {
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
                if let Some(node) = bareword::convert(name) {
                    return Ok(node);
                }
                Ok(RakuAstNode {
                    class: RakuAstClass::TermName,
                    fields: vec![node_field(None, name_from_identifier(name))],
                })
            }
            Spelling::Words(text) => Ok(word_quote(text)),
            Spelling::ParenthesizedDeclaratorAdverb => parenthesized_expression(&spelled.expr),
            Spelling::Heredoc { stop } => heredoc_node(&spelled.expr, stop),
            Spelling::WordQuote {
                quotewords,
                val,
                text,
            } => Ok(word_quote_with(*quotewords, *val, text)),
            Spelling::InterpolatingWords { val, text } => super::word_quote::convert(*val, text),
            Spelling::BareStatement => super::bare_prefix::bare_statement_node(&spelled.expr),
            Spelling::PackageIndirect { package, key } => {
                super::symbolic_deref::convert_package_indirect(package, key)
            }
            Spelling::NamedSubscript { .. } | Spelling::ConflictingSubscript { .. } => {
                Err(unsupported("subscript adverb source form"))
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
            class: RakuAstClass::OnlyStar,
            fields: Vec::new(),
        }),
        // `now` is `Term::Named`, not a call.
        Expr::Call { name, args, .. } if args.is_empty() && name.as_str() == "now" => {
            Ok(RakuAstNode {
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
            if let Some(words) = super::word_quote::convert_words_atom(name.as_str(), args) {
                return words;
            }
            if is_desugar_marker(name.as_str()) {
                if name.as_str() == "__mutsu_assign_callable_lvalue"
                    && let [Expr::DoStmt(stmt), Expr::ArrayLiteral(call_args), value] =
                        args.as_slice()
                    && call_args.is_empty()
                    && let Stmt::VarDecl { .. } = stmt.as_ref()
                {
                    let left = convert_expr(&Expr::Grouped(Box::new(Expr::DoStmt(stmt.clone()))))?;
                    return Ok(method_lvalue_assignment(left, convert_expr(value)?));
                }
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
            if let Some(coercion) = super::type_call::convert(name.as_str(), args)? {
                return Ok(coercion);
            }
            if let Some(op) = nqp_op(name.as_str()) {
                let mut fields = vec![leaf_field(None, Value::str(op.to_string()))];
                for arg in args.iter().filter(|a| !is_injected_named_arg(a)) {
                    fields.push(node_field(None, convert_expr(arg)?));
                }
                return Ok(RakuAstNode {
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
            // raku omits `args` for an argument-less `$f()` / `&f()` / `$f.()`.
            let mut call_term = RakuAstNode {
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
                if let Some(Stmt::SourceForm(form)) = body.first()
                    && let crate::ast::SourceForm::SupplyBlock(source_body) = form.as_ref()
                {
                    return super::react::convert_supply(source_body);
                }
                if let Some(node) = super::hyper_dotty_writeback::convert(body)? {
                    return Ok(node);
                }
                if let Some(declaration) =
                    crate::ast::inline_subset_term::inline_subset_declaration(body)
                {
                    let statement = convert_stmt(declaration)?
                        .ok_or_else(|| unsupported("inline subset declaration"))?;
                    return statement
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
                        .ok_or_else(|| unsupported("inline subset declaration"));
                }
                // `(LVALUES) »=» VALUE`: the record that opens the expansion.
                if let Some(Stmt::SourceForm(form)) = body.first()
                    && let crate::ast::SourceForm::HyperAssign {
                        target,
                        value,
                        dwim_left,
                        dwim_right,
                    } = form.as_ref()
                {
                    return hyper_infix(target, "=", value, *dwim_left, *dwim_right);
                }
                // `120 = 3`: the written assignment to an immutable literal.
                if let Some((literal, rhs)) = crate::parser::literal_assign_ro_parts(expr) {
                    return assignment_around(convert_expr(literal)?, false, rhs);
                }
                return Err(unsupported("desugared do-block"));
            }
            let expression = RakuAstNode {
                class: RakuAstClass::StatementPrefixDo,
                fields: vec![node_field(None, block_node(body)?)],
            };
            if label.is_some() {
                let mut fields = label_fields(label);
                fields.push(node_field(Some("expression"), expression));
                Ok(RakuAstNode {
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
                ..
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
        // A qualified name that did not resolve in this parse scope is still
        // a valid Raku name term (`A::B`). Rakudo represents it as an
        // argument-less Call::Name, including when it is later used as a
        // method-call operand; the failed lookup remains catchable at runtime.
        Expr::BareWord(name) if name_parts::is_qualified_identifier(name) => {
            call_name(name, &[], false)
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
                    | Stmt::RoleDecl { .. }
                    | Stmt::VarDecl { .. }
                    | Stmt::SubDecl { .. }
                    | Stmt::ProtoDecl { .. }
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
        // A parenthesized comma list of declarations is grouped by the parser
        // into one synthetic block. Rakudo keeps each declaration as an
        // operand of the comma ApplyListInfix.
        Expr::DoStmt(stmt)
            if matches!(stmt.as_ref(), Stmt::SyntheticBlock(stmts)
                if stmts.len() > 1 && stmts.iter().all(|stmt| matches!(stmt, Stmt::VarDecl { .. }))) =>
        {
            let Stmt::SyntheticBlock(stmts) = stmt.as_ref() else {
                unreachable!()
            };
            let mut operands = Vec::with_capacity(stmts.len());
            for declaration in stmts {
                let statement = var_decl_statement(var_decl_parts(declaration)?, false)?
                    .ok_or_else(|| unsupported("declaration term"))?;
                let expression =
                    expression_of(&statement).ok_or_else(|| unsupported("declaration term"))?;
                operands.push(Value::rakuast(Box::new(expression)));
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
        Expr::DoStmt(stmt) if is_modifier_statement(stmt) => {
            convert_stmt(stmt)?.ok_or_else(|| unsupported("empty modified expression"))
        }
        // A signature declaration in expression position (`if my ($a, $b) = …`).
        Expr::DoStmt(stmt) if source_form(stmt).is_some() => match source_form(stmt) {
            Some(crate::ast::SourceForm::DeclarationModifier {
                declaration,
                condition,
                kind,
            }) => declaration_modifier_node(declaration, condition, *kind),
            Some(crate::ast::SourceForm::SignatureDecl(decl)) => {
                super::signature_decl::convert(decl)
            }
            Some(crate::ast::SourceForm::AttributeGroup(attributes)) => {
                super::attribute_group::to_signature(attributes)
            }
            Some(crate::ast::SourceForm::AttributeColonArglist(attributes)) => {
                super::attribute_group::convert_colon(attributes)
            }
            Some(crate::ast::SourceForm::MethodAssignDecl(decl)) => {
                super::method_assign_decl::convert(decl)
            }
            Some(crate::ast::SourceForm::DeclarationCompoundAssign) => {
                declaration_compound_assign(stmt)
            }
            Some(crate::ast::SourceForm::TakeLogicalTail { value, op, right }) => {
                take_logical_tail_node(value, op, right)
            }
            Some(crate::ast::SourceForm::SignatureBind { signature, rhs }) => {
                signature_bind_node(signature, rhs)
            }
            // `do sub f { … }(ARGS)`: the call statement under the prefix.
            Some(crate::ast::SourceForm::ImmediateCall) => {
                let node = super::immediate_call::convert(stmt)
                    .ok_or_else(|| unsupported("source form"))??;
                Ok(RakuAstNode {
                    class: RakuAstClass::StatementPrefixDo,
                    fields: vec![node_field(None, statement_expression(node))],
                })
            }
            // Only the on-demand lambda opens with a supply record.
            Some(
                crate::ast::SourceForm::SupplyBlock(_)
                | crate::ast::SourceForm::ControlPointy { .. }
                | crate::ast::SourceForm::BarePhaser
                | crate::ast::SourceForm::WithPointy { .. }
                | crate::ast::SourceForm::GivenPointy { .. }
                | crate::ast::SourceForm::IfPointy { .. }
                | crate::ast::SourceForm::HyperAssign { .. }
                | crate::ast::SourceForm::InlineSubsetTerm(_)
                | crate::ast::SourceForm::RoutineTraits(_)
                | crate::ast::SourceForm::StartBlock,
            )
            | None => Err(unsupported("source form")),
        },
        // `()` -> `Circumfix::Parentheses(SemiList.new)`: no statement inside.
        Expr::Grouped(inner) if matches!(inner.as_ref(), Expr::ArrayLiteral(items) if items.is_empty()) => {
            Ok(RakuAstNode {
                class: RakuAstClass::CircumfixParentheses,
                fields: vec![node_field(
                    None,
                    RakuAstNode {
                        class: RakuAstClass::SemiList,
                        fields: Vec::new(),
                    },
                )],
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
            // A bareword target is a term (spelled by its term key,
            // ADR-12441): rakudo's left side is the `Term::Name`, and `=` is
            // a list assignment (no `:item`).
            if let Some(term) = crate::runtime::term_names::term_spelling(name) {
                let left = RakuAstNode {
                    class: RakuAstClass::TermName,
                    fields: vec![node_field(None, name_from_identifier(term))],
                };
                let infix = if *is_bind {
                    plain_infix(":=")
                } else {
                    RakuAstNode {
                        class: RakuAstClass::Assignment,
                        fields: Vec::new(),
                    }
                };
                return Ok(RakuAstNode {
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
            if let Some((target, rhs)) = super::star_lvalue::parts(name, expr) {
                return Ok(method_lvalue_assignment(
                    convert_expr(&target)?,
                    convert_expr(&rhs)?,
                ));
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
            if name == "_" && super::underscore_bindings::declares_placeholder(true) {
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
            if name == "_" && super::underscore_bindings::declares_placeholder(false) {
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
            if matches!(name.as_str(), "?BLOCK" | "?ROUTINE") {
                return Ok(RakuAstNode {
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
        } => Ok(RakuAstNode {
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
        Expr::Unary { op, expr, word } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(
                    Some("prefix"),
                    // `so EXPR` / `not EXPR` are the `?` / `!` the parser
                    // folds them into.
                    match (word, op) {
                        (true, crate::token_kind::TokenKind::Question) => RakuAstNode {
                            class: RakuAstClass::Prefix,
                            fields: vec![leaf_field(None, Value::str("so".to_string()))],
                        },
                        (true, crate::token_kind::TokenKind::Bang) => RakuAstNode {
                            class: RakuAstClass::Prefix,
                            fields: vec![leaf_field(None, Value::str("not".to_string()))],
                        },
                        // `||` only ever prefixes a subscript dimension
                        // (`splat_dimensions`).
                        (false, crate::token_kind::TokenKind::OrOr) => RakuAstNode {
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
            class: RakuAstClass::CircumfixArrayComposer,
            fields: vec![node_field(
                None,
                RakuAstNode {
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
        // `@a[|| @i] = 5`: like `@a[0] = 5`, a one-statement subscript takes
        // the value as its `assignee`.
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
            is_positional,
        } if dimensions.len() == 1 => subscript_dims_node(
            target,
            &splat_dimensions(dimensions),
            *is_positional,
            Some(value),
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
                    subscript_dims_node(
                        target,
                        &splat_dimensions(dimensions),
                        *is_positional,
                        None,
                        Vec::new(),
                    )?,
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
        // `Assignment` infix over a `%h{…}` subscript.
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
                class: RakuAstClass::ApplyInfix,
                fields: vec![
                    node_field(Some("left"), left),
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
            ..
        } => subscript_node(target, index, true, Some(value), Vec::new()),
        // `%h<k> = v`: the assignment folds into the literal hash index.
        Expr::IndexAssign {
            target,
            index,
            value,
            is_positional: false,
            spelling: crate::ast::IndexSpelling::Angle,
        } if angle_key_text(index).is_some() => {
            angle_subscript_node(target, index, Some(value), Vec::new())
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            is_positional: false,
            ..
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
                    class: RakuAstClass::Sub,
                    fields: vec![node_field(Some("body"), blockoid(body)?)],
                };
                super::declarator_docs::attach(&mut node, doc.get());
                if super::routine_source::apply(&mut node, body)? {
                    return Ok(node);
                }
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
pub(super) fn interp_segment(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
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
        // `<::(EXPR)>` -> `Assertion::Named` over `Name(Part::Empty,
        // Part::Expression(EXPR))` (measured on 2026.09).
        RegexNode::SymbolicSubrule {
            expr, capturing, ..
        } => {
            let part = RakuAstNode {
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
    multiness: Option<&str>,
    param_defs: &[ParamDef],
    export_trait: Option<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::parameters(param_defs);
    regex_declaration_with_body(
        class,
        Some(name),
        scope,
        multiness,
        param_defs,
        regex_node(&tree.body)?,
        export_trait.into_iter().collect(),
    )
}

/// [`regex_declaration`] over an already converted body (the `{*}` of a proto).
// Cost: O(p), p = size of the parameters.
fn regex_declaration_with_body(
    class: RakuAstClass,
    name: Option<&str>,
    scope: Option<&str>,
    multiness: Option<&str>,
    param_defs: &[ParamDef],
    body: RakuAstNode,
    traits: Vec<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::with_capacity(6);
    if let Some(scope) = scope {
        fields.push(leaf_field(Some("scope"), Value::str_from(scope)));
    }
    if let Some(multiness) = multiness {
        fields.push(leaf_field(Some("multiness"), Value::str_from(multiness)));
    }
    if let Some(name) = name {
        fields.push(node_field(Some("name"), name_from_identifier(name)));
    }
    if !param_defs.is_empty() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, None)?,
        ));
    }
    if !traits.is_empty() {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        });
    }
    fields.push(node_field(Some("body"), body));
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

/// The leading `labels => (Label("..."),)` field for a labelled loop, or an
/// empty vec when unlabelled. raku always renders labels first, and a `Label`
/// holds its name as a positional (`Label.new("L")`, measured on 2026.09).
pub(super) fn label_fields(label: &Option<String>) -> Vec<RakuAstField> {
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
fn topic_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::Block,
        fields: vec![
            true_field("implicit-topic"),
            true_field("required-topic"),
            true_field("may-have-signature"),
            node_field(Some("body"), blockoid(body)?),
        ],
    })
}

/// The body of a `given`: the topic block, or the `PointyBlock` of
/// `given X -> PARAM { BODY }`, from the record the parser leaves after the
/// parameter bind.
// Cost: O(n), n = size of the body.
fn given_body_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    for stmt in body {
        if let Stmt::SourceForm(form) = stmt
            && let crate::ast::SourceForm::GivenPointy {
                param_def,
                body: written,
            } = form.as_ref()
        {
            return pointy_block(std::slice::from_ref(param_def), written, None);
        }
    }
    topic_block_node(body)
}

/// The body of a `CATCH` / `CONTROL` block: it topicalizes the exception, so
/// raku marks it `implicit-topic`, `required-topic` and `exception`, all
/// `True`, and gives it no `may-have-signature`.
fn exception_block_node(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
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
            ..
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
        return literal_parameter(pd, value);
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
    let (sigil, desigil) = match pd.name.as_str() {
        ANONYMOUS_SCALAR_PARAM => ("$", ""),
        ANONYMOUS_ARRAY_PARAM => ("@", ""),
        ANONYMOUS_HASH_PARAM => ("%", ""),
        name => split_sigil(name),
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
    if pd.name == TYPE_ONLY_PARAM && !pd.is_invocant {
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
    let traits: Vec<&str> = pd
        .traits
        .iter()
        .map(String::as_str)
        .filter(|t| !matches!(*t, "invocant" | IMPLICIT_INVOCANT_TRAIT))
        .collect();
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

/// The parser's name for a parameter written as a bare type (`sub f(Int)`).
pub(super) const TYPE_ONLY_PARAM: &str = "__type_only__";

/// The parser's names for an anonymous `$` / `@` / `%` parameter.
pub(super) const ANONYMOUS_SCALAR_PARAM: &str = "__ANON_STATE#";
pub(super) const ANONYMOUS_ARRAY_PARAM: &str = "@__ANON_ARRAY__";
pub(super) const ANONYMOUS_HASH_PARAM: &str = "%__ANON_HASH__";

/// The parser's name for an anonymous `[…]` destructuring parameter.
pub(super) const ANONYMOUS_ARRAY_SUBSIGNATURE: &str = "@";
/// The parser's name for an anonymous `(…)` destructuring parameter.
pub(super) const ANONYMOUS_SUBSIGNATURE: &str = "__subsig__";

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
    if !matches!(
        pd.name.as_str(),
        super::lower::ANONYMOUS_CAPTURE | ANONYMOUS_SUBSIGNATURE
    ) {
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

pub(super) fn type_setting_any() -> RakuAstNode {
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
fn anonymous_declaration(
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
    regex_declaration_with_body(
        class,
        None,
        None,
        None,
        &params,
        regex_node(&tree.body)?,
        Vec::new(),
    )
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
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(processors),
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

/// `q:to/END/`: the heredoc's text as the quoted string it evaluates to,
/// plus the terminator line rakudo keeps as `stop`.
fn heredoc_node(body: &Expr, stop: &str) -> Result<RakuAstNode, RuntimeError> {
    let mut node = convert_expr(body)?;
    if node.class != RakuAstClass::QuotedString {
        return Err(unsupported("a heredoc that is not quoted text"));
    }
    node.class = RakuAstClass::Heredoc;
    node.fields
        .push(leaf_field(Some("stop"), Value::str(stop.to_string())));
    Ok(node)
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
    Ok(RakuAstNode { class, fields })
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
            class: RakuAstClass::ColonPairValue,
            fields: vec![
                leaf_field(Some("key"), Value::str(key.to_string())),
                node_field(Some("value"), value),
            ],
        });
    }
    let semilist = RakuAstNode {
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
        ..
    } = expr
    else {
        return Err(unsupported("literal hash index provenance"));
    };
    super::postfix_grouping::convert(
        target,
        RakuAstNode {
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
    Some(crate::ast::package_header::wrap(
        package,
        &name.resolve(),
        header,
    ))
}
