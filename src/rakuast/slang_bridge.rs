//! The RakuAST end of the slang host (ADR-15762 §2.3, §2.5): core productions
//! the Rust parser parses on a slang token's behalf become RakuAST nodes, and the
//! node a slang action attaches is lowered where the production stood.

use super::{RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

/// Parse the core production `rule` at the start of `text`: its length in bytes
/// and its RakuAST node, `None` when it does not match there, or an error when
/// the production has no shim yet.
// Cost: O(n), n = size of the production (parse and conversion).
pub(crate) fn parse_production(rule: &str, text: &str) -> Result<Option<(usize, Value)>, String> {
    // The node is read from the parse (`.AST`'s shapes), so the parse keeps the
    // term origins that reading wants.
    let _origins_wanted = crate::parser::term_origins::Wanted::enter();
    if rule == "signature" {
        return signature_production(text);
    }
    if rule == "trait" {
        return trait_production(text);
    }
    if rule == "variable" {
        return variable_production(text);
    }
    if rule == "statementlist" {
        return statementlist_production(text);
    }
    if rule == "arglist" {
        return arglist_production(text);
    }
    if rule == "EXPR" {
        return for_expr_production(text);
    }
    if rule == "pointy-block" {
        return pointy_block_production(text);
    }
    // `<statement>` is the statement node itself (a `Statement::Expression`),
    // and `<blorst>` a block or such a statement.
    if rule == "statement" || (rule == "blorst" && !text.starts_with('{')) {
        return statement_production(text);
    }
    if rule == "blockoid" {
        return blockoid_production(text);
    }
    let rule = if rule == "blorst" { "block" } else { rule };
    let parsed = crate::parser::parse_slang_shim_production(rule, text)
        .ok_or_else(|| format!("the core rule <{rule}> has no slang shim yet (ADR-15762 §2.3)"))?;
    let (rest, stmt) = match parsed {
        Ok(found) => found,
        Err(e) => return fatal_or_no_match(&e),
    };
    let node = production_node(&stmt).map_err(|e| e.message.to_string())?;
    Ok(Some((text.len() - rest.len(), node)))
}

/// The `<blockoid>` core rule: the braces and statements at the start of `text`
/// as the `RakuAST::Blockoid` of the block they parse to.
// Cost: O(n), n = size of the block (parse and conversion).
fn blockoid_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((len, block)) = parse_production("block", text)? else {
        return Ok(None);
    };
    let body = match block.view() {
        ValueView::RakuAst(node) => node.fields.iter().find_map(|f| match (&f.name, &f.value) {
            (Some("body"), RakuAstFieldValue::Node(v)) => Some(v.clone()),
            _ => None,
        }),
        _ => None,
    };
    body.map(|b| Some((len, b)))
        .ok_or_else(|| "slang shim: the block has no body".to_string())
}

/// The `<statementlist>` core rule: the statements at the start of `text` as a
/// `RakuAST::StatementList` (ADR-16888).
// Cost: O(n), n = size of the list (parse and conversion).
fn statementlist_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, mut stmts)) = crate::parser::parse_slang_statementlist(text) else {
        return Ok(None);
    };
    crate::whatever_curry::mark::mark_program(&mut stmts);
    let names = NameScope::default();
    let node =
        super::statements::statement_list(&names, &stmts).map_err(|e| e.message.to_string())?;
    Ok(Some((
        text.len() - rest.len(),
        Value::rakuast(Box::new(node)),
    )))
}

/// A failed core production: no match, unless the failure is fatal (a slang
/// action's `$/.panic` in a production nested inside it), which ends the parse
/// as rakudo's exception does instead of letting the slang backtrack (#16888).
// Cost: O(1).
fn fatal_or_no_match(e: &crate::parser::PError) -> Result<Option<(usize, Value)>, String> {
    if e.is_fatal() {
        Err(e.to_string())
    } else {
        Ok(None)
    }
}

/// The `<statement>` core rule: the one statement at the start of `text`, as the
/// node rakudo's `statement` action makes (not unwrapped to its expression).
// Cost: O(n), n = size of the statement (parse and conversion).
fn statement_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let parsed = crate::parser::parse_slang_shim_production("statement", text)
        .ok_or_else(|| "the core rule <statement> has no slang shim".to_string())?;
    let (rest, mut stmt) = match parsed {
        Ok(found) => found,
        Err(e) => return fatal_or_no_match(&e),
    };
    crate::whatever_curry::mark::mark_program(std::slice::from_mut(&mut stmt));
    let names = NameScope::default();
    let list = super::statements::statement_list(&names, std::slice::from_ref(&stmt))
        .map_err(|e| e.message.to_string())?;
    let Some(RakuAstFieldValue::Node(first)) = list.fields.first().map(|f| &f.value) else {
        return Err("slang shim: the statement has no node".to_string());
    };
    Ok(Some((text.len() - rest.len(), first.clone())))
}

/// The `<arglist>` core rule: the arguments at the start of `text` as a
/// `RakuAST::ArgList`.
// Cost: O(n), n = size of the list (parse and conversion).
fn arglist_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, args)) = crate::parser::parse_slang_arglist(text) else {
        return Ok(None);
    };
    let names = NameScope::default();
    let mut exprs = super::view::call_args_as_exprs(&args).map_err(|e| e.message.to_string())?;
    // The `*`s a priming curries are `WhateverCode::Argument`s, as the whole
    // unit's parse classifies them (ADR-0033); EEL reads them (#16888).
    crate::whatever_curry::mark::mark_call_args(&mut exprs);
    let node = super::view::arg_list(&names, &exprs).map_err(|e| e.message.to_string())?;
    let mut node = Value::rakuast(Box::new(node));
    super::annotate_slang_production(&mut node, &shadowed_by_unit);
    Ok(Some((text.len() - rest.len(), node)))
}

/// Whether the code around a production declares the routine `name`
/// (`infix:<>>`), under either spelling of its operator brackets.
// Cost: O(s), s = parser scopes.
fn shadowed_by_unit(name: &str) -> bool {
    if crate::parser::is_user_declared_sub(name) {
        return true;
    }
    let Some((category, rest)) = name.split_once(":<") else {
        return false;
    };
    let Some(op) = rest.strip_suffix('>') else {
        return false;
    };
    crate::parser::is_user_declared_sub(&format!("{category}:«{op}»"))
}

/// The `<EXPR>` core rule as a `for` reads its iterable: the expression at the
/// start of `text`, up to the block that follows.
// Cost: O(n), n = size of the expression (parse and conversion).
fn for_expr_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, mut expr)) = crate::parser::parse_slang_for_expr(text) else {
        return Ok(None);
    };
    crate::whatever_curry::mark::mark_expression(&mut expr);
    let names = NameScope::default();
    let node = super::view::view_expr(&names, &expr).map_err(|e| e.message.to_string())?;
    let mut node = Value::rakuast(Box::new(node));
    super::annotate_slang_production(&mut node, &shadowed_by_unit);
    Ok(Some((text.len() - rest.len(), node)))
}

/// The `<pointy-block>` core rule: a pointy block (`-> $x { ... }`) or a bare
/// block at the start of `text`, as its `RakuAST::PointyBlock` / `Block`.
// Cost: O(n), n = size of the block (parse and conversion).
fn pointy_block_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, expr)) = crate::parser::parse_slang_pointy_block(text) else {
        return parse_production("block", text);
    };
    let names = NameScope::default();
    let node = super::view::view_expr(&names, &expr).map_err(|e| e.message.to_string())?;
    Ok(Some((
        text.len() - rest.len(),
        Value::rakuast(Box::new(node)),
    )))
}

/// The `<variable>` core rule: the variable at the start of `text` as its
/// `RakuAST::Var::*` node.
// Cost: O(n), n = size of the variable.
fn variable_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, expr)) = crate::parser::parse_slang_variable(text) else {
        return Ok(None);
    };
    let names = NameScope::default();
    let node = super::view::view_expr(&names, &expr).map_err(|e| e.message.to_string())?;
    Ok(Some((
        text.len() - rest.len(),
        Value::rakuast(Box::new(node)),
    )))
}

/// The `<trait>` core rule: the one trait at the start of `text` as a
/// `RakuAST::Trait::*` node.
// Cost: O(n), n = size of the trait.
fn trait_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, item)) = crate::parser::parse_slang_trait(text) else {
        return Ok(None);
    };
    let names = NameScope::default();
    let node =
        super::routine_source::trait_node(&names, &item).map_err(|e| e.message.to_string())?;
    Ok(Some((
        text.len() - rest.len(),
        Value::rakuast(Box::new(node)),
    )))
}

/// The `<signature>` core rule: the parameters at the start of `text` as a
/// `RakuAST::Signature`.
// Cost: O(n), n = size of the parameter list.
fn signature_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, params, returns)) = crate::parser::parse_slang_signature(text) else {
        return Ok(None);
    };
    let names = NameScope::default();
    let node = super::routine_origins::anon_signature(&names, &params, returns.as_deref())
        .map_err(|e| e.message.to_string())?;
    Ok(Some((
        text.len() - rest.len(),
        Value::rakuast(Box::new(node)),
    )))
}

/// The node `stmt` stands for: the expression of its `Statement::Expression`
/// (a `{ ... }` block is a `RakuAST::Block`), else the statement itself.
// Cost: O(n), n = size of the statement.
fn production_node(stmt: &Stmt) -> Result<Value, RuntimeError> {
    let names = NameScope::default();
    let list = super::statements::statement_list(&names, std::slice::from_ref(stmt))?;
    let Some(RakuAstFieldValue::Node(first)) = list.fields.first().map(|f| &f.value) else {
        return Err(RuntimeError::new("slang shim: the production has no node"));
    };
    if let ValueView::RakuAst(statement) = first.view()
        && let Some(expression) = statement
            .fields
            .iter()
            .find(|f| f.name == Some("expression"))
        && let RakuAstFieldValue::Node(expression) = &expression.value
    {
        return Ok(expression.clone());
    }
    Ok(first.clone())
}

/// Lower the statement node a slang action attached into the statement it
/// stands for at the hook.
// Cost: O(n), n = size of the node.
pub(crate) fn lower_statement(node: &Value) -> Result<Stmt, RuntimeError> {
    let ValueView::RakuAst(node) = node.view() else {
        return Err(RuntimeError::new(
            "slang production produced no RakuAST node",
        ));
    };
    // A production standing for one statement may hand it over inside the
    // `StatementList` it built (a constant it installed is such a statement).
    if node.class == super::RakuAstClass::StatementList {
        let mut children = node
            .fields
            .iter()
            .filter(|f| !super::origin::is_origin(f))
            .filter_map(|f| match &f.value {
                RakuAstFieldValue::Node(child) => Some(child),
                _ => None,
            });
        match (children.next(), children.next()) {
            (Some(only), None) => return lower_statement(only),
            // A production that also re-runs its actions in the unit
            // (ADR-17243) stands for the replay call and its own node.
            (Some(first), Some(second)) => {
                // What the finalization unwraps is the outer `Lowered` only, so the
                // parts are taken out of theirs; a reader of the tree sees the
                // production's own node, not the replay call beside it.
                fn plain(stmt: Stmt) -> (Stmt, Option<Value>) {
                    match stmt {
                        Stmt::Ast(parsed) => match *parsed {
                            crate::ast::parsed::ParsedStmt::Lowered { stmt, node } => {
                                (*stmt, Some(node))
                            }
                            other => (Stmt::Ast(Box::new(other)), None),
                        },
                        other => (other, None),
                    }
                }
                let (replay, _) = plain(lower_statement(first)?);
                let (kept, kept_node) = plain(lower_statement(second)?);
                // rakudo runs a production's actions at compile time, after the
                // declarations it installs and before any statement runs. So a
                // production that declares runs first and the replay sees what
                // it declared (EEL's `pattern` reads its method's `meta-object`).
                // Any other production runs after the replay and stays the last
                // statement, so its value is the statement's value (Qwiratry's
                // `givenroot $data { ... }` as a routine's last statement).
                let mut stmts = if is_declaration(second) {
                    vec![kept, replay]
                } else {
                    vec![replay, kept]
                };
                for rest in children {
                    stmts.push(plain(lower_statement(rest)?).0);
                }
                return Ok(Stmt::Ast(Box::new(
                    crate::ast::parsed::ParsedStmt::Lowered {
                        stmt: Box::new(Stmt::SyntheticBlock(stmts)),
                        node: if declares_method(second) {
                            replay_sequence(node, kept_node)
                        } else {
                            kept_node.unwrap_or_else(|| second.clone())
                        },
                    },
                )));
            }
            _ => {}
        }
    }
    let names = NameScope::default();
    let lowered = super::lower::lower_fragment(&names, node)?;
    // A production that attaches an expression stands for a statement: rakudo's
    // `statement` action wraps it in a `Statement::Expression`, so a tree that
    // keeps the node (a block body a slang deparses) lists it as one.
    let kept =
        if super::hierarchy::does(node.class.printed_name(), "RakuAST::Expression") == Some(true) {
            super::view::statement_expression(RakuAstNode::clone(node))
        } else {
            RakuAstNode::clone(node)
        };
    Ok(Stmt::Ast(Box::new(
        crate::ast::parsed::ParsedStmt::Lowered {
            stmt: Box::new(lowered),
            node: Value::rakuast(Box::new(kept)),
        },
    )))
}

/// Whether a production's node is a declaration (a routine, package or
/// variable), looking through the `Statement::Expression` that wraps one.
// Cost: O(f), f = fields of the statement node.
fn is_declaration(node: &Value) -> bool {
    let ValueView::RakuAst(n) = node.view() else {
        return false;
    };
    if n.class.printed_name() == "RakuAST::Statement::Expression" {
        return n.fields.iter().any(|f| {
            f.name == Some("expression")
                && matches!(&f.value, RakuAstFieldValue::Node(e) if is_declaration(e))
        });
    }
    super::hierarchy::does(n.class.printed_name(), "RakuAST::Declaration") == Some(true)
}

/// Whether a production's node declares a method: one a class body holds, which
/// the unit converts back from its tree (ADR-15334 Stage 2), so the replay of the
/// production's actions must be part of the node it reads.
// Cost: O(1).
fn declares_method(node: &Value) -> bool {
    matches!(node.view(), ValueView::RakuAst(n)
        if matches!(n.class, super::RakuAstClass::Method | super::RakuAstClass::Submethod))
}

/// The node a production that declares a method and replays its actions stands
/// for, as a reader of the tree sees it: a `StatementSequence` (no scope of its
/// own) of the production's own node and the replay call, so a class body
/// converted back from its tree keeps the replay (ADR-17243). Any other
/// production is read as its own node.
// TODO: a reader that deparses such a class body sees the sequence; carry the
// replay beside the node instead once the tree has a place for it.
// Cost: O(f), f = statements of the list.
fn replay_sequence(list: &RakuAstNode, kept: Option<Value>) -> Value {
    let mut fields: Vec<_> = list
        .fields
        .iter()
        .filter(|f| !super::origin::is_origin(f))
        .cloned()
        .collect();
    // The production's own node as its statement was lowered (an expression
    // stands in its `Statement::Expression`), ahead of the replay as it runs.
    if let (Some(kept), Some(own)) = (kept, fields.get_mut(1)) {
        own.value = RakuAstFieldValue::Node(kept);
    }
    fields.swap(0, 1);
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: super::RakuAstClass::StatementSequence,
        fields,
    }))
}

/// Lower the node a slang's `statementlist` wrapper answered (ADR-16888): the
/// statements of a `RakuAST::StatementList`, each as it is lowered at a hook,
/// or the one statement of any other node.
// Cost: O(n), n = size of the node.
pub(crate) fn lower_statements(node: &Value) -> Result<Vec<Stmt>, RuntimeError> {
    let ValueView::RakuAst(list) = node.view() else {
        return Err(RuntimeError::new(
            "slang production produced no RakuAST node",
        ));
    };
    if list.class != super::RakuAstClass::StatementList {
        return Ok(vec![lower_statement(node)?]);
    }
    let mut stmts = Vec::new();
    for field in &list.fields {
        if super::origin::is_origin(field) {
            continue;
        }
        let RakuAstFieldValue::Node(child) = &field.value else {
            continue;
        };
        if let ValueView::RakuAst(inner) = child.view()
            && inner.class == super::RakuAstClass::StatementEmpty
        {
            continue;
        }
        stmts.push(lower_statement(child)?);
    }
    Ok(stmts)
}

/// Lower the expression node a slang action attached into the expression it
/// stands for at the hook (a `term`, `value` or `number` production).
// Cost: O(n), n = size of the node.
pub(crate) fn lower_expression(node: &Value) -> Result<Expr, RuntimeError> {
    let ValueView::RakuAst(inner) = node.view() else {
        return Err(RuntimeError::new(
            "slang production produced no RakuAST node",
        ));
    };
    // A production that also re-runs its actions in the unit (ADR-17243) holds the
    // replay call beside its node. A term has no statement to hold the call, so it
    // stands for its node alone.
    // TODO: replay the actions of a term too (a `do` block holding the call).
    if inner.class == super::RakuAstClass::StatementList {
        let mut children = inner
            .fields
            .iter()
            .filter(|f| !super::origin::is_origin(f))
            .filter_map(|f| match &f.value {
                RakuAstFieldValue::Node(child) => Some(child),
                _ => None,
            });
        if let (Some(_), Some(node)) = (children.next(), children.next()) {
            return lower_expression(node);
        }
    }
    let names = NameScope::default();
    let _method_scope = super::underscore_bindings::MethodScope::enter(false);
    let lowered =
        crate::parser::with_rakuast_lexical_scope(|| super::lower::lower_expr(&names, inner))?;
    Ok(Expr::Ast(Box::new(
        crate::ast::parsed::ParsedExpr::Lowered {
            expr: Box::new(lowered),
            node: node.clone(),
        },
    )))
}

/// The pass that runs a slang's overrides of core actions (`INFIX-EXPR`,
/// `PREFIX-EXPR`; ADR-15762 slice 3) over the parsed unit, bottom-up, before the
/// unit is finalized: each infix or prefix application becomes the node the
/// slang's action attached (the core node, when it attached none).
///
/// Only what follows the `use` of the activating module is touched, up to the
/// end of the block that holds the `use`.
// Cost: O(n) for the walk, plus the slang's own action per application, n =
// size of the unit; O(1) when the unit has no override.
pub(crate) fn apply_action_overrides(stmts: &mut [Stmt]) -> Result<(), RuntimeError> {
    let Some(activator) = crate::parser::slang_action_activator() else {
        return Ok(());
    };
    struct Pass {
        activator: String,
        enabled: bool,
        /// The slang overrides the `statement` action.
        statement: bool,
        failed: Option<RuntimeError>,
    }
    impl crate::ast_visit::VisitMut for Pass {
        // The refinement a `use` makes ends with the block that holds it.
        // Each statement is offered to a `statement` override after its own
        // contents were, with the span its `SetLine` marker records.
        fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
            let enclosing = self.enabled;
            let mut span = None;
            for stmt in body.iter_mut() {
                if let Stmt::SetLine(_, at) = stmt {
                    span = Some(*at);
                }
                self.visit_stmt_mut(stmt);
                if !self.statement
                    || !self.enabled
                    || self.failed.is_some()
                    || matches!(stmt, Stmt::SetLine(..) | Stmt::Use { .. })
                {
                    continue;
                }
                let Some(at) = span.take() else {
                    continue;
                };
                match super::slang_statement_action::apply(stmt, at) {
                    Ok(Some(lowered)) => *stmt = lowered,
                    Ok(None) => {}
                    Err(e) => self.failed = Some(e),
                }
            }
            self.enabled = enclosing;
        }
        fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
            crate::ast_visit::walk_stmt_mut(self, stmt);
            if matches!(stmt, Stmt::Use { module, .. } if *module == self.activator) {
                self.enabled = true;
            }
            if !self.enabled || self.failed.is_some() {
                return;
            }
            let Some(keyword) = statement_control_keyword(stmt) else {
                return;
            };
            let rule = format!("statement-control:sym<{keyword}>");
            if !crate::parser::slang_overrides_action(&rule) {
                return;
            }
            match apply_statement_action(&rule, stmt) {
                Ok(lowered) => *stmt = lowered,
                Err(e) => self.failed = Some(e),
            }
        }
        fn visit_expr_mut(&mut self, expr: &mut Expr) {
            crate::ast_visit::walk_expr_mut(self, expr);
            if !self.enabled || self.failed.is_some() {
                return;
            }
            let rule = match expr {
                Expr::Binary { .. } => "INFIX-EXPR",
                // A user-declared infix (`infix:<⩃>`) is the call of its
                // routine; `is` is the parser's vehicle for a class trait.
                Expr::InfixFunc {
                    name,
                    modifier: None,
                    ..
                } if name != "is" => "INFIX-EXPR",
                Expr::Unary { .. } => "PREFIX-EXPR",
                // A user-declared prefix operator is the call of its routine.
                Expr::Call { name, args, .. }
                    if args.len() == 1 && name.as_str().starts_with("prefix:<") =>
                {
                    "PREFIX-EXPR"
                }
                Expr::PostfixOp { .. } => "POSTFIX-EXPR",
                _ => return,
            };
            if !crate::parser::slang_overrides_action(rule) {
                return;
            }
            match apply_action(rule, expr, "") {
                Ok(lowered) => *expr = lowered,
                Err(e) => self.failed = Some(e),
            }
        }
    }
    let mut pass = Pass {
        activator,
        enabled: false,
        statement: crate::parser::slang_overrides_action("statement"),
        failed: None,
    };
    for stmt in stmts.iter_mut() {
        crate::ast_visit::VisitMut::visit_stmt_mut(&mut pass, stmt);
    }
    pass.failed.map_or(Ok(()), Err)
}

/// The keyword of the `statement-control` production the block statement `stmt`
/// is (a statement modifier is not one).
// Cost: O(1).
fn statement_control_keyword(stmt: &Stmt) -> Option<&'static str> {
    use crate::ast::parsed::{ConditionalKind, ParsedStmt};
    match stmt {
        // The parser keeps the block forms as typed nodes; a statement modifier
        // is a plain `Stmt` and not one of them.
        Stmt::Ast(parsed) => match &**parsed {
            ParsedStmt::Given { .. } => Some("given"),
            ParsedStmt::For { .. } => Some("for"),
            ParsedStmt::WhileLoop(w) if !w.until => Some("while"),
            ParsedStmt::Conditional(c) if c.kind == ConditionalKind::If => Some("if"),
            _ => None,
        },
        _ => None,
    }
}

/// Run the slang's override of the `statement-control` action `rule` over the
/// node of `stmt`: the statement for the node its action attached.
// Cost: O(n) in the statement, plus the slang's action.
fn apply_statement_action(rule: &str, stmt: &Stmt) -> Result<Stmt, RuntimeError> {
    let names = NameScope::default();
    let list = super::statements::statement_list(&names, std::slice::from_ref(stmt))?;
    let Some(RakuAstFieldValue::Node(core)) = list.fields.first().map(|f| &f.value) else {
        return Err(RuntimeError::new(format!(
            "slang action {rule}: the statement has no node"
        )));
    };
    let attached = crate::parser::run_slang_action(rule, core.clone(), "")
        .map_err(|e| RuntimeError::new(format!("slang action {rule}: {e}")))?;
    lower_statement(&attached)
}

/// The slang's `numish` override over the numeric literal `expr`, read by the
/// parser from `source`: the expression for the node its action attached.
// Cost: the slang's own action, plus O(n) for the node, n = its size.
pub(crate) fn lower_numish(expr: &Expr, source: &str) -> Result<Expr, RuntimeError> {
    apply_action("numish", expr, source)
}

/// Run the slang's override of `rule` over the node of `expr`.
// Cost: O(n) in the application, plus the slang's action.
fn apply_action(rule: &str, expr: &Expr, source: &str) -> Result<Expr, RuntimeError> {
    let names = NameScope::default();
    let node = super::view::view_expr(&names, expr)?;
    let core = Value::rakuast(Box::new(node));
    let attached = crate::parser::run_slang_action(rule, core, source)
        .map_err(|e| RuntimeError::new(format!("slang action {rule}: {e}")))?;
    lower_expression(&attached)
}

#[cfg(test)]
mod method_def_tests {
    #[test]
    fn a_method_definition_after_its_keyword_becomes_a_node() {
        let _scan = crate::parser::term_origins::Wanted::enter();
        let found = super::parse_production("method-def", " bub { return 12 } }\n");
        match found {
            Ok(Some((len, _))) => assert!(len > 0),
            other => panic!(
                "method-def production: {:?}",
                other.map(|o| o.map(|(l, _)| l))
            ),
        }
    }
}
