//! Hot, lossless families cross the executing round trip as the typed tree the
//! parser built (ADR-10723 Stage 2, section 4: "for a family whose `Expr` is
//! already lossless, the typed node is that `Expr` plus its positions").
//!
//! The `MUTSU_RAKUAST` round trip used to render every source node as a
//! `RakuAstNode` (a `Value` per field, an origin per statement), lower it and
//! free it again. For the families below the parser's `Expr` already is the
//! node `lower` would hand the compiler, so the round trip carries it as one
//! [`RakuAstClass::ParsedLeaf`] holding the tree itself: one clone in, one
//! clone out, no node per source construct.
//!
//! Only the executing round trip ([`Scope`]) does this. `.AST` and
//! `EVAL($ast)` never see a leaf: outside a scope [`expr_leaf`] and
//! [`stmt_leaf`] return `None`, and `convert` renders every construct as it
//! always did.

use std::cell::{Cell, RefCell};

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::parsed::{BlockParams, ParsedBlock, ParsedStmt};
use crate::ast::{CallArg, Expr, ParamDef, Stmt};
use crate::ast_visit::VisitMut;
use crate::value::{RuntimeError, Value, ValueView};

enum Held {
    Expr(Expr),
    Stmt(Stmt),
}

thread_local! {
    static TABLE: RefCell<Option<Vec<Held>>> = const { RefCell::new(None) };
    /// Conversions of the operands a parent reads by node class.
    static INSPECTING: Cell<u32> = const { Cell::new(0) };
    /// Conversions of the parser's desugaring, recognized by statement shape.
    static DESUGARED: Cell<u32> = const { Cell::new(0) };
}

fn blocked() -> bool {
    INSPECTING.with(|c| c.get()) > 0 || DESUGARED.with(|c| c.get()) > 0
}

/// Marks a conversion whose caller reads the class of the node it gets back
/// (a heredoc body must be a `QuotedString`, the operands of an assignment):
/// no leaf is made inside it, except in the statements of a block it holds
/// ([`Block`]).
pub(super) struct Inspecting;

impl Inspecting {
    // Cost: O(1).
    pub(super) fn enter() -> Self {
        INSPECTING.with(|c| c.set(c.get() + 1));
        Inspecting
    }
}

impl Drop for Inspecting {
    fn drop(&mut self) {
        INSPECTING.with(|c| c.set(c.get() - 1));
    }
}

/// Marks the conversion of a synthetic block: the statements inside keep the
/// shape the parser gave them, however deep.
pub(super) struct Desugared;

impl Desugared {
    // Cost: O(1).
    pub(super) fn enter() -> Self {
        DESUGARED.with(|c| c.set(c.get() + 1));
        Desugared
    }
}

impl Drop for Desugared {
    fn drop(&mut self) {
        DESUGARED.with(|c| c.set(c.get() - 1));
    }
}

/// The statements of a block: whatever operand its parent reads by class, the
/// statements are converted on their own.
pub(super) struct Block(u32);

impl Block {
    // Cost: O(1).
    pub(super) fn enter() -> Self {
        Block(INSPECTING.with(|c| c.replace(0)))
    }
}

impl Drop for Block {
    fn drop(&mut self) {
        INSPECTING.with(|c| c.set(self.0));
    }
}

const LINE: &str = "leaf-line";

/// The table of trees a round trip is carrying; installed by the executing
/// round trip, restored (so a nested one is independent) when dropped.
pub(super) struct Scope(Option<Vec<Held>>);

impl Scope {
    // Cost: O(1).
    pub(super) fn enter() -> Self {
        Scope(TABLE.with(|t| t.borrow_mut().replace(Vec::new())))
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        TABLE.with(|t| *t.borrow_mut() = self.0.take());
    }
}

fn leaf_node(held: Held, line: Option<i64>) -> Option<RakuAstNode> {
    let slot = TABLE.with(|t| {
        let mut table = t.borrow_mut();
        let table = table.as_mut()?;
        table.push(held);
        Some(table.len() as i64 - 1)
    })?;
    let mut fields = vec![RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(Value::int(slot)),
    }];
    if let Some(line) = line {
        fields.push(RakuAstField {
            name: Some(LINE),
            value: RakuAstFieldValue::Node(Value::int(line)),
        });
    }
    Some(RakuAstNode {
        class: RakuAstClass::ParsedLeaf,
        fields,
        origin: Default::default(),
    })
}

/// The leaf for `expr`, when a round trip is running and the whole tree is of
/// a lossless hot family.
// Cost: O(e) to test and clone, e = nodes of `expr`; O(1) outside a scope.
pub(super) fn expr_leaf(expr: &Expr) -> Option<RakuAstNode> {
    if blocked() || !TABLE.with(|t| t.borrow().is_some()) || !pure_expr(expr) {
        return None;
    }
    leaf_node(Held::Expr(expr.clone()), None)
}

/// [`expr_leaf`] for a call argument, whose parent reads no class from it.
// Cost: as [`expr_leaf`].
pub(super) fn arg_leaf(expr: &Expr) -> Option<RakuAstNode> {
    expr_leaf(expr)
}

/// The leaf for `stmt` written on `line`, as [`expr_leaf`].
// Cost: O(s) to test and clone, s = nodes of `stmt`; O(1) outside a scope.
pub(super) fn stmt_leaf(stmt: &Stmt, line: Option<i64>) -> Option<RakuAstNode> {
    if blocked() || !TABLE.with(|t| t.borrow().is_some()) || !pure_stmt(stmt) {
        return None;
    }
    leaf_node(Held::Stmt(stmt.clone()), line)
}

// Cost: O(1).
pub(super) fn is_leaf(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ParsedLeaf
}

fn slot(node: &RakuAstNode) -> Result<usize, RuntimeError> {
    match node.fields.first().map(|f| &f.value) {
        Some(RakuAstFieldValue::Node(v)) => match v.view() {
            ValueView::Int(i) if i >= 0 => Ok(i as usize),
            _ => Err(RuntimeError::new("malformed round-trip leaf")),
        },
        _ => Err(RuntimeError::new("malformed round-trip leaf")),
    }
}

/// The line the leaf's statement began on.
// Cost: O(1).
pub(super) fn line_of(node: &RakuAstNode) -> Option<i64> {
    let field = node.fields.iter().find(|f| f.name == Some(LINE))?;
    match &field.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::Int(i) => Some(i),
            _ => None,
        },
        _ => None,
    }
}

fn held(node: &RakuAstNode) -> Result<Held, RuntimeError> {
    let at = slot(node)?;
    let held = TABLE.with(|t| {
        let table = t.borrow();
        match table.as_ref().and_then(|t| t.get(at)) {
            Some(Held::Expr(e)) => Ok(Held::Expr(e.clone())),
            Some(Held::Stmt(s)) => Ok(Held::Stmt(s.clone())),
            None => Err(RuntimeError::new("round-trip leaf outside its scope")),
        }
    })?;
    // The spelled parse keeps the constructs of a migrated family in their
    // RakuAST shape; an executing parse lowers each where it is built.
    let mut lowering = Kept(None);
    Ok(match held {
        Held::Expr(mut e) => {
            lowering.visit_expr_mut(&mut e);
            lowering.finish(Held::Expr(e))?
        }
        Held::Stmt(mut s) => {
            lowering.visit_stmt_mut(&mut s);
            lowering.finish(Held::Stmt(s))?
        }
    })
}

/// Lowers every kept [`ParsedStmt`] of a tree, innermost first.
struct Kept(Option<RuntimeError>);

impl Kept {
    fn finish(self, held: Held) -> Result<Held, RuntimeError> {
        match self.0 {
            Some(err) => Err(err),
            None => Ok(held),
        }
    }
}

impl VisitMut for Kept {
    // A spelling only annotates the expression it wraps (ADR-12199): an
    // executing parse builds the expression alone.
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        while matches!(expr, Expr::Spelled(_)) {
            if let Expr::Spelled(spelled) = std::mem::replace(expr, Expr::Whatever) {
                *expr = spelled.expr;
            }
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        crate::ast_visit::walk_stmt_mut(self, stmt);
        if matches!(stmt, Stmt::Ast(_))
            && let Stmt::Ast(node) = std::mem::replace(stmt, Stmt::Last(None))
        {
            match super::lower_parsed_stmt(*node) {
                Ok(lowered) => *stmt = lowered,
                Err(err) => {
                    self.0.get_or_insert(err);
                }
            }
        }
    }
}

/// The expression a leaf carries.
// Cost: O(e), e = nodes of the tree (one clone).
pub(super) fn lower_expr(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match held(node)? {
        Held::Expr(e) => Ok(e),
        Held::Stmt(Stmt::Expr(e)) => Ok(e),
        Held::Stmt(_) => Err(RuntimeError::new("a statement leaf in expression position")),
    }
}

/// The statement a leaf carries.
// Cost: O(s), s = nodes of the tree (one clone).
pub(super) fn lower_stmt(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match held(node)? {
        Held::Stmt(s) => Ok(s),
        Held::Expr(e) => Ok(Stmt::Expr(e)),
    }
}

// Cost: O(s), s = nodes of `stmt`.
fn pure_stmt(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::SetLine(..) => true,
        Stmt::Ast(node) => pure_parsed(node),
        Stmt::For {
            iterable,
            param_def,
            params_def,
            body,
            ..
        } => {
            pure_expr(iterable)
                && param_def.as_ref().as_ref().is_none_or(pure_param)
                && params_def.iter().all(pure_param)
                && body.iter().all(pure_stmt)
        }
        Stmt::Expr(e) => pure_expr(e),
        Stmt::Say(args) | Stmt::Put(args) | Stmt::Print(args) | Stmt::Note(args) => {
            args.iter().all(pure_expr)
        }
        Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) => pure_expr(e),
        Stmt::Assign {
            name,
            expr,
            target_is_sigilless: false,
            ..
        } => plain_name(name) && pure_expr(expr),
        Stmt::VarDecl {
            name,
            anonymous: false,
            expr,
            is_export: false,
            export_tags,
            custom_traits,
            where_constraint: None,
            ..
        } => {
            plain_name(name)
                && export_tags.is_empty()
                && custom_traits.is_empty()
                && pure_expr(expr)
        }
        Stmt::Call { args, .. } => args.iter().all(|a| match a {
            CallArg::Positional(e) => pure_expr(e),
            CallArg::Named { value, .. } => value.as_ref().is_none_or(pure_expr),
            _ => false,
        }),
        _ => false,
    }
}

// Cost: O(n), n = nodes of the construct.
fn pure_parsed(node: &ParsedStmt) -> bool {
    match node {
        ParsedStmt::Conditional(c) => {
            pure_expr(&c.condition)
                && pure_block(&c.then)
                && c.clauses
                    .iter()
                    .all(|k| pure_expr(&k.condition) && pure_block(&k.then))
                && c.else_block.as_ref().is_none_or(pure_block)
        }
        ParsedStmt::Given { topic, block } => pure_expr(topic) && pure_block(block),
        ParsedStmt::When { condition, block } => pure_expr(condition) && pure_block(block),
        ParsedStmt::Default(block) | ParsedStmt::BareBlock(block) => pure_block(block),
        ParsedStmt::WhileLoop(w) => pure_expr(&w.condition) && pure_block(&w.block),
        ParsedStmt::For { stmt, .. } => pure_stmt(stmt),
        _ => false,
    }
}

// Cost: O(n), n = nodes of the block.
fn pure_block(block: &ParsedBlock) -> bool {
    (match &block.params {
        BlockParams::None => true,
        BlockParams::List(params) => params.iter().all(pure_param),
        BlockParams::Topic { def, .. } => def.as_deref().is_none_or(pure_param),
    }) && block.body.iter().all(pure_stmt)
}

/// A parameter with a name and, at most, a type: nothing the lowering reads
/// the signature of.
// Cost: O(1).
fn pure_param(param: &ParamDef) -> bool {
    param.default.is_none()
        && param.where_constraint.is_none()
        && param.sub_signature.is_none()
        && param.outer_sub_signature.is_none()
        && param.code_signature.is_none()
        && param.shape_constraints.is_none()
        && param.literal_value.is_none()
        && param.type_capture.is_none()
        && param.traits.is_empty()
        && param.trait_args.is_empty()
}

// Cost: O(e), e = nodes of `expr`.
fn pure_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Literal(v) => !matches!(
            v.view(),
            ValueView::Regex(_)
                | ValueView::RegexWithAdverbs(_)
                | ValueView::Sub(_)
                | ValueView::WeakSub(_)
                | ValueView::Instance { .. }
        ),
        Expr::AnonSub { body, .. } | Expr::DoBlock { body, .. } | Expr::Lambda { body, .. } => {
            body.iter().all(pure_stmt)
        }
        Expr::WhateverCurry(inner) | Expr::PositionalPair(inner) => pure_expr(inner),
        Expr::PostfixOp { expr, .. } => pure_expr(expr),
        Expr::DoStmt(stmt) => pure_stmt(stmt),
        Expr::RegexLiteral { tree, .. } | Expr::MatchRegexTree { tree, .. } => !has_code(tree),
        Expr::Exists { target, arg, .. } => {
            pure_expr(target) && arg.as_deref().is_none_or(pure_expr)
        }
        Expr::Try { body, catch } => {
            body.iter().all(pure_stmt) && catch.as_ref().is_none_or(|c| c.iter().all(pure_stmt))
        }
        Expr::HyperMethodCall { target, args, .. } | Expr::CallOn { target, args } => {
            pure_expr(target) && args.iter().all(pure_expr)
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        } => pure_expr(target) && pure_expr(name_expr) && args.iter().all(pure_expr),
        Expr::AnonSubParams {
            param_defs,
            body,
            custom_traits,
            ..
        } => {
            custom_traits.as_slice().is_empty()
                && param_defs.iter().all(pure_param)
                && body.iter().all(pure_stmt)
        }
        Expr::Whatever | Expr::WhateverArg => true,
        Expr::Hash(items, _) => items
            .iter()
            .all(|(_, value)| value.as_ref().is_none_or(pure_expr)),
        Expr::Var(name) | Expr::ArrayVar(name) | Expr::HashVar(name) | Expr::CodeVar(name) => {
            plain_name(name)
        }
        // A type name: the spelled parse leaves a lowercase bareword to the
        // lowering, which resolves it against what the unit imported.
        Expr::BareWord(word) => {
            word.starts_with(|c: char| c.is_uppercase())
                && word
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, ':' | '-' | '_'))
        }
        Expr::StringInterpolation(parts) => parts.iter().all(pure_expr),
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => args.iter().all(pure_expr),
        Expr::MethodCall { target, args, .. } => pure_expr(target) && args.iter().all(pure_expr),
        Expr::Binary { left, right, .. } => pure_expr(left) && pure_expr(right),
        Expr::Unary { expr, .. } => pure_expr(expr),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => pure_expr(cond) && pure_expr(then_expr) && pure_expr(else_expr),
        Expr::Index { target, index, .. } => pure_expr(target) && pure_expr(index),
        Expr::ArrayLiteral(items) | Expr::BracketArray(items, _) => items.iter().all(pure_expr),
        Expr::Grouped(inner) => pure_expr(inner),
        _ => false,
    }
}

/// An ordinary lexical or dynamic variable name: not a placeholder, the topic,
/// a match variable or a special, whose blocks the lowering reads from the
/// node tree.
fn plain_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some('*') => chars.next().is_some_and(|c| c.is_alphabetic()),
        Some(c) => c.is_alphabetic(),
        None => false,
    }
}

/// Whether a regex holds code (a block, an assertion, an interpolation): the
/// regex `Value` keeps its own copy of that code, which still carries the
/// spelled parse's annotations.
// Cost: O(n), n = nodes of the regex.
fn has_code(tree: &crate::regex_tree::RegexTree) -> bool {
    struct Code(bool);
    impl<'ast> crate::ast_visit::Visit<'ast> for Code {
        fn visit_stmt(&mut self, _stmt: &'ast Stmt) {
            self.0 = true;
        }
        fn visit_expr(&mut self, _expr: &'ast Expr) {
            self.0 = true;
        }
    }
    let mut code = Code(false);
    crate::ast_visit::walk_regex_tree(&mut code, tree);
    code.0
}
