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

use std::cell::RefCell;

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{CallArg, Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

enum Held {
    Expr(Expr),
    Stmt(Stmt),
}

thread_local! {
    static TABLE: RefCell<Option<Vec<Held>>> = const { RefCell::new(None) };
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
    if !TABLE.with(|t| t.borrow().is_some()) || !pure_expr(expr) {
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
    if !TABLE.with(|t| t.borrow().is_some()) || !pure_stmt(stmt) {
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
    TABLE.with(|t| {
        let table = t.borrow();
        match table.as_ref().and_then(|t| t.get(at)) {
            Some(Held::Expr(e)) => Ok(Held::Expr(e.clone())),
            Some(Held::Stmt(s)) => Ok(Held::Stmt(s.clone())),
            None => Err(RuntimeError::new("round-trip leaf outside its scope")),
        }
    })
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
        Stmt::Expr(e) => pure_expr(e),
        Stmt::Call { args, .. } => args.iter().all(|a| match a {
            CallArg::Positional(e) => pure_expr(e),
            CallArg::Named { value, .. } => value.as_ref().is_none_or(pure_expr),
            _ => false,
        }),
        _ => false,
    }
}

// Cost: O(e), e = nodes of `expr`.
fn pure_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Literal(v) => matches!(
            v.view(),
            ValueView::Int(_) | ValueView::Num(_) | ValueView::Str(_) | ValueView::Bool(_)
        ),
        Expr::Var(name) | Expr::ArrayVar(name) | Expr::HashVar(name) | Expr::CodeVar(name) => {
            plain_name(name)
        }
        Expr::BareWord(word) => word
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, ':' | '-' | '_')),
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
