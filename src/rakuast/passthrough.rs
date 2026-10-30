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
use crate::ast::parsed::{Assign, ParsedExpr, ParsedStmt};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::VisitMut;
use crate::value::{RuntimeError, Value, ValueView};

enum Held {
    Expr(Expr),
    Stmt(Box<Stmt>),
}

/// A tree a leaf carries, and whether it holds a spelling or a kept
/// construct that has to be undone before the compiler sees it.
struct Slot {
    held: Held,
    fixup: bool,
}

thread_local! {
    static TABLE: RefCell<Option<Vec<Option<Slot>>>> = const { RefCell::new(None) };
    /// Conversions of the operands a parent reads by node class.
    static INSPECTING: Cell<u32> = const { Cell::new(0) };
    /// Conversions of the parser's desugaring, recognized by statement shape.
    static DESUGARED: Cell<u32> = const { Cell::new(0) };
}

/// Whether an executing round trip is running ([`Scope`] is installed): nothing
/// then reads the source positions `.AST` reports, only the line a statement
/// began on.
// Cost: O(1).
pub(super) fn executing() -> bool {
    TABLE.with(|t| t.borrow().is_some())
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

/// The guards a conversion of `stmt` holds: the parser's desugaring is
/// recognized by the shape of the statements inside it, and every statement
/// but the plain ones lowers from the class of the expressions it holds
/// (traits, defaults, sources, headers).
// Cost: O(1).
pub(super) fn stmt_guards(stmt: &Stmt) -> Guards {
    Guards(
        matches!(stmt, Stmt::SyntheticBlock(_)).then(Desugared::enter),
        (!matches!(
            stmt,
            Stmt::Expr(_)
                | Stmt::Call { .. }
                | Stmt::Say(_)
                | Stmt::Put(_)
                | Stmt::Print(_)
                | Stmt::Note(_)
                | Stmt::Return(_)
                | Stmt::Take(..)
                | Stmt::SetLine(..)
        ))
        .then(Inspecting::enter),
    )
}

/// [`stmt_guards`] for an expression: `lower` reads the operands of an infix
/// (the loose logical operators in particular), of an assignment and of a
/// kept construct by their node class.
// Cost: O(1).
pub(super) fn expr_guards(expr: &Expr) -> Guards {
    Guards(
        // A kept `supply { }` body is expanded by the shape of its statements
        // (`emit`, `done`), as the desugaring is.
        (matches!(expr, Expr::DoStmt(inner) if matches!(inner.as_ref(), Stmt::SyntheticBlock(_)))
            // `is my $k.defined, ...`: `decl_tail::convert` finds the declared
            // variable at the bottom of the converted tail, which a leaf hides.
            || matches!(expr, Expr::DoStmt(inner) if matches!(inner.as_ref(), Stmt::Ast(node)
                if matches!(node.as_ref(), ParsedStmt::DeclTail { .. })))
            || matches!(expr, Expr::Ast(node) if !matches!(node.as_ref(), ParsedExpr::Assign(_))))
        .then(Desugared::enter),
        matches!(
            expr,
            Expr::Binary { .. }
                | Expr::AssignExpr { .. }
                | Expr::CompoundAssign { .. }
                | Expr::IndexAssign { .. }
                | Expr::MultiDimIndexAssign { .. }
                | Expr::SymbolicDerefAssign { .. }
                | Expr::Ast(_)
        )
        .then(Inspecting::enter),
    )
}

/// What [`stmt_guards`] and [`expr_guards`] hold until dropped.
#[allow(dead_code)] // held for its drop
pub(super) struct Guards(Option<Desugared>, Option<Inspecting>);

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
pub(super) struct Scope(Option<Vec<Option<Slot>>>);

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

fn leaf_node(held: Held, fixup: bool, line: Option<i64>) -> Option<RakuAstNode> {
    let slot = TABLE.with(|t| {
        let mut table = t.borrow_mut();
        let table = table.as_mut()?;
        table.push(Some(Slot { held, fixup }));
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
    if blocked() || !TABLE.with(|t| t.borrow().is_some()) {
        return None;
    }
    let fixup = analyse_expr(expr)?;
    leaf_node(Held::Expr(expr.clone()), fixup, None)
}

/// [`expr_leaf`] for a call argument, whose parent reads no class from it.
// Cost: as [`expr_leaf`].
pub(super) fn arg_leaf(expr: &Expr) -> Option<RakuAstNode> {
    expr_leaf(expr)
}

/// The leaf for `stmt` written on `line`, as [`expr_leaf`].
// Cost: O(s) to test and clone, s = nodes of `stmt`; O(1) outside a scope.
pub(super) fn stmt_leaf(stmt: &Stmt, line: Option<i64>) -> Option<RakuAstNode> {
    if blocked() || !TABLE.with(|t| t.borrow().is_some()) {
        return None;
    }
    let fixup = analyse_stmt(stmt)?;
    leaf_node(Held::Stmt(Box::new(stmt.clone())), fixup, line)
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
    // Some lowerings read a node more than once, so the tree is cloned out.
    let (held, fixup) = TABLE
        .with(|t| {
            let table = t.borrow();
            let slot = table.as_ref()?.get(at)?.as_ref()?;
            let held = match &slot.held {
                Held::Expr(e) => Held::Expr(e.clone()),
                Held::Stmt(s) => Held::Stmt(s.clone()),
            };
            Some((held, slot.fixup))
        })
        .ok_or_else(|| RuntimeError::new("round-trip leaf outside its scope"))?;
    if !fixup {
        return Ok(held);
    }
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

/// Lowers every kept [`crate::ast::parsed::ParsedStmt`] of a tree, innermost first.
struct Kept(Option<RuntimeError>);

impl Kept {
    /// The operands of a compound assignment lowered as the conversion reads
    /// them: `* *= 2` curries to `{ $_ *= 2 }`, so a priming argument target is
    /// the bare `*`, and a conditional target is restored.
    fn compound_operands(&mut self, compound: &mut crate::ast::parsed::CompoundAssign) {
        self.visit_expr_mut(&mut compound.target);
        self.visit_expr_mut(&mut compound.source);
        let target = std::mem::replace(&mut compound.target, Expr::Whatever);
        let target = match super::compound_stmt::restore_conditional_target(target) {
            Expr::WhateverArg => Expr::Whatever,
            other => other,
        };
        compound.target = target;
    }
}

impl Kept {
    fn finish(self, held: Held) -> Result<Held, RuntimeError> {
        match self.0 {
            Some(err) => Err(err),
            None => Ok(held),
        }
    }
}

impl Kept {
    /// An assignment kept in its RakuAST shape, lowered as the expression or
    /// the statement it was written as; its operands are lowered first.
    fn assignment(&mut self, mut assign: Assign) -> Assign {
        self.visit_expr_mut(&mut assign.target);
        self.visit_expr_mut(&mut assign.source);
        assign
    }
}

impl VisitMut for Kept {
    // The traits a routine was written with are compiler-inert: lowering them
    // produces no statement (a stub body `{ ... }` is the body's only one).
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        body.retain(|s| match s {
            // A `;` that ends no statement runs nothing and is no block's value.
            Stmt::Expr(Expr::Spelled(spelled)) => !pure::is_empty_statement(spelled),
            _ => true,
        });
        // The names a body declares classify the barewords written in it, as
        // they do when it is converted.
        let _scope = super::bareword::BlockScope::enter(body);
        // A routine or `&name` declared in the body is in scope for the
        // statements after it, as it is for the parser.
        crate::parser::with_rakuast_lexical_scope(|| {
            for stmt in body.iter_mut() {
                self.visit_stmt_mut(stmt);
                crate::parser::register_rakuast_user_sub_binding(stmt);
            }
        });
    }

    // A spelling only annotates the expression it wraps (ADR-12199): an
    // executing parse builds the expression alone.
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        // An expression the leaf cannot carry (a bareword to classify, a
        // regex with code, ...) is converted and lowered on its own.
        let mut inner = &*expr;
        while let Expr::Spelled(spelled) = inner {
            inner = &spelled.expr;
        }
        if !pure::expr_node_allowed(inner) {
            let converted = {
                let _inspecting = Inspecting::enter();
                super::convert::convert_expr(expr).and_then(|node| super::lower::lower_expr(&node))
            };
            match converted {
                Ok(lowered) => *expr = lowered,
                Err(err) => {
                    self.0.get_or_insert(err);
                }
            }
            return;
        }
        while matches!(expr, Expr::Spelled(_)) {
            if let Expr::Spelled(spelled) = std::mem::replace(expr, Expr::Whatever) {
                *expr = spelled.expr;
            }
        }
        if matches!(expr, Expr::Ast(node) if matches!(node.as_ref(), ParsedExpr::Assign(_)))
            && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
        {
            let ParsedExpr::Assign(assign) = *node else {
                unreachable!("checked above");
            };
            *expr = super::lower_expression_assign(self.assignment(assign));
            return;
        }
        if matches!(expr, Expr::Ast(node) if matches!(node.as_ref(), ParsedExpr::CompoundAssign(_)))
            && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
        {
            let ParsedExpr::CompoundAssign(mut compound) = *node else {
                unreachable!("checked above");
            };
            self.compound_operands(&mut compound);
            match super::compound::lower::lower_compound_expression_with(
                compound,
                super::shadowed_terms::is_value_term,
            ) {
                Ok(lowered) => *expr = lowered,
                Err(err) => {
                    self.0.get_or_insert(RuntimeError::new(format!("{err:?}")));
                }
            }
            return;
        }
        // A word quote is the runtime's word list; its words are lowered
        // where they stand.
        if matches!(expr, Expr::Ast(node) if matches!(node.as_ref(), ParsedExpr::WordList(_) | ParsedExpr::WordsAtom(_)))
            && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
        {
            *expr = match *node {
                ParsedExpr::WordList(list) => {
                    super::word_list::lower_word_list(list.items, list.val)
                }
                ParsedExpr::WordsAtom(atom) => {
                    super::word_list::lower_words_atom(atom.string, atom.allomorphic)
                }
                _ => unreachable!("checked above"),
            };
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        if matches!(stmt, Stmt::Expr(Expr::Ast(node)) if matches!(node.as_ref(), ParsedExpr::Assign(_)))
            && let Stmt::Expr(Expr::Ast(node)) = std::mem::replace(stmt, Stmt::Last(None))
        {
            let ParsedExpr::Assign(assign) = *node else {
                unreachable!("checked above");
            };
            *stmt = super::lower_statement_assign(self.assignment(assign));
            return;
        }
        if matches!(stmt, Stmt::Expr(Expr::Ast(node)) if matches!(node.as_ref(), ParsedExpr::CompoundAssign(_)))
            && let Stmt::Expr(Expr::Ast(node)) = std::mem::replace(stmt, Stmt::Last(None))
        {
            let ParsedExpr::CompoundAssign(mut compound) = *node else {
                unreachable!("checked above");
            };
            self.compound_operands(&mut compound);
            match super::compound::lower::lower_compound_statement_with(
                compound,
                super::shadowed_terms::is_value_term,
            ) {
                Ok(lowered) => *stmt = lowered,
                Err(err) => {
                    self.0.get_or_insert(RuntimeError::new(format!("{err:?}")));
                }
            }
            return;
        }
        // A modifier lowers the declaration it governs itself (`my ($a, $b) =
        // ... if $c` declares unconditionally), so that statement stays kept
        // while its own operands are lowered.
        let mut pending = None;
        if let Stmt::Ast(node) = stmt
            && let ParsedStmt::Modified(modified) = node.as_mut()
            && matches!(modified.stmt.as_ref(), Stmt::Ast(inner) if matches!(inner.as_ref(), ParsedStmt::SignatureDecl(_)))
        {
            pending = Some(std::mem::replace(modified.stmt.as_mut(), Stmt::Last(None)));
        }
        if let Some(inner) = pending.as_mut() {
            crate::ast_visit::walk_stmt_mut(self, inner);
        }
        crate::ast_visit::walk_stmt_mut(self, stmt);
        if let Some(inner) = pending
            && let Stmt::Ast(node) = stmt
            && let ParsedStmt::Modified(modified) = node.as_mut()
        {
            *modified.stmt = inner;
        }
        // The traits a routine was written with stay in its body (the runtime
        // reads them); they are compiler-inert.
        if matches!(stmt, Stmt::Ast(node) if !matches!(node.as_ref(), ParsedStmt::RoutineTraits(_)))
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

/// The routine names the tree a leaf carries declares: a named `sub`, a `&name`
/// parameter. A node outside any leaf says it with its own class
/// (`declared_routines::declared_by`).
// Cost: O(n), n = nodes of the carried tree.
pub(super) fn declared_routines_of(
    node: &RakuAstNode,
    found: &mut std::collections::HashSet<String>,
) {
    struct Names<'f>(&'f mut std::collections::HashSet<String>);
    impl<'ast> crate::ast_visit::Visit<'ast> for Names<'_> {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if let Stmt::SubDecl { name, .. } = stmt {
                self.0.insert(name.resolve());
            }
            // `my &name` declares a routine of the unit.
            if let Stmt::VarDecl { name, .. } = stmt
                && let Some(code) = name.strip_prefix('&')
            {
                self.0.insert(code.to_string());
            }
            crate::ast_visit::walk_stmt(self, stmt);
        }
        fn visit_param(&mut self, param: &'ast crate::ast::ParamDef) {
            if let Some(name) = param.name.strip_prefix('&') {
                self.0.insert(name.to_string());
            }
            crate::ast_visit::walk_param(self, param);
        }
    }
    let Ok(at) = slot(node) else { return };
    TABLE.with(|t| {
        let table = t.borrow();
        let Some(Some(slot)) = table.as_ref().and_then(|t| t.get(at)) else {
            return;
        };
        let mut names = Names(found);
        match &slot.held {
            Held::Expr(e) => crate::ast_visit::Visit::visit_expr(&mut names, e),
            Held::Stmt(s) => crate::ast_visit::Visit::visit_stmt(&mut names, s),
        }
    });
}

/// The names a unit declares that `lower` resolves other names against
/// (`shadowed_terms`): sigilless terms and the packages a carried tree holds.
#[derive(Default)]
pub(super) struct ScopeFacts {
    pub(super) terms: std::collections::HashSet<String>,
    /// Names a constant or enum declaration makes the unit's own (`shadowed_terms`).
    pub(super) shadowed: std::collections::HashSet<String>,
    pub(super) packages: std::collections::HashSet<String>,
}

/// The scope facts the tree a leaf carries declares; the node scans of
/// `shadowed_terms` cannot see inside a leaf, so they ask it.
// Cost: O(n), n = nodes of the carried tree.
pub(super) fn scope_facts_of(node: &RakuAstNode, facts: &mut ScopeFacts) {
    struct Facts<'f>(&'f mut ScopeFacts);
    impl<'ast> crate::ast_visit::Visit<'ast> for Facts<'_> {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            match stmt {
                Stmt::Ast(node) => {
                    if let ParsedStmt::SigillessDecl(source) = node.as_ref() {
                        self.0.terms.insert(source.name.clone());
                    }
                }
                Stmt::VarDecl {
                    name,
                    constant: Some(sigil),
                    ..
                } => {
                    // `constant term:<$box> = ...` declares the term `$box`.
                    // A sigiled constant is the term `term:<$x>`, as the
                    // conversion spells it; the parser stores the bare name.
                    if matches!(sigil, crate::ast::ConstantSigil::Term) {
                        self.0.terms.insert(name.clone());
                    }
                    self.0.shadowed.insert(name.clone());
                }
                Stmt::EnumDecl { name, variants, .. } => {
                    self.0.shadowed.insert(name.resolve());
                    self.0
                        .shadowed
                        .extend(variants.iter().map(|(variant, _)| variant.clone()));
                }
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::Package { name, .. } => {
                    self.0.packages.insert(name.resolve());
                }
                _ => {}
            }
            crate::ast_visit::walk_stmt(self, stmt);
        }
        // `-> \p { ... }`: a sigilless parameter is a term of its body.
        fn visit_param(&mut self, param: &'ast crate::ast::ParamDef) {
            if param.sigilless {
                self.0.terms.insert(param.name.clone());
            }
            crate::ast_visit::walk_param(self, param);
        }
    }
    let Ok(at) = slot(node) else { return };
    TABLE.with(|t| {
        let table = t.borrow();
        let Some(Some(slot)) = table.as_ref().and_then(|t| t.get(at)) else {
            return;
        };
        let mut facts = Facts(facts);
        match &slot.held {
            Held::Expr(e) => crate::ast_visit::Visit::visit_expr(&mut facts, e),
            Held::Stmt(s) => crate::ast_visit::Visit::visit_stmt(&mut facts, s),
        }
    });
}

/// The expression a leaf carries.
// Cost: O(e), e = nodes of the tree (one clone).
pub(super) fn lower_expr(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match held(node)? {
        Held::Expr(e) => Ok(e),
        Held::Stmt(stmt) => match *stmt {
            Stmt::Expr(e) => Ok(e),
            _ => Err(RuntimeError::new("a statement leaf in expression position")),
        },
    }
}

/// The statement a leaf carries.
// Cost: O(s), s = nodes of the tree (one clone).
pub(super) fn lower_stmt(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match held(node)? {
        Held::Stmt(s) => Ok(*s),
        Held::Expr(e) => Ok(Stmt::Expr(e)),
    }
}

mod pure;
#[cfg(test)]
mod tests;

use pure::{analyse_expr, analyse_stmt};
