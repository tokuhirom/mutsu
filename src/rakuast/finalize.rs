//! The executing finalization of a compilation unit (ADR-15334 Stage 2, slice 9).
//!
//! An executing parse keeps the constructs of the migrated families in their
//! RakuAST shape (`ParsedStmt` / `ParsedExpr`) and wraps a few terms in
//! `Expr::Spelled`; the unit is finalized in place: each typed node is lowered
//! where it stands, each spelling is taken off, and a construct the parser's own
//! tree cannot stand for (a bareword to classify, a regex with code, an
//! operator declaration, ...) is converted to RakuAST and lowered on its own.
//! No `RakuAstNode` is built for the rest of the unit.
//!
//! `.AST` and `EVAL($ast)` do not come through here: they render and read the
//! nodes themselves (`convert`, `lower`).

use crate::scope::NameScope;
use std::cell::Cell;

use crate::ast::parsed::{Assign, ParsedExpr, ParsedStmt};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::VisitMut;
use crate::value::RuntimeError;

use super::carried;

thread_local! {
    /// Depth of the executing finalizations running on this thread.
    static EXECUTING: Cell<u32> = const { Cell::new(0) };
}

/// Whether an executing finalization is running: nothing then reads the source
/// positions `.AST` reports, only the line a statement began on, and a bare `$`
/// is converted with the `state` declaration the parser put with it.
// Cost: O(1).
pub(super) fn executing() -> bool {
    EXECUTING.with(|c| c.get()) > 0
}

struct Executing;

impl Executing {
    // Cost: O(1).
    fn enter() -> Self {
        EXECUTING.with(|c| c.set(c.get() + 1));
        Executing
    }
}

impl Drop for Executing {
    fn drop(&mut self) {
        EXECUTING.with(|c| c.set(c.get() - 1));
    }
}

/// Finalize the spelled compilation unit `stmts` for execution: the statements
/// the compiler receives.
// Cost: O(n), n = size of the unit.
pub(super) fn finalize_unit(names: &NameScope, mut stmts: Vec<Stmt>) -> Result<Vec<Stmt>, RuntimeError> {
    let _executing = Executing::enter();
    let _underscore_scope = super::underscore_bindings::Scope::reset();
    let _block_scope = super::underscore_bindings::Scope::block(&stmts);
    let _declared = super::bareword::DeclaredNames::collect(names, &stmts);
    let _method_scope = super::underscore_bindings::MethodScope::enter(false);
    super::shadowed_terms::scan_stmts(&stmts);
    super::infix_func::reset();
    let _operators = super::infix_func::enter_scope();
    let _keywords = super::shadowed_terms::enter_scope();
    let mut finalize = Finalize {
        names,
        failed: None,
        line: None,
        top: true,
    };
    crate::parser::with_rakuast_lexical_scope(|| {
        finalize.visit_stmts_mut(&mut stmts);
    });
    if let Some(err) = finalize.failed {
        return Err(err);
    }
    super::lower::finish_unit(&mut stmts)?;
    Ok(stmts)
}

/// Whether a statement declares a routine the statements after it can call:
/// a `sub` or a `&name` variable (see `register_rakuast_user_sub_binding`).
// Cost: O(1).
fn declares_routine(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::SubDecl { .. } => true,
        Stmt::VarDecl { name, .. } => name.starts_with('&'),
        _ => false,
    }
}

/// Lowers every typed node and spelling of a tree where it stands, innermost
/// first.
struct Finalize<'s> {
    names: &'s NameScope,
    failed: Option<RuntimeError>,
    /// The line of the last `SetLine` of the list being finalized.
    line: Option<i64>,
    /// The next list is the unit's own.
    top: bool,
}

impl Finalize<'_> {
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

    /// An assignment kept in its RakuAST shape, lowered as the expression or
    /// the statement it was written as; its operands are lowered first.
    fn assignment(&mut self, mut assign: Assign) -> Assign {
        self.visit_expr_mut(&mut assign.target);
        self.visit_expr_mut(&mut assign.source);
        assign
    }

    fn fail(&mut self, err: RuntimeError) {
        self.failed.get_or_insert(err);
    }

    /// A statement the parser's own tree cannot stand for, converted and
    /// lowered on its own.
    fn convert_statement(&mut self, stmt: &mut Stmt) {
        let line = self.line;
        let converted = super::convert::convert_stmt(stmt).and_then(|node| match node {
            Some(node) => {
                super::infix_func::note_declaration(&node);
                super::shadowed_terms::note_keyword_declaration(&node);
                super::origin::with_line(line, || super::lower::lower_stmt(&node)).map(Some)
            }
            None => Ok(None),
        });
        match converted {
            Ok(Some(lowered)) => *stmt = lowered,
            // A bookkeeping statement that renders as no node stays as it is.
            Ok(None) => {}
            Err(err) => self.fail(err),
        }
    }
}

impl VisitMut for Finalize<'_> {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        body.retain(|s| match s {
            // A `;` that ends no statement runs nothing and is no block's value.
            Stmt::Expr(Expr::Spelled(spelled)) => !carried::is_empty_statement(spelled),
            _ => true,
        });
        let top = std::mem::take(&mut self.top);
        let line = self.line.take();
        // The names a body declares classify the barewords written in it, as
        // they do when it is converted.
        let _scope = super::bareword::BlockScope::enter(self.names, body);
        // A routine or `&name` declared in the body is in scope for the
        // statements after it, as it is for the parser; a body that declares
        // none needs no scope of its own (the unit's own list is finalized in
        // the scope `finalize_unit` opened).
        if top {
            self.visit_listed_all(body, true);
        } else if body.iter().any(declares_routine) {
            crate::parser::with_rakuast_lexical_scope(|| self.visit_listed_all(body, true));
        } else {
            self.visit_listed_all(body, false);
        }
        self.line = line;
    }

    // A spelling only annotates the expression it wraps (ADR-12199): an
    // executing parse builds the expression alone.
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        // An expression the parser's tree cannot stand for (a bareword to
        // classify, a regex with code, ...) is converted and lowered on its own.
        let mut inner = &*expr;
        while let Expr::Spelled(spelled) = inner {
            inner = &spelled.expr;
        }
        if !carried::expr_carried(inner) {
            match super::convert::convert_expr(expr)
                .and_then(|node| super::lower::lower_expr(&node))
            {
                Ok(lowered) => *expr = lowered,
                Err(err) => self.fail(err),
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
                Err(err) => self.fail(RuntimeError::new(format!("{err:?}"))),
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
        if !carried::stmt_carried(stmt) {
            self.convert_statement(stmt);
            return;
        }
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
                Err(err) => self.fail(RuntimeError::new(format!("{err:?}"))),
            }
            return;
        }
        // A modifier lowers the declaration it governs itself (`my ($a, $b) =
        // ... if $c` declares unconditionally), so that statement stays
        // kept while its own operands are lowered.
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
        // The traits a package was written with (`is T`, `does R`) are the
        // package's own fields; only a routine keeps them in its body.
        if let Stmt::ClassDecl { body, .. }
        | Stmt::RoleDecl { body, .. }
        | Stmt::Package { body, .. } = stmt
        {
            body.retain(|s| {
                !matches!(s, Stmt::Ast(node) if matches!(node.as_ref(), ParsedStmt::RoutineTraits(_)))
            });
        }
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
                Err(err) => self.fail(err),
            }
        }
    }
}

impl Finalize<'_> {
    /// The statements of a list in order, each registered as a routine of the
    /// scope when `register` is set.
    fn visit_listed_all(&mut self, body: &mut [Stmt], register: bool) {
        for stmt in body.iter_mut() {
            self.visit_listed(stmt);
            if register {
                crate::parser::register_rakuast_user_sub_binding(stmt);
            }
        }
    }

    /// One statement of a list: the line a `SetLine` names is the line of the
    /// statements after it.
    fn visit_listed(&mut self, stmt: &mut Stmt) {
        if let Stmt::SetLine(line, _) = stmt {
            self.line = Some(*line);
            return;
        }
        self.visit_stmt_mut(stmt);
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Stmt;

    /// The statements without their line markers, whose positions the
    /// finalization does not keep.
    fn shape(stmts: Vec<Stmt>) -> String {
        let stmts: Vec<Stmt> = stmts
            .into_iter()
            .filter(|s| !matches!(s, Stmt::SetLine(..)))
            .collect();
        format!("{stmts:?}")
    }

    fn finalized(source: &str) -> String {
        let (spelled, _) = crate::parse_dispatch::parse_source_spelled(source).unwrap();
        shape(crate::rakuast::frontend::round_trip(&spelled).unwrap())
    }

    fn ordinary(source: &str) -> String {
        shape(crate::parse_dispatch::parse_source(source).unwrap().0)
    }

    /// A unit finalizes to the tree an ordinary parse builds.
    #[test]
    fn finalization_is_the_ordinary_tree() {
        for source in [
            "say 1 + 2;",
            "my $x = 5; say $x * 2;",
            "my @a = 1, 2, 3; say @a.map({ $_ + 1 });",
            "for 1..3 -> $i { say $i }\nsay 4;",
            "if 1 { say 'a' } else { say 'b' }\nsay 2;",
            "say 'x' if 1;",
            "my %h = a => 1; say %h<a>;",
            "my $s = 'abc'; say \"v: $s\";",
        ] {
            assert_eq!(finalized(source), ordinary(source), "{source}");
        }
    }

    /// A regex with code is converted where it stands, not carried.
    #[test]
    fn a_regex_with_code_is_converted_where_it_stands() {
        let (spelled, _) =
            crate::parse_dispatch::parse_source_spelled("say 'a' ~~ /a <?{ 1 }>/;").unwrap();
        let _ = crate::rakuast::frontend::round_trip(&spelled).expect("it lowers");
    }
}
