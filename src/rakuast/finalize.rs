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
//! nodes themselves (`view`, `lower`).

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
pub(super) fn finalize_unit(
    names: &NameScope,
    mut stmts: Vec<Stmt>,
) -> Result<Vec<Stmt>, RuntimeError> {
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
        later_terms: Vec::new(),
        seen_terms: Vec::new(),
        declared_types: unit_type_names(&stmts),
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
    /// Per enclosing list, the terms it declares at a later statement that
    /// are capitalised (`constant X = 3` in a list whose earlier statements say
    /// `X`). Rakudo hoists routines only: until the
    /// declaration, `X` is the setting's.
    later_terms: Vec<std::collections::HashSet<String>>,
    /// Per open list, parallel to `later_terms`: the terms its statements
    /// declared so far. Such a term shadows an outer list's later
    /// redeclaration of the name for the statements after it (`method m {
    /// enum Mode <Normal>; say Normal }` before a top-level `enum V
    /// (:Normal)`).
    seen_terms: Vec<std::collections::HashSet<String>>,
    /// The short names of the types the unit declares anywhere: a bareword of
    /// that name may be one of them, not the setting's.
    declared_types: std::collections::HashSet<String>,
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

    /// A subscript with colonpairs: the subscript and the adverb values are
    /// lowered where they stand, then the adverbs expand into the shapes the
    /// compiler executes.
    fn subscript_adverbs(&mut self, mut adverbs: crate::ast::parsed::SubscriptAdverbs) -> Expr {
        self.visit_expr_mut(&mut adverbs.subscript);
        for pair in &mut adverbs.pairs {
            self.visit_expr_mut(pair);
        }
        match super::subscript_adverb::lower_parsed(adverbs.subscript, adverbs.pairs) {
            Ok(lowered) => lowered,
            Err(err) => {
                self.fail(err);
                Expr::Whatever
            }
        }
    }

    /// `supply { BODY }`: the body is lowered where it stands, in a lexical
    /// scope of its own, then wrapped around its emitter.
    fn supply(&mut self, mut body: Vec<Stmt>) -> Expr {
        crate::parser::with_rakuast_lexical_scope(|| self.visit_stmts_mut(&mut body));
        super::supply_lower::supply_method_call(body)
    }

    /// The body of a `start { ... }` whose placeholder Raku rejects: lowered
    /// where it stands, in a lexical scope of its own, then wrapped in the
    /// do-block that reports the placeholder.
    fn start_body(&mut self, mut body: Vec<Stmt>) -> Expr {
        crate::parser::with_rakuast_lexical_scope(|| self.visit_stmts_mut(&mut body));
        Expr::desugar_block(body)
    }

    /// A typed term whose lowering is one function over its operands: the
    /// operands are lowered where they stand, then the function runs.
    fn parsed_term(&mut self, node: ParsedExpr) -> Expr {
        match node {
            ParsedExpr::HyperPrefix(mut prefix) => {
                self.visit_expr_mut(&mut prefix.operand);
                match super::hyper_prefix::lower_hyper_prefix(&prefix.symbol, prefix.operand) {
                    Some(lowered) => lowered,
                    None => {
                        self.fail(RuntimeError::new(format!(
                            "hyper prefix `{}` has no lowering",
                            prefix.symbol
                        )));
                        Expr::Whatever
                    }
                }
            }
            ParsedExpr::AtomicVar(mut atomic) => {
                if let crate::ast::parsed::AtomicVar::Store { value, .. } = &mut atomic {
                    self.visit_expr_mut(value);
                }
                crate::ast::atomic_op::lower_var(atomic)
            }
            ParsedExpr::ObjectHash(mut entries) => {
                for entry in &mut entries {
                    self.visit_expr_mut(entry);
                }
                Expr::Call {
                    name: crate::symbol::Symbol::intern("__object_hash"),
                    args: entries,
                    listop: false,
                }
            }
            ParsedExpr::HyperAssign(mut assign) => {
                self.visit_expr_mut(&mut assign.target);
                self.visit_expr_mut(&mut assign.value);
                super::hyper_assign::lower_hyper_assignment(
                    assign.target,
                    assign.value,
                    assign.dwim_left,
                    assign.dwim_right,
                )
            }
            ParsedExpr::InlineSubset(mut declaration) => {
                self.visit_stmt_mut(&mut declaration);
                crate::ast::inline_subset_term::lower(*declaration)
            }
            other => unreachable!("not a one-function term: {other:?}"),
        }
    }

    fn fail(&mut self, err: RuntimeError) {
        self.failed.get_or_insert(err);
    }

    /// A statement the parser's own tree cannot stand for, converted and
    /// lowered on its own.
    fn view_statement(&mut self, stmt: &mut Stmt) {
        let names = self.names;
        let line = self.line;
        let converted = super::view::view_stmt(names, stmt).and_then(|node| match node {
            Some(node) => {
                super::infix_func::note_declaration(&node);
                super::shadowed_terms::note_keyword_declaration(&node);
                super::origin::with_line(line, || super::lower::lower_stmt(names, &node)).map(Some)
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
        self.later_terms.push(setting_terms_declared_in(body));
        self.seen_terms.push(std::collections::HashSet::new());
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
        self.later_terms.pop();
        self.seen_terms.pop();
        self.line = line;
    }

    // A spelling only annotates the expression it wraps (ADR-12199): an
    // executing parse builds the expression alone.
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        let names = self.names;
        // A bareword naming a setting term that a later statement of an
        // enclosing list redeclares is still the setting's (`say X; constant
        // X = 3`).
        if let Expr::BareWord(word) = &*expr
            && self.later_terms.iter().enumerate().any(|(i, later)| {
                later.contains(word)
                    && !self.seen_terms[i + 1..]
                        .iter()
                        .any(|seen| seen.contains(word))
            })
            && !self.declared_types.contains(word)
        {
            if !names_setting_item(word) {
                // The setting has no such name either: as undeclared as a
                // lowercase one, at compile time.
                let line = self.line.unwrap_or(0);
                self.fail(undeclared_routine(word, line));
                return;
            }
            *expr =
                Expr::BareWord(crate::qualified::qualified_text("CORE", word.as_str()).resolve());
            return;
        }
        // An expression the parser's tree cannot stand for (a bareword to
        // classify, a regex with code, ...) is converted and lowered on its own.
        let mut inner = &*expr;
        while let Expr::Spelled(spelled) = inner {
            inner = &spelled.expr;
        }
        // An `nqp::` bareword: the classifier's answer is the lowering's own.
        if let Expr::BareWord(word) = inner
            && let Some(term) = super::bareword::nqp_term(names, word)
        {
            *expr = term;
            return;
        }
        // A core bare call (`callsame`, `done`, ...): see `bareword::core_bare_call`.
        if let Expr::BareWord(word) = inner
            && let Some(call) = super::bareword::core_bare_call(names, word)
        {
            *expr = call;
            return;
        }
        // A plain term the classifier resolves (`my \x`, a constant): see
        // `bareword::term_expr`.
        if let Expr::BareWord(word) = inner
            && let Some(term) = super::bareword::term_expr(names, word)
        {
            *expr = term;
            return;
        }
        if !carried::expr_carried(inner) {
            match super::view::view_expr(names, expr)
                .and_then(|node| super::lower::lower_expr(names, &node))
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
        // A subscript with colonpairs: see `Finalize::subscript_adverbs`.
        if matches!(expr, Expr::Ast(node) if matches!(node.as_ref(), ParsedExpr::SubscriptAdverbs(_)))
            && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
        {
            let ParsedExpr::SubscriptAdverbs(adverbs) = *node else {
                unreachable!("checked above");
            };
            *expr = self.subscript_adverbs(adverbs);
            return;
        }
        // The other typed terms, each lowered by its one function over the
        // operands lowered where they stand: see `Finalize::parsed_term`.
        if matches!(expr, Expr::Ast(node) if matches!(
            node.as_ref(),
            ParsedExpr::HyperPrefix(_)
                | ParsedExpr::AtomicVar(_)
                | ParsedExpr::ObjectHash(_)
                | ParsedExpr::HyperAssign(_)
                | ParsedExpr::InlineSubset(_)
        )) && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
        {
            *expr = self.parsed_term(*node);
            return;
        }
        // `supply { ... }`: see `Finalize::supply`.
        if matches!(expr, Expr::Ast(node) if matches!(node.as_ref(), ParsedExpr::Supply(_)))
            && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
        {
            let ParsedExpr::Supply(body) = *node else {
                unreachable!("checked above");
            };
            *expr = self.supply(body);
            return;
        }
        // `start { ... }` over a rejected placeholder: see `Finalize::start_body`.
        if matches!(expr, Expr::Ast(node) if matches!(node.as_ref(), ParsedExpr::StartBody(_)))
            && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
        {
            let ParsedExpr::StartBody(body) = *node else {
                unreachable!("checked above");
            };
            *expr = self.start_body(body);
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
            self.view_statement(stmt);
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
        // A term is declared where its statement begins, so its own
        // initializer sees it (`my \S := gather { S.iterator }`). Judged
        // before the statement is lowered in place.
        if let Some(later) = self.later_terms.last_mut()
            && let Some(seen) = self.seen_terms.last_mut()
        {
            for name in declared_terms(stmt) {
                later.remove(&name);
                seen.insert(name);
            }
        }
        self.visit_stmt_mut(stmt);
    }
}

/// The sigilless terms a statement declares in its own list: a `constant`, an
/// enum's values, a `my \x`; a parser desugaring's statements count as the
/// list's own.
// Cost: O(1) for a statement, O(v) for an enum, v = its values.
fn declared_terms(stmt: &Stmt) -> Vec<String> {
    match stmt {
        Stmt::SyntheticBlock(inner) => inner.iter().flat_map(declared_terms_one).collect(),
        _ => declared_terms_one(stmt),
    }
}

fn declared_terms_one(stmt: &Stmt) -> Vec<String> {
    if let Stmt::VarDecl {
        name,
        constant: Some(crate::ast::ConstantSigil::Term),
        ..
    } = stmt
    {
        return vec![name.clone()];
    }
    if let Stmt::EnumDecl { variants, .. } = stmt {
        return crate::parser::enum_value_names(variants);
    }
    if let Stmt::Ast(node) = stmt
        && let ParsedStmt::SigillessDecl(decl) = node.as_ref()
    {
        return vec![decl.name.clone()];
    }
    Vec::new()
}

/// The capitalised terms `body` declares: a use before the declaration can only
/// be the setting's.
// Cost: O(m), m = statements in the list.
fn setting_terms_declared_in(body: &[Stmt]) -> std::collections::HashSet<String> {
    body.iter()
        .flat_map(declared_terms)
        .filter(|name| name.starts_with(|c: char| c.is_uppercase()))
        .collect()
}

/// Whether the setting has a type, term or package of this name.
// Cost: O(k), k = length of the name.
fn names_setting_item(name: &str) -> bool {
    crate::known_types::is_known_type_constraint(name)
        || super::core_term_names::kind(name).is_some()
        || matches!(name, "X" | "Metamodel" | "CORE")
}

/// Rakudo's compile-time answer to a bareword nothing declares.
// Cost: O(k), k = length of the name.
fn undeclared_routine(name: &str, line: i64) -> RuntimeError {
    let mut err = RuntimeError::undeclared_routine_symbols(
        name,
        format!("Undeclared routine:\n    {name} used at line {line}"),
        Vec::new(),
    );
    err.set_code(Some(crate::value::RuntimeErrorCode::ParseGeneric));
    if line > 0 {
        err.set_line(Some(line as usize));
    }
    err
}

/// The short names of the types a unit declares, wherever.
// Cost: O(n), n = size of the unit.
fn unit_type_names(stmts: &[Stmt]) -> std::collections::HashSet<String> {
    use crate::ast_visit::{Visit, walk_stmt};
    #[derive(Default)]
    struct Types(std::collections::HashSet<String>);
    impl<'ast> Visit<'ast> for Types {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if let Stmt::ClassDecl { name, .. }
            | Stmt::RoleDecl { name, .. }
            | Stmt::SubsetDecl { name, .. }
            | Stmt::Package { name, .. }
            | Stmt::EnumDecl { name, .. } = stmt
            {
                self.0.insert(name.resolve());
                self.0
                    .insert(crate::qualified::unqualified_part(*name).resolve());
            }
            walk_stmt(self, stmt);
        }
    }
    let mut types = Types::default();
    for stmt in stmts {
        types.visit_stmt(stmt);
    }
    types.0
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

    fn finalized(names: &crate::scope::NameScope, source: &str) -> String {
        let (spelled, _) = crate::parse_dispatch::parse_source_spelled(source).unwrap();
        shape(crate::rakuast::frontend::round_trip(names, &spelled).unwrap())
    }

    fn ordinary(source: &str) -> String {
        shape(crate::parse_dispatch::parse_source(source).unwrap().0)
    }

    /// A unit finalizes to the tree an ordinary parse builds.
    #[test]
    fn finalization_is_the_ordinary_tree() {
        let names = &crate::scope::NameScope::default();
        for source in [
            "say 1 + 2;",
            "my $x = 5; say $x * 2;",
            "my @a = 1, 2, 3; say @a.map({ $_ + 1 });",
            "for 1..3 -> $i { say $i }\nsay 4;",
            "if 1 { say 'a' } else { say 'b' }\nsay 2;",
            "say 'x' if 1;",
            "my %h = a => 1; say %h<a>;",
            "my $s = 'abc'; say \"v: $s\";",
            "sub f($x) { $x + 1 }; my &g = &f; say g(1); say &f.name; say &g.(2);",
            "my @l = (1, 2).map(&f); say &say.name; say &infix:<+>(1, 2);",
            "say (* + 1)(2); say (1, 2).map(* * 2); my @a = 1..3; say @a[*-1]; say (*.uc)('a');",
            "my @a = 1..5; say @a[]; say @a[1, 2]; @a[0] = 9; my %h; %h<a> = 1; say %h<a>:exists;",
            "my @m = [1, 2], [3, 4]; say @m[1;0]; @m[0;1] = 7; say @m.raku;",
            "'abc' ~~ /(b)/; say $<x>; say $/[0]; say $0;",
            "my $v = 3; say 1 < $v < 5; my $y = ($v = 4); say $y;",
            "my $n = 'v'; my $v = 1; say $::($n); say ::('Int'); say $::('v');",
            "my @l = (1, 2, 3); say @l>>.\"abs\"(); say CORE::<&say>.name;",
            "(1, 2, 3) ==> map({ $_ * 2 }) ==> my @out; say @out; for 1..3 { next if $_ == 2; last if $_ == 3; say $_ }",
            "my $t = q:to/E/;\n  hi $*PID\n  E\nsay $t.chars > 0;",
            "use nqp; my $t = nqp::time; say $t > 0; say nqp::const::CCLASS_WHITESPACE > 0;",
            "sub e { callsame }; sub f { nextsame }; sub g { lastcall }; sub h { nextcallee }; say 1;",
            "my \\s = 5; say s; constant zz = 3; say zz + 1; say pi; say tau; say True; say Empty.elems;",
            "my \\target = 'a'; say target; sub f(\\c) { c }; say f(1);",
            "my $s = 'abc'; $s ~~ s/b/X/; say $s;",
            "my $s = 'aaa'; $s ~~ s:g/a/b$s/; say $s;",
            "my $s = 'abc'; say $s.subst(/b/, 'X'); say S/b/Y/ given $s;",
            "my $s = 'abc'; $s ~~ s[b] = uc $/; $s ~~ s:2nd/a/c/;",
            "my $s = 'abc'; $s ~~ s:i:x(2)/a/{$s}/; say $s;",
            "my $s = 'abc'; $s ~~ tr/a-c/A-C/; say $s; say $s.trans('A' => 'x');",
            "my $s = 'abc'; say (S:g:ii/b/Z/ given $s); $s ~~ s[b] += 1;",
        ] {
            assert_eq!(finalized(names, source), ordinary(source), "{source}");
        }
    }

    /// A regex with code is converted where it stands, not carried.
    #[test]
    fn a_regex_with_code_is_converted_where_it_stands() {
        let names = &crate::scope::NameScope::default();
        let (spelled, _) =
            crate::parse_dispatch::parse_source_spelled("say 'a' ~~ /a <?{ 1 }>/;").unwrap();
        let _ = crate::rakuast::frontend::round_trip(names, &spelled).expect("it lowers");
    }
}
