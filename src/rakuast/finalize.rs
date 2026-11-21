//! The executing finalization of a compilation unit (ADR-15334 Stage 2, slice 9).
//!
//! An executing parse keeps the constructs of the migrated families in their
//! RakuAST shape (`ParsedStmt` / `ParsedExpr`) and builds no spellings
//! (ADR-15760 §2.4); the unit is finalized in place: each typed node is lowered
//! where it stands, and a construct the parser's own tree cannot stand for (a
//! bareword to classify, a regex with code, an operator declaration, ...) is
//! converted to RakuAST and lowered on its own.
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

/// Finalize the parsed compilation unit `stmts` for execution: the statements
/// the compiler receives.
// Cost: O(n), n = size of the unit.
pub(super) fn finalize_unit(
    names: &NameScope,
    mut stmts: Vec<Stmt>,
) -> Result<Vec<Stmt>, RuntimeError> {
    let mut unit = UnitFinalizer::begin(names, &stmts);
    for stmt in stmts.iter_mut() {
        unit.commit(stmt);
    }
    unit.finish(&mut stmts)?;
    Ok(stmts)
}

/// The finalization of one compilation unit, fed its top-level statements in
/// source order (ADR-15760 §7.2 (b), slice 2d): [`UnitFinalizer::commit`]
/// finalizes one statement, with every list nested in it, and
/// [`UnitFinalizer::finish`] runs the passes that need the whole lowered unit.
///
/// The unit's lookahead inputs (the routines it declares, which are hoisted,
/// the capitalised terms and core-named types its later statements declare,
/// and the `@_` / `%_` it declares) are read off the whole unit when it
/// begins.
pub(super) struct UnitFinalizer<'s> {
    finalize: Finalize<'s>,
    /// The index of the next top-level statement.
    next: usize,
    // The scopes the finalization runs in, in the reverse of the order they
    // are entered: fields are dropped in declaration order, and each restores
    // what the one entered before it installed.
    _top_scope: super::bareword::BlockScope<'s>,
    _keywords: crate::scope::ListScope<'s>,
    _operators: crate::scope::ListScope<'s>,
    _unit: crate::scope::UnitGuard<'s>,
    _method_scope: super::underscore_bindings::MethodScope,
    _declared: super::bareword::DeclaredNames<'s>,
    _block_scope: super::underscore_bindings::Scope,
    _underscore_scope: super::underscore_bindings::Scope,
    _spellings: crate::ast::spelled::KeepSpellings,
    _executing: Executing,
}

impl<'s> UnitFinalizer<'s> {
    /// Begin finalizing the unit whose top-level statements are `unit`.
    // Cost: O(n), n = size of the unit (its lookahead inputs).
    pub(super) fn begin(names: &'s NameScope, unit: &[Stmt]) -> Self {
        if std::env::var_os("MUTSU_ROUTINE_CENSUS").is_some() {
            super::declared_routines::CENSUS_SO_FAR.with(|s| *s.borrow_mut() = Some(Default::default()));
        }
        let executing = Executing::enter();
        // The lowering of a converted construct builds no spellings either.
        let spellings = crate::ast::spelled::KeepSpellings::enter(false);
        let underscore_scope = super::underscore_bindings::Scope::reset();
        let block_scope = super::underscore_bindings::Scope::block(unit);
        // The unit's names are declared in source order (ADR-15760 §7.2 (c)):
        // each top-level statement declares its own as it is committed
        // (`Finalize::top_statement`); only its routines, which are hoisted,
        // are known up front.
        let declared = super::bareword::DeclaredNames::start(names);
        let method_scope = super::underscore_bindings::MethodScope::enter(false);
        let unit_guard = super::shadowed_terms::scan_routines(names, unit);
        let operators = super::infix_func::enter_scope(names);
        let keywords = super::shadowed_terms::enter_scope(names);
        // The unit's own list declares its names in source order, not up front
        // as a nested list does.
        let top_scope = super::bareword::BlockScope::enter(names, &[]);
        let finalize = Finalize {
            names,
            failed: None,
            line: None,
            later_terms: vec![setting_terms_declared_in(unit)],
            seen_terms: vec![std::collections::HashSet::new()],
            declared_types: std::collections::HashSet::new(),
            qualify: crate::parser::UnitQualify::new(unit),
        };
        UnitFinalizer {
            finalize,
            next: 0,
            _top_scope: top_scope,
            _keywords: keywords,
            _operators: operators,
            _unit: unit_guard,
            _method_scope: method_scope,
            _declared: declared,
            _block_scope: block_scope,
            _underscore_scope: underscore_scope,
            _spellings: spellings,
            _executing: executing,
        }
    }

    /// Finalize the unit's next top-level statement.
    // Cost: O(s), s = size of the statement.
    pub(super) fn commit(&mut self, stmt: &mut Stmt) {
        let index = self.next;
        self.next += 1;
        crate::parser::with_rakuast_lexical_scope(|| self.finalize.top_statement(index, stmt));
    }

    /// The unit's statements are all committed: `stmts` is the finalized
    /// unit, put through the passes that need it whole.
    // Cost: O(n), n = size of the unit.
    pub(super) fn finish(mut self, stmts: &mut [Stmt]) -> Result<(), RuntimeError> {
        // The unit's own list is closed.
        drop(self._top_scope);
        if let Some(err) = self.finalize.failed.take() {
            return Err(err);
        }
        super::lower::finish_unit(stmts)
    }
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

/// Lowers every typed node of a tree where it stands, innermost
/// first.
struct Finalize<'s> {
    names: &'s NameScope,
    failed: Option<RuntimeError>,
    /// The line of the last `SetLine` of the list being finalized.
    line: Option<i64>,
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
    /// The type positions of each finalized top-level statement are qualified
    /// from the packages declared so far (ADR-15760 §7.2 (d)).
    qualify: crate::parser::UnitQualify,
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
                super::infix_func::note_declaration(names, &node);
                super::shadowed_terms::note_keyword_declaration(names, &node);
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
        let line = self.line.take();
        self.later_terms.push(setting_terms_declared_in(body));
        self.seen_terms.push(std::collections::HashSet::new());
        // The names a body declares classify the barewords written in it, as
        // they do when it is converted. (The unit's own list is not finalized
        // here but statement by statement, `UnitFinalizer::commit`.)
        let _scope = super::bareword::BlockScope::enter(self.names, body);
        // A routine or `&name` declared in the body is in scope for the
        // statements after it, as it is for the parser; a body that declares
        // none needs no scope of its own.
        if body.iter().any(declares_routine) {
            crate::parser::with_rakuast_lexical_scope(|| self.visit_listed_all(body, true));
        } else {
            self.visit_listed_all(body, false);
        }
        self.later_terms.pop();
        self.seen_terms.pop();
        self.line = line;
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        // What a slang production stands for was lowered when the parser built it.
        if matches!(expr, Expr::Ast(node) if matches!(node.as_ref(), ParsedExpr::Lowered { .. }))
            && let Expr::Ast(node) = std::mem::replace(expr, Expr::Whatever)
            && let ParsedExpr::Lowered { expr: inner, .. } = *node
        {
            *expr = *inner;
            return;
        }
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
        let inner = &*expr;
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
        // An interpolating heredoc: its body is read as a `qq` string here, in the
        // finalization, where a parse that keeps spellings has none left to read. A
        // heredoc that closes an enclosing block on its marker line stays converted.
        if let Expr::HeredocInterpolation(content, false) = inner
            && let Ok(body) = crate::parser::interpolate_heredoc_content(content)
        {
            *expr = body;
            crate::ast_visit::walk_expr_mut(self, expr);
            return;
        }
        if !carried::expr_carried(inner) {
            // A plain term the classifier resolves (`my \x`, a constant): see
            // `bareword::term_expr`. Only a word that is not carried as a type
            // name.
            if let Expr::BareWord(word) = &*expr
                && let Some(term) = super::bareword::term_expr(names, word)
            {
                *expr = term;
                return;
            }
            // An imported term: see `bareword::imported_term_expr`.
            if let Expr::Ast(node) = &*expr
                && let ParsedExpr::ImportedTerm(word) = node.as_ref()
                && let Some(term) = super::bareword::imported_term_expr(names, word)
            {
                *expr = term;
                return;
            }
            // A parameterized type over a lowercase alias (`gpointer[T]`): see
            // `bareword::parameterized_type_word_expr`.
            if let Expr::BareWord(word) = &*expr
                && let Some(ty) = super::bareword::parameterized_type_word_expr(names, word)
            {
                *expr = ty;
                return;
            }
            // A native type with a smiley (`int:D`): see `bareword::smiley_type_word_expr`.
            if let Expr::BareWord(word) = &*expr
                && let Some(ty) = super::bareword::smiley_type_word_expr(names, word)
            {
                *expr = ty;
                return;
            }
            // A routine word (`die-on-fail`, an imported function): see
            // `bareword::call_word_expr`.
            if let Expr::BareWord(word) = &*expr
                && let Some(call) = super::bareword::call_word_expr(names, word)
            {
                *expr = call;
                return;
            }
            // A type word the classifier resolves to `Type::Simple` (`uint8`):
            // see `bareword::type_word_expr`.
            if let Expr::BareWord(word) = &*expr
                && let Some(ty) = super::bareword::type_word_expr(names, word)
            {
                *expr = ty;
                return;
            }
            match super::view::view_expr(names, expr)
                .and_then(|node| super::lower::lower_expr(names, &node))
            {
                Ok(lowered) => *expr = lowered,
                Err(err) => self.fail(err),
            }
            return;
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
            match super::compound::lower::lower_compound_expression_with(compound, &|name| {
                super::shadowed_terms::is_value_term(self.names, name)
            }) {
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
        // A regex keeps two copies of the code inside it: the `tree` the walk reaches and the
        // `value` some paths execute directly (a smart match against `m/.../`). Both are
        // finalized, so the value carries the finalized tree.
        match expr {
            Expr::Literal(value) => {
                if let Some(tree) = value.regex_source_tree()
                    && carried::has_code(tree)
                {
                    let mut tree = tree.clone();
                    crate::ast_visit::walk_regex_tree_mut(self, &mut tree);
                    *value = value.with_regex_source_tree(tree);
                }
            }
            Expr::RegexLiteral { value, tree } | Expr::MatchRegexTree { value, tree }
                if carried::has_code(tree) =>
            {
                crate::ast_visit::walk_regex_tree_mut(self, tree);
                *value = value.with_regex_source_tree(tree.clone());
                return;
            }
            // `m:p($pos)/.../`: the regex lives only in the value; its adverb expressions
            // are reached by the walk below.
            Expr::MatchRegexDynamicAdverbs { value, .. } => {
                if let Some(tree) = value.regex_source_tree()
                    && carried::has_code(tree)
                {
                    let mut tree = tree.clone();
                    crate::ast_visit::walk_regex_tree_mut(self, &mut tree);
                    *value = value.with_regex_source_tree(tree);
                }
            }
            _ => {}
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        // What a slang production stands for was lowered when the parser built it.
        if matches!(stmt, Stmt::Ast(node) if matches!(node.as_ref(), ParsedStmt::Lowered { .. }))
            && let Stmt::Ast(node) = std::mem::replace(stmt, Stmt::Last(None))
            && let ParsedStmt::Lowered { stmt: inner, .. } = *node
        {
            *stmt = *inner;
            return;
        }
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
            match super::compound::lower::lower_compound_statement_with(compound, &|name| {
                super::shadowed_terms::is_value_term(self.names, name)
            }) {
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
            match super::lower_parsed_stmt(self.names, *node) {
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

    /// The unit's `index`-th statement, which declares its names for itself
    /// and the statements after it before it is finalized (ADR-15760 §7.2 (c)):
    /// a statement before a declaration sees the setting's meaning of the name,
    /// as in rakudo, even inside a routine body.
    // Cost: O(s·d), s = size of the statement, d = package nesting depth.
    fn top_statement(&mut self, index: usize, stmt: &mut Stmt) {
        if std::env::var_os("MUTSU_ROUTINE_CENSUS").is_some() {
            let r = super::shadowed_terms::census_routines(stmt);
            super::declared_routines::CENSUS_SO_FAR.with(|s| {
                s.borrow_mut().get_or_insert_with(Default::default).extend(r)
            });
        }
        super::bareword::declare_statement(self.names, stmt);
        super::shadowed_terms::declare_statement(self.names, stmt);
        self.declared_types
            .extend(unit_type_names(std::slice::from_ref(stmt)));
        self.visit_listed(stmt);
        self.qualify.statement(index, stmt);
        crate::parser::register_rakuast_user_sub_binding(stmt);
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
#[path = "finalize_purity_tests.rs"]
mod purity_tests;

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
        let (spelled, _) = crate::parse_dispatch::parse_source_unfinalized(source).unwrap();
        shape(crate::rakuast::frontend::finish_unit_in(names, spelled).unwrap())
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
            "sub die-on-fail { 7 }; say die-on-fail; my $x = die-on-fail + 1; say $x;",
            "sub w { 'a' }; say w ~ w; sub foo-bar { 1 }; say foo-bar;",
            "my $self = 3; say $self; sub g($self, $y) { \"$self-$y\" }; say g(1, 2); my $f = sub ($self) { $self }; say $f(4);",
            "=begin pod\nhi\n=end pod\nsay $=pod.elems; say $=pod[0].contents.elems;",
            "say Int(Str).^name; say Int().^name; say Int(\"5\").raku;",
            "sub f(|c) { c.raku }; say f(1, :a); say \\(1, 2).raku; say \\(:x(1)).raku; my $c = \\(1, \"a\"); say $c.raku; say \\((3, 4)).elems;",
            "my @a = 1, 2; @a[0] = 5; my %h; %h<a> = 1; %h{'b'} = 2; @a[1] += 3; say @a.raku; say %h.sort.raku; my @m; @m[0][1] = 7; say @m.raku; %h<x><y> = 4; say %h<x>.raku;",
            "my @a = 1, 2; @a[1] = @a[0] = 8; say @a.raku; my %h; %h<k>{'z'} = 1; say %h.raku; @a[2, 3] = 5, 6; say @a.raku;",
            "my $t = token { <[+-]> \\d+ }; say 'x-5' ~~ /x <$t>/; my $r = rule { a b }; say 'a b' ~~ $r; my $q = regex { x+ }; say so 'xx' ~~ $q;",
            "my @s; 'ab' ~~ / a { @s.push(1) } b /; say @s.raku; my $t = token { \\d+ { @s.push(2) } }; say so '12' ~~ /<$t>/; say @s.raku; my $n = 0; 'aaa' ~~ / [a { $n++ }]+ /; say $n; say so 'ab' ~~ /a <?{ $n > 1 }> b/;",
            "my uint8 $a = 1; my int32 $b = 2; say $a + $b; say uint16.^name; say int64 ~~ Int;",
            "my $s = 'abc'; $s ~~ s/b/X/; say $s;",
            "my $s = 'aaa'; $s ~~ s:g/a/b$s/; say $s;",
            "my $s = 'abc'; say $s.subst(/b/, 'X'); say S/b/Y/ given $s;",
            "my $s = 'abc'; $s ~~ s[b] = uc $/; $s ~~ s:2nd/a/c/;",
            "my $s = 'abc'; $s ~~ s:i:x(2)/a/{$s}/; say $s;",
            "my $s = 'abc'; $s ~~ tr/a-c/A-C/; say $s; say $s.trans('A' => 'x');",
            "my $s = 'abc'; say (S:g:ii/b/Z/ given $s); $s ~~ s[b] += 1;",
            "use NativeCall; sub f(int:D $x, num:_ $y) { $x }; say f(2, 1e0); say int:D.^name;",
            "my @a = 1, 2; my $v = 9; @a[0] := $v; my %h; %h<k> := $v; %h{'j'} := 1 => 2; say @a.raku;",
            "use NativeCall; my uint16 $n = 3; sub f(int64 $x) { $x }; say f(2); say int32.^name; say num64 ~~ Num;",
            "my $t = 'xxabcabc'; my $p = 2; say so $t ~~ m:p($p)/ abc /; say $/.from; $p = 5; say so $t ~~ m:c($p)/ abc /;",
            "sub plus($x, $y) { $x + $y }; my @a = 1, 2; say (@a >>[&plus]<< @a).raku; say (@a >>[&plus]>> 5).raku;",
            "my $w = 'x'; my $t = qq:to/E/;\n  hi $w {1 + 1}\n  E\nsay $t; say \"a\" ~ q:to/F/;\n  b\n  F\n",
        ] {
            assert_eq!(finalized(names, source), ordinary(source), "{source}");
        }
    }

    /// A regex with code is converted where it stands, not carried.
    #[test]
    fn a_regex_with_code_is_converted_where_it_stands() {
        let names = &crate::scope::NameScope::default();
        let (spelled, _) =
            crate::parse_dispatch::parse_source_unfinalized("say 'a' ~~ /a <?{ 1 }>/;").unwrap();
        let _ = crate::rakuast::frontend::finish_unit_in(names, spelled).expect("it lowers");
    }
}
