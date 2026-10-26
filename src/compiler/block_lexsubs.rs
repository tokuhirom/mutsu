//! A routine declared in a block is a lexical binding of that block for the
//! closures nested in it (ADR-12529 phase 2, #12850, #12851).
//!
//! In rakudo `sub g { ... }` is `my &g`: a closure nested in the declaring
//! block captures the binding like any other lexical, so the closure keeps
//! calling the `g` of the activation it was created in, after the block has
//! exited and while a later activation runs:
//!
//! ```raku
//! my @a; for 1, 2 -> $v { my $q = $v; sub h { $q }; @a.push(-> { h() }) }
//! say @a.map({ $_() });   # (1 2)
//! ```
//!
//! mutsu resolved such a call by name through the routine registry, which an
//! inlined block restores on exit ("Unknown function") and which holds only
//! the latest activation's routine. Here the compiler treats the routine as
//! the binding it is, for the closures nested in its block only:
//!
//! - an inline block, a loop body or a closure body records the plain
//!   routines it declares ([`Compiler::note_block_lexsubs`]); a routine body's
//!   own top level does not (ADR-0113 keeps those frame-lexical, and a routine
//!   activation already binds its nested subs' free variables, ADR-0114);
//! - a closure body inherits the records of the scopes around it
//!   ([`Compiler::inherit_block_lexsubs`]); a bare call `g(...)` there compiles
//!   as `&g(...)`, and a `&g` read is noted, so `&g` becomes an upvalue
//!   (`CompiledCode::lexsub_refs`, `compute_upvalues`);
//! - the frame that creates the closure fills that upvalue with the code
//!   object of the activation that declared `g` (`bind_lexsub_upvalues` in
//!   the VM), which is what slice 9 of phase 3 gave a `&g` read in the
//!   declaring code itself.
//!
//! A closure built from a body compiled elsewhere (a runtime recompile of its
//! AST) has no upvalue array, so its `&g(...)` resolves by name as before.

use super::Compiler;
use crate::ast::Stmt;
use crate::symbol::Symbol;

impl Compiler {
    /// Record the plain routines `stmts` declares at its top level as bindings
    /// of the scope it is compiled in: the innermost active scope, or the one
    /// about to be entered when its frame is still pending (a `for` body hoists
    /// before its frame is pushed).
    ///
    /// Cost: O(n * d), n = `stmts`, d = routines recorded so far (a short list).
    pub(super) fn note_block_lexsubs(&mut self, stmts: &[Stmt]) {
        let level = self.local_scopes.len() + usize::from(self.pending_scope_frame.is_some());
        for stmt in stmts {
            if let Some(name) = Self::plain_lexsub_name(stmt)
                && !self.block_lexsub_decls.contains(&(level, name))
            {
                self.block_lexsub_decls.push((level, name));
            }
        }
    }

    /// The name of a declaration a closure may read as a lexical binding: a
    /// `sub` or `multi` with a plain identifier name that no builtin answers
    /// to, not `our`-scoped, exported, `is rw`/`is raw`, or carrying a user
    /// trait or alternate signatures. Anything else keeps the by-name
    /// dispatch. A `multi`'s binding is its family's dispatcher value, which
    /// holds every candidate visible at the block, an outer scope's included
    /// (`sub_value_from_multi_candidates`; ADR-12529 phase 2, slice 6).
    fn plain_lexsub_name(stmt: &Stmt) -> Option<Symbol> {
        let Stmt::SubDecl {
            name,
            name_expr: None,
            is_rw: false,
            is_raw: false,
            is_export: false,
            signature_alternates,
            scope,
            custom_traits,
            ..
        } = stmt
        else {
            return None;
        };
        if !signature_alternates.is_empty()
            || *scope == crate::ast::SubScope::Our
            || custom_traits.iter().any(|(t, _)| !t.starts_with("__"))
        {
            return None;
        }
        let plain = name.with_str(|n| {
            n.starts_with(|c: char| c.is_alphabetic() || c == '_')
                && n.chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''))
                && !crate::runtime::Interpreter::is_builtin_function(n)
        });
        plain.then_some(*name)
    }

    /// Hand a closure body's compiler the block-declared routines visible at
    /// its definition: the enclosing compilations' and this one's active
    /// records, minus a name an active scope here redeclares as something
    /// else (a `multi`, an `our sub`), which shadows the outer binding.
    ///
    /// Cost: O(e + b * r), e = inherited names, b = this compilation's
    /// records, r = its active routine declarations.
    pub(super) fn inherit_block_lexsubs(&self, sub: &mut Compiler) {
        if self.enclosing_lexsubs.is_empty() && self.block_lexsub_decls.is_empty() {
            return;
        }
        let mut names: Vec<Symbol> = self
            .enclosing_lexsubs
            .iter()
            .copied()
            .filter(|n| !self.scope_routine_decls.iter().any(|(_, d)| d == n))
            .collect();
        for &(_, name) in &self.block_lexsub_decls {
            if !names.contains(&name) {
                names.push(name);
            }
        }
        sub.enclosing_lexsubs = names;
    }

    /// Whether a bare `name` read here names a routine an enclosing block
    /// declared (see the module doc): this closure body inherited it and
    /// declares no routine or `&name` binding of that name itself.
    ///
    /// Cost: O(e + r), e = inherited names, r = active routine declarations;
    /// O(1) outside a closure nested in a declaring block.
    pub(super) fn reads_enclosing_lexsub(&self, name: Symbol) -> bool {
        !self.enclosing_lexsubs.is_empty()
            && self.enclosing_lexsubs.contains(&name)
            && !self.scope_routine_decls.iter().any(|(_, d)| *d == name)
            && !name.with_str(|n| self.local_map.contains_key(format!("&{n}").as_str()))
    }

    /// Note that this chunk reads the enclosing block's routine `name`
    /// through its binding (`CompiledCode::lexsub_refs`).
    pub(super) fn note_lexsub_ref(&mut self, name: Symbol) {
        if !self.code.lexsub_refs.contains(&name) {
            self.code.lexsub_refs.push(name);
        }
    }

    /// [`Self::note_lexsub_ref`] for a `&name` read or call when `name` is an
    /// enclosing block's routine.
    ///
    /// Cost: O(1) outside a closure nested in a declaring block, else as
    /// [`Self::reads_enclosing_lexsub`].
    pub(super) fn note_lexsub_code_var(&mut self, name: &str) {
        if self.enclosing_lexsubs.is_empty() {
            return;
        }
        let sym = Symbol::intern(name);
        if self.reads_enclosing_lexsub(sym) {
            self.note_lexsub_ref(sym);
        }
    }

    /// Compile the next whole compilation as a closure body's block (see
    /// `Compiler::top_level_is_block_body`).
    pub(crate) fn compile_as_block_body(&mut self) {
        self.top_level_is_block_body = true;
    }
}
