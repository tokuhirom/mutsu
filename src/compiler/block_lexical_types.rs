//! Block-declared lexical types read from a closure body (#17711).
//!
//! A `my class Inner` / `my role Inner` binds its storage name in the
//! declaring scope's env, under the bare name and under its type-only key
//! (`term_names::lexical_type_key`). A bare `Inner` compiles to
//! `GetBareWord`, which resolves the name in the env that is live when it
//! runs. For the declaring block itself that is right. A closure written in
//! that block, however, may run after the block has exited, when its binding
//! is gone from the live env and the name falls through to a same-named
//! package type -- the closure answered the outer `class Inner` where Rakudo
//! resolves the name lexically, at the place the closure was written.
//!
//! So a closure body reads an enclosing block's lexical type the way it reads
//! an enclosing `my \x`: as a by-name read of the type-only key.
//! `GetGlobal` is in `op_name_const_idx`, so `compute_free_vars` records the
//! key as a free variable and the closure captures the binding of the frame
//! that created it.
//!
//! Only declarations below a unit's top scope are recorded. A file-scope
//! `my class` is visible to every routine of the unit through the unit's
//! scope, which the existing bareword resolution already reaches, including
//! from a routine called from another compilation unit, where a by-name read
//! of the creating frame's env would not find it.

use super::Compiler;
use crate::symbol::Symbol;

impl Compiler {
    /// Record a lexical type declared in the scope being compiled, unless that
    /// scope is the unit's top scope (see the module doc).
    ///
    /// Cost: O(b), b = lexical types recorded in the active scopes (a short list).
    pub(super) fn note_block_lexical_type(&mut self, name: Symbol) {
        let at_unit_top = !self.in_lexical_scope
            && self.local_scopes.len().checked_sub(1) == Some(self.unit_root_scope);
        if at_unit_top || name.with_str(crate::qualified::is_qualified_str) {
            return;
        }
        let level = self.local_scopes.len();
        if !self.block_lexical_type_decls.contains(&(level, name)) {
            self.block_lexical_type_decls.push((level, name));
        }
    }

    /// Hand a closure body's compiler the block-declared lexical types visible
    /// at its definition: the enclosing compilations' and this one's active
    /// records.
    ///
    /// Cost: O(e + b), e = inherited names, b = this compilation's records.
    pub(super) fn inherit_block_lexical_types(&self, sub: &mut Compiler) {
        if self.enclosing_lexical_types.is_empty() && self.block_lexical_type_decls.is_empty() {
            return;
        }
        let mut names = self.enclosing_lexical_types.clone();
        for &(_, name) in &self.block_lexical_type_decls {
            if !names.contains(&name) {
                names.push(name);
            }
        }
        sub.enclosing_lexical_types = names;
    }

    /// Whether a bare `name` read here names a lexical type an enclosing
    /// block declared: this closure body inherited it and binds nothing of
    /// that spelling itself (its own lexical type, a sigilless term, a local
    /// or a type capture).
    ///
    /// Cost: O(e + b), e = inherited names, b = this compilation's records;
    /// O(1) outside a closure nested in a declaring block.
    pub(super) fn reads_enclosing_lexical_type(&self, name: &str) -> bool {
        if self.enclosing_lexical_types.is_empty() {
            return false;
        }
        let sym = Symbol::intern(name);
        self.enclosing_lexical_types.contains(&sym)
            && !self.block_lexical_type_decls.iter().any(|&(_, d)| d == sym)
            && !self.sigilless_locals.contains(name)
            && !self.local_map.contains_key(name)
            && !self.type_capture_locals.contains(name)
    }

    /// Record a lexical type's env bindings -- its bare name and its
    /// type-only key -- as declarations of the scope-isolating block being
    /// compiled, so the block's exit reverts them like a `my $x` (#18178: a
    /// `do { my class Inner }` left `Inner` bound to the block's class).
    ///
    /// Cost: O(1).
    pub(super) fn record_block_lexical_type_decl(&mut self, name: Symbol) {
        if self.block_decl_tracker.is_empty() {
            return;
        }
        self.record_block_decl(&name.resolve());
        self.record_block_decl(&crate::term_names::lexical_type_key_sym(name).resolve());
    }

    /// Record a `class`/`role` declaration in the scope being compiled, by
    /// the name the registry will hold it under, so an `OUTER::` lookup of
    /// its spelling from a nested scope resolves to this very type (#18213).
    /// Only a declaration in the `GLOBAL` package is recorded: elsewhere the
    /// registry name also depends on the package the declaration runs in.
    ///
    /// Cost: O(|name|).
    pub(super) fn record_scope_type_decl(&mut self, stmt: &crate::ast::Stmt) {
        let (name, is_lexical, decl_id) = match stmt {
            crate::ast::Stmt::ClassDecl {
                name,
                is_lexical,
                decl_id,
                ..
            }
            | crate::ast::Stmt::RoleDecl {
                name,
                is_lexical,
                decl_id,
                ..
            } => (*name, *is_lexical, *decl_id),
            _ => return,
        };
        if self.runtime_current_package() != "GLOBAL" || self.in_unit_package {
            return;
        }
        let spelled = name.resolve();
        if spelled.is_empty() || spelled.contains("::") || spelled.starts_with("__") {
            return;
        }
        let registered = if is_lexical {
            if decl_id == 0 {
                return;
            }
            format!("{spelled}\u{0}{decl_id}")
        } else {
            spelled.clone()
        };
        if let Some(frame) = self.local_scopes.last_mut() {
            frame.record_type(&spelled, registered);
        }
    }

    /// The type an `OUTER::` lookup of `bare` at `depth` names, when the
    /// scope it reaches declares one of that spelling.
    ///
    /// Cost: O(s), s = scope frames (the chain is cloned).
    pub(super) fn outer_declared_type(&self, bare: &str, depth: usize) -> Option<String> {
        if bare.starts_with(['$', '@', '%', '&', '\\']) {
            return None;
        }
        let chain = self.full_scope_chain();
        let at = chain.len().checked_sub(1 + depth)?;
        chain[at].declared_type(bare).map(str::to_string)
    }

    /// `OUTER::OUTER::Name` split into `Name` and the number of `OUTER::`
    /// steps; `None` for any other bareword.
    ///
    /// Cost: O(|name|).
    pub(super) fn outer_qualified_bareword(name: &str) -> Option<(&str, usize)> {
        let mut rest = name;
        let mut depth = 0;
        while let Some(after) = rest.strip_prefix("OUTER::") {
            rest = after;
            depth += 1;
        }
        (depth > 0 && !rest.is_empty() && !rest.contains("::")).then_some((rest, depth))
    }
}
