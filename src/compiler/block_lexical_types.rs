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
}
