//! The interned names a [`MethodDef`](super::MethodDef) dispatch needs.
//!
//! A method's parameter names and its source file never change once the
//! method is declared, yet the compiled-method fast path used to hand each of
//! them to [`Symbol::intern`] on every call — a thread-local string-hash probe
//! per name per call (#10961). [`MethodDefSyms`] interns them once, on the
//! first dispatch, and every later call (the def lives behind an `Arc` in the
//! method resolve caches) reads the stored `Symbol`s.
//!
//! The cache is filled lazily rather than at each of the many `MethodDef`
//! construction sites, so a site cannot forget it. That relies on the cached
//! fields never being reassigned after construction — nothing does today; a
//! new mutation of `params`, `param_defs[..].name` or `source_file` on an
//! existing def must build a fresh `MethodDefSyms::default()` alongside it.

use crate::symbol::Symbol;
use std::sync::OnceLock;

#[derive(Debug, Clone, Default)]
pub(crate) struct MethodDefSyms {
    params: OnceLock<Box<[Symbol]>>,
    param_def_names: OnceLock<Box<[Symbol]>>,
    source_file: OnceLock<Option<Symbol>>,
}

impl super::MethodDef {
    /// `params[i]`, interned.
    // Cost: O(1) after the first call; O(p) once, p = number of parameters.
    #[inline]
    pub(crate) fn param_syms(&self) -> &[Symbol] {
        self.syms
            .params
            .get_or_init(|| self.params.iter().map(|n| Symbol::intern(n)).collect())
    }

    /// `param_defs[i].name`, interned.
    // Cost: O(1) after the first call; O(p) once, p = number of parameters.
    #[inline]
    pub(crate) fn param_def_name_syms(&self) -> &[Symbol] {
        self.syms.param_def_names.get_or_init(|| {
            self.param_defs
                .iter()
                .map(|pd| Symbol::intern(&pd.name))
                .collect()
        })
    }

    /// `source_file`, interned.
    // Cost: O(1) after the first call.
    #[inline]
    pub(crate) fn source_file_sym(&self) -> Option<Symbol> {
        *self
            .syms
            .source_file
            .get_or_init(|| self.source_file.as_deref().map(Symbol::intern))
    }
}
