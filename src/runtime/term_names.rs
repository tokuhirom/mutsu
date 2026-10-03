//! Term resolution for sigil-less `constant`s: how the runtime reaches a
//! constant stored under its [`term_key`]. The key construction itself is a
//! pure function of the name and lives below the parser, in
//! [`crate::term_names`] (issue #10779); it is re-exported here.

pub(crate) use crate::term_names::*;

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::Value;

impl Interpreter {
    /// The live `env` value of the sigil-less constant spelled `name`, if one
    /// is in scope. This is how term resolution reaches a constant; a plain
    /// probe under `name` finds a same-named `$`-scalar instead.
    ///
    /// An `our`-scoped constant declared in a block that has since exited is
    /// not found here but by [`Self::term_binding`].
    // Cost: O(1) expected.
    pub(crate) fn term_value(&self, name: &str) -> Option<&Value> {
        if name.is_empty() || name.starts_with(['$', '@', '%', '&', TERM_PREFIX]) {
            return None;
        }
        let name_sym = Symbol::intern(name);
        if crate::qualified::is_qualified(name_sym) {
            return None;
        }
        self.env().get_sym(term_key_sym(name_sym))
    }

    /// [`Self::term_value`], falling back to the running module's own (or
    /// imported) module-scope copy of the constant, to the enclosing package
    /// blocks' lexicals, and to the package store — what a routine sees once
    /// the body that declared the constant has exited. Callers resolving a
    /// bareword that may also name a type check the type first (see
    /// `push_bare_word_value`).
    // Cost: O(1) expected, plus O(p) for the module-scope probe, p = packages the running routine could belong to.
    pub(crate) fn term_binding(&self, name: &str) -> Option<Value> {
        if name.is_empty() || name.starts_with(['$', '@', '%', '&', TERM_PREFIX]) {
            return None;
        }
        self.term_binding_sym(Symbol::intern(name))
    }

    /// [`Self::term_binding`] for a caller that already holds the name's
    /// `Symbol` (and has ruled out a sigiled or term-key name). Interning the
    /// name once and reusing the cached term key keeps a type check that
    /// probes an alias from re-interning and re-formatting the name.
    // Cost: O(1) expected, plus O(p) for the module-scope probe (see above).
    pub(crate) fn term_binding_sym(&self, name_sym: Symbol) -> Option<Value> {
        if crate::qualified::is_qualified(name_sym) {
            return None;
        }
        let key_sym = term_key_sym(name_sym);
        if let Some(v) = self.env().get_sym(key_sym) {
            return Some(v.clone());
        }
        // A definiteness-smiley spelling (`Int:D`) is a type constraint, never
        // a constant's name: no declaration can store `\Int:D`, so the
        // module-scope, package-chain and `our` probes below cannot hit.
        if name_sym.with_str(|n| crate::runtime::types::strip_type_smiley(n).1.is_some()) {
            return None;
        }
        key_sym.with_str(|key| {
            self.module_imported_lexical(key)
                .or_else(|| self.module_scope_lexical(key))
                .cloned()
                // A package block's own `my constant`, kept for its routines in
                // `package_lexicals` once the block has exited.
                .or_else(|| self.package_chain_var_fallback(key))
                // An `our`-scoped constant of a block that has since exited.
                .or_else(|| self.get_our_var(key).cloned())
        })
    }

    /// What a bare `name` written where a TYPE goes is bound to: a sigil-less
    /// constant ([`Self::term_binding`]) first, else a plain `env` binding (an
    /// imported short type name, a `::T` capture). A constant used as a type
    /// alias or a value constraint is found here even when a same-named
    /// `$`-scalar is in scope (#9962).
    // Cost: O(1) expected, plus [`Self::term_binding`]'s module-scope probe on a miss.
    pub(crate) fn type_name_binding(&self, name: &str) -> Option<Value> {
        if name.is_empty() {
            return None;
        }
        let name_sym = Symbol::intern(name);
        if name.starts_with(['$', '@', '%', '&', TERM_PREFIX]) {
            return self.env().get_sym(name_sym).cloned();
        }
        self.term_binding_sym(name_sym)
            .or_else(|| self.env().get_sym(name_sym).cloned())
    }
}
