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
            // A term the running routine's own compunit imported at its top
            // level (`use M` binding `constant C`) was moved into the
            // compunit's lexicals when the load ended; a frame that is not a
            // routine call -- a class attribute's default -- finds it there.
            self.unit_lexical_slot(key, Some(key_sym))
                .map(|v| v.deref_container())
                .or_else(|| {
                    // A core type name (`Bool`, `Str`) asked on every type-check
                    // memo hit is bound in none of the stores below; their
                    // name filters say so without walking the package chain
                    // (#18274).
                    if !self.term_key_may_be_bound_outside_env(key, key_sym) {
                        debug_assert!(
                            self.term_key_stores_lookup(key, key_sym).is_none(),
                            "the term-key filter missed {key:?}"
                        );
                        return None;
                    }
                    self.term_key_stores_lookup(key, key_sym)
                })
        })
    }

    /// The term stores past `env` and the unit lexicals, in resolution order.
    // Cost: O(p) for the module-scope and package-chain probes, p = packages
    // the running routine could belong to.
    fn term_key_stores_lookup(&self, key: &str, key_sym: Symbol) -> Option<Value> {
        self.module_imported_lexical(key)
            .or_else(|| self.module_scope_lexical(key))
            // A package block's own `my constant`, kept for its routines in
            // `package_lexicals` once the block has exited.
            .or_else(|| self.package_chain_var_fallback(key_sym))
            // An `our`-scoped constant of a block that has since exited.
            .or_else(|| self.get_our_var(key))
    }

    /// Whether any store [`Self::term_key_stores_lookup`] asks may hold the
    /// term key `key`, from the stores' own name filters: the module-scope
    /// table's key union (it also decides `module_imported_lexical`), the
    /// package-lexical table's key union (the package chain's
    /// `package_lexicals` probe), and the `our` table's key set and its
    /// append-only index of unqualified parts (the package chain's
    /// `Pkg::\X` probes and the bare `\X` probe share the unqualified part
    /// `\X`). The package chain's env probe of `Pkg::\X` is not filtered:
    /// nothing binds a package-qualified term key in an env, which the
    /// debug assertion in `package_chain_var_fallback` checks. `false` is a
    /// definitive miss.
    // Cost: four hash probes.
    fn term_key_may_be_bound_outside_env(&self, key: &str, key_sym: Symbol) -> bool {
        if self
            .module
            .world
            .module_scope_lexicals
            .read()
            .contains_name(key)
            || self.lexicals.package_lexicals.read().contains_name(key)
        {
            return true;
        }
        let our = self.our_table();
        our.contains_key(key) || our.has_unqualified(crate::qualified::unqualified_part(key_sym))
    }

    /// What a bare `name` written where a TYPE goes is bound to: a sigil-less
    /// constant ([`Self::term_binding`]) first, else a plain `env` binding (an
    /// imported short type name, a `::T` capture). A constant used as a type
    /// alias or a value constraint is found here even when a same-named
    /// `$`-scalar is in scope (#9962). A lexical type's storage key
    /// (`Name\u{0}<decl-id>`) is never a term's name, so only `env` is asked
    /// for it: the term probes' miss walked the package chain on every type
    /// check against a module's `my class` (#15786).
    // Cost: O(1) expected, plus [`Self::term_binding`]'s module-scope probe on a miss.
    pub(crate) fn type_name_binding(&self, name: &str) -> Option<Value> {
        if name.is_empty() {
            return None;
        }
        let name_sym = Symbol::intern(name);
        if name.starts_with(['$', '@', '%', '&', TERM_PREFIX]) || is_type_storage_key(name) {
            return self.env().get_sym(name_sym).cloned();
        }
        self.term_binding_sym(name_sym)
            .or_else(|| self.env().get_sym(name_sym).cloned())
    }

    /// Whether the bare `name` written where a type goes is a `constant` term
    /// bound to a definite value (`constant G = Point.new(..); multi f(G)`).
    /// The term shadows a same-named type declared elsewhere (a `class G` in
    /// the importing script), as the lexical scope the signature was written
    /// in decides in Rakudo.
    // Cost: O(1) expected, see `term_binding_sym`.
    pub(crate) fn names_value_constant(&self, name: &str) -> bool {
        !crate::runtime::utils::is_known_type_constraint(name)
            && self
                .type_name_binding(name)
                .is_some_and(|v| !matches!(v.view(), crate::value::ValueView::Package(_)))
    }

    /// A lexical declaration's type identity, independent of a same-named scalar.
    // Cost: O(1) expected after the type-namespace key's first memoized lookup.
    pub(crate) fn lexical_type_binding_sym(&self, name: Symbol) -> Option<Value> {
        if crate::qualified::is_qualified(name) {
            return None;
        }
        let key = lexical_type_key_sym(name);
        self.env()
            .get_lexical_type_binding(key, name)
            .cloned()
            .or_else(|| self.package_chain_var_fallback(key))
    }
}

/// Whether `name` is a type's registry storage key (`Name\u{0}<decl-id>`, a
/// lexical type's NUL-mangled name, possibly nested) rather than a spelling
/// a program can write. The term stores are keyed by source spellings, and a
/// NUL cannot appear in a source identifier (the same fact
/// `types/role_candidate.rs` relies on), so such a name is never a
/// constant's or a term's: the declaring scope binds the *bare* name to the
/// storage key's type object. A name that starts with the NUL (an internal
/// env slot such as a regex's code-ran flag) is not a type key.
// Cost: O(n), n = the name's length.
fn is_type_storage_key(name: &str) -> bool {
    !name.starts_with('\u{0}') && name.contains('\u{0}')
}

#[cfg(test)]
mod tests {
    use super::is_type_storage_key;

    #[test]
    fn type_storage_keys_are_recognized() {
        assert!(is_type_storage_key("NotFound\u{0}1288262438"));
        assert!(is_type_storage_key("Store\u{0}1::Session\u{0}2"));
        assert!(!is_type_storage_key("NotFound"));
        assert!(!is_type_storage_key("\u{0}code-ran"));
        assert!(!is_type_storage_key(""));
    }
}
