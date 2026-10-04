//! Writes to an `@`/`%` variable addressed through a package stash
//! (`@GLOBAL::a`, `%P::h`), whose canonical store is `our_vars`.
//!
//! A slot nobody declared with `our` is, in Rakudo, an auto-created **Scalar**:
//! `@GLOBAL::u = 1, 2` item-assigns the List (`.raku` is `$(1, 2)`), and an
//! element write vivifies an itemized container in it (`$[1]`, `${:a(1)}`).
//! A declared `our @a` / `our %h` keeps its plain container and ordinary list
//! assignment. Element writes run in the running frame's env, which dies with
//! the frame, so they are bracketed by a prologue that loads the stored
//! container and an epilogue that persists a replaced one (#10901, #11000).

use super::*;

impl Interpreter {
    /// The `our_vars` key a package-qualified name persists under: the name
    /// itself, unless only its pseudo-package-stripped spelling is stored (a
    /// top-level `our %h` reached as `%GLOBAL::h`), or its qualifier is a
    /// constant naming another package. An explicitly stored spelling wins.
    fn package_container_key(&self, name: &str) -> String {
        if self.get_our_var(name).is_none() {
            if let Some(bare) = Self::pseudo_package_unqualified_name(name)
                && self.get_our_var(&bare).is_some()
            {
                return bare;
            }
            if let Some(real) = self.package_alias_var_name(name) {
                return real;
            }
        }
        name.to_string()
    }

    /// The name a whole-container store to `name` lands on: the bare
    /// spelling for a pseudo-package name, or the real package spelling for
    /// a constant package alias, else `name`.
    // Cost: O(n) for a qualified name, n = the length of `name`; O(1) otherwise.
    pub(crate) fn package_container_store_name(&self, name_sym: Symbol) -> String {
        let name = name_sym.as_str();
        if crate::qualified::is_package_array(name_sym)
            || crate::qualified::is_package_hash(name_sym)
        {
            return self.package_container_key(name);
        }
        name.to_string()
    }

    /// Whether `val` is the plain container a declared `our @a` / `our %h`
    /// holds, as opposed to the content of an auto-created Scalar slot.
    fn is_declared_package_container(val: &Value, positional: bool) -> bool {
        match val.view() {
            ValueView::Array(_, kind) => positional && !kind.is_itemized(),
            ValueView::Hash(_) => !positional && !val.hash_is_itemized(),
            _ => false,
        }
    }

    /// `@PKG::a = RHS` / `%PKG::h = RHS` on a slot no `our` declared: store
    /// RHS itemized, as rakudo's item assignment into the auto-created Scalar
    /// does, instead of list-assigning it into a fresh Array/Hash. Pops the
    /// RHS and answers `true` when it handled the store; a declared slot (or a
    /// `PROCESS::` dynamic) is left to the ordinary list assignment. The
    /// `our @a = ...` declaration itself publishes through this same store
    /// before the slot exists, which `code.our_locals` identifies.
    // Cost: O(o + n), o = `our` declarations of the running chunk, n = the
    // length of a qualified name; O(1) for an unqualified name.
    pub(crate) fn package_container_item_assign(
        &mut self,
        code: &CompiledCode,
        name_sym: Symbol,
    ) -> bool {
        let positional = crate::qualified::is_package_array(name_sym);
        if !positional && !crate::qualified::is_package_hash(name_sym) {
            return false;
        }
        let name = name_sym.as_str();
        if code
            .our_locals
            .iter()
            .any(|(_, qualified)| qualified == name)
        {
            return false;
        }
        let bare = Self::pseudo_package_unqualified_name(name);
        if bare.as_deref().is_some_and(|b| b[1..].starts_with('*')) {
            return false;
        }
        let key = self.package_container_key(name);
        let existing = self.get_our_var(&key).or_else(|| self.env().get(name));
        if existing.is_some_and(|v| Self::is_declared_package_container(v, positional)) {
            return false;
        }
        let rhs = self.stack.pop().unwrap_or(Value::NIL);
        let stored = Self::itemize_scalar_store_value(rhs);
        self.env_mut().insert(name.to_string(), stored.clone());
        self.set_our_var(key, stored);
        true
    }

    /// Before an element write to a package-qualified container
    /// (`%GLOBAL::h<a>++`, `@P::a[0] = v`): make the running frame's env hold
    /// the persisted container, so the op mutates it rather than a frame-local
    /// one. An empty slot is vivified as rakudo's auto-created Scalar holding a
    /// fresh container (`${...}` / `$[...]`); the element ops themselves only
    /// vivify a declared container. Returns the container the op starts from,
    /// for [`Self::package_container_elem_epilogue`].
    // Cost: O(n) for a qualified name, n = the length of `name`; O(1) otherwise.
    pub(crate) fn package_container_elem_prologue(&mut self, name_sym: Symbol) -> Option<Value> {
        let positional = crate::qualified::is_package_array(name_sym);
        let name = name_sym.as_str();
        let key = self.package_container_key(name);
        let env_val = self.env().get(name).cloned();
        let stored = match self.get_our_var(&key).cloned() {
            Some(stored) => stored,
            None => {
                let fresh = env_val
                    .clone()
                    .filter(|v| match v.view() {
                        ValueView::Array(..) => positional,
                        ValueView::Hash(_) => !positional,
                        _ => false,
                    })
                    .unwrap_or_else(|| {
                        if positional {
                            Self::itemize_scalar_store_value(Value::real_array(Vec::new()))
                        } else {
                            Value::hash(crate::value::ValueMap::default()).with_hash_itemized(true)
                        }
                    });
                self.set_our_var(key, fresh.clone());
                fresh
            }
        };
        if !env_val.is_some_and(|v| Self::same_container_arc(&v, &stored)) {
            self.env_mut().insert(name.to_string(), stored.clone());
        }
        Some(stored)
    }

    /// After the element write: persist env's container into `our_vars` unless
    /// the op mutated the prologue's container in place.
    // Cost: O(n) for a qualified name, n = the length of `name`; O(1) otherwise.
    pub(crate) fn package_container_elem_epilogue(&mut self, name_sym: Symbol, pre: Option<Value>) {
        let name = name_sym.as_str();
        let Some(val) = self.env().get(name).cloned() else {
            return;
        };
        if !matches!(val.view(), ValueView::Hash(_) | ValueView::Array(..))
            || pre.is_some_and(|pre| Self::same_container_arc(&pre, &val))
        {
            return;
        }
        let key = self.package_container_key(name);
        self.set_our_var(key, val);
    }
}
