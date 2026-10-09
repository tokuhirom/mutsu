//! Re-binding a package body's lexicals for the body's run-time part
//! (ADR-0134, #10332).
//!
//! The BEGIN prologue registers a class or package with the BEGIN-time part
//! of its body. Its `my` lexicals then live in the package's static store
//! (`package_lexicals`), where its methods and subs read them. The body's
//! run-time part runs later, at the declaration's source position, in an
//! `OpCode::PackageScope` that names those lexicals. They are bound from the
//! store for the duration of the body, so a same-named outer lexical is
//! shadowed as it is in the source, and written back to it afterwards.
//!
//! Outside the body, a routine of the package reaches such a `my @a` / `my %h`
//! only through the store (`package_scope_lexical`), so an in-place mutation
//! from the routine has to land in the store too, not in a copy under the
//! name in the routine's own env (#10343).

use super::*;
use crate::compiler::CLASS_LEXICAL;

impl Interpreter {
    /// Bind each of `lexicals` (newline-joined `VarDecl` names) from
    /// `package`'s static store into the env. `$?CLASS` (named for a class
    /// body) is bound to the class itself.
    // Cost: O(k), k = lexicals named.
    pub(super) fn bind_package_body_lexicals(&mut self, package: &str, lexicals: &str) {
        let store = self.lexicals.package_lexicals.get(package);
        let values: Vec<(&str, Value)> = lexicals
            .split('\n')
            .filter_map(|name| {
                if name == CLASS_LEXICAL {
                    return Some((name, Value::package(Symbol::intern(package))));
                }
                store?.get(name).map(|value| (name, value.clone()))
            })
            .collect();
        for (name, value) in values {
            self.env_mut().insert(name.to_string(), value);
        }
    }

    /// Write each of `lexicals` the store holds back to it, then put back
    /// whatever the enclosing scope bound under the name.
    // Cost: O(k), k = lexicals named.
    pub(super) fn unbind_package_body_lexicals(
        &mut self,
        package: &str,
        lexicals: &str,
        saved_env: &crate::env::Env,
    ) {
        for name in lexicals.split('\n') {
            // Only a name the store holds was bound from it; anything else
            // under the name belongs to the enclosing scope.
            if self
                .lexicals
                .package_lexicals
                .get(package)
                .is_some_and(|store| store.contains_key(name))
                && let Some(value) = self.env().get(name).cloned()
            {
                self.package_lexicals_cow_mut()
                    .entry(package.to_string())
                    .or_default()
                    .insert(name.to_string(), value);
            }
            match saved_env.get(name) {
                Some(previous) => {
                    let previous = previous.clone();
                    self.env_mut().insert(name.to_string(), previous);
                }
                None => {
                    self.env_mut().remove(name);
                }
            }
        }
    }

    /// Rebind companion of [`Self::writeback_package_scope_var`]: a `:=` of the
    /// package-block `my` lexical `name` to a value installs a fresh container
    /// in the package store instead of writing through the cell that every
    /// name `:=`-bound to the old binding shares (#12129). Reports `false`,
    /// touching nothing, when the current package's store has no such entry.
    // Cost: O(1) beyond `rebound_container`'s; the table is copied (O(p)) only
    // while a thread clone still shares it.
    pub(super) fn package_scope_lexical_rebind(
        &mut self,
        name: &str,
        val: &Value,
        source_kind: Option<crate::ast::ReadonlyKind>,
    ) -> bool {
        if self.lexicals.package_lexicals.is_empty() {
            return false;
        }
        let cur_sym = self.current_package_sym();
        let cur: &str = cur_sym.as_str();
        if !self
            .lexicals
            .package_lexicals
            .get(cur)
            .is_some_and(|m| m.contains_key(name))
        {
            return false;
        }
        let scalar = !name.starts_with(['@', '%', '&']);
        let (container, decision) = Self::rebound_container(val.clone(), scalar, source_kind);
        if scalar && let ValueView::ContainerRef(fresh) = container.view() {
            fresh.set_binding_decision(decision);
        }
        let Some(slot) = crate::runtime::cow_table_mut(&mut self.lexicals.package_lexicals)
            .get_value_mut(cur, name)
        else {
            return false;
        };
        *slot = container.clone();
        // A live `@`/`%` binding in `env` is read ahead of the store; keep it in
        // step so a stale entry does not keep resolving to the old container.
        if self.env().contains_key(name) {
            self.env_mut().insert(name.to_string(), container);
        }
        // TRIR's free-variable cache may hold the replaced container.
        self.lexicals.unit_lexical_gen = self.lexicals.unit_lexical_gen.wrapping_add(1);
        true
    }

    /// The stored root of the package-block `@`/`%` lexical `name` resolves
    /// to through [`Self::package_scope_lexical`], for a write chokepoint
    /// that mutates the container in place (`env_root_descended_mut`). A
    /// routine of the block reaches the lexical only through the store, so a
    /// mutation made anywhere else (the routine's own env) is a copy no later
    /// read consults (#10343).
    // Cost: O(1) beyond `package_scope_lexical_key`'s; the table is copied
    // (O(p), p = packages) only while a thread clone still shares it.
    pub(crate) fn package_scope_lexical_root_mut(&mut self, name: &str) -> Option<&mut Value> {
        if !name.starts_with(['@', '%']) {
            return None;
        }
        let (cur, key) = self.package_scope_lexical_key(name)?;
        let key = key.into_owned();
        if !self
            .lexicals
            .package_lexicals
            .get(cur)
            .is_some_and(|m| m.contains_key(key.as_str()))
        {
            return None;
        }
        // `get_value_mut`: only the entry's value is reached, so the table's
        // name filter stays valid.
        crate::runtime::cow_table_mut(&mut self.lexicals.package_lexicals).get_value_mut(cur, &key)
    }

    /// Run the element store `store` on the package-block `@`/`%` lexical
    /// `name` when that is what a frame with no binding of its own for `name`
    /// resolves it to: the env-centric store paths find the container under
    /// the name for the duration, and whatever they leave there is the
    /// store's new value. Anything else runs `store` unchanged.
    // Cost: O(1) around `store` (one env probe, one store lookup and, when the
    // store ran against a package lexical, one store write).
    pub(super) fn with_package_lexical_seeded<T>(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        store: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let name = Self::const_str(code, name_idx);
        let sym = code.const_sym(name_idx);
        let seed = if name.starts_with(['@', '%'])
            && !self.lexicals.package_lexicals.is_empty()
            && code.local_slots_of(sym).is_empty()
            && !self.env().contains_key_sym(sym)
        {
            self.package_scope_lexical(name)
        } else {
            None
        };
        let Some(seed) = seed else {
            return store(self);
        };
        self.env_mut().insert_sym(sym, seed);
        let result = store(self);
        if let Some(stored) = self.env_mut().remove_sym(sym)
            && let Some(root) = self.package_scope_lexical_root_mut(name)
        {
            *root = stored;
            // TRIR's free-variable cache may hold the replaced value.
            self.lexicals.unit_lexical_gen = self.lexicals.unit_lexical_gen.wrapping_add(1);
        }
        result
    }
}
