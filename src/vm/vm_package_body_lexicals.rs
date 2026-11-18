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
    pub(super) fn bind_package_body_lexicals(
        &mut self,
        package: &str,
        lexicals: &str,
    ) -> Option<usize> {
        let package_lexicals = self.lexicals.package_lexicals.read();
        let store = package_lexicals.get(package);
        let values: Vec<(&str, Value)> = lexicals
            .split('\n')
            .filter_map(|name| {
                if name == CLASS_LEXICAL {
                    return None;
                }
                store?.get(name).map(|value| (name, value.clone()))
            })
            .collect();
        drop(package_lexicals);
        for (name, value) in values {
            self.env_mut().insert(name.to_string(), value);
        }
        // `$?CLASS` is the class's frame record while the body runs
        // (ADR-14843); the caller closes the scope.
        lexicals
            .split('\n')
            .any(|name| name == CLASS_LEXICAL)
            .then(|| self.begin_class_scope(Symbol::intern(package)))
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
            if name == CLASS_LEXICAL {
                continue;
            }
            // Only a name the store holds was bound from it; anything else
            // under the name belongs to the enclosing scope.
            if self
                .lexicals
                .package_lexicals.read()
                .get(package)
                .is_some_and(|store| store.contains_key(name))
                && let Some(value) = self.env().get(name).cloned()
            {
                self.package_lexicals_write(|table| {
                    table
                        .entry(package.to_string())
                        .or_default()
                        .insert(name.to_string(), value)
                });
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
        if self.lexicals.package_lexicals.read().is_empty() {
            return false;
        }
        let cur_sym = self.current_package_sym();
        let cur: &str = cur_sym.as_str();
        if !self
            .lexicals
            .package_lexicals.read()
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
        let rebound = self.lexicals.package_lexicals.write(|table| {
            table
                .get_value_mut(cur, name)
                .map(|slot| *slot = container.clone())
                .is_some()
        });
        if !rebound {
            return false;
        }
        // A live `@`/`%` binding in `env` is read ahead of the store; keep it in
        // step so a stale entry does not keep resolving to the old container.
        if self.env().contains_key(name) {
            self.env_mut().insert(name.to_string(), container);
        }
        // TRIR's free-variable cache may hold the replaced container.
        self.lexicals.unit_lexical_gen = self.lexicals.unit_lexical_gen.wrapping_add(1);
        true
    }

    /// The cell holding the package-block `@`/`%` lexical `name` resolves to
    /// through [`Self::package_scope_lexical`], for a write chokepoint that
    /// mutates the container in place (`env_root_descended_mut`). A routine of
    /// the block reaches the lexical only through the store, so a mutation
    /// made anywhere else (the routine's own env) is a copy no later read
    /// consults (#10343).
    ///
    /// The store is a world table shared by every thread (ADR-17216 slice
    /// 4a), so no reference into it may outlive its lock: an entry that is
    /// not a cell yet is boxed into one, under the write lock, and the
    /// mutation goes through the cell. The caller holds the returned cell
    /// for as long as it mutates through it (another thread's rebind may
    /// drop it from the table meanwhile).
    // Cost: O(1) beyond `package_scope_lexical_key`'s; one table write the
    // first time an entry is boxed (the copy-on-write clone, O(n), n =
    // entries, only while another thread holds a snapshot).
    pub(crate) fn package_scope_lexical_cell(&mut self, name: &str) -> Option<Value> {
        if !name.starts_with(['@', '%']) {
            return None;
        }
        let (cur, key) = self.package_scope_lexical_key(name)?;
        let key = key.into_owned();
        let entry = self
            .lexicals
            .package_lexicals
            .read()
            .get(cur)
            .and_then(|m| m.get(key.as_str()).cloned())?;
        if entry.is_container_ref() {
            return Some(entry);
        }
        // `get_value_mut`: only the entry's value is reached, so the table's
        // name filter stays valid.
        self.package_lexicals_write(|table| {
            let slot = table.get_value_mut(cur, &key)?;
            if !slot.is_container_ref() {
                *slot = slot.clone().into_container_ref();
            }
            Some(slot.clone())
        })
    }

    /// Store `value` as the package-block `@`/`%` lexical `name` resolves to
    /// (the entry's cell when it is boxed, else the entry itself).
    // Cost: O(1) beyond `package_scope_lexical_key`'s, plus one table write
    // for an unboxed entry.
    fn store_package_scope_lexical(&mut self, name: &str, value: Value) -> bool {
        let Some((cur, key)) = self.package_scope_lexical_key(name) else {
            return false;
        };
        let key = key.into_owned();
        let cell = self
            .lexicals
            .package_lexicals
            .read()
            .get(cur)
            .and_then(|m| m.get(key.as_str()))
            .and_then(|slot| match slot.view() {
                ValueView::ContainerRef(cell) => Some(cell.clone()),
                _ => None,
            });
        match cell {
            Some(cell) => {
                // The seeded value was the cell itself: the store already
                // wrote through it.
                if let ValueView::ContainerRef(stored) = value.view()
                    && crate::gc::Gc::ptr_eq(&stored, &cell)
                {
                    return true;
                }
                cell.lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .clone_from(&value);
                true
            }
            None => self.package_lexicals_write(|table| match table.get_value_mut(cur, &key) {
                Some(slot) => {
                    *slot = value;
                    true
                }
                None => false,
            }),
        }
    }

    /// Run the element store `store` on the package-block `@`/`%` lexical
    /// `name` when that is what a frame with no binding of its own for `name`
    /// resolves it to: the env-centric store paths find the container under
    /// the name for the duration, and whatever they leave there is the
    /// store's new value. Anything else runs `store` unchanged.
    ///
    /// Still needed for the `Named` shape only: a package-block lexical lives in
    /// the package store, not in a local slot of the running code, so no
    /// `StorePlace::Slot` ever reaches it (ADR-14632).
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
            && !self.lexicals.package_lexicals.read().is_empty()
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
        if let Some(stored) = self.env_mut().remove_sym(sym) {
            self.store_package_scope_lexical(name, stored);
            // TRIR's free-variable cache may hold the replaced value.
            self.lexicals.unit_lexical_gen = self.lexicals.unit_lexical_gen.wrapping_add(1);
        }
        result
    }
}
