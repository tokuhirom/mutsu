//! Named phases of `register_class_decl` (ADR-0019 D0): the rollback
//! snapshot, redeclaration/stub checks, and the initial class-shell
//! publication. Parent validation itself lives next door in
//! `registration_class_parents.rs`.

use super::*;

/// Snapshot of the previous registry state for this class, taken under a
/// single read guard (all values are owned/cloned, no re-entry) so a
/// redefinition can be rolled back if the new body fails.
#[derive(Clone)]
pub(crate) struct ClassRegSnapshot {
    prev_class: Option<ClassDef>,
    prev_hidden: bool,
    prev_lexical: bool,
    prev_hidden_defer: Option<rustc_hash::FxHashSet<String>>,
    prev_composed_roles: Option<Vec<String>>,
    prev_role_param_bindings: Option<rustc_hash::FxHashMap<String, Value>>,
    /// ADR-0019 F4c-8(a): dual-write mirror of `prev_class`'s methods
    /// through the mutator API, forward-looking for F4c-9b -- once
    /// `ClassDef::methods` is deleted, `prev_class.clone()` alone can no
    /// longer capture them. Deliberately does NOT capture `MethodEntry::
    /// proto` or `method_wrap_chains` -- see `restore`'s own comment for
    /// why both gaps are preserved on purpose, not silently inherited.
    prev_method_rows: Vec<(crate::symbol::Symbol, Vec<MethodDef>)>,
}

impl ClassRegSnapshot {
    pub(super) fn capture(interp: &Interpreter, name: &str) -> Self {
        let reg = interp.registry();
        Self {
            prev_class: reg.classes.get(name).cloned(),
            prev_hidden: reg.hidden_classes.contains(name),
            prev_lexical: reg.lexical_classes.contains(name),
            prev_hidden_defer: reg.hidden_defer_parents.get(name).cloned(),
            prev_composed_roles: reg.class_composed_roles.get(name).cloned(),
            prev_role_param_bindings: reg.class_role_param_bindings.get(name).cloned(),
            prev_method_rows: reg.user_method_rows_for_owner(crate::symbol::Symbol::intern(name)),
        }
    }

    /// Whether this class already existed when the snapshot was taken. A
    /// rollback of a declaration that created the class from scratch has more
    /// to undo than one that merely replaced an earlier definition — see
    /// `Interpreter::rollback_deferred_trait_class_decl`.
    pub(crate) fn had_previous_class(&self) -> bool {
        self.prev_class.is_some()
    }

    /// Rollback writes are purely registry mutations with no user-code
    /// re-entry, so they take a single write guard for the whole block.
    pub(crate) fn restore(&self, this: &mut Interpreter, name: &str) {
        let mut reg = this.registry_mut();
        if let Some(class_def) = self.prev_class.clone() {
            reg.classes.insert(name.to_string(), class_def);
        } else {
            reg.classes.remove(name);
        }
        if self.prev_hidden {
            reg.hidden_classes.insert(name.to_string());
        } else {
            reg.hidden_classes.remove(name);
        }
        if self.prev_lexical {
            reg.lexical_classes.insert(name.to_string());
        } else {
            reg.lexical_classes.remove(name);
        }
        if let Some(hidden) = self.prev_hidden_defer.clone() {
            reg.hidden_defer_parents.insert(name.to_string(), hidden);
        } else {
            reg.hidden_defer_parents.remove(name);
        }
        if let Some(composed) = self.prev_composed_roles.clone() {
            reg.class_composed_roles.insert(name.to_string(), composed);
        } else {
            reg.class_composed_roles.remove(name);
        }
        if let Some(bindings) = self.prev_role_param_bindings.clone() {
            reg.class_role_param_bindings
                .insert(name.to_string(), bindings);
        } else {
            reg.class_role_param_bindings.remove(name);
        }
        // ADR-0019 F4c-9b: `restore_user_method_rows` is now the sole
        // mechanism restoring the method rows (there is no `ClassDef::
        // methods` left for a `sync_user_method_entries`-style re-derive to
        // read). `restore_user_method_rows` only ever touches
        // `user_candidates`, so `MethodEntry::proto` survives untouched
        // here exactly as pre-existing behavior left it; `method_wrap_
        // chains` is untouched by either path, also matching pre-existing
        // behavior. Both are deliberate, pre-existing gaps -- see the
        // design note's own instruction not to fold that behavior change
        // into this box. The accessor column still needs its own re-derive
        // from the just-restored `prev_class.attributes`, which
        // `sync_user_method_entries` used to also do as its surviving half.
        let owner = crate::symbol::Symbol::intern(name);
        reg.restore_user_method_rows(owner, self.prev_method_rows.clone());
        reg.sync_accessor_entries(owner);
    }
}

impl Interpreter {
    /// Detect X::Redeclaration when a class redefines a role in the same scope.
    /// Only check user-declared roles (not pre-registered builtins like Iterator).
    /// Lexical classes (`my class`) are allowed to shadow outer role names.
    /// `is_stub` is precomputed by the compiler at plan lowering (ADR-0019 D1,
    /// `crate::opcode::is_stub_routine_body`) — a non-stub class body redefining
    /// a role name is always a genuine redeclaration.
    pub(super) fn check_class_role_redeclaration(
        &self,
        name: &str,
        is_lexical: bool,
        is_stub: bool,
    ) -> Result<(), RuntimeError> {
        if !is_lexical && !is_stub && self.registry().user_declared_roles.contains(name) {
            return Err(RuntimeError::redeclaration("symbol", name));
        }
        Ok(())
    }

    /// Build the initial `ClassDef` for the declaration and record the
    /// `is hidden` / `hides` bookkeeping.
    pub(super) fn begin_class_def(
        &mut self,
        name: &str,
        parents: &[String],
        non_inheritance_parents: &HashSet<String>,
        is_hidden: bool,
        hidden_parents: &[String],
    ) -> ClassDef {
        // Drop the parents `validate_class_parents` classified as
        // non-inheritance: a `does`-role sharing the class's own name (composed
        // as a role below; leaving it here would make the class its own
        // ancestor) and a name deferred to `trait_mod:<is>` (a trait, never a
        // parent) — see `non_inheritance_parents`.
        let inheritance_parents: Vec<String> = if non_inheritance_parents.is_empty() {
            parents.to_vec()
        } else {
            parents
                .iter()
                .filter(|p| !non_inheritance_parents.contains(*p))
                .cloned()
                .collect()
        };
        let class_def = ClassDef {
            parents: inheritance_parents,
            attributes: Vec::new(),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            embedded_attributes: HashSet::new(),
            native_methods: HashSet::new(),
            mro: [].into(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: HashMap::new(),
        };
        if is_hidden {
            self.registry_mut().hidden_classes.insert(name.to_string());
        } else {
            self.registry_mut().hidden_classes.remove(name);
        }
        if hidden_parents.is_empty() {
            self.registry_mut().hidden_defer_parents.remove(name);
        } else {
            self.registry_mut()
                .hidden_defer_parents
                .insert(name.to_string(), hidden_parents.iter().cloned().collect());
        }
        class_def
    }

    /// Publish the class shell before the body walk: record `trusts`, clear
    /// stale wrap chains, record `hides` parents and `does`-only roles, and
    /// insert the class so it is visible while its body executes. Returns
    /// `true` when this was a stub registration (the caller returns early).
    pub(super) fn publish_class_shell(
        &mut self,
        name: &str,
        trusts: &[Symbol],
        class_def: &ClassDef,
        hidden_parents: &[String],
        does_parents: &[String],
        is_stub_body: bool,
    ) -> Result<bool, RuntimeError> {
        for trusted_class in trusts {
            let trusted = trusted_class.resolve();
            let mut reg = self.registry_mut();
            let entry = reg.class_trusts.entry(name.to_string()).or_default();
            // Declaration order is observable through `.^trusts`, so append
            // rather than inserting into a set -- but keep it de-duplicated so
            // a re-registered class body (an `augment`, a re-`EVAL`) does not
            // grow the list.
            if !entry.contains(&trusted) {
                entry.push(trusted);
            }
        }
        // Make the class visible while its body executes so introspection calls
        // like `A.^add_method(...)` inside the declaration can resolve `A`.
        // Clear stale method wrap chains from a previous class with the same name.
        self.registry_mut().clear_method_wrap_chains_for_class(name);
        // `class C hides P` marks parent P hidden from C's (and descendants')
        // `.^mro_unhidden`. Record it so the mro_unhidden filter can drop P.
        if !hidden_parents.is_empty() {
            self.registry_mut()
                .hidden_defer_parents
                .entry(name.to_string())
                .or_default()
                .extend(hidden_parents.iter().cloned());
        }
        // Roles composed via `does` (not `is Role` puns) are not MRO entries in
        // Rakudo's `.^mro_unhidden`; record them so the filter can drop them.
        if !does_parents.is_empty() {
            let does_roles: Vec<String> = does_parents
                .iter()
                .filter(|p| {
                    let base = p.split_once('[').map(|(b, _)| b).unwrap_or(p);
                    self.registry().roles.contains_key(base)
                })
                .cloned()
                .collect();
            if !does_roles.is_empty() {
                self.registry_mut()
                    .class_does_only_roles
                    .entry(name.to_string())
                    .or_default()
                    .extend(does_roles);
            }
        }
        // ADR-0019 F4c-9b: no `sync_user_method_entries` needed here anymore
        // -- the registry's method rows for `name` were already brought to
        // a clean, fully-composed state before this call (see
        // `register_class_decl`'s pre-composition clear).
        self.registry_mut()
            .classes
            .insert(name.to_string(), class_def.clone());
        if is_stub_body {
            self.registry_mut().class_stubs.insert(name.to_string());
            self.registry_mut()
                .classes
                .insert(name.to_string(), class_def.clone());
            let mut stack = Vec::new();
            let _ = self.compute_class_mro(name, &mut stack)?;
            return Ok(true);
        }
        // Clear stub status now that the class has a real body (also clears
        // package stub status for `package Foo { ... }; class Foo { }`).
        {
            let mut reg = self.registry_mut();
            reg.class_stubs.remove(name);
            reg.package_stubs.remove(name);
            // A resolved stub is no longer a stub at all, so a future re-use
            // of this name that stubs it again must be free to report its own
            // X::Package::Stubbed error (see `reported_stub_errors`'s doc).
            reg.reported_stub_errors.remove(name);
        }
        Ok(false)
    }
}
