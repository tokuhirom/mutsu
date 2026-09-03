//! Named phases of `register_class_decl` (ADR-0019 D0): the rollback
//! snapshot, redeclaration/stub checks, parent validation, and the initial
//! class-shell publication. Pure mechanical extraction from
//! `registration_class_decl.rs` — no behavior change.

use super::registration_class::is_non_composable_builtin;
use super::registration_class_decl::BUILTIN_PARENT_TYPES;
use super::*;

/// Short (unqualified) name of the class being declared, for detecting a
/// `does`-role that shares the class's own name (see below).
fn short_of(s: &str) -> &str {
    s.rsplit("::").next().unwrap_or(s)
}

/// Snapshot of the previous registry state for this class, taken under a
/// single read guard (all values are owned/cloned, no re-entry) so a
/// redefinition can be rolled back if the new body fails.
pub(super) struct ClassRegSnapshot {
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

    /// Rollback writes are purely registry mutations with no user-code
    /// re-entry, so they take a single write guard for the whole block.
    pub(super) fn restore(&self, this: &mut Interpreter, name: &str) {
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

    /// `X::Inheritance::UnknownParent`: `name` (the class being declared)
    /// gave `parent_name` as an `is` parent that names no known class, role,
    /// enum, or builtin type. Shared by the immediate check below and by the
    /// deferred-custom-trait dispatch (`vm_typedecl_ops.rs`) for when a
    /// lowercase parent name was optimistically deferred to a user
    /// `trait_mod:<is>` candidate that turns out not to match this call's
    /// shape after all (mirrors the sibling variable-/attribute-trait
    /// no-candidate fallback).
    pub(crate) fn unknown_parent_error(&self, name: &str, parent_name: &str) -> RuntimeError {
        // `name` may be a lexical class's mangled storage name (ADR-0047 P1:
        // `Foo\u{0}<decl-id>`) — show the user-facing bare name in the message
        // and every `child*` attribute.
        let name = crate::value::user_facing_type_name(name);
        // Suggest close known type names (Did-you-mean).
        let suggestions = self.suggest_type_names(parent_name);
        let mut msg = format!(
            "'{}' cannot inherit from '{}' because it is unknown.",
            name, parent_name
        );
        if suggestions.len() == 1 {
            msg.push_str(&format!("\nDid you mean '{}'?", suggestions[0]));
        } else if suggestions.len() > 1 {
            msg.push_str("\nDid you mean one of these?\n");
            for s in &suggestions {
                msg.push_str(&format!("    '{}'\n", s));
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert("child-name".to_string(), Value::str(name.to_string()));
        attrs.insert("child".to_string(), Value::str(name.to_string()));
        attrs.insert(
            "parent-name".to_string(),
            Value::str(parent_name.to_string()),
        );
        attrs.insert("parent".to_string(), Value::str(parent_name.to_string()));
        attrs.insert(
            "suggestions".to_string(),
            Value::array(suggestions.into_iter().map(Value::str).collect()),
        );
        attrs.insert("message".to_string(), Value::str(msg));
        RuntimeError::typed("X::Inheritance::UnknownParent", attrs)
    }

    /// Validate that all parent classes exist.
    /// Allow inheriting from built-in types that may not be in the classes HashMap.
    /// Returns the parents that must NOT enter the C3 inheritance MRO because
    /// they are a `does`-role whose (short) name collides with the class's own
    /// name — e.g. `class Iterator does Iterator` (Rakudo composes the CORE
    /// `Iterator` role, not the class itself). Such a parent is still composed
    /// as a role by the role-composition loop; keeping it in the inheritance
    /// parent list would make the class its own C3 ancestor (self-cycle /
    /// self-inherit). Also returns the unknown lowercase parents deferred to
    /// custom `trait_mod:<is>` dispatch.
    pub(super) fn validate_class_parents(
        &mut self,
        name: &str,
        parents: &[String],
        does_parents: &[String],
        hidden_parents: &[String],
    ) -> Result<(HashSet<String>, Vec<String>), RuntimeError> {
        const BUILTIN_TYPES: &[&str] = BUILTIN_PARENT_TYPES;
        // `name` is the REGISTRY storage name, which for a lexically-scoped
        // declaration is mangled (ADR-0047 P1: `Foo\u{0}<decl-id>`) while every
        // `is`/`does` parent name below is compared/resolved as WRITTEN in the
        // source. Do every self-name comparison and message against the
        // demangled, user-facing name instead, or e.g. `my class Foobar is
        // Foobar { }` never trips X::Inheritance::SelfInherit because the
        // mangled storage name can never equal the bare parent name it is
        // supposed to collide with.
        let name = crate::value::user_facing_type_name(name);
        let self_short = short_of(&name);
        let mut self_named_does_roles: HashSet<String> = HashSet::new();
        let mut deferred_custom_traits: Vec<String> = Vec::new();
        for parent in parents {
            let resolved_parent_name = self.resolve_declared_type_name(parent);
            // Strip type arguments for validation (e.g., "R[Str:D(Numeric)]" -> "R")
            let base_parent = if let Some(bracket) = resolved_parent_name.find('[') {
                &resolved_parent_name[..bracket]
            } else {
                resolved_parent_name.as_str()
            };
            // Strip leading `::` for comparison (e.g., `is ::F` refers to `F`)
            let resolved_parent = base_parent.strip_prefix("::").unwrap_or(base_parent);
            // A `does`-role of the class's own short name resolves to the like-named
            // CORE/existing role (a class cannot compose itself), so it is neither a
            // self-inheritance error nor a real inheritance parent.
            // The parameterised pun of a role (`R[Int]` composing `R[Int]`, built
            // by `ensure_parametric_role_pun_class`) collides on the *full* name,
            // which the short-name test above misses because it strips the type
            // arguments off the parent but not off the class.
            let is_self_named_does_role = does_parents.contains(parent)
                && (short_of(resolved_parent) == self_short
                    || resolved_parent_name == name.as_ref())
                && self.registry().roles.contains_key(resolved_parent);
            if is_self_named_does_role {
                self_named_does_roles.insert(parent.clone());
                continue;
            }
            if resolved_parent == name.as_ref() {
                let mut attrs = HashMap::new();
                attrs.insert("name".to_string(), Value::str(name.to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("'{}' cannot inherit from itself.", name)),
                );
                return Err(RuntimeError::typed("X::Inheritance::SelfInherit", attrs));
            }
            // A core role mutsu models natively rather than as a registered
            // `RoleDef` (`PositionalBindFailover`, `Sequence`, `QuantHash`) is a
            // legal `does` parent even though it appears in neither the class
            // registry nor `BUILTIN_TYPES`. Consult the single core-role oracle
            // instead of growing a fourth private list here.
            if !self.registry().classes.contains_key(base_parent)
                && !BUILTIN_TYPES.contains(&base_parent)
                && !crate::runtime::types::is_builtin_role_name(base_parent)
                && !self.registry().roles.contains_key(base_parent)
                && !self.registry().enum_types.contains_key(base_parent)
            {
                // Use X::InvalidType for `does`/`hides` parents,
                // X::Inheritance::UnknownParent for `is` parents.
                if does_parents.contains(parent) || hidden_parents.contains(parent) {
                    return Err(RuntimeError::new(format!(
                        "X::InvalidType: Invalid typename '{}'",
                        resolved_parent_name
                    )));
                }
                // If trait_mod:<is> is defined, defer unknown lowercase parents
                // to custom trait dispatch instead of erroring.
                if (self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>"))
                    && resolved_parent
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_lowercase())
                {
                    deferred_custom_traits.push(resolved_parent_name.to_string());
                    continue;
                }
                // A name that is declared as a `package` (or module) exists but
                // does not support inheritance: `package A {}; class B is A {}`
                // is X::Inheritance::Unsupported, not an unknown-parent error.
                if self.chain_declared_packages.contains(base_parent)
                    || self
                        .chain_declared_packages
                        .contains(resolved_parent_name.as_str())
                {
                    let msg = format!(
                        "{} does not support inheritance, so {} cannot inherit from it",
                        resolved_parent_name, name
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("child-typename".to_string(), Value::str(name.to_string()));
                    attrs.insert(
                        "parent".to_string(),
                        Value::package(crate::symbol::Symbol::intern(
                            resolved_parent_name.as_str(),
                        )),
                    );
                    attrs.insert("message".to_string(), Value::str(msg));
                    return Err(RuntimeError::typed("X::Inheritance::Unsupported", attrs));
                }
                return Err(self.unknown_parent_error(name.as_ref(), resolved_parent_name.as_str()));
            }
            // A `does` target that is a non-composable built-in concrete class
            // (Int, Str, Num, Cool, Any, Mu, ...) — as opposed to a composable
            // built-in role (Real, Numeric, Positional, Iterable, ...) — raises
            // X::Composition::NotComposable.
            if does_parents.contains(parent)
                && !self.registry().roles.contains_key(resolved_parent)
                && BUILTIN_TYPES.contains(&resolved_parent)
                && is_non_composable_builtin(resolved_parent)
            {
                // `name` is already the demangled, user-facing name (see the
                // shadowing at the top of this function) — safe to use
                // directly in the message and `target-name` attribute.
                let msg = format!(
                    "{} is not composable, so {} cannot compose it",
                    resolved_parent, name
                );
                let mut attrs = HashMap::new();
                attrs.insert("target-name".to_string(), Value::str(name.to_string()));
                attrs.insert(
                    "composer".to_string(),
                    Value::package(crate::symbol::Symbol::intern(resolved_parent)),
                );
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Composition::NotComposable"),
                    attrs,
                );
                let mut err = RuntimeError::new(msg.to_string());
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
            // Check that `does` targets are actually roles, not classes
            if does_parents.contains(parent)
                && self.registry().classes.contains_key(resolved_parent)
                && !self.registry().roles.contains_key(resolved_parent)
                && !BUILTIN_TYPES.contains(&resolved_parent)
            {
                let msg = format!(
                    "{} is not composable, so {} cannot compose it",
                    resolved_parent, name
                );
                let mut attrs = HashMap::new();
                attrs.insert("target-name".to_string(), Value::str(name.to_string()));
                attrs.insert(
                    "composer".to_string(),
                    Value::package(crate::symbol::Symbol::intern(resolved_parent)),
                );
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Composition::NotComposable"),
                    attrs,
                );
                let mut err = RuntimeError::new(msg.to_string());
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
            // Check if parent is a stub (not yet composed)
            if self.registry().class_stubs.contains(resolved_parent) {
                let message = format!(
                    "'{}' cannot inherit from '{}' because '{}' isn't composed yet (maybe it is stubbed)",
                    name, resolved_parent, resolved_parent
                );
                let mut attrs = HashMap::new();
                attrs.insert("child-name".to_string(), Value::str(name.to_string()));
                attrs.insert(
                    "parent-name".to_string(),
                    Value::str(resolved_parent.to_string()),
                );
                attrs.insert("message".to_string(), Value::str(message.clone()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Inheritance::NotComposed"),
                    attrs,
                );
                let mut err =
                    RuntimeError::new(format!("X::Inheritance::NotComposed: {}", message));
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }
        Ok((self_named_does_roles, deferred_custom_traits))
    }

    /// Build the initial `ClassDef` for the declaration and record the
    /// `is hidden` / `hides` bookkeeping.
    pub(super) fn begin_class_def(
        &mut self,
        name: &str,
        parents: &[String],
        self_named_does_roles: &HashSet<String>,
        is_hidden: bool,
        hidden_parents: &[String],
    ) -> ClassDef {
        // Drop any `does`-role that shares the class's own name from the C3
        // inheritance parents (it is composed as a role below; leaving it here
        // would make the class its own ancestor — see `self_named_does_roles`).
        let inheritance_parents: Vec<String> = if self_named_does_roles.is_empty() {
            parents.to_vec()
        } else {
            parents
                .iter()
                .filter(|p| !self_named_does_roles.contains(*p))
                .cloned()
                .collect()
        };
        let class_def = ClassDef {
            parents: inheritance_parents,
            attributes: Vec::new(),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
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
