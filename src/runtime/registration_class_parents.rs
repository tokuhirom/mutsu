//! Parent-list validation for `class` declarations: the
//! X::Inheritance::UnknownParent constructor and `validate_class_parents`,
//! which decides for every declared `is`/`does`/`hides` parent whether it
//! exists, is composed rather than inherited, is deferred to a custom
//! `trait_mod:<is>`, or -- for an `also is` named in the class body -- is
//! deferred past the body entirely (issue #8099,
//! `registration_class_deferred_parents.rs`).

use super::registration_class::is_non_composable_builtin;
use super::registration_class_decl::{BUILTIN_INHERITABLE_TYPES, BUILTIN_PARENT_TYPES};
use super::*;

/// Short (unqualified) name of the class being declared, for detecting a
/// `does`-role that shares the class's own name (see below).
fn short_of(s: &str) -> &str {
    s.rsplit("::").next().unwrap_or(s)
}

/// What `validate_class_parents` learned about a class's declared parents.
pub(super) struct ParentValidation {
    /// Parents that must NOT enter the C3 inheritance MRO: a `does`-role whose
    /// (short) name collides with the class's own name, and a name deferred to
    /// custom `trait_mod:<is>` dispatch (`is Marked` there is a trait, not a
    /// parent). See `validate_class_parents`'s own doc comment.
    pub(super) non_inheritance_parents: HashSet<String>,
    /// Unknown parents deferred to custom `trait_mod:<is>` dispatch, in source
    /// order, for the dispatch site in `vm_typedecl_ops.rs` to call with.
    pub(super) deferred_custom_traits: Vec<String>,
    /// `also is Parent` parents that name nothing known yet. They are dropped
    /// from the header phase and re-resolved after the body has run, since
    /// `also is` executes at its position in the body and the body itself can
    /// be what introduces the parent (issue #8099).
    pub(super) deferred_body_parents: Vec<String>,
}

impl Interpreter {
    /// `X::Inheritance::UnknownParent`: `name` (the class being declared)
    /// gave `parent_name` as an `is` parent that names no known class, role,
    /// enum, or builtin type. Shared by the immediate check below and by the
    /// deferred-custom-trait dispatch (`vm_typedecl_ops.rs`) for when an
    /// unknown parent name was optimistically deferred to a user
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
    /// Returns the parents that must NOT enter the C3 inheritance MRO, which
    /// are of two kinds:
    ///
    /// - a `does`-role whose (short) name collides with the class's own name —
    ///   e.g. `class Iterator does Iterator` (Rakudo composes the CORE
    ///   `Iterator` role, not the class itself). Such a parent is still
    ///   composed as a role by the role-composition loop; keeping it in the
    ///   inheritance parent list would make the class its own C3 ancestor
    ///   (self-cycle / self-inherit);
    /// - an unknown name deferred to custom `trait_mod:<is>` dispatch. `is
    ///   Marked` there is a TRAIT, not a parent, so rakudo reports
    ///   `Alpha.^parents` as empty and `Alpha.^mro` as `Alpha, Any, Mu`;
    ///   leaving the trait name in gave mutsu a phantom `Marked` ancestor.
    ///
    /// Also returns those deferred names, in source order, for the dispatch
    /// site in `vm_typedecl_ops.rs` to call `trait_mod:<is>` with.
    pub(super) fn validate_class_parents(
        &mut self,
        name: &str,
        parents: &[String],
        does_parents: &[String],
        hidden_parents: &[String],
        body_parents: &[String],
    ) -> Result<ParentValidation, RuntimeError> {
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
        let mut non_inheritance_parents: HashSet<String> = HashSet::new();
        let mut deferred_custom_traits: Vec<String> = Vec::new();
        let mut deferred_body_parents: Vec<String> = Vec::new();
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
                non_inheritance_parents.insert(parent.clone());
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
                // A plain `is` parent may also name a core type mutsu models
                // natively instead of registering a `ClassDef` for it
                // (`Attribute`, `CX::Warn`, `Metamodel::SubsetHOW`, ...).
                // Consulted only here, on the `is` path, so it moves neither
                // the `does`-composability verdict below nor the `but`-mixin
                // fast path — see `BUILTIN_INHERITABLE_TYPES`'s own comment.
                if BUILTIN_INHERITABLE_TYPES.contains(&base_parent) {
                    continue;
                }
                // A name that is declared as a `package` (or module) exists but
                // does not support inheritance: `package A {}; class B is A {}`
                // is X::Inheritance::Unsupported, not an unknown-parent error.
                // Checked BEFORE the custom-trait deferral below: a declared
                // package is not an unknown name, so rakudo passes it to
                // `trait_mod:<is>` positionally (as the package object) rather
                // than as the `:Name` named argument the deferral synthesises.
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
                // `also is Parent` executes at its position in the class
                // BODY, so a parent named there is allowed to be unknown right
                // now: the body itself may be what brings it into scope (a
                // `use` inside the body, or a type the body declares). Collect
                // it and let `apply_deferred_body_parents` re-resolve it once
                // the body has run.
                //
                // Ahead of the `trait_mod:<is>` deferral below, not after it:
                // that deferral now claims EVERY unknown name once any
                // `trait_mod:<is>` is in scope, which merely importing `Test`
                // arranges, so leaving it first would swallow every `also is`
                // parent the body introduces. A body parent the body turns out
                // NOT to introduce still reaches the trait dispatch -- the
                // caller hands it back there once the body has had its chance
                // (`register_class_decl`).
                if body_parents.contains(parent) {
                    deferred_body_parents.push(parent.clone());
                    continue;
                }
                // `is Foo` on a name that is NOT a known type is rakudo's
                // spelling of the named trait argument `trait_mod:<is>($type,
                // :Foo)` — that is how `class Foo is Static { }` reaches the
                // `Staticish` distribution's
                // `multi trait_mod:<is>(Mu:U $doee, :$Static!)`. Defer any such
                // parent to custom trait dispatch whenever the program defines
                // a `trait_mod:<is>` at all; the dispatch site
                // (`vm_typedecl_ops.rs`) turns a no-matching-candidate result
                // back into `unknown_parent_error`, so a genuine typo still
                // raises X::Inheritance::UnknownParent. This used to be
                // restricted to lowercase names, which made every uppercase
                // trait name (the overwhelmingly common spelling) an
                // unknown-parent error instead.
                if self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>") {
                    deferred_custom_traits.push(resolved_parent_name.to_string());
                    // `is Marked` is then a trait, not inheritance: keep the
                    // name out of the C3 parents (keyed by the source spelling,
                    // which is what `begin_class_def` filters on).
                    non_inheritance_parents.insert(parent.clone());
                    continue;
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
        Ok(ParentValidation {
            non_inheritance_parents,
            deferred_custom_traits,
            deferred_body_parents,
        })
    }
}
