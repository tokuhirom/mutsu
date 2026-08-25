//! `register_class_decl` — the AST registration walker for `class`
//! declarations. ADR-0019 D0 split the former single ~2,500-line function
//! into named phase functions with explicit inputs, so the D1–D9 slices can
//! replace one phase at a time:
//!
//! - rollback snapshot / redeclaration and stub checks / parent validation /
//!   shell publication: `registration_class_validate.rs`
//! - role composition (header `does`/`is` roles): `registration_class_compose.rs`
//!   and `registration_class_compose_body.rs` (deferred role bodies, role
//!   parents, punning)
//! - the body walk: `registration_class_body.rs` (driver and small arms),
//!   `registration_class_body_attr.rs` (`has`), `registration_class_body_method.rs`
//!   (+ `_forms.rs`) (`method`), `registration_class_body_does.rs` (`also does`)
//! - body exit, finalization, and custom-HOW install:
//!   `registration_class_body_exit.rs`
//!
//! This file keeps the orchestrating entry point and the shared
//! builtin-parent table.

use super::registration_class::language_revision_letter;
use super::registration_class_compose::{RoleCompositionCx, RoleCompositionOutcome};
use super::registration_class_validate::ClassRegSnapshot;
use super::*;

/// Built-in types a user class may name as an `is`/`does` parent.
pub(crate) const BUILTIN_PARENT_TYPES: &[&str] = &[
    "Mu",
    "Any",
    "Cool",
    "Int",
    "Num",
    "Str",
    "Bool",
    "Rat",
    "FatRat",
    "Complex",
    "Array",
    "Hash",
    "List",
    "Map",
    "Set",
    "Bag",
    "Mix",
    "SetHash",
    "BagHash",
    "MixHash",
    "Range",
    "Pair",
    "IO",
    "IO::Path",
    "IO::Handle",
    "IO::Spec",
    "IO::Spec::Unix",
    "IO::Spec::Win32",
    "IO::Spec::Cygwin",
    "IO::Spec::QNX",
    "Regex",
    "Match",
    "Junction",
    "Exception",
    "Failure",
    "Version",
    "Nil",
    "Block",
    "Code",
    "Routine",
    "Sub",
    "Method",
    "Seq",
    "Slip",
    "Whatever",
    "WhateverCode",
    "HyperWhatever",
    "Callable",
    "Numeric",
    "Real",
    "Stringy",
    "Positional",
    "Associative",
    "Order",
    "Endian",
    "Proc",
    "Proc::Async",
    "Supply",
    "Supplier",
    // Scheduler is a composable built-in role in Raku (ParametricRoleGroupHOW);
    // a class may `does Scheduler` and supply its own `cue` (e.g. the
    // Test::Scheduler dist: `class Test::Scheduler does Scheduler {...}`).
    "Scheduler",
    "Setty",
    "Baggy",
    "Mixy",
    "Date",
    "DateTime",
    // Dateish is a real composable role in Raku (both Date and DateTime
    // themselves `does Dateish`); a user class may `does Dateish` and supply
    // its own state/private `!formatter`, e.g. TOML::Thumb's `Time::Local`.
    "Dateish",
    "Capture",
    "Grammar",
    "Parameter",
    "Proxy",
    "Signature",
    "Stash",
    "Metamodel::ClassHOW",
    "Perl6::Metamodel::ClassHOW",
    "Metamodel::GrammarHOW",
    "Perl6::Metamodel::GrammarHOW",
];

impl Interpreter {
    pub(crate) fn register_class_decl(
        &mut self,
        name: &str,
        parents: &[String],
        modifiers: ClassDeclModifiers<'_>,
    ) -> Result<Vec<String>, RuntimeError> {
        self.clear_private_zeroarg_method_cache();
        // Mark this as a user-declared class so its collected attribute list is
        // authoritative for accessor resolution (undeclared `.name` -> NotFound).
        self.user_declared_classes.insert(name.to_string());
        let ClassDeclModifiers {
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            language_version: class_language_version,
            is_stub: is_stub_body,
            trusts,
            own_attribute_names,
            attr_decls,
            method_name_chunks,
            method_decls,
            declared_static_names,
            parent_pre_args,
            compiled_fns,
            body_plan,
            is_hoisted_shell,
        } = modifiers;
        let class_lang_rev = language_revision_letter(class_language_version);
        // Normalize parent names: strip leading `::` (indirect name lookup syntax).
        // `is ::Foo` means the same as `is Foo` in Raku.
        let strip_colons = |s: &str| s.strip_prefix("::").unwrap_or(s).to_string();
        // Resolve generic type captures in parent names so a class nested in a
        // parametric role body (`class A is Array[T] {}`, composed with `T = Int`)
        // inherits from the concrete `Array[Int]`. Outside a role composition no
        // captures are bound, so `resolved_type_capture_name` is a no-op.
        let parents: Vec<String> = parents
            .iter()
            .map(|p| self.resolved_type_capture_name(&strip_colons(p)))
            .collect();
        let parents = parents.as_slice();
        let does_parents: Vec<String> = does_parents.iter().map(|p| strip_colons(p)).collect();
        let does_parents = does_parents.as_slice();
        let hidden_parents: Vec<String> = hidden_parents.iter().map(|p| strip_colons(p)).collect();
        let hidden_parents = hidden_parents.as_slice();
        // Snapshot the previous registry state for this class so a redefinition
        // can be rolled back if the new body fails.
        let snapshot = ClassRegSnapshot::capture(self, name);
        // Clear `is Type` trait entries for this class (they'll be re-populated from the body).
        self.registry_mut()
            .class_attribute_is_types
            .retain(|(cn, _), _| cn != name);

        self.check_class_role_redeclaration(name, is_lexical, is_stub_body)?;

        // If this is a stub registration but the class already exists and is
        // NOT a stub (i.e., it was already filled in by a hoisted real
        // declaration), skip the stub registration to avoid overwriting the
        // real class definition.
        if is_stub_body
            && self.registry().classes.contains_key(name)
            && !self.registry().class_stubs.contains(name)
        {
            return Ok(Vec::new());
        }

        // Track whether this registry entry came from a `my`-scoped
        // declaration (see `Registry::lexical_classes`'s doc comment) so a
        // later bare-name `has_class` query can tell a real package-scope
        // class apart from a lexical one that is only still present because
        // mutsu has no scope-exit cleanup for `registry().classes`.
        if is_lexical {
            self.registry_mut().lexical_classes.insert(name.to_string());
        } else {
            self.registry_mut().lexical_classes.remove(name);
        }

        let (self_named_does_roles, deferred_custom_traits) =
            self.validate_class_parents(name, parents, does_parents, hidden_parents)?;
        let mut class_def = self.begin_class_def(
            name,
            parents,
            &self_named_does_roles,
            is_hidden,
            hidden_parents,
        );
        // ADR-0019 F4c-9b: composition writes methods straight to the
        // registry now (there is no `ClassDef::methods` buffer for
        // `publish_class_shell` to later rebuild the table from), so a
        // redeclaration must start this owner's method rows from a clean
        // slate right here, before composition runs -- otherwise a stale
        // row from the PREVIOUS declaration would survive alongside the
        // freshly composed one (`push_user_method` appends, it does not
        // replace). Only `user_candidates` are cleared (the `builtin`/
        // `accessor`/`proto` columns are untouched, matching every other
        // mutator's liveness semantics). If composition itself fails,
        // restore from `snapshot` so the attempt does not leave the
        // still-valid previous class (whose `ClassDef` in `registry.
        // classes` is untouched at this point either way) methodless.
        self.registry_mut()
            .clear_user_methods_for_owner(crate::symbol::Symbol::intern(name));
        // Compose roles listed in the parents (from "does Role" or "is Role" in class header)
        let RoleCompositionOutcome {
            composed_roles_list,
            direct_composed_roles,
            punned_roles,
            hidden_punned_role_bases,
            class_role_param_bindings,
        } = {
            let mut cx = RoleCompositionCx {
                name,
                class_lang_rev: &class_lang_rev,
                class_def: &mut class_def,
                out: RoleCompositionOutcome::default(),
                is_hoisted_shell,
            };
            if let Err(err) =
                self.compose_class_parent_roles(&mut cx, parents, does_parents, parent_pre_args)
            {
                snapshot.restore(self, name);
                return Err(err);
            }
            cx.out
        };
        if class_role_param_bindings.is_empty() {
            self.registry_mut().class_role_param_bindings.remove(name);
        } else {
            self.registry_mut()
                .class_role_param_bindings
                .insert(name.to_string(), class_role_param_bindings);
        }
        self.install_role_puns(&punned_roles, &hidden_punned_role_bases);
        self.record_class_composed_roles(
            name,
            &mut class_def,
            &composed_roles_list,
            &direct_composed_roles,
        );
        if self.publish_class_shell(
            name,
            trusts,
            &class_def,
            hidden_parents,
            does_parents,
            is_stub_body,
        )? {
            return Ok(deferred_custom_traits);
        }
        let class_def = self.run_class_body(
            name,
            class_def,
            is_hidden,
            class_is_rw,
            &class_lang_rev,
            own_attribute_names,
            attr_decls,
            method_name_chunks,
            method_decls,
            declared_static_names,
            compiled_fns,
            body_plan,
            is_hoisted_shell,
        )?;
        self.finalize_class_registration(name, parents, class_def, &snapshot)?;
        self.install_class_exporthow(name, parents)?;
        Ok(deferred_custom_traits)
    }
}
