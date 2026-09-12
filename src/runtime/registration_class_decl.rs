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
use super::registration_class_deferred_parents::DeferredParentCx;
use super::registration_class_parents::ParentValidation;
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
    "PseudoStash",
    "Metamodel::ClassHOW",
    "Perl6::Metamodel::ClassHOW",
    "Metamodel::GrammarHOW",
    "Perl6::Metamodel::GrammarHOW",
    "Metamodel::ParametricRoleHOW",
    "Perl6::Metamodel::ParametricRoleHOW",
];

/// Core types a user class may name as an `is` parent, but which mutsu models
/// natively rather than as an entry in `registry.classes`. They answer to
/// [`Interpreter::is_builtin_type`], most of them resolve as a type object
/// through `::('Name')`, and every one of them was checked against rakudo with
/// `class Zz is <Name> { }` — which compiles for all of them. Without this
/// list, `class ValueClass::Attribute is Attribute { }` (the `ValueClass`
/// distribution, and `Functional::Queue` / `Functional::Stack` through it),
/// `class CX::Warn::Timezones::UnknownID is CX::Warn { }` (`Timezones::
/// ZoneInfo`) and `class MetamodelX::Protocol is Metamodel::SubsetHOW { }`
/// (`Protocol`) all died as X::Inheritance::UnknownParent.
///
/// Deliberately SEPARATE from [`BUILTIN_PARENT_TYPES`] rather than folded
/// into it: that table also decides whether a `does` target is composable
/// (`registration_class_validate.rs`) and whether a `but`-mixin may take the
/// class-declaration path instead of the wrapper one
/// (`types::role_mixin_class`, whose own comment names `Attribute` as the
/// example that must keep the wrapper). Only the `is`-parent existence check
/// consults this list, so neither of those decisions moves.
pub(crate) const BUILTIN_INHERITABLE_TYPES: &[&str] = &[
    "Attribute",
    "CallFrame",
    "CompUnit",
    "CX::Return",
    "CX::Warn",
    "Cursor",
    "Deprecation",
    "Duration",
    "Instant",
    "Label",
    "NFC",
    "NFD",
    "NFKC",
    "NFKD",
    "ObjAt",
    "Scalar",
    "StrDistance",
    "Submethod",
    "Uni",
    // The rest of the metamodel HOW family. `Metamodel::ClassHOW`,
    // `::GrammarHOW` and `::ParametricRoleHOW` are in `BUILTIN_PARENT_TYPES`
    // above because a subclass of those three also needs the metamodel
    // dispatch frame (`Interpreter::is_metamodel_class_name`); the ones here
    // have no native metamethods to inherit yet, so naming one as a parent
    // yields an ordinary class — but that is still much closer to rakudo than
    // refusing the declaration outright.
    "Metamodel::ConcreteRoleHOW",
    "Metamodel::CurriedRoleHOW",
    "Metamodel::EnumHOW",
    "Metamodel::ModuleHOW",
    "Metamodel::PackageHOW",
    "Metamodel::ParametricRoleGroupHOW",
    "Metamodel::SubsetHOW",
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
        crate::runtime::cow_table_mut(&mut self.user_declared_classes).insert(name.to_string());
        let ClassDeclModifiers {
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            body_parents,
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
        self.note_compound_declared_type(name);

        let ParentValidation {
            non_inheritance_parents,
            mut deferred_custom_traits,
            deferred_body_parents,
        } =
            self.validate_class_parents(name, parents, does_parents, hidden_parents, body_parents)?;
        // A deferred `also is` parent takes no part in the header phase (issue
        // #8099): it is re-resolved after the body has run. Drop it -- together
        // with its position-aligned bracket-argument chunks, so the two stay in
        // lockstep the way the auto-`Grammar` filter at the VM call site keeps
        // them -- and hand the rest to the header phase unchanged.
        let (held_parents, held_pre_args);
        let (parents, parent_pre_args) = if deferred_body_parents.is_empty() {
            (parents, parent_pre_args)
        } else {
            let keep: Vec<bool> = parents
                .iter()
                .map(|p| !deferred_body_parents.contains(p))
                .collect();
            held_parents = parents
                .iter()
                .zip(&keep)
                .filter(|(_, k)| **k)
                .map(|(p, _)| p.clone())
                .collect::<Vec<String>>();
            // `parent_pre_args` is empty for registration paths with no
            // compiled plan; only filter it when it is actually aligned.
            held_pre_args = if parent_pre_args.len() == keep.len() {
                parent_pre_args
                    .iter()
                    .zip(&keep)
                    .filter(|(_, k)| **k)
                    .map(|(a, _)| *a)
                    .collect::<Vec<_>>()
            } else {
                parent_pre_args.to_vec()
            };
            (held_parents.as_slice(), held_pre_args.as_slice())
        };
        let mut class_def = self.begin_class_def(
            name,
            parents,
            &non_inheritance_parents,
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
            mut composed_roles_list,
            mut direct_composed_roles,
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
        // A parent deferred to `trait_mod:<is>` may still turn out to be a
        // genuine unknown parent, and the dispatch that decides that runs in
        // `exec_register_class_op`, AFTER this function has published the
        // class shell (the trait handler has to be able to see the type
        // object). Hand `snapshot` on so that site can undo the declaration;
        // see `Interpreter::deferred_trait_class_rollback`.
        if !deferred_custom_traits.is_empty() {
            self.deferred_trait_class_rollback = Some((name.to_string(), snapshot.clone()));
        }
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
        let mut class_def = self.run_class_body(
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
        // `also is Parent` applies at its position in the BODY, so its parent
        // is resolved here -- after the body has had its chance to introduce
        // it, and still before `finalize_class_registration` computes the C3
        // MRO (issue #8099).
        let final_parents: Vec<String> = if deferred_body_parents.is_empty() {
            parents.to_vec()
        } else {
            let outcome = match self.apply_deferred_body_parents(
                name,
                &mut class_def,
                &deferred_body_parents,
                DeferredParentCx {
                    class_lang_rev: &class_lang_rev,
                    is_hoisted_shell,
                    composed_roles_list: &mut composed_roles_list,
                    direct_composed_roles: &mut direct_composed_roles,
                },
            ) {
                Ok(outcome) => outcome,
                Err(err) => {
                    snapshot.restore(self, name);
                    return Err(err);
                }
            };
            // A name the body never introduced is not this pass's to reject:
            // it falls through to the same custom `trait_mod:<is>` dispatch an
            // unknown HEADER parent takes, which raises
            // X::Inheritance::UnknownParent itself when no candidate claims
            // it. The shell is already published by now, so arm the rollback
            // the dispatch site needs here too.
            if !outcome.unclaimed.is_empty() {
                deferred_custom_traits.extend(outcome.unclaimed);
                self.deferred_trait_class_rollback = Some((name.to_string(), snapshot.clone()));
            }
            parents.iter().cloned().chain(outcome.parents).collect()
        };
        self.finalize_class_registration(name, &final_parents, class_def, &snapshot)?;
        // Construction-time attribute defaults and BUILD parameter defaults
        // execute after this declaration, often from another compunit. Keep
        // the declaring unit so those evaluations can still see this file's
        // private top-level routines.
        let declaring_unit = self.unit_of_declaring_file(self.current_source_file().as_deref());
        crate::runtime::cow_table_mut(&mut self.class_declaring_units)
            .insert(name.to_string(), declaring_unit);
        self.install_class_exporthow(name, &final_parents)?;
        Ok(deferred_custom_traits)
    }
}
