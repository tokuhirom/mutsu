//! Compiled proto registration and trait arguments.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    // Cost: O(p + b + t + a), p = signature size, b = proto body size,
    // t = custom traits, a = argument evaluation, plus trait handler execution.
    pub(super) fn exec_register_proto_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let crate::opcode::CompiledProtoDeclPlan {
            name,
            params,
            param_defs,
            return_type,
            is_export,
            export_tags,
            custom_traits,
            is_method,
            is_our,
            legacy_body: body,
            compiled_routine_key,
            trait_args,
        } = &code.proto_decl_plans[idx as usize];
        let name_str = name.resolve();
        // The plan-compiled bytecode for the `{*}`-rewritten body (ADR-0019
        // C8), `None` for a trivial proto or a method proto — see
        // `CompiledProtoDeclPlan::compiled_routine_key`.
        let compiled = compiled_routine_key.and_then(|key| compiled_fns.get_lazy(&key));
        // A `proto method`/`proto submethod` (`is_method`) is a *method*-level
        // proto: its `{*}` dispatches over the type's multi-method candidates
        // via the class method table, not the package-level proto-sub table.
        // Registering it as a package proto sub is not only unnecessary but
        // breaks role composition: the role body's `RegisterDecl` runs once
        // when the role is declared and again when a class does the role, so the
        // second registration hits the already-present `GLOBAL::<name>` proto and
        // wrongly raises `X::Redeclaration` (lizmat's `Enumify` proto+multi
        // pattern, SBOM::CycloneDX). Skip the package-level registration for
        // method protos; the method-table path already handles them.
        if !*is_method {
            // Marked by the compiler when this `proto` is declared directly in a
            // routine/closure body, where it lexically shadows an outer routine
            // of the same name instead of redeclaring it.
            let is_lexical_hoist = custom_traits.iter().any(|t| t == "__lexical_hoist");
            self.register_proto_decl(
                &name_str,
                params,
                param_defs,
                return_type.as_ref(),
                body,
                *is_our,
                compiled,
                is_lexical_hoist,
            )?;
        }
        if *is_export {
            // The GLOBAL alias stands in for the default import; a proto
            // exported only under other tags (`is export(:SUPPORTED)`, Sub::Util)
            // is installed by the `use` that names its tag, so registering it
            // here made it visible to `::('&name')` without any import.
            let exported_by_default = export_tags.is_empty()
                || export_tags
                    .iter()
                    .any(|t| matches!(t.as_str(), "DEFAULT" | "MANDATORY"));
            if exported_by_default {
                self.register_proto_decl_as_global(
                    &name_str,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    body,
                    compiled,
                )?;
            }
            // Record the export so consumers/MAIN-dispatch see the whole multi
            // family. A `proto … is export` exports its candidates too (raku),
            // e.g. zef's `proto MAIN(|) is export` over `multi sub MAIN(…)`.
            if !self.module.suppress_exports {
                let pkg = self.current_package();
                self.register_exported_sub(pkg, name_str.clone(), export_tags.clone());
            }
        } else if *is_our && !self.module.suppress_exports {
            // An `our proto sub` declared directly inside a module's own `my
            // package EXPORT::<tag> { ... }` block is part of that tag's
            // export list by construction, the proto-family counterpart of
            // the `our sub`/`our multi sub` handling in `exec_register_sub_op`
            // (`export_implicit_stash_sub`) -- raku rejects `our multi sub`
            // outright, so an `our`-scoped proto is the only way to put a
            // multi family into an export stash this way. A no-op unless
            // `current_package()` actually names such a stash.
            self.export_implicit_stash_proto(&name_str);
        }
        // A method proto's traits are `trait_mod:<is>(Method ...)` applications
        // made once, at declaration (`class_body_proto_method_decl`,
        // `role_body_deferred_proto_method`); this op also re-runs for every
        // composition of a role, and passing a `Sub` would never match.
        if *is_method {
            return Ok(());
        }
        // Apply custom trait_mod:<is> for each non-builtin trait (only if defined)
        if !custom_traits.is_empty() {
            let has_trait_mod = self.has_trait_mod_handler("trait_mod:<is>");
            for (trait_index, trait_name) in custom_traits.iter().enumerate().filter(|(_, t)| {
                !t.starts_with("__")
                    && *t != "default"
                    && !t.starts_with("DEPRECATED")
                    && *t != "deep"
                    // `is cached` is a built-in routine trait, not a
                    // user-defined trait_mod:<is> application.
                    && *t != "cached"
            }) {
                if !has_trait_mod {
                    return Err(RuntimeError::new(format!(
                        "Can't use unknown trait 'is' -> '{}' in sub declaration.",
                        trait_name
                    )));
                }
                // A proto is the live dispatcher, including candidates that
                // are declared later. Its name handle uses the existing
                // compiled forwarding path when a trait installs a wrapper.
                let sub_val = Value::routine_parts(
                    Symbol::intern(&self.current_package()),
                    Symbol::intern(&name_str),
                    false,
                );
                let argument = match trait_args.get(trait_index).and_then(Option::as_ref) {
                    Some(argument) => self.eval_decl_trait_arg(argument)?,
                    None => Value::TRUE,
                };
                let named_arg = Value::pair(trait_name.clone(), argument);
                let result = loan_env!(
                    self,
                    call_trait_mod("trait_mod:<is>", vec![sub_val, named_arg])
                )?;
                // If the trait_mod returned a modified sub (e.g. with CALL-ME mixed in),
                // store it in the env so function dispatch can find it.
                if matches!(result.view(), ValueView::Mixin(..)) {
                    self.env_mut().insert(format!("&{}", name), result);
                }
            }
        }
        Ok(())
    }
}
