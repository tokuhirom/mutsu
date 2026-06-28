use super::*;
use crate::runtime::types::unwrap_varref_value;

impl Interpreter {
    /// Switch `current_package` to a compiled routine's declaring package for the
    /// duration of its body, returning the previous package to restore on exit
    /// (`None` when no switch was needed). This is what makes package-scoped
    /// variable resolution (`our $x` / a `package { my $x }` lexical read or
    /// written from inside the routine) work on EVERY dispatch path — fast, light,
    /// positional-light — not just the OTF/named path that already set it. The
    /// gate (real, non-mangled, non-GLOBAL package) keeps the hot GLOBAL-function
    /// path (fib, ...) free of any lock/string churn: it returns `None` cheaply.
    pub(super) fn enter_routine_package(&mut self, cf: &CompiledFunction) -> Option<String> {
        if !cf.package.is_empty() && cf.package != "GLOBAL" && !cf.package.contains("::&") {
            let saved = self.current_package();
            self.set_current_package(cf.package.clone());
            Some(saved)
        } else {
            None
        }
    }

    /// Restore the package saved by [`enter_routine_package`].
    pub(super) fn leave_routine_package(&mut self, saved: Option<String>) {
        if let Some(s) = saved {
            self.set_current_package(s);
        }
    }

    /// Check if a compiled function is eligible for the fast call path.
    /// Returns true for simple functions that don't need the full call machinery.
    /// State variables are supported: both `state_locals` load/sync and anonymous
    /// state variable sync (via `sync_anon_state_value` at the opcode level) work
    /// correctly in the fast path.
    pub(super) fn is_fast_call_eligible(cf: &CompiledFunction, fn_name: &str) -> bool {
        cf.params.is_empty()
            && cf.param_defs.is_empty()
            && cf.return_type.is_none()
            && !fn_name.is_empty()
    }

    /// Check if a compiled function is eligible for the light call path.
    /// Light calls avoid the expensive env clone/restore cycle by directly
    /// binding parameters and restoring them after the call. This is safe
    /// for simple functions that don't need callframe/caller introspection,
    /// don't have where constraints, state variables, or return type checks.
    /// Check if a compiled function is eligible for the light call path.
    /// The light call skips the heavyweight env clone/restore, Sub value
    /// creation, block/routine push, and callable_id lookup. It does inline
    /// argument binding with minimal overhead.
    ///
    /// Currently restricted to functions where ALL param_defs are named
    /// (no positional params) to avoid correctness issues with positional
    /// argument binding edge cases (optional arrays, @_, VarRef unwrapping, etc.).
    /// Also excludes functions with legacy placeholder params (cf.params).
    pub(super) fn is_light_call_eligible(cf: &CompiledFunction, fn_name: &str) -> bool {
        !fn_name.is_empty()
            && cf.return_type.is_none()
            && cf.code.state_locals.is_empty()
            && !cf.is_rw
            && !cf.is_raw
            && !cf.empty_sig
            && !cf.param_defs.is_empty()
            && cf.param_defs.iter().all(|pd| {
                pd.named
                    && pd.where_constraint.is_none()
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && pd.default.is_none()
                    && pd.type_constraint.is_none()
                    && pd.code_signature.is_none()
                    && !pd.sigilless
                    && !pd.is_invocant
                    && pd.traits.is_empty()
            })
    }

    /// Check if eligible for the positional light call path.
    /// This path avoids push_call_frame, Sub value creation, block/routine push,
    /// callable_id lookup, and full bind_function_args_values. Parameters are
    /// bound to pre-computed local slots and written to env.
    pub(super) fn is_positional_light_call_eligible(cf: &CompiledFunction, fn_name: &str) -> bool {
        !fn_name.is_empty()
            && cf.code.state_locals.is_empty()
            && !cf.is_rw
            && !cf.is_raw
            && !cf.empty_sig
            && cf.param_local_slots.is_some()
            // Exclude functions with inner closures/blocks (may use phasers, closures, etc.)
            && !cf.has_inner_subs
            // Only allow return types that light_return_type_check can handle
            && cf
                .return_type
                .as_deref()
                .is_none_or(Self::is_fast_type_name)
            && !cf.param_defs.is_empty()
            && cf.param_defs.iter().all(|pd| {
                !pd.named
                    && pd.where_constraint.is_none()
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && pd.default.is_none()
                    && !pd.optional_marker
                    && pd.code_signature.is_none()
                    && !pd.sigilless
                    && !pd.is_invocant
                    && pd.traits.is_empty()
                    && pd.sub_signature.is_none()
                    // Only allow basic type constraints that fast_type_check handles.
                    // Excludes subset types, type captures (::T), parametric roles, etc.
                    && pd
                        .type_constraint
                        .as_deref()
                        .is_none_or(Self::is_fast_type_name)
                    // Exclude @/% params which need special collection semantics
                    // and & params which need callable binding
                    && !pd.name.starts_with('@')
                    && !pd.name.starts_with('%')
                    && !pd.name.starts_with('&')
                    // Exclude internal/anonymous params (__ANON_STATE__, __type_only__, etc.)
                    && !pd.name.starts_with("__")
                    // Exclude dynamic variable params ($*var)
                    && !pd.name.starts_with('*')
                    // Exclude $_ topic param (used implicitly by regex, ff, etc.)
                    && pd.name != "_"
            })
    }

    /// Slice 2d: detect a call that passes an array/hash *value* to a plain
    /// readonly scalar `$` param. Raku binds the same mutable container in that
    /// case (`$n.push` / `$n[0]=` / `my @a := @$n` all mutate the caller's
    /// container), which the slot-only fast paths (light / positional_light)
    /// cannot express — they bind a detached copy into a local slot. Such calls
    /// must take the slow `bind_function_args_values` path, which promotes the
    /// param to a shared cell and registers the rw writeback. The common case
    /// (scalar args to `$` params, e.g. `fib($n)`) returns false cheaply, so the
    /// fast paths are preserved.
    pub(super) fn call_shares_container_into_scalar_param(
        cf: &CompiledFunction,
        args: &[Value],
    ) -> bool {
        // Cheapest rejection first: most calls pass non-container args (e.g.
        // `fib($n-1)`), so check the arg value before touching the param meta.
        args.iter().enumerate().any(|(i, arg)| {
            Self::arg_is_container_value(arg)
                && cf.param_defs.get(i).is_some_and(|pd| {
                    Self::is_plain_scalar_param_name(&pd.name)
                        && !pd.slurpy
                        && !pd.double_slurpy
                        && !pd.is_invocant
                        && !pd.sigilless
                        && pd.traits.is_empty()
                        && pd.sub_signature.is_none()
                })
        })
    }

    /// True when `arg` is a varref-capture for an `@`/`%` *array/hash variable*
    /// holding an `Array`/`Hash`. Only an `@`/`%` source needs the cell-promotion:
    /// the fast paths copy the array out of the `@`-variable into the param slot
    /// and detach it. A `$`-scalar source (`my $aref = [0]; f($aref)`) already
    /// shares the array by reference (`$aref[0]++` propagates without promotion),
    /// and a non-variable literal (`f([1,2])`) has no caller to share with — both
    /// must keep the fast path. A variable arg reaches the binder as a
    /// `__mutsu_varref_*` capture, so peek inside without cloning.
    pub(super) fn arg_is_container_value(arg: &Value) -> bool {
        let Value::Capture { positional, named } = arg else {
            return false;
        };
        positional.is_empty()
            && matches!(
                named.get("__mutsu_varref_name"),
                Some(Value::Str(name)) if name.starts_with('@') || name.starts_with('%')
            )
            && matches!(
                named.get("__mutsu_varref_value"),
                Some(Value::Array(..)) | Some(Value::Hash(..))
            )
    }

    /// Method analogue of `call_shares_container_into_scalar_param`: true when
    /// an array/hash *variable* argument is passed into a plain readonly scalar
    /// `$` param of a method. Raku binds the same mutable container, so such a
    /// call must take the slow `bind_function_args_values` path (which promotes
    /// the param to a shared cell and writes it back) instead of the slot-only
    /// fast path that detaches a copy. The alignment differs from the sub case:
    /// a method's `param_defs` include the invocant, but `args` do not — so we
    /// skip invocant (and named) params and align positional params to `args`
    /// by an independent counter.
    pub(super) fn method_shares_container_into_scalar_param(
        &self,
        method_def: &crate::runtime::MethodDef,
        args: &[Value],
    ) -> bool {
        // Unlike the sub path, method-call arguments are NOT wrapped in varref
        // captures — the source variable name lives in the separate
        // `arg_sources` table (`set_pending_call_arg_sources`). So a plain
        // `Array`/`Hash` value paired with an `@`/`%` source name in that table
        // is the method-call equivalent of the sub path's varref container arg.
        let arg_sources = self.pending_call_arg_sources();
        let mut arg_idx = 0usize;
        for pd in &method_def.param_defs {
            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") || pd.named {
                continue;
            }
            let Some(arg) = args.get(arg_idx) else {
                break;
            };
            let src = arg_sources
                .and_then(|s| s.get(arg_idx))
                .and_then(|n| n.as_ref());
            arg_idx += 1;
            let eligible_scalar_param = Self::is_plain_scalar_param_name(&pd.name)
                && !pd.slurpy
                && !pd.double_slurpy
                && !pd.sigilless
                && pd.traits.is_empty()
                && pd.sub_signature.is_none();
            if !eligible_scalar_param {
                continue;
            }
            // A varref capture carries its own `@`/`%` source name; a plain
            // container value needs the source name from `arg_sources`. Either
            // way the binder promotes the bound value to a shared cell.
            let varref_container = Self::arg_is_container_value(arg);
            let plain_container_with_source = matches!(arg, Value::Array(..) | Value::Hash(..))
                && src.is_some_and(|n| n.starts_with('@') || n.starts_with('%'));
            if varref_container || plain_container_with_source {
                return true;
            }
        }
        false
    }

    /// A plain readonly scalar (`$`) parameter is stored sigil-less in
    /// `ParamDef::name` (e.g. `$n` -> `"n"`), unlike `@`/`%`/`&` params which
    /// keep their sigil. Detect it by a leading identifier char, excluding the
    /// twigil/dynamic forms (`$!x` -> `"!x"`, `$*x` -> `"*x"`, `$.x` -> `".x"`)
    /// and the `$_` topic.
    pub(super) fn is_plain_scalar_param_name(name: &str) -> bool {
        name != "_"
            && name
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
    }

    /// True when an `@`/`%` *variable* is passed by name to a plain readonly
    /// scalar `$` *named* param (`sub f(:$n) { $n.push }` called as
    /// `f(n => @a)`). Raku binds the same mutable container; the slot-only
    /// named light-call path binds a copy whose `.push` COW-detaches, so such a
    /// call must take the slow `bind_function_args_values` path (which promotes
    /// the bound value to a shared cell and registers the rw writeback — see the
    /// named branch in binding.rs). The source variable name is encoded
    /// "key=source" in `arg_sources` (`positional_arg_source_name`); a
    /// `$`-scalar source (`:$n` over `my $n = @a`) is excluded (it shares by
    /// reference already, like the positional scalar source).
    pub(super) fn call_shares_container_into_named_scalar_param(
        cf: &CompiledFunction,
        args: &[Value],
        arg_sources: Option<&[Option<String>]>,
    ) -> bool {
        let Some(sources) = arg_sources else {
            return false;
        };
        args.iter().enumerate().any(|(i, arg)| {
            let unwrapped = unwrap_varref_value(arg.clone());
            let Value::Pair(key, val) = &unwrapped else {
                return false;
            };
            if !matches!(**val, Value::Array(..) | Value::Hash(..)) {
                return false;
            }
            let has_container_source = sources
                .get(i)
                .and_then(|s| s.as_ref())
                .and_then(|enc| enc.split_once('='))
                .is_some_and(|(_, src)| src.starts_with('@') || src.starts_with('%'));
            if !has_container_source {
                return false;
            }
            cf.param_defs.iter().any(|pd| {
                pd.named
                    && Self::named_param_share_match_key(pd) == key.as_str()
                    && Self::is_eligible_named_scalar_share_param(pd)
            })
        })
    }

    /// The Pair key a named param matches (`:$n` -> "n", `:foo($x)` -> "foo").
    /// Scalar named params are stored sigil-less, so this strips a leading `:`.
    fn named_param_share_match_key(pd: &crate::ast::ParamDef) -> &str {
        pd.name.strip_prefix(':').unwrap_or(&pd.name)
    }

    /// Eligibility mirror of binding.rs `named_scalar_container_share_eligible`:
    /// a plain readonly scalar named param (not `@`/`%`/`&`, no attr twigil, not
    /// `$_`, no `is copy`/`is rw`/`is raw`, not slurpy, no sub-signature).
    fn is_eligible_named_scalar_share_param(pd: &crate::ast::ParamDef) -> bool {
        let bare = Self::named_param_share_match_key(pd);
        !pd.traits
            .iter()
            .any(|t| t == "copy" || t == "rw" || t == "raw")
            && !pd.slurpy
            && !pd.double_slurpy
            && pd.sub_signature.is_none()
            && bare != "_"
            && !bare.starts_with(['@', '%', '&', '!', '.'])
            && bare
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
    }

    /// Ultra-fast call for simple positional-only functions (e.g. `sub fib($n)`).
    /// Avoids push_call_frame, Sub value creation, block/routine push,
    /// callable_id lookup, and full bind_function_args_values. Parameters are
    /// bound directly to pre-computed local slots AND written to env so that
    /// closures and dynamic lookups work correctly.
    /// Build the common `X::TypeCheck::Argument` attribute map (message,
    /// objname, signature, arguments) shared by the positional-light arity and
    /// type-mismatch error paths. Type-mismatch callers additionally insert
    /// `expected`/`got`. Mirrors the interpreter path in
    /// `Interpreter::enhance_binding_error`.
    pub(super) fn type_check_argument_attrs(
        func_name: &str,
        param_defs: &[crate::ast::ParamDef],
        args: &[Value],
        message: String,
    ) -> std::collections::HashMap<String, Value> {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        attrs.insert("objname".to_string(), Value::str(func_name.to_string()));
        attrs.insert(
            "signature".to_string(),
            Value::str(crate::runtime::Interpreter::build_signature_string(
                param_defs,
            )),
        );
        let arg_type_values: Vec<Value> = crate::runtime::Interpreter::arg_type_names(args)
            .into_iter()
            .map(Value::str)
            .collect();
        attrs.insert("arguments".to_string(), Value::array(arg_type_values));
        attrs
    }
}
