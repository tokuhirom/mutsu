//! Dispatch of a method's custom `is` traits to a user `trait_mod:<is>`,
//! shared by `method` and `proto method` declarations in class and role
//! bodies.

use super::*;
use crate::symbol::Symbol;

/// Whether a `custom_traits` entry is an internal parser marker rather than a
/// user trait. The parser records the return-type spelling (`returns` vs `of`)
/// as a `__`-prefixed pseudo-trait so the RakuAST converter can tell the two
/// apart; nothing in trait application should ever see it.
pub(super) fn is_parser_marker(trait_name: &str) -> bool {
    trait_name.starts_with("__")
}

impl Interpreter {
    /// Apply every user (non-marker) trait of a method declared in package
    /// `pkg` by calling `trait_mod:<is>(Method $m, ...)`, with `$*PACKAGE`
    /// bound to `pkg` for the duration of the call (a trait handler such as
    /// `Method::Also`'s reads `$*PACKAGE.HOW`).
    // Cost: O(t * c), t = number of custom traits, c = trait_mod:<is> candidates.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn apply_method_is_traits(
        &mut self,
        pkg: &str,
        method_name: &str,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[crate::ast::Stmt],
        is_rw: bool,
        is_proto: bool,
        return_type: Option<&str>,
        traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<(), RuntimeError> {
        if !traits.iter().any(|(t, _)| !is_parser_marker(t)) {
            return Ok(());
        }
        if !(self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>")) {
            return Ok(());
        }
        let saved_package_var = self.env.get("*PACKAGE").cloned();
        self.env
            .insert("*PACKAGE".to_string(), Value::package(Symbol::intern(pkg)));
        let result = self.apply_method_is_traits_inner(
            pkg,
            method_name,
            params,
            param_defs,
            body,
            is_rw,
            is_proto,
            return_type,
            traits,
        );
        match saved_package_var {
            Some(v) => self.env.insert("*PACKAGE".to_string(), v),
            None => self.env.remove("*PACKAGE"),
        };
        result
    }

    /// A `proto method` inside a role body: its traits dispatch at role
    /// declaration time, like a role `method`'s.
    // Cost: O(t * c), t = number of custom traits, c = trait_mod:<is> candidates.
    pub(super) fn role_body_deferred_proto_method(
        &mut self,
        role_name: &str,
        stmt: &crate::ast::Stmt,
    ) -> Result<(), RuntimeError> {
        let crate::ast::Stmt::ProtoDecl {
            name,
            param_defs,
            return_type,
            body,
            trait_args,
            ..
        } = stmt
        else {
            return Ok(());
        };
        let effective_param_defs =
            crate::method_signature_shared::effective_method_param_defs(param_defs, false);
        let params: Vec<String> = effective_param_defs
            .iter()
            .map(|p| p.name.clone())
            .collect();
        self.apply_method_is_traits(
            role_name,
            &name.resolve(),
            &params,
            &effective_param_defs,
            body,
            false,
            true,
            return_type.as_deref(),
            trait_args,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_method_is_traits_inner(
        &mut self,
        pkg: &str,
        method_name: &str,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[crate::ast::Stmt],
        is_rw: bool,
        is_proto: bool,
        return_type: Option<&str>,
        traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<(), RuntimeError> {
        // One code object for the whole declaration: every trait handler
        // composes onto, and binds `$!do` on, the same Method (ADR-11827
        // §2.4), as the named-sub trait loop threads one `$r`.
        let mut trait_env = self.env.clone();
        // Add method lookup markers so .wrap stores in
        // method_wrap_chains (keyed by class+method).
        trait_env.insert(
            "__mutsu_lookup_class".to_string(),
            Value::str(pkg.to_string()),
        );
        trait_env.insert(
            "__mutsu_lookup_method".to_string(),
            Value::str(method_name.to_string()),
        );
        // A `proto method` is the dispatcher itself, so it carries no
        // candidate index: that absence is what makes `.is_dispatcher`
        // answer True (`sub_multi_method_dispatcher_name`).
        if !is_proto {
            trait_env.insert("__mutsu_lookup_candidate_idx".to_string(), Value::int(0));
        }
        // The code object passed to a user `trait_mod:<is>` candidate
        // must report as a `Method`, not a `Sub`, the same way
        // `sub_value_from_function_def` tags a real method's code
        // object — otherwise a candidate typed `(Method $m, ...)`
        // (the only form `raku` accepts for a method-level trait)
        // never type-checks and the trait application silently does
        // nothing.
        trait_env.insert(
            "__mutsu_callable_type".to_string(),
            Value::str_from("Method"),
        );
        // `.returns` / `.signature.returns`, which upstream NativeCall's
        // `is native` reads to marshal the result.
        trait_env.remove_sym(crate::symbol::well_known::return_type());
        if let Some(return_type) = return_type {
            trait_env.insert(
                "__mutsu_return_type".to_string(),
                Value::str(return_type.to_string()),
            );
        }
        let sub_val = Value::make_sub(
            Symbol::intern(pkg),
            Symbol::intern(method_name),
            params.to_vec(),
            param_defs.to_vec(),
            body.to_vec(),
            is_rw,
            trait_env,
        );
        for (trait_name, trait_arg) in traits {
            if is_parser_marker(trait_name) {
                continue;
            }
            let trait_arg_val = if let Some(arg_expr) = trait_arg {
                Some(self.eval_block_value(&[crate::ast::Stmt::Expr(arg_expr.clone())])?)
            } else {
                None
            };
            let type_obj = self.resolve_type_object(trait_name);
            let mut args = vec![sub_val.clone()];
            if let Some(type_val) = type_obj {
                args.push(type_val);
                if let Some(arg_val) = trait_arg_val {
                    args.push(arg_val);
                }
            } else {
                let named_val = if let Some(arg_val) = trait_arg_val {
                    Value::pair(trait_name.clone(), arg_val)
                } else {
                    Value::pair(trait_name.clone(), Value::TRUE)
                };
                args.push(named_val);
            }
            let _ = self.call_function("trait_mod:<is>", args);
        }
        Ok(())
    }
}
