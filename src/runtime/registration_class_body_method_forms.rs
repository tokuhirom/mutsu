//! Named phases of `register_class_decl` (ADR-0019 D0): the importable
//! sub-forms a class method can take — the exported operator-method sub and
//! the `our method` / `my method` function-parameter shapes. Pure mechanical
//! extraction from `registration_class_decl.rs` — no behavior change.

use super::*;
use crate::ast::ParamDef;
use crate::symbol::Symbol;

/// Compute the function-form parameter lists for an `our method` / `my
/// method` registered as a package-scoped / lexically-scoped function: with
/// an explicit invocant (`$self:`) the signature is kept (minus a literal
/// `self` param def); otherwise a `self` parameter is prepended so the first
/// argument gets bound as `self` when calling this as a function.
pub(super) fn method_sub_form_params(
    effective_params: &[String],
    effective_param_defs: &[ParamDef],
) -> (Vec<String>, Vec<ParamDef>) {
    let has_explicit_invocant = effective_param_defs
        .iter()
        .any(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"));
    if has_explicit_invocant {
        (
            effective_params.to_vec(),
            effective_param_defs
                .iter()
                .filter(|p| p.name.as_str() != "self")
                .cloned()
                .collect(),
        )
    } else {
        // Prepend "self" as first param so the first argument
        // gets bound as `self` when calling this as a function.
        let mut sub_params = vec!["self".to_string()];
        sub_params.extend(
            effective_params
                .iter()
                .filter(|p| p.as_str() != "self")
                .cloned(),
        );
        let self_param = crate::ast::ParamDef {
            name: "self".to_string(),
            default: None,
            multi_invocant: true,
            required: false,
            named: false,
            slurpy: false,
            double_slurpy: false,
            onearg: false,
            sigilless: false,
            type_constraint: None,
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            traits: Vec::new(),
            optional_marker: false,
            outer_sub_signature: None,
            code_signature: None,
            is_invocant: false,
            shape_constraints: None,
            block_param: false,
        };
        let mut sub_param_defs = vec![self_param];
        sub_param_defs.extend(
            effective_param_defs
                .iter()
                .filter(|p| !(p.is_invocant || p.traits.iter().any(|t| t == "invocant")))
                .cloned(),
        );
        (sub_params, sub_param_defs)
    }
}

impl Interpreter {
    /// Register an importable *sub form* for an `is export` operator method.
    ///
    /// Raku exposes `method prefix:<~> is export` (etc.) as a sub whose invocant
    /// becomes the first positional argument and whose body re-dispatches to the
    /// method. `import ClassName` copies it into the importing package so that
    /// `~$obj` / `$obj as $x` resolve to the class's operator overload.
    pub(super) fn register_exported_operator_method_sub(
        &mut self,
        class_name: &str,
        op_name: &str,
        method_param_defs: &[ParamDef],
        tags: Vec<String>,
    ) {
        let mut sub_param_defs: Vec<ParamDef> = Vec::new();
        let mut forward_args: Vec<crate::ast::Expr> = Vec::new();
        // The invocant (`$self:`) turns into the first positional argument, typed
        // to the declaring class so dispatch only fires for that class's values.
        let rest: &[ParamDef] = if method_param_defs.first().is_some_and(|p| p.is_invocant) {
            let mut inv = method_param_defs[0].clone();
            inv.is_invocant = false;
            inv.traits.retain(|t| t != "invocant");
            if inv.type_constraint.is_none() {
                inv.type_constraint = Some(class_name.to_string());
            }
            sub_param_defs.push(inv);
            &method_param_defs[1..]
        } else {
            sub_param_defs.push(ParamDef {
                name: "self".to_string(),
                default: None,
                multi_invocant: true,
                required: true,
                named: false,
                slurpy: false,
                double_slurpy: false,
                onearg: false,
                sigilless: false,
                type_constraint: Some(class_name.to_string()),
                literal_value: None,
                sub_signature: None,
                where_constraint: None,
                traits: Vec::new(),
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
                block_param: false,
            });
            method_param_defs
        };
        let invocant_name = sub_param_defs[0].name.clone();
        for p in rest {
            sub_param_defs.push(p.clone());
            if !p.named && !p.slurpy && !p.is_capture_subsignature() {
                forward_args.push(crate::ast::Expr::Var(p.name.clone()));
            }
        }
        let body = vec![Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Var(invocant_name)),
            name: Symbol::intern(op_name),
            args: forward_args,
            modifier: None,
            quoted: true,
        })];
        let params: Vec<String> = sub_param_defs.iter().map(|p| p.name.clone()).collect();
        let def = FunctionDef {
            is_cached: false,
            package: Symbol::intern(class_name),
            name: Symbol::intern(op_name),
            params,
            param_defs: sub_param_defs.clone(),
            body,
            is_test_assertion: false,
            is_rw: false,
            is_raw: false,
            is_method: false,
            empty_sig: false,
            is_stub: false,
            return_type: None,
            is_default: false,
            deprecated_message: None,
            source_file: self.current_source_file(),
            source_line: None,
            decl_order: crate::runtime::resolution::next_decl_order(),
            compiled: None,
            body_fp_cache: std::sync::OnceLock::new(),
            body_facts_cache: std::sync::OnceLock::new(),
            rw_tail_expr: None,
        };
        // Register as a typed multi candidate under the class package, mirroring
        // the `multi sub` registration keys so `import` copies it and operator
        // dispatch type-checks the invocant.
        let is_positional = |p: &ParamDef| {
            !p.named && (!p.slurpy || p.name == "_capture") && !p.is_capture_subsignature()
        };
        let arity = sub_param_defs.iter().filter(|p| is_positional(p)).count();
        let type_sig: Vec<&str> = sub_param_defs
            .iter()
            .filter(|p| is_positional(p))
            .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
            .collect();
        let arc = std::sync::Arc::new(def);
        if type_sig.iter().any(|t| *t != "Any") {
            let typed_fq = format!(
                "{}::{}/{}:{}",
                class_name,
                op_name,
                arity,
                type_sig.join(",")
            );
            self.registry_mut()
                .functions
                .entry(Symbol::intern(&typed_fq))
                .or_insert_with(|| arc.clone());
        }
        let fq = format!("{}::{}/{}", class_name, op_name, arity);
        self.registry_mut()
            .functions
            .entry(Symbol::intern(&fq))
            .or_insert(arc);
        self.register_exported_sub(class_name.to_string(), op_name.to_string(), tags);
    }
}
