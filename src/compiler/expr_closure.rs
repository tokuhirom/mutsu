use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl Compiler {
    /// Compile AnonSub expression.
    pub(super) fn compile_expr_anon_sub(&mut self, body: &[Stmt], is_rw: bool) {
        if is_rw {
            let compiled = self.compile_routine_closure_body(&[], &[], body);
            let cc_idx = self.code.add_closure_code(compiled);
            let idx = self.code.add_stmt(Stmt::SubDecl {
                name: Symbol::intern(""),
                name_expr: None,
                params: Vec::new(),
                param_defs: Vec::new(),
                return_type: None,
                associativity: None,
                precedence_trait: None,
                signature_alternates: Vec::new(),
                body: body.to_vec(),
                multi: false,
                is_rw: true,
                is_raw: false,
                is_export: false,
                export_tags: Vec::new(),
                is_test_assertion: false,
                supersede: false,
                custom_traits: Vec::new(),
            });
            self.code
                .emit(OpCode::MakeAnonSubParams(idx, Some(cc_idx), false));
        } else {
            let placeholders = crate::ast::collect_placeholders(body);
            // Immediate block calls (`{ ... }(...)`) are parsed as AnonSub.
            // Apply the same placeholder conflict checks as block/sub declarations.
            if let Some(err_val) = self.check_placeholder_conflicts(&placeholders, body, None) {
                let idx = self.code.add_constant(err_val);
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die);
                return;
            }
            let compiled = self.compile_routine_closure_body(&placeholders, &[], body);
            let cc_idx = self.code.add_closure_code(compiled);
            let idx = self.code.add_stmt(Stmt::Block(body.to_vec()));
            self.code.emit(OpCode::MakeAnonSub(idx, Some(cc_idx)));
        }
    }

    /// Compile AnonSubParams expression.
    pub(super) fn compile_expr_anon_sub_params(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        return_type: &Option<String>,
        body: &[Stmt],
        is_rw: bool,
        is_whatever_code: bool,
    ) {
        // Validate for placeholder conflicts
        if let Some(err_val) = self.check_placeholder_conflicts(params, body, None) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        // Placeholders cannot appear in blocks/subs with explicit signatures
        {
            let is_placeholder_param = |p: &str| {
                let name = p
                    .strip_prefix('&')
                    .or_else(|| p.strip_prefix('@'))
                    .or_else(|| p.strip_prefix('%'))
                    .unwrap_or(p);
                name.starts_with('^') || name.starts_with(':')
            };
            let has_explicit_sig =
                params.is_empty() || params.iter().all(|p| !is_placeholder_param(p));
            let body_placeholders = crate::ast::collect_placeholders(body);
            if has_explicit_sig && !body_placeholders.is_empty() {
                let ph_name = &body_placeholders[0];
                let display = if let Some(stripped) = ph_name.strip_prefix('^') {
                    format!("$^{}", stripped)
                } else {
                    format!("${}", ph_name)
                };
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Placeholder variable '{}' cannot override existing signature",
                        display
                    )),
                );
                attrs.insert("placeholder".to_string(), Value::str(display));
                let err = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Signature::Placeholder"),
                    attrs,
                );
                let idx = self.code.add_constant(err);
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die);
                return;
            }
        }
        // Compile-time check: assignment to native-typed read-only params
        if let Some(err_val) = Self::check_native_readonly_param_assignment(param_defs, body) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        // Check if this is a pointy block (-> { }) vs a named anonymous sub.
        // Pointy blocks inject a SetLine as the first body statement.
        let is_pointy = body
            .first()
            .is_some_and(|s| matches!(s, crate::ast::Stmt::SetLine(_)));
        let mut compiled = self.compile_routine_closure_body(params, param_defs, body);
        if is_pointy {
            compiled.is_pointy_block = true;
        }
        let cc_idx = self.code.add_closure_code(compiled);
        let idx = self.code.add_stmt(Stmt::SubDecl {
            name: Symbol::intern(""),
            name_expr: None,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            return_type: return_type.clone(),
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body: body.to_vec(),
            multi: false,
            is_rw,
            is_raw: false,
            is_export: false,
            export_tags: Vec::new(),
            is_test_assertion: false,
            supersede: false,
            custom_traits: Vec::new(),
        });
        self.code.emit(OpCode::MakeAnonSubParams(
            idx,
            Some(cc_idx),
            is_whatever_code,
        ));
    }

    /// Compile Lambda expression.
    pub(super) fn compile_expr_lambda(
        &mut self,
        param: &str,
        body: &[Stmt],
        is_whatever_code: bool,
    ) {
        let params: Vec<String> = if param.is_empty() {
            Vec::new()
        } else {
            vec![param.to_string()]
        };
        let compiled = self.compile_closure_body(&params, &[], body);
        let cc_idx = self.code.add_closure_code(compiled);
        let idx = self.code.add_stmt(Stmt::SubDecl {
            name: Symbol::intern(""),
            name_expr: None,
            params: params.clone(),
            param_defs: Vec::new(),
            return_type: None,
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body: body.to_vec(),
            multi: false,
            is_rw: false,
            is_raw: false,
            is_export: false,
            export_tags: Vec::new(),
            is_test_assertion: false,
            supersede: false,
            custom_traits: Vec::new(),
        });
        self.code
            .emit(OpCode::MakeLambda(idx, Some(cc_idx), is_whatever_code));
    }

    /// Compile IndexAssign expression.
    pub(super) fn compile_expr_index_assign(&mut self, target: &Expr, index: &Expr, value: &Expr) {
        if let Expr::BareWord(name) = target
            && name == "s"
            && let Some(pattern) = Self::topic_subst_pattern_from_index(index)
        {
            let rewritten = Expr::AssignExpr {
                name: "_".to_string(),
                expr: Box::new(Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: Symbol::intern("subst"),
                    args: vec![
                        Expr::Literal(Value::Regex(Arc::new(pattern))),
                        Expr::AnonSub {
                            body: vec![Stmt::Expr(value.clone())],
                            is_rw: false,
                        },
                    ],
                    modifier: None,
                    quoted: false,
                }),
            };
            self.compile_expr(&rewritten);
            return;
        }
        if let Expr::PseudoStash(stash_name) = target
            && stash_name == "MY::"
            && let Expr::Literal(Value::Str(key_name)) = index
        {
            self.compile_expr(value);
            let stash_name_idx = self.code.add_constant(Value::str(stash_name.clone()));
            let key_name_idx = self
                .code
                .add_constant(Value::str(key_name.as_ref().clone()));
            self.code.emit(OpCode::IndexAssignPseudoStashNamed {
                stash_name_idx,
                key_name_idx,
            });
            return;
        }
        if let Some(name) = Self::index_assign_target_name(target) {
            if Self::index_assign_target_requires_eval(target) {
                self.compile_expr(target);
                self.code.emit(OpCode::Pop);
            }
            self.compile_expr(value);
            self.compile_expr(index);
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::IndexAssignExprNamed(name_idx));
        } else if let Some((name, inner_index)) = Self::index_assign_nested_target(target) {
            self.compile_expr(value);
            self.compile_expr(index);
            self.compile_expr(inner_index);
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::IndexAssignExprNested(name_idx));
        } else if let Expr::MethodCall {
            target: method_target,
            name: method_name,
            args: method_args,
            ..
        } = target
            && method_args.is_empty()
            && let Some(var_name) = Self::method_call_target_var_name(method_target)
        {
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_index_assign_method_lvalue"),
                args: vec![
                    (**method_target).clone(),
                    Expr::Literal(Value::str(method_name.resolve().to_string())),
                    index.clone(),
                    value.clone(),
                    Expr::Literal(Value::str(var_name)),
                ],
            };
            self.compile_expr(&rewritten);
        } else if let Some(arr_name) = Self::map_rw_identity_target_name(target) {
            // @arr.map(-> $v is rw {$v})[idx] = val  →  @arr[idx] = val
            // When map's closure has an `is rw` parameter and returns it unchanged,
            // the result is a list of containers bound to the original array elements.
            // Assignment through them writes back to the original array.
            self.compile_expr(value);
            self.compile_expr(index);
            let name_idx = self.code.add_constant(Value::str(arr_name));
            self.code.emit(OpCode::IndexAssignExprNamed(name_idx));
        } else {
            // Generic fallback: compile target, then index, then value
            // and emit IndexAssignGeneric to do runtime assignment.
            self.compile_expr(target);
            self.compile_expr(index);
            self.compile_expr(value);
            self.code.emit(OpCode::IndexAssignGeneric);
        }
    }

    /// Compile MultiDimIndexAssign expression.
    pub(super) fn compile_expr_multidim_index_assign(
        &mut self,
        target: &Expr,
        dimensions: &[Expr],
        value: &Expr,
    ) {
        if let Some(var_name) = Self::index_assign_target_name(target) {
            self.compile_expr(value);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            let name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::MultiDimIndexAssign {
                name_idx,
                ndims: dimensions.len() as u32,
            });
        } else {
            // Fallback: compile target, dims, value, use generic handler
            self.compile_expr(target);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            self.compile_expr(value);
            self.code
                .emit(OpCode::MultiDimIndexAssignGeneric(dimensions.len() as u32));
        }
    }
}
