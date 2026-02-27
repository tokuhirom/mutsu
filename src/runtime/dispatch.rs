use super::*;

impl Interpreter {
    pub(super) fn resolve_function_with_alias(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        if let Some(def) = self.resolve_function_with_types(name, arg_values) {
            return Some(def);
        }
        if name.contains(':') || name.contains("::") {
            return None;
        }
        for alias in [format!("prefix:<{name}>"), format!("postfix:<{name}>")] {
            if let Some(def) = self.resolve_function_with_types(&alias, arg_values) {
                return Some(def);
            }
        }
        None
    }

    pub(super) fn resolve_function_with_arity(
        &self,
        name: &str,
        arity: usize,
    ) -> Option<FunctionDef> {
        if name.contains("::") {
            let multi_key = format!("{}/{}", name, arity);
            if let Some(def) = self.functions.get(&multi_key) {
                return Some(def.clone());
            }
            return self.functions.get(name).cloned();
        }
        // Try multi-dispatch with arity first
        let multi_local = format!("{}::{}/{}", self.current_package, name, arity);
        if let Some(def) = self.functions.get(&multi_local) {
            return Some(def.clone());
        }
        let multi_global = format!("GLOBAL::{}/{}", name, arity);
        if let Some(def) = self.functions.get(&multi_global) {
            return Some(def.clone());
        }
        // Fall back to regular lookup
        self.resolve_function(name)
    }

    pub(crate) fn resolve_function_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        let arity = arg_values.len();
        if name.contains("::") {
            let type_sig: Vec<&str> = arg_values
                .iter()
                .map(|v| super::value_type_name(v))
                .collect();
            let typed_key = format!("{}/{}:{}", name, arity, type_sig.join(","));
            if let Some(def) = self.functions.get(&typed_key) {
                return Some(def.clone());
            }
            let prefix = format!("{}/{arity}:", name);
            let candidates: Vec<FunctionDef> = self
                .functions
                .iter()
                .filter(|(key, _)| key.starts_with(&prefix))
                .map(|(_, def)| def.clone())
                .collect();
            for def in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    return Some(def);
                }
            }
            let untyped_key = format!("{}/{}", name, arity);
            let mut untyped_candidates: Vec<(String, FunctionDef)> = self
                .functions
                .iter()
                .filter(|(key, _)| {
                    *key == &untyped_key || key.starts_with(&format!("{}__m", untyped_key))
                })
                .map(|(key, def)| (key.clone(), def.clone()))
                .collect();
            untyped_candidates.sort_by(|a, b| a.0.cmp(&b.0));
            for (_, def) in untyped_candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    return Some(def);
                }
            }
            return self.functions.get(name).cloned();
        }
        let type_sig: Vec<&str> = arg_values
            .iter()
            .map(|v| super::value_type_name(v))
            .collect();
        let typed_key = format!(
            "{}::{}/{}:{}",
            self.current_package,
            name,
            arity,
            type_sig.join(",")
        );
        if let Some(def) = self.functions.get(&typed_key) {
            return Some(def.clone());
        }
        let typed_global = format!("GLOBAL::{}/{}:{}", name, arity, type_sig.join(","));
        if let Some(def) = self.functions.get(&typed_global) {
            return Some(def.clone());
        }
        // Try matching against all typed candidates for this name/arity
        let prefix_local = format!("{}::{}/{}:", self.current_package, name, arity);
        let prefix_global = format!("GLOBAL::{}/{}:", name, arity);
        let candidates: Vec<FunctionDef> = self
            .functions
            .iter()
            .filter(|(key, _)| key.starts_with(&prefix_local) || key.starts_with(&prefix_global))
            .map(|(_, def)| def.clone())
            .collect();
        for def in candidates {
            if self.args_match_param_types(arg_values, &def.param_defs) {
                return Some(def);
            }
        }
        // If there is an untyped-arity slot candidate for this arity, check it too.
        // This covers catch-all multis and where-constrained captures.
        let generic_keys = [
            format!("{}::{}/{}", self.current_package, name, arity),
            format!("GLOBAL::{}/{}", name, arity),
        ];
        let mut found_multi_candidates = false;
        for key in &generic_keys {
            let mut candidates: Vec<(String, FunctionDef)> = self
                .functions
                .iter()
                .filter(|(k, _)| *k == key || k.starts_with(&format!("{}__m", key)))
                .map(|(k, def)| (k.clone(), def.clone()))
                .collect();
            if candidates.len() > 1 {
                found_multi_candidates = true;
            }
            // Sort candidates: those with sub-signatures first (more specific),
            // then by key name for stable ordering
            candidates.sort_by(|a, b| {
                let a_has_subsig = a.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                let b_has_subsig = b.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                b_has_subsig.cmp(&a_has_subsig).then(a.0.cmp(&b.0))
            });
            for (_, def) in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    return Some(def);
                }
            }
        }
        // Try slurpy candidates with different arities (slurpy params accept
        // variable number of args, so the registered arity may differ from call arity).
        let slurpy_prefixes = [
            format!("{}::{}/", self.current_package, name),
            format!("GLOBAL::{}/", name),
        ];
        let mut slurpy_candidates: Vec<(String, FunctionDef)> = self
            .functions
            .iter()
            .filter(|(k, def)| {
                slurpy_prefixes.iter().any(|prefix| k.starts_with(prefix))
                    && def.param_defs.iter().any(|p| p.slurpy)
            })
            .map(|(k, def)| (k.clone(), def.clone()))
            .collect();
        if !slurpy_candidates.is_empty() {
            found_multi_candidates = true;
        }
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        for (_, def) in slurpy_candidates {
            if self.args_match_param_types(arg_values, &def.param_defs) {
                return Some(def);
            }
        }
        // Fall back to arity-only if no proto declared and no multi candidates were found.
        // When multi candidates exist but none matched (e.g., sub-signature arity mismatch),
        // falling back would bypass the sub-signature check.
        if self.has_proto(name) || found_multi_candidates {
            None
        } else {
            self.resolve_function_with_arity(name, arity)
        }
    }

    /// Collect all matching multi dispatch candidates for a function call,
    /// sorted by specificity (most specific first). Used by callsame/nextcallee.
    pub(crate) fn resolve_all_matching_candidates(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Vec<FunctionDef> {
        let arity = arg_values.len();
        let mut all_matches = Vec::new();

        // Collect from typed candidates
        for prefix_base in [
            format!("{}::{}/{}:", self.current_package, name, arity),
            format!("GLOBAL::{}/{}:", name, arity),
        ] {
            let candidates: Vec<FunctionDef> = self
                .functions
                .iter()
                .filter(|(key, _)| key.starts_with(&prefix_base))
                .map(|(_, def)| def.clone())
                .collect();
            for def in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    all_matches.push(def);
                }
            }
        }

        // Collect from generic (untyped) candidates
        let generic_keys = [
            format!("{}::{}/{}", self.current_package, name, arity),
            format!("GLOBAL::{}/{}", name, arity),
        ];
        for key in &generic_keys {
            let mut candidates: Vec<(String, FunctionDef)> = self
                .functions
                .iter()
                .filter(|(k, _)| *k == key || k.starts_with(&format!("{}__m", key)))
                .map(|(k, def)| (k.clone(), def.clone()))
                .collect();
            candidates.sort_by(|a, b| {
                let a_has_subsig = a.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                let b_has_subsig = b.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                b_has_subsig.cmp(&a_has_subsig).then(a.0.cmp(&b.0))
            });
            for (_, def) in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    let fp = crate::ast::function_body_fingerprint(
                        &def.params,
                        &def.param_defs,
                        &def.body,
                    );
                    if !all_matches.iter().any(|m: &FunctionDef| {
                        crate::ast::function_body_fingerprint(&m.params, &m.param_defs, &m.body)
                            == fp
                    }) {
                        all_matches.push(def);
                    }
                }
            }
        }

        // Collect from slurpy candidates
        let slurpy_prefixes = [
            format!("{}::{}/", self.current_package, name),
            format!("GLOBAL::{}/", name),
        ];
        let mut slurpy_candidates: Vec<(String, FunctionDef)> = self
            .functions
            .iter()
            .filter(|(k, def)| {
                slurpy_prefixes.iter().any(|prefix| k.starts_with(prefix))
                    && def.param_defs.iter().any(|p| p.slurpy)
            })
            .map(|(k, def)| (k.clone(), def.clone()))
            .collect();
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        for (_, def) in slurpy_candidates {
            if self.args_match_param_types(arg_values, &def.param_defs) {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if !all_matches.iter().any(|m: &FunctionDef| {
                    crate::ast::function_body_fingerprint(&m.params, &m.param_defs, &m.body) == fp
                }) {
                    all_matches.push(def);
                }
            }
        }

        all_matches
    }

    pub(super) fn eval_token_call_values(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Result<Option<String>, RuntimeError> {
        let defs = match self.resolve_token_defs(name) {
            Some(defs) => defs,
            None => return Ok(None),
        };
        let subject = match self.env.get("_") {
            Some(Value::Str(s)) => Some(s.clone()),
            _ => None,
        };
        let mut best: Option<(usize, String)> = None;
        for def in defs {
            if let Some(pattern) = self.eval_token_def(&def, arg_values)? {
                if let Some(ref text) = subject {
                    if let Some(len) = self.regex_match_len_at_start(&pattern, text) {
                        let better = best
                            .as_ref()
                            .map(|(best_len, _)| len > *best_len)
                            .unwrap_or(true);
                        if better {
                            best = Some((len, pattern));
                        }
                    }
                } else if best.is_none() {
                    best = Some((0, pattern));
                }
            }
        }
        if let Some((_, pattern)) = best {
            return Ok(Some(pattern));
        }
        if self.has_proto_token(name) {
            return Err(RuntimeError::new(format!(
                "No matching candidates for proto token: {}",
                name
            )));
        }
        Ok(None)
    }

    pub(super) fn eval_token_def(
        &mut self,
        def: &FunctionDef,
        arg_values: &[Value],
    ) -> Result<Option<String>, RuntimeError> {
        if def.empty_sig && !arg_values.is_empty() {
            return Err(Self::reject_args_for_empty_sig(arg_values));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        let rw_bindings =
            match self.bind_function_args_values(&def.param_defs, &def.params, arg_values) {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.env = saved_env;
                    self.restore_readonly_vars(saved_readonly);
                    return Err(e);
                }
            };
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        let result = self.eval_block_value(&def.body);
        self.routine_stack.pop();
        let mut restored_env = saved_env;
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
        self.env = restored_env;
        self.restore_readonly_vars(saved_readonly);
        let value = match result {
            Ok(v) => v,
            Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
            Err(e) => return Err(e),
        };
        match value {
            Value::Regex(pat) => Ok(Some(pat)),
            Value::Str(s) => Ok(Some(s)),
            Value::Nil => Ok(None),
            other => Ok(Some(other.to_string_value())),
        }
    }

    pub(crate) fn has_proto(&self, name: &str) -> bool {
        if name.contains("::") {
            return self.proto_subs.contains(name);
        }
        let local = format!("{}::{}", self.current_package, name);
        if self.proto_subs.contains(&local) {
            return true;
        }
        self.proto_subs.contains(&format!("GLOBAL::{}", name))
    }

    /// Check if any multi candidates exist for this function name (any arity).
    pub(crate) fn has_multi_candidates(&self, name: &str) -> bool {
        let prefixes = [
            format!("{}::{}/", self.current_package, name),
            format!("GLOBAL::{}/", name),
        ];
        self.functions
            .keys()
            .any(|k| prefixes.iter().any(|p| k.starts_with(p)))
    }

    pub(super) fn resolve_proto_function_with_alias(
        &self,
        name: &str,
    ) -> Option<(String, FunctionDef)> {
        if let Some(def) = self.resolve_proto_function(name) {
            return Some((name.to_string(), def));
        }
        if name.contains(':') || name.contains("::") {
            return None;
        }
        for alias in [format!("prefix:<{name}>"), format!("postfix:<{name}>")] {
            if let Some(def) = self.resolve_proto_function(&alias) {
                return Some((alias, def));
            }
        }
        None
    }

    fn resolve_proto_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.proto_functions.get(name).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        if let Some(def) = self.proto_functions.get(&local) {
            return Some(def.clone());
        }
        self.proto_functions
            .get(&format!("GLOBAL::{}", name))
            .cloned()
    }

    pub(super) fn call_proto_function(
        &mut self,
        proto_name: &str,
        def: &FunctionDef,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if def.empty_sig && !args.is_empty() {
            return Err(Self::reject_args_for_empty_sig(args));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        let rw_bindings = match self.bind_function_args_values(&def.param_defs, &def.params, args) {
            Ok(bindings) => bindings,
            Err(e) => {
                self.env = saved_env;
                self.restore_readonly_vars(saved_readonly);
                return Err(e);
            }
        };
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        self.proto_dispatch_stack
            .push((proto_name.to_string(), args.to_vec()));
        let result = if def.body.is_empty() {
            // Bodyless proto behaves as implicit {*} dispatch.
            self.call_proto_dispatch()
        } else {
            let rewritten = Self::rewrite_proto_dispatch_stmts(&def.body);
            self.eval_block_value(&rewritten)
        };
        self.proto_dispatch_stack.pop();
        self.routine_stack.pop();
        let mut restored_env = saved_env.clone();
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.restore_env_preserving_existing(&restored_env, &def.params);
        self.restore_readonly_vars(saved_readonly);
        match result {
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            other => other,
        }
    }

    pub(super) fn call_proto_dispatch(&mut self) -> Result<Value, RuntimeError> {
        let (proto_name, args) = self
            .proto_dispatch_stack
            .last()
            .cloned()
            .ok_or_else(|| RuntimeError::new("{*} used outside proto".to_string()))?;
        let Some(def) = self.resolve_proto_candidate_with_types(&proto_name, &args) else {
            let mut err = RuntimeError::new(format!(
                "No matching candidates for proto sub: {}",
                proto_name
            ));
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::Str(format!(
                    "Cannot resolve caller {}; none of these signatures matches",
                    proto_name
                )),
            );
            err.exception = Some(Box::new(Value::make_instance(
                "X::Multi::NoMatch".to_string(),
                attrs,
            )));
            return Err(err);
        };
        if def.empty_sig && !args.is_empty() {
            return Err(Self::reject_args_for_empty_sig(&args));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        let rw_bindings = match self.bind_function_args_values(&def.param_defs, &def.params, &args)
        {
            Ok(bindings) => bindings,
            Err(e) => {
                self.env = saved_env;
                self.restore_readonly_vars(saved_readonly);
                return Err(e);
            }
        };
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        let result = self.run_block(&def.body);
        self.routine_stack.pop();
        let implicit_return = self.env.get("_").cloned().unwrap_or(Value::Nil);
        let mut restored_env = saved_env.clone();
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.restore_env_preserving_existing(&restored_env, &def.params);
        self.restore_readonly_vars(saved_readonly);
        match result {
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
            Ok(()) => Ok(implicit_return),
        }
    }

    fn rewrite_proto_dispatch_stmts(body: &[Stmt]) -> Vec<Stmt> {
        body.iter().map(Self::rewrite_proto_dispatch_stmt).collect()
    }

    fn resolve_proto_candidate_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        let arity = arg_values.len();
        if name.contains("::") {
            let type_sig: Vec<&str> = arg_values
                .iter()
                .map(|v| super::value_type_name(v))
                .collect();
            let typed_key = format!("{}/{}:{}", name, arity, type_sig.join(","));
            if let Some(def) = self.functions.get(&typed_key) {
                return Some(def.clone());
            }
            let prefix = format!("{}/{arity}:", name);
            let candidates: Vec<FunctionDef> = self
                .functions
                .iter()
                .filter(|(key, _)| key.starts_with(&prefix))
                .map(|(_, def)| def.clone())
                .collect();
            for def in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    return Some(def);
                }
            }
            let untyped_key = format!("{}/{}", name, arity);
            if let Some(def) = self.functions.get(&untyped_key).cloned()
                && self.args_match_param_types(arg_values, &def.param_defs)
            {
                return Some(def);
            }
            return None;
        }
        self.resolve_function_with_types(name, arg_values)
    }

    fn rewrite_proto_dispatch_stmt(stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::Expr(Expr::Whatever) => Stmt::Expr(Expr::Call {
                name: "__PROTO_DISPATCH__".to_string(),
                args: Vec::new(),
            }),
            Stmt::Expr(expr) => Stmt::Expr(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Return(expr) => Stmt::Return(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Take(expr) => Stmt::Take(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Die(expr) => Stmt::Die(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Fail(expr) => Stmt::Fail(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
            } => Stmt::VarDecl {
                name: name.clone(),
                expr: Self::rewrite_proto_dispatch_expr(expr),
                type_constraint: type_constraint.clone(),
                is_state: *is_state,
                is_our: *is_our,
                is_dynamic: *is_dynamic,
                is_export: *is_export,
                export_tags: export_tags.clone(),
            },
            Stmt::Assign { name, expr, op } => Stmt::Assign {
                name: name.clone(),
                expr: Self::rewrite_proto_dispatch_expr(expr),
                op: *op,
            },
            Stmt::TempMethodAssign {
                var_name,
                method_name,
                method_args,
                value,
            } => Stmt::TempMethodAssign {
                var_name: var_name.clone(),
                method_name: method_name.clone(),
                method_args: method_args
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
                value: Self::rewrite_proto_dispatch_expr(value),
            },
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            } => Stmt::If {
                cond: Self::rewrite_proto_dispatch_expr(cond),
                then_branch: Self::rewrite_proto_dispatch_stmts(then_branch),
                else_branch: Self::rewrite_proto_dispatch_stmts(else_branch),
                binding_var: binding_var.clone(),
            },
            Stmt::While { cond, body, label } => Stmt::While {
                cond: Self::rewrite_proto_dispatch_expr(cond),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
            },
            Stmt::For {
                iterable,
                body,
                label,
                param,
                param_def,
                params,
            } => Stmt::For {
                iterable: Self::rewrite_proto_dispatch_expr(iterable),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
                param: param.clone(),
                param_def: Box::new((**param_def).clone()),
                params: params.clone(),
            },
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                label,
                repeat,
            } => Stmt::Loop {
                init: init
                    .as_ref()
                    .map(|s| Box::new(Self::rewrite_proto_dispatch_stmt(s))),
                cond: cond.as_ref().map(Self::rewrite_proto_dispatch_expr),
                step: step.as_ref().map(Self::rewrite_proto_dispatch_expr),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
                repeat: *repeat,
            },
            Stmt::Block(stmts) => Stmt::Block(Self::rewrite_proto_dispatch_stmts(stmts)),
            Stmt::SyntheticBlock(stmts) => {
                Stmt::SyntheticBlock(Self::rewrite_proto_dispatch_stmts(stmts))
            }
            other => other.clone(),
        }
    }

    fn rewrite_proto_dispatch_expr(expr: &Expr) -> Expr {
        match expr {
            Expr::AnonSub { body, .. }
                if body.len() == 1 && matches!(body[0], Stmt::Expr(Expr::Whatever)) =>
            {
                Expr::Call {
                    name: "__PROTO_DISPATCH__".to_string(),
                    args: Vec::new(),
                }
            }
            Expr::Unary { op, expr } => Expr::Unary {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::PostfixOp { op, expr } => Expr::PostfixOp {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                op: op.clone(),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
            },
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => Expr::Ternary {
                cond: Box::new(Self::rewrite_proto_dispatch_expr(cond)),
                then_expr: Box::new(Self::rewrite_proto_dispatch_expr(then_expr)),
                else_expr: Box::new(Self::rewrite_proto_dispatch_expr(else_expr)),
            },
            Expr::Call { name, args } => Expr::Call {
                name: name.clone(),
                // Keep call arguments intact so closure literals like `{*}`
                // used as callbacks are not treated as proto dispatch.
                args: args.to_vec(),
            },
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } => Expr::MethodCall {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                name: name.clone(),
                // Same rule as Expr::Call: don't rewrite callback arguments.
                args: args.to_vec(),
                modifier: *modifier,
                quoted: *quoted,
            },
            Expr::ArrayLiteral(items) => Expr::ArrayLiteral(
                items
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
            ),
            Expr::Index { target, index } => Expr::Index {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                index: Box::new(Self::rewrite_proto_dispatch_expr(index)),
            },
            Expr::IndexAssign {
                target,
                index,
                value,
            } => Expr::IndexAssign {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                index: Box::new(Self::rewrite_proto_dispatch_expr(index)),
                value: Box::new(Self::rewrite_proto_dispatch_expr(value)),
            },
            Expr::AssignExpr { name, expr } => Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::Block(stmts) => Expr::Block(Self::rewrite_proto_dispatch_stmts(stmts)),
            Expr::DoBlock { body, label } => Expr::DoBlock {
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
            },
            Expr::Try { body, catch } => Expr::Try {
                body: Self::rewrite_proto_dispatch_stmts(body),
                catch: catch
                    .as_ref()
                    .map(|b| Self::rewrite_proto_dispatch_stmts(b)),
            },
            Expr::Gather(body) => Expr::Gather(Self::rewrite_proto_dispatch_stmts(body)),
            Expr::HyperOp {
                op,
                left,
                right,
                dwim_left,
                dwim_right,
            } => Expr::HyperOp {
                op: op.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
                dwim_left: *dwim_left,
                dwim_right: *dwim_right,
            },
            Expr::MetaOp {
                meta,
                op,
                left,
                right,
            } => Expr::MetaOp {
                meta: meta.clone(),
                op: op.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
            },
            Expr::Reduction { op, expr } => Expr::Reduction {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::InfixFunc {
                name,
                left,
                right,
                modifier,
            } => Expr::InfixFunc {
                name: name.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: right
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
                modifier: modifier.clone(),
            },
            Expr::CallOn { target, args } => Expr::CallOn {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                args: args.to_vec(),
            },
            Expr::Lambda { param, body } => Expr::Lambda {
                param: param.clone(),
                body: Self::rewrite_proto_dispatch_stmts(body),
            },
            Expr::AnonSub { body, is_rw } => Expr::AnonSub {
                body: Self::rewrite_proto_dispatch_stmts(body),
                is_rw: *is_rw,
            },
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw,
            } => Expr::AnonSubParams {
                params: params.clone(),
                param_defs: param_defs.clone(),
                return_type: return_type.clone(),
                body: Self::rewrite_proto_dispatch_stmts(body),
                is_rw: *is_rw,
            },
            other => other.clone(),
        }
    }

    fn restore_env_preserving_existing(
        &mut self,
        saved_env: &std::collections::HashMap<String, Value>,
        params: &[String],
    ) {
        let current = self.env.clone();
        let mut restored = saved_env.clone();
        for key in saved_env.keys() {
            if params.iter().any(|p| p == key) || key == "@_" {
                continue;
            }
            if let Some(v) = current.get(key) {
                restored.insert(key.clone(), v.clone());
            }
        }
        self.env = restored;
    }
}
