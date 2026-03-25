use super::*;
use crate::value::ArrayKind;

impl Interpreter {
    pub(super) fn sub_call_args_from_value(arg: Option<&Value>) -> Vec<Value> {
        match arg {
            Some(Value::Array(items, _)) => items.to_vec(),
            Some(Value::Nil) | None => Vec::new(),
            Some(other) => vec![other.clone()],
        }
    }

    pub(crate) fn maybe_fetch_rw_proxy(
        &mut self,
        result: Value,
        is_rw: bool,
    ) -> Result<Value, RuntimeError> {
        if !is_rw || self.in_lvalue_assignment {
            return Ok(result);
        }
        if let Value::Proxy {
            fetcher,
            decontainerized,
            ..
        } = result.clone()
        {
            if decontainerized {
                return Ok(result);
            }
            if matches!(fetcher.as_ref(), Value::Nil) {
                return Ok(Value::Nil);
            }
            return self.call_sub_value(*fetcher, vec![result], true);
        }
        Ok(result)
    }

    /// Auto-FETCH a Proxy value. If the value is a Proxy, call its FETCH callback.
    /// Used when a Proxy-bound variable is read in value context.
    pub(crate) fn auto_fetch_proxy(&mut self, value: &Value) -> Result<Value, RuntimeError> {
        if let Value::Proxy { fetcher, .. } = value {
            if matches!(fetcher.as_ref(), Value::Nil) {
                return Ok(Value::Nil);
            }
            return self.call_sub_value(*fetcher.clone(), vec![value.clone()], true);
        }
        Ok(value.clone())
    }

    fn rw_sub_target_expr(body: &[Stmt]) -> Option<Expr> {
        for stmt in body.iter().rev() {
            match stmt {
                Stmt::Expr(expr) | Stmt::Return(expr) => return Some(expr.clone()),
                _ => continue,
            }
        }
        None
    }

    fn is_explicit_return_rw_target(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Call { name, args }
                if name == "return-rw"
                    && args.len() == 1
                    && matches!(&args[0], Expr::Var(_))
        ) || matches!(
            expr,
            Expr::MethodCall {
                target,
                name,
                args,
                ..
            } if name == "return-rw" && args.is_empty() && matches!(target.as_ref(), Expr::Var(_))
        )
    }

    pub(crate) fn assign_proxy_lvalue(
        &mut self,
        proxy: Value,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let Value::Proxy {
            fetcher, storer, ..
        } = proxy.clone()
        else {
            return Err(RuntimeError::new(
                "X::Assignment::RO: target is not assignable",
            ));
        };
        let store_result =
            self.call_sub_value(*storer.clone(), vec![proxy.clone(), value.clone()], true);
        if let Err(err) = store_result {
            if err.message.contains("Too many positionals") {
                self.call_sub_value(*storer.clone(), vec![value.clone()], true)?;
            } else {
                return Err(err);
            }
        }
        // After STORE executes, propagate its closure env changes to FETCH's
        // closure env override so that shared captured variables stay in sync.
        // This is needed because mutsu closures capture environments by value
        // (copy-on-write), so two closures from the same scope diverge on mutation.
        self.sync_proxy_closure_envs(&fetcher, &storer);
        if matches!(fetcher.as_ref(), Value::Nil) {
            return Ok(Value::Nil);
        }
        let fetched = self.call_sub_value(*fetcher.clone(), vec![proxy.clone()], true);
        match fetched {
            Ok(value) => Ok(value),
            Err(err) if err.message.contains("Too many positionals") => {
                let value = self.call_sub_value(*fetcher, Vec::new(), true)?;
                Ok(value)
            }
            Err(err) => Err(err),
        }
    }

    /// Synchronize closure environment overrides between Proxy FETCH and STORE.
    /// After STORE modifies captured variables, propagate those changes to FETCH
    /// so both closures see the same state for shared variables.
    fn sync_proxy_closure_envs(&mut self, fetcher: &Value, storer: &Value) {
        let (Some(fetch_data), Some(store_data)) = (
            match fetcher {
                Value::Sub(d) => Some(d),
                _ => None,
            },
            match storer {
                Value::Sub(d) => Some(d),
                _ => None,
            },
        ) else {
            return;
        };
        let fetch_id = fetch_data.id;
        let store_id = store_data.id;
        // Get the updated STORE env (after call_sub_value persisted it)
        let store_env = match self.closure_env_overrides.get(&store_id) {
            Some(env) => env.clone(),
            None => return,
        };
        // Find variables that are shared between FETCH and STORE captured envs
        let fetch_base = self
            .closure_env_overrides
            .get(&fetch_id)
            .cloned()
            .unwrap_or_else(|| fetch_data.env.clone());
        let mut updated_fetch = fetch_base.clone();
        let mut changed = false;
        for key in fetch_base.keys() {
            // Skip internal/metadata keys
            if key.starts_with("__mutsu_") || key.starts_with("&?") || key == "?LINE" {
                continue;
            }
            if let Some(store_val) = store_env.get(key)
                && fetch_base.get(key) != Some(store_val)
            {
                updated_fetch.insert(key.clone(), store_val.clone());
                changed = true;
            }
        }
        if changed {
            self.closure_env_overrides.insert(fetch_id, updated_fetch);
        }
    }

    fn assign_rw_target_expr(
        &mut self,
        target: &Expr,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        match target {
            Expr::Var(name) => {
                self.env.insert(name.clone(), value.clone());
                Ok(value)
            }
            Expr::Call { name, args } => {
                let mut eval_args = Vec::with_capacity(args.len());
                for arg in args {
                    eval_args.push(self.eval_block_value(&[Stmt::Expr(arg.clone())])?);
                }
                self.assign_named_sub_lvalue_with_values(&name.resolve(), eval_args, value)
            }
            Expr::CallOn { target, args } => {
                let callable = self.eval_block_value(&[Stmt::Expr(*target.clone())])?;
                let mut eval_args = Vec::with_capacity(args.len());
                for arg in args {
                    eval_args.push(self.eval_block_value(&[Stmt::Expr(arg.clone())])?);
                }
                self.assign_callable_lvalue_with_values(callable, eval_args, value)
            }
            Expr::MethodCall {
                target, name, args, ..
            } if name == "return-rw" && args.is_empty() => {
                if let Expr::Var(var_name) = target.as_ref() {
                    self.env.insert(var_name.clone(), value.clone());
                    return Ok(value);
                }
                Err(RuntimeError::new(
                    "X::Assignment::RO: return-rw target is not assignable",
                ))
            }
            _ => Err(RuntimeError::new(
                "X::Assignment::RO: rw sub does not expose an assignable target",
            )),
        }
    }

    pub(super) fn assign_named_sub_lvalue_with_values(
        &mut self,
        name: &str,
        call_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // Perl-style slurp idiom used in roast/t/fudge.t:
        //   local(@ARGV, $/) = $path; <>
        // Preserve support when `local(...) = ...` is lowered as named-sub lvalue assignment.
        if name == "local" {
            self.env
                .insert("ARGV".to_string(), Value::array(vec![value.clone()]));
            self.env.insert("/".to_string(), Value::Nil);
            return Ok(value);
        }

        // substr-rw as a function: substr-rw($str, from, len) = $value
        if name == "substr-rw" && !call_args.is_empty() {
            let target = call_args[0].clone();
            let method_args = call_args[1..].to_vec();
            let target_var = {
                let mut found = None;
                for (k, v) in self.env.iter() {
                    if crate::runtime::values_identical(v, &target) && !k.starts_with("__") {
                        found = Some(k.clone());
                        break;
                    }
                }
                found
            };
            return self.assign_method_lvalue_with_values(
                target_var.as_deref(),
                target,
                "substr-rw",
                method_args,
                value,
            );
        }

        // subbuf-rw as a function: subbuf-rw($buf, from, len) = $value
        if name == "subbuf-rw" && !call_args.is_empty() {
            let target = call_args[0].clone();
            let method_args = call_args[1..].to_vec();
            // We need to find the variable name for the target to update it.
            // Search the env for a variable whose value matches the target by identity.
            let target_var = {
                let mut found = None;
                for (k, v) in self.env.iter() {
                    if crate::runtime::values_identical(v, &target) && !k.starts_with("__") {
                        found = Some(k.clone());
                        break;
                    }
                }
                found
            };
            return self.assign_method_lvalue_with_values(
                target_var.as_deref(),
                target,
                "subbuf-rw",
                method_args,
                value,
            );
        }

        if let Some(def) = self.resolve_function_with_alias(name, &call_args) {
            if let Some(target_expr) = Self::rw_sub_target_expr(&def.body) {
                let allow_target_assign =
                    def.is_rw || Self::is_explicit_return_rw_target(&target_expr);
                if allow_target_assign {
                    match self.assign_rw_target_expr(&target_expr, value.clone()) {
                        Ok(result) => return Ok(result),
                        Err(err) if Self::is_explicit_return_rw_target(&target_expr) => {
                            return Err(err);
                        }
                        Err(_) => {}
                    }
                }
            }
            let was_lvalue = self.in_lvalue_assignment;
            self.in_lvalue_assignment = true;
            let result = self.call_function(name, call_args);
            self.in_lvalue_assignment = was_lvalue;
            let result = result?;

            if def.is_rw
                && let Value::Proxy { .. } = result
            {
                return self.assign_proxy_lvalue(result, value);
            }
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: sub '{}' is not rw",
                name
            )));
        }
        if let Some(err) = self.take_pending_dispatch_error() {
            return Err(err);
        }

        if let Some(callable) = self.env.get(&format!("&{}", name)).cloned() {
            return self.assign_callable_lvalue_with_values(callable, call_args, value);
        }

        Err(RuntimeError::new(format!("Unknown call: {}", name)))
    }

    pub(super) fn assign_callable_lvalue_with_values(
        &mut self,
        callable: Value,
        call_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        match callable {
            Value::Routine { name, .. } => {
                self.assign_named_sub_lvalue_with_values(&name.resolve(), call_args, value)
            }
            Value::Sub(data) => {
                if let Some(target_expr) = Self::rw_sub_target_expr(&data.body) {
                    let allow_target_assign =
                        data.is_rw || Self::is_explicit_return_rw_target(&target_expr);
                    if allow_target_assign {
                        match self.assign_rw_target_expr(&target_expr, value.clone()) {
                            Ok(result) => return Ok(result),
                            Err(err) if Self::is_explicit_return_rw_target(&target_expr) => {
                                return Err(err);
                            }
                            Err(_) => {}
                        }
                    }
                }
                let was_lvalue = self.in_lvalue_assignment;
                self.in_lvalue_assignment = true;
                let result = self.call_sub_value(Value::Sub(data), call_args, true);
                self.in_lvalue_assignment = was_lvalue;
                let result = result?;
                if let Value::Proxy { .. } = result {
                    return self.assign_proxy_lvalue(result, value);
                }
                Err(RuntimeError::assignment_ro(Some("sub is not rw")))
            }
            Value::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => {
                    self.assign_callable_lvalue_with_values(Value::Sub(strong), call_args, value)
                }
                None => Err(RuntimeError::new("Callable has been freed")),
            },
            _ => Err(RuntimeError::assignment_ro(Some(
                "cannot assign through non-callable value",
            ))),
        }
    }

    pub(super) fn builtin_assign_named_sub_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_assign_named_sub_lvalue expects name, call args, and value",
            ));
        }
        let name = args[0].to_string_value();
        let call_args = Self::sub_call_args_from_value(args.get(1));
        let value = args[2].clone();
        self.assign_named_sub_lvalue_with_values(&name, call_args, value)
    }

    pub(super) fn builtin_assign_callable_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_assign_callable_lvalue expects callable, call args, and value",
            ));
        }
        let callable = args[0].clone();
        let call_args = Self::sub_call_args_from_value(args.get(1));
        let value = args[2].clone();
        self.assign_callable_lvalue_with_values(callable, call_args, value)
    }

    pub(super) fn builtin_assignment_ro(&mut self, _args: &[Value]) -> Result<Value, RuntimeError> {
        Err(RuntimeError::assignment_ro(None))
    }

    pub(super) fn builtin_star_lvalue_rhs(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_star_lvalue_rhs expects target name and rhs value",
            ));
        }
        let target_name = args[0].to_string_value();
        let marker_key = format!("__mutsu_bound_array_len::{target_name}");
        let Some(limit) = self.env.get(&marker_key).and_then(|v| match v {
            Value::Int(i) if *i >= 0 => usize::try_from(*i).ok(),
            _ => None,
        }) else {
            return Ok(args[1].clone());
        };

        let mut items = crate::runtime::value_to_list(&args[1]);
        if items.len() > limit {
            items.truncate(limit);
        }
        Ok(Value::real_array(items))
    }

    pub(super) fn builtin_record_bound_array_len(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(
                "__mutsu_record_bound_array_len expects target name",
            ));
        }
        let target_name = args[0].to_string_value();
        if !target_name.starts_with('@') {
            return Ok(Value::Nil);
        }
        let bound_len = self
            .env
            .get(&target_name)
            .map(|v| crate::runtime::value_to_list(v).len() as i64)
            .unwrap_or(0);
        self.env.insert(
            format!("__mutsu_bound_array_len::{target_name}"),
            Value::Int(bound_len),
        );
        Ok(Value::Nil)
    }

    pub(super) fn builtin_record_shaped_array_dims(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(
                "__mutsu_record_shaped_array_dims expects target name",
            ));
        }
        let target_name = args[0].to_string_value();
        if !target_name.starts_with('@') {
            return Ok(Value::Nil);
        }
        let key = format!("__mutsu_shaped_array_dims::{target_name}");
        let dims = self
            .env
            .get(&target_name)
            .and_then(Self::infer_array_shape)
            .filter(|shape| shape.len() > 1);
        if let Some(shape) = dims {
            let dims_val = Value::Array(
                std::sync::Arc::new(shape.into_iter().map(|n| Value::Int(n as i64)).collect()),
                ArrayKind::List,
            );
            self.env.insert(key, dims_val);
        } else {
            self.env.remove(&key);
        }
        Ok(Value::Nil)
    }
}
