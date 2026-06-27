use super::*;

impl Interpreter {
    /// Whether a named parameter is a plain readonly scalar `$` param eligible
    /// for container sharing (Slice 2d named follow-up). A scalar named param is
    /// stored sigil-less in `ParamDef::name` (`:$n` -> "n"), unlike `:@l`/`:%h`
    /// which keep their sigil; exclude attribute twigils (`:$!x` -> "!x"), `$_`,
    /// `is copy`/`is rw`/`is raw`, slurpy, and sub-signature params.
    pub(crate) fn named_scalar_container_share_eligible(&self, pd: &crate::ast::ParamDef) -> bool {
        !pd.traits
            .iter()
            .any(|t| t == "copy" || t == "rw" || t == "raw")
            && !pd.slurpy
            && !pd.double_slurpy
            && pd.sub_signature.is_none()
            && pd.name != "_"
            && !pd.name.starts_with(['@', '%', '&', '!', '.'])
            && pd
                .name
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
    }

    /// Evaluate a parameter's default expression under Raku's parameter
    /// scoping: the parameter being bound is in scope (as its undefined type
    /// object) *within its own default*, shadowing any outer symbol of the same
    /// name. This is why `sub f(&foo = &foo)` defaults to the (undefined)
    /// parameter — not the outer routine `foo` — and `$x = $x` is undefined.
    /// Earlier parameters remain visible because they were bound in prior loop
    /// iterations. The topic (`$_`/`_`) is saved/restored so the eval does not
    /// leak it into the caller's scope.
    pub(crate) fn eval_param_default(
        &mut self,
        pd: &ParamDef,
        default_expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        let saved_topic = self.env.get("_").cloned();
        let saved_dollar_topic = self.env.get("$_").cloned();
        // Shadow the parameter with its undefined type object (or Nil) so a
        // self-reference in the default resolves to the parameter rather than
        // an outer symbol of the same name.
        let saved_self = if !pd.name.is_empty() {
            let prev = self.env.get(&pd.name).cloned();
            let shadow = pd
                .type_constraint
                .as_deref()
                .filter(|c| !c.starts_with("::"))
                .and_then(|c| self.resolve_type_object(c))
                .unwrap_or(Value::Nil);
            self.env.insert(pd.name.clone(), shadow);
            Some(prev)
        } else {
            None
        };
        let value = self.eval_block_value(&[Stmt::Expr(default_expr.clone())]);
        // Restore the parameter's prior env binding; the caller performs the
        // real bind right after. On the error path this prevents the shadow
        // from leaking.
        if let Some(prev) = saved_self {
            match prev {
                Some(v) => {
                    self.env.insert(pd.name.clone(), v);
                }
                None => {
                    self.env.remove(&pd.name);
                }
            }
        }
        if let Some(t) = saved_topic {
            self.env.insert("_".to_string(), t);
        } else {
            self.env.remove("_");
        }
        if let Some(t) = saved_dollar_topic {
            self.env.insert("$_".to_string(), t);
        } else {
            self.env.remove("$_");
        }
        value
    }

    /// Compute the implicit `*%_` slurpy of a method: a Hash of the named
    /// arguments not consumed by an explicit named parameter (an empty Hash when
    /// there are none). Every method exposes `%_` so its body can reference it --
    /// e.g. forwarding `|%_` -- without declaring it, and so `%_` reads as an
    /// empty Hash rather than `Any` (which would otherwise splat as a stray
    /// positional). A method that declares its own hash slurpy (`*%foo`)
    /// replaces `*%_`; callers skip this then.
    pub(crate) fn implicit_method_named_slurpy(param_defs: &[ParamDef], args: &[Value]) -> Value {
        let mut implicit_named = std::collections::HashMap::new();
        for arg in args.iter() {
            if let Value::Pair(key, val) = unwrap_varref_value(arg.clone()) {
                if key.is_empty() {
                    continue;
                }
                let consumed = param_defs.iter().any(|pd| {
                    (pd.named && pd.name == key)
                        || pd.name == format!(":{}", key)
                        || (pd.named
                            && (pd.name == format!("@{}", key) || pd.name == format!("%{}", key)))
                });
                if !consumed {
                    implicit_named.insert(key.to_string(), *val);
                }
            }
        }
        Value::hash(implicit_named)
    }

    /// Check shape constraint for array parameters in signatures.
    pub(crate) fn check_shape_constraint(
        &mut self,
        param_name: &str,
        value: &Value,
        shape_exprs: &[Expr],
        all_args: &[Value],
    ) -> Result<(), RuntimeError> {
        let _ = all_args; // reserved for future use
        let actual_shape = crate::runtime::utils::shaped_array_shape(value);

        // Evaluate expected dimensions from shape expressions
        let mut expected_dims: Vec<Option<usize>> = Vec::new();
        for expr in shape_exprs {
            match expr {
                Expr::Whatever | Expr::HyperWhatever => {
                    // * means any size for this dimension
                    expected_dims.push(None);
                }
                _ => {
                    let dim_val = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                    match &dim_val {
                        Value::Whatever | Value::HyperWhatever => {
                            expected_dims.push(None);
                        }
                        Value::Int(n) => {
                            expected_dims.push(Some(*n as usize));
                        }
                        Value::BigInt(n) => {
                            use num_traits::ToPrimitive;
                            expected_dims.push(Some(n.to_usize().unwrap_or(0)));
                        }
                        Value::Num(n) if n.is_infinite() || n.is_nan() => {
                            // Inf/NaN means wildcard (e.g. * coerced to Inf)
                            expected_dims.push(None);
                        }
                        Value::Num(n) => {
                            expected_dims.push(Some(*n as usize));
                        }
                        _ => {
                            let coerced = crate::runtime::utils::coerce_to_numeric(dim_val);
                            match &coerced {
                                Value::Int(n) => expected_dims.push(Some(*n as usize)),
                                Value::Num(n) if n.is_infinite() || n.is_nan() => {
                                    expected_dims.push(None);
                                }
                                _ => expected_dims.push(None),
                            }
                        }
                    }
                }
            }
        }

        // Get actual shape
        let actual = match &actual_shape {
            Some(shape) => shape.clone(),
            None => {
                // Unshaped array - reject unless shape constraint is just [*]
                // (single wildcard dimension accepts unshaped arrays)
                if expected_dims.len() == 1 && expected_dims[0].is_none() {
                    return Ok(());
                }
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected shaped array with {} dimension(s)",
                    param_name,
                    expected_dims.len()
                )));
            }
        };

        // Check number of dimensions matches
        if actual.len() != expected_dims.len() {
            return Err(RuntimeError::new(format!(
                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {} dimension(s), got {}",
                param_name,
                expected_dims.len(),
                actual.len()
            )));
        }

        // Check each dimension
        for (i, (expected, &actual_dim)) in expected_dims.iter().zip(actual.iter()).enumerate() {
            if let Some(expected_dim) = expected
                && *expected_dim != actual_dim
            {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; dimension {} expected {}, got {}",
                    param_name, i, expected_dim, actual_dim
                )));
            }
        }

        Ok(())
    }
}
