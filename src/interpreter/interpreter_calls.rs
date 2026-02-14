use super::*;

impl Interpreter {
    pub(crate) fn exec_call_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        match self.call_function(name, args.clone()) {
            Ok(_) => Ok(()),
            Err(e)
                if e.message
                    .starts_with("Unknown function (call_function fallback disabled):") =>
            {
                let call_args: Vec<CallArg> = args
                    .into_iter()
                    .map(|v| CallArg::Positional(Expr::Literal(v)))
                    .collect();
                self.exec_call(name, &call_args)
            }
            Err(e) => Err(e),
        }
    }

    pub(crate) fn exec_call_mixed_values(
        &mut self,
        name: &str,
        template_args: &[CallArg],
        eval_values: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        let mut value_iter = eval_values.into_iter();
        let mut rebuilt = Vec::with_capacity(template_args.len());
        for arg in template_args {
            match arg {
                CallArg::Positional(expr) => {
                    if Self::call_arg_needs_raw_expr(expr) {
                        rebuilt.push(CallArg::Positional(expr.clone()));
                    } else {
                        let value = value_iter.next().unwrap_or(Value::Nil);
                        rebuilt.push(CallArg::Positional(Expr::Literal(value)));
                    }
                }
                CallArg::Named { name, value } => {
                    let rebuilt_arg = if let Some(expr) = value {
                        if Self::call_arg_needs_raw_expr(expr) {
                            CallArg::Named {
                                name: name.clone(),
                                value: Some(expr.clone()),
                            }
                        } else {
                            let v = value_iter.next().unwrap_or(Value::Nil);
                            CallArg::Named {
                                name: name.clone(),
                                value: Some(Expr::Literal(v)),
                            }
                        }
                    } else {
                        CallArg::Named {
                            name: name.clone(),
                            value: None,
                        }
                    };
                    rebuilt.push(rebuilt_arg);
                }
            }
        }
        self.exec_call(name, &rebuilt)
    }

    pub(super) fn call_arg_needs_raw_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Block(_) => true,
            Expr::Hash(pairs) => pairs
                .iter()
                .any(|(_, v)| v.as_ref().is_some_and(Self::call_arg_needs_raw_expr)),
            _ => false,
        }
    }

    pub(super) fn positional_arg<'a>(
        &self,
        args: &'a [CallArg],
        index: usize,
        message: &str,
    ) -> Result<&'a Expr, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(expr);
                }
                count += 1;
            }
        }
        Err(RuntimeError::new(message))
    }

    pub(super) fn positional_arg_value(
        &mut self,
        args: &[CallArg],
        index: usize,
    ) -> Result<String, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(self.eval_expr(expr)?.to_string_value());
                }
                count += 1;
            }
        }
        Ok(String::new())
    }

    pub(super) fn named_arg_bool(
        &mut self,
        args: &[CallArg],
        name: &str,
    ) -> Result<bool, RuntimeError> {
        for arg in args {
            if let CallArg::Named {
                name: arg_name,
                value,
            } = arg
                && arg_name == name
            {
                if let Some(expr) = value {
                    return Ok(self.eval_expr(expr)?.truthy());
                }
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn named_arg_value(
        &mut self,
        args: &[CallArg],
        name: &str,
    ) -> Result<Option<String>, RuntimeError> {
        for arg in args {
            if let CallArg::Named {
                name: arg_name,
                value,
            } = arg
                && arg_name == name
            {
                if let Some(expr) = value {
                    return Ok(Some(self.eval_expr(expr)?.to_string_value()));
                }
                return Ok(Some(String::new()));
            }
        }
        Ok(None)
    }

    pub(super) fn test_ok(
        &mut self,
        success: bool,
        desc: &str,
        todo: bool,
    ) -> Result<(), RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.ran += 1;
        let forced = state
            .force_todo
            .iter()
            .any(|(start, end)| state.ran >= *start && state.ran <= *end);
        let todo = todo || forced;
        if !success && !todo {
            state.failed += 1;
        }
        let mut line = String::new();
        if success {
            line.push_str("ok ");
        } else {
            line.push_str("not ok ");
        }
        line.push_str(&state.ran.to_string());
        if !desc.is_empty() {
            line.push_str(" - ");
            line.push_str(desc);
        }
        if todo {
            line.push_str(" # TODO");
        }
        line.push('\n');
        self.output.push_str(&line);
        Ok(())
    }

    pub(super) fn rewrite_call_expr(name: &str, args: &[Expr]) -> Option<Expr> {
        if name == "indir"
            && args.len() >= 2
            && let Expr::Block(body) = &args[1]
        {
            let mut rewritten = args.to_vec();
            rewritten[1] = Expr::AnonSub(body.clone());
            return Some(Expr::Call {
                name: name.to_string(),
                args: rewritten,
            });
        }
        None
    }
}
