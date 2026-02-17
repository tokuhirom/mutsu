use super::*;
use crate::token_kind::TokenKind;

impl Interpreter {
    pub(crate) fn eval_call_on_value(
        &mut self,
        target_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Sub {
            package,
            name,
            params,
            body,
            env,
            ..
        } = target_val
        {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in env {
                if matches!(new_env.get(&k), Some(Value::Array(_))) && matches!(v, Value::Array(_))
                {
                    continue;
                }
                new_env.insert(k, v);
            }
            // Bind named params
            for (i, pname) in params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    new_env.insert(pname.clone(), value.clone());
                }
            }
            // Bind placeholder variables ($^a, $^b, ...)
            let placeholders = collect_placeholders(&body);
            if !placeholders.is_empty() {
                for (i, ph) in placeholders.iter().enumerate() {
                    if let Some(val) = args.get(i) {
                        new_env.insert(ph.clone(), val.clone());
                    }
                }
            }
            let block_sub = Value::Sub {
                package: package.clone(),
                name: name.clone(),
                params: params.clone(),
                body: body.clone(),
                env: new_env.clone(),
                id: next_instance_id(),
            };
            self.env = new_env;
            self.routine_stack.push((package.clone(), name.clone()));
            self.block_stack.push(block_sub);
            let result = self.eval_block_value(&body);
            self.block_stack.pop();
            self.routine_stack.pop();
            let mut merged = saved_env;
            for (k, v) in self.env.iter() {
                if matches!(v, Value::Array(_)) {
                    merged.insert(k.clone(), v.clone());
                }
            }
            self.env = merged;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        if matches!(target_val, Value::Routine { .. }) {
            return self.call_sub_value(target_val, args, false);
        }
        Ok(Value::Nil)
    }

    pub(crate) fn call_function(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        crate::trace::trace_log!("call", "call_function: {} ({} args)", name, args.len());
        match name {
            // Error / control flow
            "die" | "fail" => self.builtin_die(&args),
            "exit" => self.builtin_exit(&args),
            // Type coercion
            "Int" | "Num" | "Str" | "Bool" => self.builtin_coerce(name, &args),
            // Grammar helpers
            "make" => self.builtin_make(&args),
            "made" => self.builtin_made(),
            "undefine" => Ok(Value::Nil),
            "VAR" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            // Array operations
            "shift" => self.builtin_shift(&args),
            "pop" => self.builtin_pop(&args),
            "push" | "unshift" | "append" | "prepend" => Ok(Value::Nil),
            // Introspection
            "callframe" => self.builtin_callframe(&args, 0),
            "caller" => self.builtin_callframe(&args, 1),
            // EVAL
            "EVALFILE" => self.builtin_evalfile(&args),
            "EVAL" => self.builtin_eval(&args),
            // Debug
            "dd" => self.builtin_dd(&args),
            // Collection constructors / queries
            "elems" => self.builtin_elems(&args),
            "set" => self.builtin_set(&args),
            "bag" => self.builtin_bag(&args),
            "mix" => self.builtin_mix(&args),
            "hash" => self.builtin_hash(&args),
            "any" | "all" | "one" | "none" => self.builtin_junction(name, args),
            "pair" => self.builtin_pair(&args),
            "keys" => self.builtin_keys(&args),
            "values" => self.builtin_values(&args),
            "abs" => self.builtin_abs(&args),
            "min" => self.builtin_min(&args),
            "max" => self.builtin_max(&args),
            // List operations
            "join" => self.builtin_join(&args),
            "item" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "list" => self.builtin_list(&args),
            "lol" => Ok(Value::Array(args.clone())),
            "flat" => self.builtin_flat(&args),
            "slip" => self.builtin_slip(&args),
            "reverse" => self.builtin_reverse(&args),
            "sort" => self.builtin_sort(&args),
            // Higher-order functions
            "map" => self.builtin_map(&args),
            "grep" => self.builtin_grep(&args),
            "classify" | "categorize" => self.builtin_classify(name, &args),
            // String functions
            "chrs" => self.builtin_chrs(&args),
            "chr" => self.builtin_chr(&args),
            "ord" => self.builtin_ord(&args),
            "ords" => self.builtin_ords(&args),
            "flip" => self.builtin_flip(&args),
            "lc" => self.builtin_lc(&args),
            "uc" => self.builtin_uc(&args),
            "tc" => self.builtin_tc(&args),
            "trim" => self.builtin_trim(&args),
            "chars" => self.builtin_chars(&args),
            "sprintf" => self.builtin_sprintf(&args),
            // File I/O
            "slurp" => self.builtin_slurp(&args),
            "spurt" => self.builtin_spurt(&args),
            "unlink" => self.builtin_unlink(&args),
            "open" => self.builtin_open(&args),
            "close" => self.builtin_close(&args),
            "dir" => self.builtin_dir(&args),
            "copy" => self.builtin_copy(&args),
            "rename" | "move" => self.builtin_rename(&args),
            "chmod" => self.builtin_chmod(&args),
            "mkdir" => self.builtin_mkdir(&args),
            "rmdir" => self.builtin_rmdir(&args),
            "chdir" => self.builtin_chdir(&args),
            "indir" => self.builtin_indir(&args),
            "tmpdir" => self.builtin_tmpdir(&args),
            "homedir" => self.builtin_homedir(&args),
            "link" => self.builtin_link(&args),
            "symlink" => self.builtin_symlink(&args),
            // I/O functions
            "print" | "say" | "note" | "warn" => self.builtin_print(name, &args),
            "sink" => Ok(Value::Nil), // sink evaluates args (already done) and returns Nil
            "prompt" => self.builtin_prompt(&args),
            "get" => self.builtin_get(&args),
            "lines" => self.builtin_lines(&args),
            "words" => self.builtin_words(&args),
            // System
            "getlogin" => Ok(Value::Str(Self::get_login_name().unwrap_or_default())),
            "gethost" => self.builtin_gethost(&args),
            "chroot" => self.builtin_chroot(&args),
            "shell" => self.builtin_shell(&args),
            "kill" => self.builtin_kill(&args),
            "syscall" => self.builtin_syscall(&args),
            "sleep" => self.builtin_sleep(&args),
            "sleep-timer" => self.builtin_sleep_timer(&args),
            "sleep-till" => self.builtin_sleep_till(&args),
            // Fallback
            _ => self.call_function_fallback(name, &args),
        }
    }

    fn call_function_fallback(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(op) = name
            .strip_prefix("infix:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            return self.call_infix_routine(op, args);
        }
        if (self.loaded_modules.contains("Test")
            || self.loaded_modules.iter().any(|m| m.starts_with("Test::")))
            && let Some(result) = self.call_test_function(name, args)?
        {
            return Ok(result);
        }
        if let Some(pattern) = self.eval_token_call_values(name, args)? {
            return Ok(Value::Regex(pattern));
        }
        if let Some(native_result) = crate::builtins::native_function(name, args) {
            return native_result;
        }
        if let Some(def) = self.resolve_function_with_alias(name, args) {
            let saved_env = self.env.clone();
            self.bind_function_args_values(&def.param_defs, &def.params, args)?;
            self.routine_stack
                .push((def.package.clone(), def.name.clone()));
            let result = self.eval_block_value(&def.body);
            self.routine_stack.pop();
            self.env = saved_env;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        if self.has_proto(name) {
            return Err(RuntimeError::new(format!(
                "No matching candidates for proto sub: {}",
                name
            )));
        }
        if let Some(callable) = self
            .env
            .get(name)
            .cloned()
            .or_else(|| self.env.get(&format!("&{}", name)).cloned())
            && matches!(callable, Value::Sub { .. } | Value::Routine { .. })
        {
            return self.eval_call_on_value(callable, args.to_vec());
        }

        Err(RuntimeError::new(format!(
            "X::Undeclared: Unknown function (call_function fallback disabled): {}",
            name
        )))
    }

    fn call_infix_routine(&mut self, op: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(reduction_identity(op));
        }
        if args.len() == 1 {
            return Ok(args[0].clone());
        }
        let mut acc = args[0].clone();
        for rhs in &args[1..] {
            acc =
                self.eval_block_value(&[Stmt::Expr(Self::build_infix_expr(op, acc, rhs.clone()))])?;
        }
        Ok(acc)
    }

    fn build_infix_expr(op: &str, left: Value, right: Value) -> Expr {
        if let Some(inner) = op.strip_prefix("![")
            && let Some(inner) = inner.strip_suffix(']')
        {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr(inner, left, right)),
            };
        }
        if let Some(inner) = op.strip_prefix('!')
            && !inner.is_empty()
        {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr(inner, left, right)),
            };
        }
        Expr::Binary {
            left: Box::new(Expr::Literal(left)),
            op: Self::infix_token(op),
            right: Box::new(Expr::Literal(right)),
        }
    }

    fn infix_token(op: &str) -> TokenKind {
        match op {
            "+" => TokenKind::Plus,
            "-" => TokenKind::Minus,
            "*" | "×" => TokenKind::Star,
            "/" | "÷" => TokenKind::Slash,
            "**" => TokenKind::StarStar,
            "%" => TokenKind::Percent,
            "%%" => TokenKind::PercentPercent,
            "!%%" => TokenKind::BangPercentPercent,
            "==" => TokenKind::EqEq,
            "=" => TokenKind::EqEq,
            "!=" => TokenKind::BangEq,
            "===" | "=:=" => TokenKind::EqEqEq,
            "~" => TokenKind::Tilde,
            "+&" => TokenKind::BitAnd,
            "+|" => TokenKind::BitOr,
            "+^" => TokenKind::BitXor,
            "(|)" | "∪" => TokenKind::SetUnion,
            "(&)" | "∩" => TokenKind::SetIntersect,
            "(^)" | "⊖" => TokenKind::SetSymDiff,
            "(elem)" | "∈" => TokenKind::SetElem,
            "(cont)" | "∋" => TokenKind::SetCont,
            _ => TokenKind::Ident(op.to_string()),
        }
    }

    fn builtin_die(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let msg = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "Died".to_string());
        Err(RuntimeError::new(&msg))
    }

    fn builtin_exit(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let _code = match args.first() {
            Some(Value::Int(i)) => *i,
            _ => 0,
        };
        self.halted = true;
        Ok(Value::Nil)
    }

    fn builtin_make(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        self.env.insert("made".to_string(), value.clone());
        Ok(value)
    }

    fn builtin_made(&self) -> Result<Value, RuntimeError> {
        Ok(self.env.get("made").cloned().unwrap_or(Value::Nil))
    }

    fn builtin_callframe(
        &self,
        args: &[Value],
        default_depth: usize,
    ) -> Result<Value, RuntimeError> {
        let depth = args
            .first()
            .and_then(|v| match v {
                Value::Int(i) if *i >= 0 => Some(*i as usize),
                Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                _ => None,
            })
            .unwrap_or(default_depth);
        if let Some(frame) = self.callframe_value(depth) {
            return Ok(frame);
        }
        Ok(Value::Nil)
    }

    fn builtin_evalfile(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("EVALFILE requires a filename"))?;
        let code = fs::read_to_string(&path)
            .map_err(|err| RuntimeError::new(format!("Failed to read {}: {}", path, err)))?;
        self.eval_eval_string(&code)
    }

    fn builtin_eval(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if code.contains("&?ROUTINE") && self.routine_stack.is_empty() {
            return Err(RuntimeError::new("X::Undeclared::Symbols"));
        }
        self.eval_eval_string(&code)
    }

    fn builtin_dd(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned().unwrap_or(Value::Nil);
        self.output.push_str(&format!("{:?}\n", val));
        Ok(val)
    }

    pub(super) fn is_builtin_function(name: &str) -> bool {
        matches!(
            name,
            "defined"
                | "undefine"
                | "say"
                | "print"
                | "put"
                | "note"
                | "die"
                | "warn"
                | "sink"
                | "exit"
                | "abs"
                | "sqrt"
                | "floor"
                | "ceiling"
                | "ceil"
                | "round"
                | "exp"
                | "log"
                | "sin"
                | "cos"
                | "tan"
                | "asin"
                | "acos"
                | "atan"
                | "chr"
                | "ord"
                | "chars"
                | "chomp"
                | "chop"
                | "flip"
                | "lc"
                | "uc"
                | "tc"
                | "trim"
                | "elems"
                | "keys"
                | "values"
                | "pairs"
                | "sort"
                | "reverse"
                | "join"
                | "map"
                | "grep"
                | "push"
                | "pop"
                | "shift"
                | "unshift"
                | "indir"
                | "splice"
                | "flat"
                | "unique"
                | "squish"
                | "min"
                | "max"
                | "sum"
                | "any"
                | "all"
                | "none"
                | "one"
                | "so"
                | "not"
                | "truncate"
        )
    }

    pub(super) fn call_lambda_with_arg(
        &mut self,
        func: &Value,
        item: Value,
    ) -> Result<Value, RuntimeError> {
        if let Value::Sub {
            params, body, env, ..
        } = func
        {
            let saved_env = self.env.clone();
            for (k, v) in env {
                self.env.insert(k.clone(), v.clone());
            }
            if let Some(p) = params.first() {
                self.env.insert(p.clone(), item.clone());
            }
            let placeholders = collect_placeholders(body);
            if let Some(ph) = placeholders.first() {
                self.env.insert(ph.clone(), item.clone());
            }
            self.env.insert("_".to_string(), item.clone());
            let result = self.eval_block_value(body);
            self.env = saved_env;
            return result;
        }
        Err(RuntimeError::new("Expected callable"))
    }
}
