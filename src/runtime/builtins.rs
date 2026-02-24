use super::*;
use crate::token_kind::TokenKind;

enum OpAssoc {
    Left,
    Right,
    Chain,
}

impl Interpreter {
    fn coerce_to_enum_variant(
        &mut self,
        enum_name: &str,
        variants: &[(String, i64)],
        value: Value,
    ) -> Option<Value> {
        let by_index = |idx: usize| -> Option<Value> {
            variants.get(idx).map(|(key, val)| Value::Enum {
                enum_type: enum_name.to_string(),
                key: key.clone(),
                value: *val,
                index: idx,
            })
        };

        match value {
            Value::Enum {
                enum_type, index, ..
            } if enum_type == enum_name => by_index(index),
            Value::Enum { value, .. } => variants
                .iter()
                .enumerate()
                .find(|(_, (_, v))| *v == value)
                .and_then(|(idx, _)| by_index(idx)),
            Value::Int(int_value) => variants
                .iter()
                .enumerate()
                .find(|(_, (_, v))| *v == int_value)
                .and_then(|(idx, _)| by_index(idx)),
            Value::Num(num_value) => {
                if num_value.fract() == 0.0 {
                    let int_value = num_value as i64;
                    variants
                        .iter()
                        .enumerate()
                        .find(|(_, (_, v))| *v == int_value)
                        .and_then(|(idx, _)| by_index(idx))
                } else {
                    None
                }
            }
            Value::Str(name) => variants
                .iter()
                .enumerate()
                .find(|(_, (key, _))| *key == name)
                .and_then(|(idx, _)| by_index(idx)),
            other => self
                .call_method_with_values(other, enum_name, vec![])
                .ok()
                .and_then(|resolved| self.coerce_to_enum_variant(enum_name, variants, resolved)),
        }
    }

    pub(crate) fn eval_call_on_value(
        &mut self,
        target_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Upgrade WeakSub to Sub transparently
        let target_val = match target_val {
            Value::WeakSub(ref weak) => match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => return Err(RuntimeError::new("Callable has been freed")),
            },
            other => other,
        };
        if let Value::Sub(data) = target_val {
            let mut call_args = args.clone();
            if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
                let mut positional = data.assumed_positional.clone();
                let mut named = data.assumed_named.clone();
                for arg in args {
                    if let Value::Pair(key, boxed) = arg {
                        named.insert(key, *boxed);
                    } else {
                        positional.push(arg);
                    }
                }
                call_args = positional;
                for (key, value) in named {
                    call_args.push(Value::Pair(key, Box::new(value)));
                }
            }
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in &data.env {
                if matches!(new_env.get(k), Some(Value::Array(..))) && matches!(v, Value::Array(..))
                {
                    continue;
                }
                new_env.insert(k.clone(), v.clone());
            }
            self.env = new_env.clone();
            self.bind_function_args_values(&data.param_defs, &data.params, &call_args)?;
            new_env = self.env.clone();
            // Bind implicit $_ for bare blocks called with arguments
            if data.params.is_empty() && !call_args.is_empty() {
                new_env.insert("_".to_string(), call_args[0].clone());
            }
            // &?BLOCK: weak self-reference to break reference cycles
            let block_arc = std::sync::Arc::new(crate::value::SubData {
                package: data.package.clone(),
                name: data.name.clone(),
                params: data.params.clone(),
                param_defs: data.param_defs.clone(),
                body: data.body.clone(),
                env: new_env.clone(),
                assumed_positional: data.assumed_positional.clone(),
                assumed_named: data.assumed_named.clone(),
                id: crate::value::next_instance_id(),
            });
            new_env.insert(
                "&?BLOCK".to_string(),
                Value::WeakSub(std::sync::Arc::downgrade(&block_arc)),
            );
            let block_sub = Value::make_sub(
                data.package.clone(),
                data.name.clone(),
                data.params.clone(),
                data.param_defs.clone(),
                data.body.clone(),
                new_env.clone(),
            );
            self.env = new_env;
            self.routine_stack
                .push((data.package.clone(), data.name.clone()));
            self.block_stack.push(block_sub);
            let result = self.eval_block_value(&data.body);
            self.block_stack.pop();
            self.routine_stack.pop();
            let mut merged = saved_env;
            for (k, v) in self.env.iter() {
                if matches!(v, Value::Array(..)) {
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
        let (args, callsite_line) = self.sanitize_call_args(&args);
        self.test_pending_callsite_line = callsite_line;
        crate::trace::trace_log!("call", "call_function: {} ({} args)", name, args.len());
        match name {
            // Error / control flow
            "die" => self.builtin_die(&args),
            "fail" => self.builtin_fail(&args),
            "return-rw" => self.builtin_return_rw(&args),
            "__mutsu_assign_method_lvalue" => self.builtin_assign_method_lvalue(&args),
            "__mutsu_stub_die" => self.builtin_stub_die(&args),
            "__mutsu_stub_warn" => self.builtin_stub_warn(&args),
            "exit" => self.builtin_exit(&args),
            "__PROTO_DISPATCH__" => self.call_proto_dispatch(),
            // Type coercion
            "Int" | "Num" | "Str" | "Bool" => self.builtin_coerce(name, &args),
            // Grammar helpers
            "make" => self.builtin_make(&args),
            "made" => self.builtin_made(),
            "undefine" => Ok(Value::Nil),
            "VAR" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "HOW" => {
                if args.len() != 1 {
                    return Err(RuntimeError::new(
                        "X::Syntax::Argument::MOPMacro: HOW expects exactly one argument",
                    ));
                }
                self.call_method_with_values(args[0].clone(), "HOW", vec![])
            }
            "__MUTSU_SET_META__" => {
                if args.len() < 3 {
                    return Ok(Value::Nil);
                }
                let type_name = args[0].to_string_value();
                let key = args[1].to_string_value();
                let value = args[2].clone();
                self.type_metadata
                    .entry(type_name)
                    .or_default()
                    .insert(key, value);
                Ok(Value::Nil)
            }
            "__mutsu_set_newline" => {
                let pair = args.first().cloned().unwrap_or(Value::Nil);
                let Value::Pair(name, value) = pair else {
                    return Err(RuntimeError::new(
                        "use newline expects a colonpair argument",
                    ));
                };
                if !value.truthy() {
                    return Err(RuntimeError::new("use newline expects a true mode adverb"));
                }
                self.newline_mode = match name.as_str() {
                    "lf" => NewlineMode::Lf,
                    "cr" => NewlineMode::Cr,
                    "crlf" => NewlineMode::Crlf,
                    _ => {
                        return Err(RuntimeError::new(format!("Unknown newline mode: {}", name)));
                    }
                };
                Ok(Value::Nil)
            }
            "BEGIN" => {
                let Some(first) = args.first().cloned() else {
                    return Ok(Value::Nil);
                };
                match first {
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                        self.call_sub_value(first, Vec::new(), false)
                    }
                    other => Ok(other),
                }
            }
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
            "cross" => self.builtin_cross(args),
            "roundrobin" => self.builtin_roundrobin(&args),
            // List operations
            "join" => self.builtin_join(&args),
            "item" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "list" => self.builtin_list(&args),
            "lol" => Ok(Value::array(args.clone())),
            "flat" => self.builtin_flat(&args),
            "slip" | "Slip" => self.builtin_slip(&args),
            "reverse" => self.builtin_reverse(&args),
            "sort" => self.builtin_sort(&args),
            // Higher-order functions
            "map" => self.builtin_map(&args),
            "grep" => self.builtin_grep(&args),
            "classify" | "categorize" => self.builtin_classify(name, &args),
            // String functions
            "index" => {
                if args.is_empty() {
                    return Err(RuntimeError::new("Too few positionals passed to 'index'"));
                }
                let target = args[0].clone();
                let method_args = args[1..].to_vec();
                // Junction auto-threading on first argument
                if let Value::Junction { kind, values } = &target {
                    let kind = kind.clone();
                    let mut results = Vec::new();
                    for v in values.iter() {
                        results.push(self.call_method_with_values(
                            v.clone(),
                            "index",
                            method_args.clone(),
                        )?);
                    }
                    return Ok(Value::Junction {
                        kind,
                        values: std::sync::Arc::new(results),
                    });
                }
                self.call_method_with_values(target, "index", method_args)
            }
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
            "warn" => self.builtin_warn(&args),
            "print" | "say" | "put" | "note" => self.builtin_print(name, &args),
            "sink" => Ok(Value::Nil), // sink evaluates args (already done) and returns Nil
            "quietly" => {
                let Some(first) = args.first().cloned() else {
                    return Ok(Value::Nil);
                };
                match first {
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                        self.push_warn_suppression();
                        let result = self.call_sub_value(first, Vec::new(), false);
                        self.pop_warn_suppression();
                        match result {
                            Err(e) if e.is_warn => Ok(Value::Nil),
                            other => other,
                        }
                    }
                    other => Ok(other),
                }
            }
            "prompt" => self.builtin_prompt(&args),
            "get" => self.builtin_get(&args),
            "lines" => self.builtin_lines(&args),
            "words" => self.builtin_words(&args),
            // System
            "getlogin" => Ok(Value::Str(Self::get_login_name().unwrap_or_default())),
            "gethost" => self.builtin_gethost(&args),
            "chroot" => self.builtin_chroot(&args),
            "run" => self.builtin_run(&args),
            "shell" => self.builtin_shell(&args),
            "kill" => self.builtin_kill(&args),
            "syscall" => self.builtin_syscall(&args),
            "sleep" => self.builtin_sleep(&args),
            "sleep-timer" => self.builtin_sleep_timer(&args),
            "sleep-till" => self.builtin_sleep_till(&args),
            // Concurrency (single-threaded simulation)
            "start" => self.builtin_start(args),
            "await" => self.builtin_await(&args),
            // Boolean coercion functions
            "not" => Ok(Value::Bool(!args.first().unwrap_or(&Value::Nil).truthy())),
            "so" => Ok(Value::Bool(args.first().unwrap_or(&Value::Nil).truthy())),
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
        if let Some(op) = name
            .strip_prefix("prefix:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            if args.is_empty() {
                return Ok(Value::Nil);
            }
            let arg = &args[0];
            return match op {
                "!" => Ok(Value::Bool(!arg.truthy())),
                "+" => Ok(Value::Int(crate::runtime::to_int(arg))),
                "-" => crate::builtins::arith_negate(arg.clone()),
                "~" => Ok(Value::Str(crate::runtime::utils::coerce_to_str(arg))),
                "?" => Ok(Value::Bool(arg.truthy())),
                "so" => Ok(Value::Bool(arg.truthy())),
                "not" => Ok(Value::Bool(!arg.truthy())),
                _ => Err(RuntimeError::new(format!(
                    "Unknown prefix operator: {}",
                    op
                ))),
            };
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
        if let Some(variants) = self.enum_types.get(name).cloned() {
            let Some(first) = args.first().cloned() else {
                return Ok(Value::Nil);
            };
            if let Some(enum_value) = self.coerce_to_enum_variant(name, &variants, first) {
                return Ok(enum_value);
            }
            return Ok(Value::Nil);
        }
        // Handle zip:with — zip with a custom combining function
        if name == "zip"
            && args
                .iter()
                .any(|a| matches!(a, Value::Pair(k, _) if k == "with"))
        {
            return self.builtin_zip_with(args);
        }
        if let Some(native_result) = crate::builtins::native_function(name, args) {
            return native_result;
        }
        // Coerce user-defined types for builtin functions via .Numeric/.Bridge
        if Self::is_builtin_function(name)
            && args.iter().any(|a| matches!(a, Value::Instance { .. }))
        {
            let mut coerced_args: Vec<Value> = Vec::with_capacity(args.len());
            let mut all_ok = true;
            for arg in args {
                if matches!(arg, Value::Instance { .. }) {
                    let coerced = self
                        .call_method_with_values(arg.clone(), "Numeric", vec![])
                        .or_else(|_| self.call_method_with_values(arg.clone(), "Bridge", vec![]));
                    match coerced {
                        Ok(val) => coerced_args.push(val),
                        Err(_) => {
                            all_ok = false;
                            break;
                        }
                    }
                } else {
                    coerced_args.push(arg.clone());
                }
            }
            if all_ok
                && let Some(native_result) = crate::builtins::native_function(name, &coerced_args)
            {
                return native_result;
            }
        }
        if let Some((proto_name, proto_def)) = self.resolve_proto_function_with_alias(name) {
            return self.call_proto_function(&proto_name, &proto_def, args);
        }
        if let Some(def) = self.resolve_function_with_alias(name, args) {
            let saved_env = self.env.clone();
            self.bind_function_args_values(&def.param_defs, &def.params, args)?;
            let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
            self.routine_stack
                .push((def.package.clone(), def.name.clone()));
            let result = self.eval_block_value(&def.body);
            self.routine_stack.pop();
            self.pop_test_assertion_context(pushed_assertion);
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
        let callable_from_code_sigil = self.env.get(&format!("&{}", name)).cloned();
        let callable_from_plain = self.env.get(name).cloned();
        if let Some(callable) = callable_from_code_sigil
            .filter(|v| matches!(v, Value::Sub(_) | Value::Routine { .. }))
            .or_else(|| {
                callable_from_plain.filter(|v| matches!(v, Value::Sub(_) | Value::Routine { .. }))
            })
        {
            return self.eval_call_on_value(callable, args.to_vec());
        }
        if self.has_role(name) {
            return Ok(Value::Pair(
                name.to_string(),
                Box::new(Value::array(args.to_vec())),
            ));
        }
        if name.starts_with("X::") {
            return Ok(Value::Package(name.to_string()));
        }

        Err(RuntimeError::new(format!(
            "X::Undeclared::Symbols: Unknown function: {}",
            name
        )))
    }

    fn call_infix_routine(&mut self, op: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() == 1 && matches!(op, "=:=" | "===" | "eqv") {
            return Ok(Value::Bool(true));
        }
        // 1-arg Iterable gets flattened (like +@foo slurpy)
        let args: Vec<Value> = if args.len() == 1 {
            match &args[0] {
                Value::Array(items, ..) => items.to_vec(),
                _ => return Ok(args[0].clone()),
            }
        } else {
            args.to_vec()
        };
        if args.is_empty() {
            return Ok(reduction_identity(op));
        }
        if args.len() == 1 {
            return Ok(args[0].clone());
        }
        // Sequence operators: dispatch directly to eval_sequence
        match op {
            "..." | "...^" | "^..." | "^...^" => {
                let exclude_end = op == "...^" || op == "^...^";
                let exclude_start = op.starts_with('^');

                let mut result = if args.len() > 2 {
                    // Chained sequence: multiple waypoints
                    // args = [seed, waypoint1, waypoint2, ..., endpoint]
                    // Each waypoint can be a list: first element is endpoint for
                    // previous segment, whole list is seed for next segment.
                    self.eval_chained_sequence(&args, exclude_end)?
                } else {
                    let left = args[0].clone();
                    let right = args.last().cloned().unwrap_or(Value::Nil);
                    self.eval_sequence(left, right, exclude_end)?
                };

                if exclude_start {
                    // Remove the first element
                    if let Value::Array(items, is_real) = &result
                        && !items.is_empty()
                    {
                        let new_items = items[1..].to_vec();
                        result = if *is_real {
                            Value::real_array(new_items)
                        } else {
                            Value::array(new_items)
                        };
                    }
                }
                return Ok(result);
            }
            _ => {}
        }
        // Short-circuit operators need special handling
        match op {
            "andthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if !crate::runtime::types::value_is_defined(&acc) {
                        return Ok(acc);
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            "notandthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Value::Nil);
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            _ => {}
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
            "..." => TokenKind::DotDotDot,
            "...^" => TokenKind::DotDotDotCaret,
            ".." => TokenKind::DotDot,
            _ => TokenKind::Ident(op.to_string()),
        }
    }

    fn runtime_error_from_die_value(
        &self,
        value: &Value,
        default_message: &str,
        is_fail: bool,
    ) -> RuntimeError {
        if matches!(value, Value::Nil) {
            let mut err = RuntimeError::new(default_message);
            err.is_fail = is_fail;
            return err;
        }

        let msg = if let Value::Instance { attributes, .. } = value {
            attributes
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| value.to_string_value())
        } else {
            value.to_string_value()
        };

        let mut err = RuntimeError::new(&msg);
        err.is_fail = is_fail;
        if let Value::Instance { class_name, .. } = value
            && (class_name == "Exception"
                || class_name.starts_with("X::")
                || class_name.starts_with("CX::"))
        {
            err.exception = Some(Box::new(value.clone()));
        }
        err
    }

    fn builtin_die(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(v) = args.first() {
            return Err(self.runtime_error_from_die_value(v, "Died", false));
        }
        if let Some(current) = self.env.get("!")
            && !matches!(current, Value::Nil)
        {
            return Err(self.runtime_error_from_die_value(current, "Died", false));
        }
        Err(RuntimeError::new("Died"))
    }

    fn builtin_fail(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(v) = args.first() {
            return Err(self.runtime_error_from_die_value(v, "Failed", true));
        }
        if let Some(current) = self.env.get("!")
            && !matches!(current, Value::Nil)
        {
            return Err(self.runtime_error_from_die_value(current, "Failed", true));
        }
        let mut err = RuntimeError::new("Failed");
        err.is_fail = true;
        Err(err)
    }

    fn builtin_return_rw(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        Err(RuntimeError {
            return_value: Some(value),
            ..RuntimeError::new("")
        })
    }

    fn builtin_assign_method_lvalue(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_assign_method_lvalue expects target, method name, method args, and value",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let method_args = match &args[2] {
            Value::Array(items, ..) => items.to_vec(),
            Value::Nil => Vec::new(),
            other => vec![other.clone()],
        };
        let value = args[3].clone();
        let target_var = args.get(4).and_then(|v| {
            let name = v.to_string_value();
            if name.is_empty() { None } else { Some(name) }
        });
        self.assign_method_lvalue_with_values(
            target_var.as_deref(),
            target,
            &method,
            method_args,
            value,
        )
    }

    fn builtin_warn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut message = String::new();
        for arg in args {
            message.push_str(&arg.to_string_value());
        }
        Err(RuntimeError::warn_signal(message))
    }

    fn make_stub_exception(message: String) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::Str(message));
        Value::make_instance("X::StubCode".to_string(), attrs)
    }

    fn builtin_stub_die(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut message = String::new();
        for arg in args {
            message.push_str(&arg.to_string_value());
        }
        let ex = Self::make_stub_exception(message);
        Err(self.runtime_error_from_die_value(&ex, "Stub code executed", false))
    }

    fn builtin_stub_warn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut message = String::new();
        for arg in args {
            message.push_str(&arg.to_string_value());
        }
        Err(RuntimeError::warn_signal(message))
    }

    fn builtin_exit(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = match args.first() {
            Some(Value::Int(i)) => *i,
            _ => 0,
        };
        self.halted = true;
        self.exit_code = code;
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
        let code = Self::positional_string(args, 0);
        if let Some(lang) = Self::named_value(args, "lang") {
            let lang = lang.to_string_value();
            if !lang.eq_ignore_ascii_case("raku") && !lang.eq_ignore_ascii_case("perl6") {
                return Err(RuntimeError::new(format!(
                    "EVAL with :lang<{}> is not supported",
                    lang
                )));
            }
        }
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
                | "quietly"
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
                | "sec"
                | "cosec"
                | "cotan"
                | "asec"
                | "acosec"
                | "acotan"
                | "sinh"
                | "cosh"
                | "tanh"
                | "sech"
                | "cosech"
                | "cotanh"
                | "asinh"
                | "acosh"
                | "atanh"
                | "asech"
                | "acosech"
                | "acotanh"
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
                | "roundrobin"
                | "push"
                | "pop"
                | "shift"
                | "unshift"
                | "indir"
                | "run"
                | "splice"
                | "flat"
                | "unique"
                | "squish"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "any"
                | "all"
                | "none"
                | "one"
                | "so"
                | "not"
                | "truncate"
                | "atan2"
        )
    }

    pub(super) fn call_lambda_with_arg(
        &mut self,
        func: &Value,
        item: Value,
    ) -> Result<Value, RuntimeError> {
        if let Value::Sub(data) = func {
            let saved_env = self.env.clone();
            for (k, v) in &data.env {
                self.env.insert(k.clone(), v.clone());
            }
            if let Some(p) = data.params.first() {
                self.env.insert(p.clone(), item.clone());
            }
            self.env.insert("_".to_string(), item.clone());
            let result = self.eval_block_value(&data.body);
            self.env = saved_env;
            return result;
        }
        Err(RuntimeError::new("Expected callable"))
    }

    /// zip:with — zip lists using a custom combining function.
    fn builtin_zip_with(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut lists: Vec<Vec<Value>> = Vec::new();
        let mut with_fn: Option<Value> = None;
        for arg in args {
            if let Value::Pair(key, val) = arg
                && key == "with"
            {
                with_fn = Some((**val).clone());
                continue;
            }
            lists.push(crate::runtime::value_to_list(arg));
        }
        let combiner = with_fn.ok_or_else(|| RuntimeError::new("zip: missing :with argument"))?;
        if lists.is_empty() {
            return Ok(Value::array(vec![]));
        }
        // Determine associativity from the operator name
        let assoc = Self::op_associativity(&combiner);
        let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
        let mut result = Vec::with_capacity(min_len);
        for i in 0..min_len {
            let elements: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
            let combined = if elements.len() <= 1 {
                elements.into_iter().next().unwrap_or(Value::Nil)
            } else {
                match assoc {
                    OpAssoc::Right => {
                        // Right-associative: fold from right
                        let mut acc = elements.last().unwrap().clone();
                        for elem in elements[..elements.len() - 1].iter().rev() {
                            acc = self.call_sub_value(
                                combiner.clone(),
                                vec![elem.clone(), acc],
                                false,
                            )?;
                        }
                        acc
                    }
                    OpAssoc::Chain => {
                        // Chain-associative: all pairwise comparisons must be true
                        let mut all_true = true;
                        for pair in elements.windows(2) {
                            let r = self.call_sub_value(
                                combiner.clone(),
                                vec![pair[0].clone(), pair[1].clone()],
                                false,
                            )?;
                            if !r.truthy() {
                                all_true = false;
                                break;
                            }
                        }
                        Value::Bool(all_true)
                    }
                    OpAssoc::Left => {
                        // Left-associative (default): fold from left
                        let mut acc = elements[0].clone();
                        for elem in &elements[1..] {
                            acc = self.call_sub_value(
                                combiner.clone(),
                                vec![acc, elem.clone()],
                                false,
                            )?;
                        }
                        acc
                    }
                }
            };
            result.push(combined);
        }
        Ok(Value::array(result))
    }

    /// Determine the associativity of an operator from its name.
    fn op_associativity(func: &Value) -> OpAssoc {
        let name = match func {
            Value::Routine { name, .. } => name.as_str(),
            _ => return OpAssoc::Left,
        };
        // Extract the operator from "infix:<op>"
        let op = name
            .strip_prefix("infix:<")
            .and_then(|s| s.strip_suffix('>'))
            .unwrap_or(name);
        match op {
            "**" => OpAssoc::Right,
            "=" | ":=" | "=>" | "x" | "xx" => OpAssoc::Right,
            "eqv" | "===" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "eq" | "ne" | "lt" | "gt"
            | "le" | "ge" | "~~" | "=~=" | "=:=" => OpAssoc::Chain,
            _ => OpAssoc::Left,
        }
    }
}
