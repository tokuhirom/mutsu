use super::*;
use crate::symbol::Symbol;
use std::sync::atomic::{AtomicU64, Ordering};

static ATOMIC_VAR_KEY_COUNTER: AtomicU64 = AtomicU64::new(1);

enum OpAssoc {
    Left,
    Right,
    Chain,
}

impl Interpreter {
    fn has_invalid_anonymous_rw_trait(code: &str) -> bool {
        let bytes = code.as_bytes();
        let mut i = 0usize;
        while i < bytes.len() {
            if bytes[i] != b'$' {
                i += 1;
                continue;
            }
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            if j + 1 >= bytes.len() || bytes[j] != b'i' || bytes[j + 1] != b's' {
                i += 1;
                continue;
            }
            j += 2;
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            if j + 1 < bytes.len() && bytes[j] == b'r' && bytes[j + 1] == b'w' {
                return true;
            }
            i += 1;
        }
        false
    }

    pub(super) fn coerce_to_enum_variant(
        &mut self,
        enum_name: &str,
        variants: &[(String, i64)],
        value: Value,
    ) -> Option<Value> {
        let by_index = |idx: usize| -> Option<Value> {
            variants.get(idx).map(|(key, val)| Value::Enum {
                enum_type: Symbol::intern(enum_name),
                key: Symbol::intern(key),
                value: *val,
                index: idx,
            })
        };

        match value {
            Value::Enum {
                enum_type, index, ..
            } if enum_type == enum_name => by_index(index),
            Value::Enum { key, .. } => {
                // For foreign enum types, look up by string name, not numeric value
                variants
                    .iter()
                    .enumerate()
                    .find(|(_, (k, _))| k == &key.resolve())
                    .and_then(|(idx, _)| by_index(idx))
            }
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
        if matches!(target_val, Value::Sub(_)) {
            return self.call_sub_value(target_val, args, true);
        }
        if matches!(target_val, Value::Routine { .. }) {
            return self.call_sub_value(target_val, args, false);
        }
        // Mixin wrapping a Sub/Routine: check for CALL-ME from mixed-in roles
        if let Value::Mixin(ref inner, ref mixins) = target_val {
            // Check if any mixed-in role provides CALL-ME
            for key in mixins.keys() {
                if let Some(role_name) = key.strip_prefix("__mutsu_role__")
                    && self.role_has_method(role_name, "CALL-ME")
                {
                    return self.call_method_with_values(target_val, "CALL-ME", args);
                }
            }
            // Otherwise, delegate to the inner callable
            match inner.as_ref() {
                Value::Sub(_) => return self.call_sub_value(*inner.clone(), args, true),
                Value::Routine { .. } => return self.call_sub_value(*inner.clone(), args, false),
                _ => {}
            }
        }
        if matches!(target_val, Value::Instance { .. }) {
            return self.call_method_with_values(target_val, "CALL-ME", args);
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
            "leave" => self.builtin_leave(&args),
            "return-rw" => self.builtin_return_rw(&args),
            "__mutsu_assign_method_lvalue" => self.builtin_assign_method_lvalue(&args),
            "__mutsu_assign_named_sub_lvalue" => self.builtin_assign_named_sub_lvalue(&args),
            "__mutsu_assign_callable_lvalue" => self.builtin_assign_callable_lvalue(&args),
            "__mutsu_feed_whatever" => self.builtin_feed_whatever(&args),
            "__mutsu_feed_append" => self.builtin_feed_append(&args),
            "__mutsu_feed_append_whatever" => self.builtin_feed_append_whatever(&args),
            "__mutsu_feed_array_assign" => self.builtin_feed_array_assign(&args),
            "__mutsu_reverse_xx" => self.builtin_reverse_xx(&args),
            "__mutsu_reverse_andthen" => self.builtin_reverse_andthen(&args),
            "__mutsu_bind_index_value" => Ok(Value::Pair(
                "__mutsu_bind_index_value".to_string(),
                Box::new(args.first().cloned().unwrap_or(Value::Nil)),
            )),
            "__mutsu_subscript_adverb" => self.builtin_subscript_adverb(&args),
            "__mutsu_stub_die" => self.builtin_stub_die(&args),
            "__mutsu_stub_warn" => self.builtin_stub_warn(&args),
            "__mutsu_incdec_nomatch" => self.builtin_incdec_nomatch(&args),
            "exit" => self.builtin_exit(&args),
            "__PROTO_DISPATCH__" => self.call_proto_dispatch(),
            // Multi dispatch control flow
            "callsame" => self.builtin_callsame(),
            "nextsame" => self.builtin_callsame(),
            "nextcallee" => self.builtin_nextcallee(),
            // Type coercion
            "Int" | "Num" | "Str" | "Bool" => self.builtin_coerce(name, &args),
            // Grammar helpers
            "make" => self.builtin_make(&args),
            "made" => self.builtin_made(),
            "__mutsu_make_format" => self.builtin_make_format(&args),
            "undefine" => Ok(Value::Nil),
            "local" => Ok(Value::Nil),
            "VAR" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "WHAT" => {
                if args.len() != 1 {
                    return Err(RuntimeError::new(
                        "X::Syntax::Argument::MOPMacro: WHAT expects exactly one argument",
                    ));
                }
                self.call_method_with_values(args[0].clone(), "WHAT", vec![])
            }
            "HOW" => {
                if args.len() != 1 {
                    return Err(RuntimeError::new(
                        "X::Syntax::Argument::MOPMacro: HOW expects exactly one argument",
                    ));
                }
                self.call_method_with_values(args[0].clone(), "HOW", vec![])
            }
            "__MUTSU_UNREGISTER_CLASS__" => {
                if let Some(name) = args.first() {
                    let class_name = name.to_string_value();
                    self.classes.remove(&class_name);
                    self.env.remove(&class_name);
                    self.suppress_name(&class_name);
                }
                Ok(Value::Nil)
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
            "require" => self.builtin_require(&args),
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
            "end" => self.builtin_end(&args),
            "set" => self.builtin_set(&args),
            "bag" => self.builtin_bag(&args),
            "mix" => self.builtin_mix(&args),
            "hash" => self.builtin_hash(&args),
            "any" | "all" | "one" | "none" => self.builtin_junction(name, args),
            "pair" => self.builtin_pair(&args),
            "keys" => self.builtin_keys(&args),
            "values" => self.builtin_values(&args),
            "kv" => self.builtin_kv(&args),
            "pairs" => self.builtin_pairs(&args),
            "abs" => self.builtin_abs(&args),
            "val" => Ok(super::builtins_collection::builtin_val(&args)),
            "min" => self.builtin_min(&args),
            "max" => self.builtin_max(&args),
            "minmax" => self.builtin_minmax(&args),
            "cross" => self.builtin_cross(args),
            "roundrobin" => self.builtin_roundrobin(&args),
            // List operations
            "join" => self.builtin_join(&args),
            "item" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "list" => self.builtin_list(&args),
            "circumfix:<[ ]>" => Ok(Value::array(args.clone())),
            "lol" => Ok(Value::array(args.clone())),
            "flat" => self.builtin_flat(&args),
            "slip" | "Slip" => self.builtin_slip(&args),
            "take" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                self.take_value(value.clone());
                Ok(value)
            }
            "reverse" => self.builtin_reverse(&args),
            "sort" => self.builtin_sort(&args),
            "unique" => self.builtin_unique(&args),
            "squish" => self.builtin_squish(&args),
            "produce" => self.builtin_produce(&args),
            // Higher-order functions
            "map" => self.builtin_map(&args),
            "grep" => self.builtin_grep(&args),
            "first" => self.builtin_first(&args),
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
            "unival" => self.builtin_unival(&args),
            "flip" => self.builtin_flip(&args),
            "lc" => self.builtin_lc(&args),
            "uc" => self.builtin_uc(&args),
            "tc" => self.builtin_tc(&args),
            "trim" => self.builtin_trim(&args),
            "chars" => self.builtin_chars(&args),
            "sprintf" | "zprintf" => self.builtin_sprintf(&args),
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
            "sink" => {
                // sink evaluates args and returns Nil.
                // If the argument is a block/sub, call it first.
                if let Some(func @ Value::Sub(_)) = args.first() {
                    let value = self.call_sub_value(func.clone(), Vec::new(), false)?;
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = &value
                        && class_name == "Failure"
                        && let Some(ex) = attributes.get("exception")
                    {
                        let mut err = RuntimeError::new(ex.to_string_value());
                        err.exception = Some(Box::new(ex.clone()));
                        return Err(err);
                    }
                } else if let Some(Value::Instance {
                    class_name,
                    attributes,
                    ..
                }) = args.first()
                    && class_name == "Failure"
                    && let Some(ex) = attributes.get("exception")
                {
                    let mut err = RuntimeError::new(ex.to_string_value());
                    err.exception = Some(Box::new(ex.clone()));
                    return Err(err);
                }
                Ok(Value::Nil)
            }
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
            "QX" | "qx" => self.builtin_qx(&args),
            "kill" => self.builtin_kill(&args),
            "syscall" => self.builtin_syscall(&args),
            "sleep" => self.builtin_sleep(&args),
            "sleep-timer" => self.builtin_sleep_timer(&args),
            "sleep-till" => self.builtin_sleep_till(&args),
            // Concurrency (single-threaded simulation)
            "start" => self.builtin_start(args),
            "await" => self.builtin_await(&args),
            "atomic-fetch" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "__mutsu_atomic_fetch_var" => self.builtin_atomic_fetch_var(&args),
            "__mutsu_atomic_store_var" => self.builtin_atomic_store_var(&args),
            "__mutsu_atomic_add_var" => self.builtin_atomic_add_var(&args),
            "__mutsu_atomic_post_inc_var" => self.builtin_atomic_post_inc_var(&args),
            "__mutsu_atomic_pre_inc_var" => self.builtin_atomic_pre_inc_var(&args),
            "__mutsu_atomic_post_dec_var" => self.builtin_atomic_post_dec_var(&args),
            "__mutsu_atomic_pre_dec_var" => self.builtin_atomic_pre_dec_var(&args),
            "__mutsu_hyper_prefix" => self.builtin_hyper_prefix(&args),
            "signal" => self.builtin_signal(&args),
            // Boolean coercion functions
            "not" => Ok(Value::Bool(!args.first().unwrap_or(&Value::Nil).truthy())),
            "so" => Ok(Value::Bool(args.first().unwrap_or(&Value::Nil).truthy())),
            // Fallback
            // CREATE: allocate bare instance (used as a method found via find_method)
            "CREATE" => {
                if let Some(target) = args.first() {
                    self.call_method_with_values(target.clone(), "CREATE", vec![])
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => self.call_function_fallback(name, &args),
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
                || class_name.resolve().starts_with("X::")
                || class_name.resolve().starts_with("CX::"))
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

    fn leave_return_value(args: &[Value]) -> Option<Value> {
        match args {
            [] => None,
            [single] => Some(single.clone()),
            _ => Some(Value::Slip(std::sync::Arc::new(args.to_vec()))),
        }
    }

    pub(crate) fn builtin_leave(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_leave_with_target(None, args)
    }

    pub(crate) fn builtin_leave_method(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.builtin_leave_with_target(Some(target), args)
    }

    fn builtin_leave_with_target(
        &mut self,
        target: Option<Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut sig = RuntimeError::last_signal();
        sig.is_leave = true;
        sig.return_value = Self::leave_return_value(args);

        let current_callable_id = self.env.get("__mutsu_callable_id").and_then(|v| match v {
            Value::Int(i) if *i > 0 => Some(*i as u64),
            _ => None,
        });
        let current_block_id = self.env.get("&?BLOCK").and_then(|v| match v {
            Value::WeakSub(weak) => weak.upgrade().map(|sub| sub.id),
            Value::Sub(sub) => Some(sub.id),
            _ => None,
        });

        match target {
            None => {}
            Some(Value::WeakSub(weak)) => {
                if let Some(sub) = weak.upgrade() {
                    if Some(sub.id) != current_callable_id && Some(sub.id) != current_block_id {
                        sig.leave_callable_id = Some(sub.id);
                    }
                } else {
                    return Err(RuntimeError::new("Callable has been freed"));
                }
            }
            Some(Value::Sub(data)) => {
                if Some(data.id) != current_callable_id && Some(data.id) != current_block_id {
                    sig.leave_callable_id = Some(data.id);
                }
            }
            Some(Value::Routine { package, name, .. }) => {
                sig.leave_routine = Some(format!("{package}::{name}"));
            }
            Some(Value::Nil) => {}
            Some(Value::Package(name)) if name == "Any" => {}
            Some(Value::Package(name)) if name == "Sub" => {
                let caller_callable_id = self
                    .caller_env_stack
                    .last()
                    .and_then(|env| env.get("__mutsu_callable_id"))
                    .and_then(|v| match v {
                        Value::Int(i) if *i > 0 => Some(*i as u64),
                        _ => None,
                    });
                if let Some(id) = caller_callable_id {
                    sig.leave_callable_id = Some(id);
                } else if let Some((package, routine)) = self.routine_stack_top() {
                    sig.leave_routine = Some(format!("{package}::{routine}"));
                }
            }
            Some(Value::Package(name)) if name == "Block" => {}
            Some(Value::Str(label)) => {
                sig.label = Some(label);
            }
            Some(other) => {
                sig.label = Some(other.to_string_value());
            }
        }

        Err(sig)
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

    fn builtin_subscript_adverb(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_subscript_adverb expects target, index, and mode",
            ));
        }
        let target = args[0].clone();
        let index = args[1].clone();
        let mode = args[2].to_string_value();
        let var_name = args
            .get(3)
            .map(Value::to_string_value)
            .filter(|s| !s.is_empty());

        let (kind, keep_missing) = match mode.as_str() {
            "kv" => ("kv", false),
            "not-kv" => ("kv", true),
            "p" => ("p", false),
            "not-p" => ("p", true),
            "k" => ("k", false),
            "not-k" => ("k", true),
            "v" => ("v", false),
            "not-v" => ("v", true),
            _ => return Ok(Value::Nil),
        };

        let mut indices = match index {
            Value::Array(items, ..) => items.to_vec(),
            other => vec![other],
        };
        if matches!(indices.first(), Some(Value::Whatever))
            || matches!(indices.first(), Some(Value::Num(f)) if f.is_infinite() && *f > 0.0)
        {
            indices = match &target {
                Value::Array(items, ..) => (0..items.len())
                    .map(|i| Value::Int(i as i64))
                    .collect::<Vec<_>>(),
                Value::Hash(map) => map
                    .keys()
                    .map(|k| Value::Str(k.clone()))
                    .collect::<Vec<_>>(),
                _ => Vec::new(),
            };
        }
        let is_multi = indices.len() != 1;

        let mut rows: Vec<(Value, Value, bool)> = Vec::with_capacity(indices.len());
        match &target {
            Value::Array(items, ..) => {
                let bound_map = var_name
                    .as_ref()
                    .and_then(|name| self.env.get(&format!("__mutsu_initialized_index::{name}")))
                    .and_then(|v| match v {
                        Value::Hash(map) => Some(map),
                        _ => None,
                    });
                for idx in &indices {
                    let i = match idx {
                        Value::Int(i) => *i,
                        Value::Num(f) => *f as i64,
                        _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
                    };
                    let key = Value::Int(i);
                    if i < 0 || i as usize >= items.len() {
                        rows.push((key, Value::Package(Symbol::intern("Any")), false));
                        continue;
                    }
                    let ui = i as usize;
                    let exists = if let Some(map) = bound_map {
                        if map.contains_key(&i.to_string()) {
                            true
                        } else {
                            !matches!(&items[ui], Value::Package(name) if name == "Any")
                        }
                    } else {
                        true
                    };
                    rows.push((key, items[ui].clone(), exists));
                }
            }
            Value::Hash(map) => {
                for idx in &indices {
                    let key_str = idx.to_string_value();
                    let key =
                        super::builtins_collection::builtin_val(&[Value::Str(key_str.clone())]);
                    let exists = map.contains_key(&key_str);
                    let value = map
                        .get(&key_str)
                        .cloned()
                        .unwrap_or_else(|| Value::Package(Symbol::intern("Any")));
                    rows.push((key, value, exists));
                }
            }
            _ => return Ok(Value::Nil),
        }

        if !is_multi {
            let Some((key, value, exists)) = rows.into_iter().next() else {
                return Ok(Value::Nil);
            };
            if !keep_missing && !exists {
                return Ok(Value::array(Vec::new()));
            }
            return Ok(match kind {
                "kv" => Value::array(vec![key, value]),
                "p" => Value::ValuePair(Box::new(key), Box::new(value)),
                "k" => key,
                "v" => value,
                _ => Value::Nil,
            });
        }

        let mut out = Vec::new();
        for (key, value, exists) in rows {
            if !keep_missing && !exists {
                continue;
            }
            match kind {
                "kv" => {
                    out.push(key);
                    out.push(value);
                }
                "p" => out.push(Value::ValuePair(Box::new(key), Box::new(value))),
                "k" => out.push(key),
                "v" => out.push(value),
                _ => {}
            }
        }
        Ok(Value::array(out))
    }

    fn builtin_feed_whatever(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        let list = crate::runtime::value_to_list(&value);
        let list_value = Value::array(list.clone());
        let mut hash_items = std::collections::HashMap::new();
        for chunk in list.chunks(2) {
            if let [k, v] = chunk {
                hash_items.insert(k.to_string_value(), v.clone());
            }
        }
        self.env.insert("$(*)".to_string(), value.clone());
        self.env.insert("@(*)".to_string(), list_value);
        self.env.insert("%(*)".to_string(), Value::hash(hash_items));
        Ok(value)
    }

    fn builtin_feed_append(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "__mutsu_feed_append expects sink and source values",
            ));
        }
        let mut out = crate::runtime::value_to_list(&args[0]);
        out.extend(crate::runtime::value_to_list(&args[1]));
        Ok(Value::array(out))
    }

    fn builtin_feed_append_whatever(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let source = args.first().cloned().unwrap_or(Value::Nil);
        let current = self
            .env
            .get("@(*)")
            .cloned()
            .unwrap_or_else(|| Value::array(Vec::new()));
        let appended = self.builtin_feed_append(&[current, source])?;
        let list = crate::runtime::value_to_list(&appended);
        let mut hash_items = std::collections::HashMap::new();
        for chunk in list.chunks(2) {
            if let [k, v] = chunk {
                hash_items.insert(k.to_string_value(), v.clone());
            }
        }
        self.env.insert("$(*)".to_string(), appended.clone());
        self.env
            .insert("@(*)".to_string(), Value::array(list.clone()));
        self.env.insert("%(*)".to_string(), Value::hash(hash_items));
        Ok(appended)
    }

    fn builtin_feed_array_assign(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        let int_max = i64::MAX;
        let infinite = match &value {
            Value::Range(_, end) | Value::RangeExcl(_, end) => *end == int_max,
            Value::GenericRange { end, .. } => end.to_f64().is_infinite(),
            Value::Int(end) => *end == int_max,
            _ => false,
        };
        if infinite {
            return Err(RuntimeError::new(
                "Cannot eagerly assign an infinite feed source to an array",
            ));
        }
        Ok(crate::runtime::coerce_to_array(value))
    }

    fn builtin_reverse_xx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_reverse_xx expects count and thunk",
            ));
        }
        let count = crate::runtime::to_int(&args[0]);
        if count <= 0 {
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        }
        let thunk = args[1].clone();
        let mut values = Vec::with_capacity(count as usize);
        for _ in 0..count {
            values.push(self.eval_call_on_value(thunk.clone(), Vec::new())?);
        }
        Ok(Value::Seq(std::sync::Arc::new(values)))
    }

    fn builtin_reverse_andthen(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_reverse_andthen expects condition and thunk",
            ));
        }
        let cond = args[0].clone();
        if !crate::runtime::types::value_is_defined(&cond) {
            return Ok(cond);
        }
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), cond);
        let result = self.eval_call_on_value(args[1].clone(), Vec::new());
        match saved_topic {
            Some(value) => {
                self.env.insert("_".to_string(), value);
            }
            None => {
                self.env.remove("_");
            }
        }
        result
    }

    fn sub_call_args_from_value(arg: Option<&Value>) -> Vec<Value> {
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
        if let Value::Proxy { fetcher, .. } = result.clone() {
            if matches!(fetcher.as_ref(), Value::Nil) {
                return Ok(Value::Nil);
            }
            return self.call_sub_value(*fetcher, vec![result], true);
        }
        Ok(result)
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

    fn assign_proxy_lvalue(&mut self, proxy: Value, value: Value) -> Result<Value, RuntimeError> {
        let Value::Proxy { fetcher, storer } = proxy.clone() else {
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

    fn assign_named_sub_lvalue_with_values(
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

        if let Some(callable) = self.env.get(&format!("&{}", name)).cloned() {
            return self.assign_callable_lvalue_with_values(callable, call_args, value);
        }

        Err(RuntimeError::new(format!("Unknown call: {}", name)))
    }

    fn assign_callable_lvalue_with_values(
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
                Err(RuntimeError::new("X::Assignment::RO: sub is not rw"))
            }
            Value::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => {
                    self.assign_callable_lvalue_with_values(Value::Sub(strong), call_args, value)
                }
                None => Err(RuntimeError::new("Callable has been freed")),
            },
            _ => Err(RuntimeError::new(
                "X::Assignment::RO: cannot assign through non-callable value",
            )),
        }
    }

    fn builtin_assign_named_sub_lvalue(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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

    fn builtin_assign_callable_lvalue(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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

    fn atomic_var_name_arg(args: &[Value]) -> Result<String, RuntimeError> {
        let Some(name) = args.first() else {
            return Err(RuntimeError::new(
                "atomic variable operation requires variable name",
            ));
        };
        Ok(name.to_string_value())
    }

    fn atomic_shared_value_key(id: u64) -> String {
        format!("__mutsu_atomic_value::{id}")
    }

    fn atomic_shared_name_key(name: &str) -> String {
        format!("__mutsu_atomic_name::{name}")
    }

    fn atomic_assign_coerced_value(
        &mut self,
        name: &str,
        mut value: Value,
    ) -> Result<Value, RuntimeError> {
        self.check_readonly_for_modify(name)?;
        if let Some(constraint) = self.var_type_constraint(name)
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(value, Value::Nil) {
                value = Value::Package(Symbol::intern(&constraint));
            } else if !self.type_matches_value(&constraint, &value) {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Assignment: Type check failed in assignment to '{}'; expected {}, got {}",
                    name,
                    constraint,
                    crate::runtime::utils::value_type_name(&value)
                )));
            }
            if !matches!(value, Value::Nil | Value::Package(_)) {
                value = self.try_coerce_value_for_constraint(&constraint, value)?;
            }
        }
        Ok(value)
    }

    fn atomic_value_key_for_name(&mut self, name: &str) -> String {
        let name_key = Self::atomic_shared_name_key(name);
        if let Some(Value::Str(existing)) = self.env.get(&name_key) {
            return existing.clone();
        }
        let value_key = {
            let mut shared = self.shared_vars.lock().unwrap();
            if let Some(Value::Str(existing)) = shared.get(&name_key) {
                existing.clone()
            } else {
                let id = ATOMIC_VAR_KEY_COUNTER.fetch_add(1, Ordering::Relaxed);
                let value_key = Self::atomic_shared_value_key(id);
                shared.insert(name_key.clone(), Value::Str(value_key.clone()));
                value_key
            }
        };
        self.env.insert(name_key, Value::Str(value_key.clone()));
        value_key
    }

    fn atomic_current_value(
        &self,
        shared: &std::collections::HashMap<String, Value>,
        name: &str,
        value_key: &str,
    ) -> Value {
        shared
            .get(value_key)
            .cloned()
            .or_else(|| self.env.get(name).cloned())
            .unwrap_or(Value::Nil)
    }

    fn builtin_atomic_fetch_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let name = Self::atomic_var_name_arg(args)?;
        let value_key = self.atomic_value_key_for_name(&name);
        let shared = self.shared_vars.lock().unwrap();
        Ok(self.atomic_current_value(&shared, &name, &value_key))
    }

    fn builtin_atomic_store_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "atomic store requires variable name and value",
            ));
        }
        let name = args[0].to_string_value();
        let value = self.atomic_assign_coerced_value(&name, args[1].clone())?;
        let value_key = self.atomic_value_key_for_name(&name);
        self.env.insert(name, value.clone());
        self.shared_vars
            .lock()
            .unwrap()
            .insert(value_key, value.clone());
        Ok(value)
    }

    fn builtin_atomic_add_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "atomic add requires variable name and increment value",
            ));
        }
        let name = args[0].to_string_value();
        let delta = args[1].clone();
        self.check_readonly_for_modify(&name)?;
        let value_key = self.atomic_value_key_for_name(&name);
        let mut shared = self.shared_vars.lock().unwrap();
        let current = self.atomic_current_value(&shared, &name, &value_key);
        let next = crate::builtins::arith_add(current, delta)?;
        self.env.insert(name.clone(), next.clone());
        shared.insert(value_key, next.clone());
        Ok(next)
    }

    fn builtin_atomic_update_unit(
        &mut self,
        args: &[Value],
        delta: i64,
        return_old: bool,
    ) -> Result<Value, RuntimeError> {
        let name = Self::atomic_var_name_arg(args)?;
        self.check_readonly_for_modify(&name)?;
        let value_key = self.atomic_value_key_for_name(&name);
        let mut shared = self.shared_vars.lock().unwrap();
        let current = self.atomic_current_value(&shared, &name, &value_key);
        let next = crate::builtins::arith_add(current.clone(), Value::Int(delta))?;
        self.env.insert(name.clone(), next.clone());
        shared.insert(value_key, next.clone());
        if return_old { Ok(current) } else { Ok(next) }
    }

    fn builtin_atomic_post_inc_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, 1, true)
    }

    fn builtin_atomic_pre_inc_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, 1, false)
    }

    fn builtin_atomic_post_dec_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, -1, true)
    }

    fn builtin_atomic_pre_dec_var(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_atomic_update_unit(args, -1, false)
    }

    fn builtin_hyper_prefix(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Ok(Value::array(vec![]));
        }
        let op = args[0].to_string_value();
        let routine = format!("prefix:<{}>", op);
        fn apply_hyper_prefix(
            interp: &mut Interpreter,
            routine: &str,
            value: Value,
        ) -> Result<Value, RuntimeError> {
            match value {
                Value::Array(items, is_array) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::Array(std::sync::Arc::new(mapped), is_array))
                }
                Value::Seq(items) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::Seq(std::sync::Arc::new(mapped)))
                }
                Value::Slip(items) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::Slip(std::sync::Arc::new(mapped)))
                }
                other => interp.call_function(routine, vec![other]),
            }
        }
        let items = crate::runtime::value_to_list(&args[1]);
        let mut results = Vec::with_capacity(items.len());
        for item in items {
            results.push(apply_hyper_prefix(self, &routine, item)?);
        }
        Ok(Value::array(results))
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
        Value::make_instance(Symbol::intern("X::StubCode"), attrs)
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

    fn builtin_incdec_nomatch(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let caller = args
            .first()
            .map(Value::to_string_value)
            .unwrap_or_else(|| "postfix:<++>".to_string());
        let msg = format!(
            "Cannot resolve caller {}(...); the parameter requires mutable arguments",
            caller
        );
        let mut err = RuntimeError::new(msg.clone());
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::Str(msg));
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Multi::NoMatch"),
            attrs,
        )));
        Err(err)
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

    fn builtin_callsame(&mut self) -> Result<Value, RuntimeError> {
        if !self.method_dispatch_stack.is_empty() {
            let frame_idx = self.method_dispatch_stack.len() - 1;
            let (receiver_class, invocant, orig_args, owner_class, method_def) = {
                let frame = &mut self.method_dispatch_stack[frame_idx];
                let Some((owner_class, method_def)) = frame.remaining.first().cloned() else {
                    return Ok(Value::Nil);
                };
                frame.remaining.remove(0);
                (
                    frame.receiver_class.clone(),
                    frame.invocant.clone(),
                    frame.args.clone(),
                    owner_class,
                    method_def,
                )
            };
            let (result, updated_invocant) = match &invocant {
                Value::Instance {
                    class_name,
                    attributes,
                    id: target_id,
                } => {
                    let (result, updated) = self.run_instance_method_resolved(
                        &receiver_class,
                        &owner_class,
                        method_def,
                        (**attributes).clone(),
                        orig_args,
                        Some(invocant.clone()),
                    )?;
                    self.overwrite_instance_bindings_by_identity(
                        &class_name.resolve(),
                        *target_id,
                        updated.clone(),
                    );
                    (
                        result,
                        Some(Value::Instance {
                            class_name: *class_name,
                            attributes: std::sync::Arc::new(updated),
                            id: *target_id,
                        }),
                    )
                }
                _ => {
                    let (result, _) = self.run_instance_method_resolved(
                        &receiver_class,
                        &owner_class,
                        method_def,
                        HashMap::new(),
                        orig_args,
                        Some(invocant.clone()),
                    )?;
                    (result, None)
                }
            };
            if let Some(new_invocant) = updated_invocant
                && let Some(frame) = self.method_dispatch_stack.get_mut(frame_idx)
            {
                frame.invocant = new_invocant;
            }
            return Ok(result);
        }
        let Some((candidates, orig_args)) = self.multi_dispatch_stack.last().cloned() else {
            return Ok(Value::Nil);
        };
        let Some(next_def) = candidates.first().cloned() else {
            return Ok(Value::Nil);
        };
        // Update the stack: remove the candidate we're about to call
        let remaining = candidates[1..].to_vec();
        let stack_len = self.multi_dispatch_stack.len();
        self.multi_dispatch_stack[stack_len - 1] = (remaining, orig_args.clone());
        self.call_function_def(&next_def, &orig_args)
    }

    fn builtin_nextcallee(&mut self) -> Result<Value, RuntimeError> {
        let Some((candidates, _orig_args)) = self.multi_dispatch_stack.last().cloned() else {
            return Ok(Value::Nil);
        };
        let Some(next_def) = candidates.first().cloned() else {
            return Ok(Value::Nil);
        };
        // Remove this candidate from the remaining list
        let remaining = candidates[1..].to_vec();
        let stack_len = self.multi_dispatch_stack.len();
        self.multi_dispatch_stack[stack_len - 1] = (remaining, Vec::new());
        // Return as a callable Sub value
        Ok(Value::make_sub(
            next_def.package,
            next_def.name,
            next_def.params.clone(),
            next_def.param_defs.clone(),
            next_def.body.clone(),
            next_def.is_rw,
            self.env.clone(),
        ))
    }

    fn builtin_make(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        self.env.insert("made".to_string(), value.clone());
        Ok(value)
    }

    fn builtin_made(&self) -> Result<Value, RuntimeError> {
        Ok(self.env.get("made").cloned().unwrap_or(Value::Nil))
    }

    pub(crate) fn builtin_callframe(
        &self,
        args: &[Value],
        default_depth: usize,
    ) -> Result<Value, RuntimeError> {
        let mut depth = default_depth;
        let mut callsite_line: Option<i64> = None;
        for arg in args {
            match arg {
                Value::Int(i) if *i >= 0 => depth = *i as usize,
                Value::Num(f) if *f >= 0.0 => depth = *f as usize,
                Value::Pair(k, v) if k == "__callframe_line" => {
                    if let Value::Int(line) = v.as_ref() {
                        callsite_line = Some(*line);
                    }
                }
                _ => {}
            }
        }
        if let Some(frame) = self.callframe_value(depth, callsite_line) {
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
        let saved_file = self.env.get("?FILE").cloned();
        self.env.insert("?FILE".to_string(), Value::Str(path));
        let result = self.eval_eval_string(&code);
        if let Some(prev) = saved_file {
            self.env.insert("?FILE".to_string(), prev);
        } else {
            self.env.remove("?FILE");
        }
        result
    }

    fn builtin_eval(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = Self::positional_string(args, 0);
        if Self::has_invalid_anonymous_rw_trait(&code) {
            return Err(RuntimeError::new(
                "X::Trait::Invalid: trait 'rw' is not valid on anonymous parameter",
            ));
        }
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
        self.emit_output(&format!("{:?}\n", val));
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
                | "val"
                | "sqrt"
                | "floor"
                | "ceiling"
                | "ceil"
                | "round"
                | "exp"
                | "log"
                | "cis"
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
                | "unival"
                | "chars"
                | "chomp"
                | "chop"
                | "flip"
                | "lc"
                | "uc"
                | "tc"
                | "trim"
                | "elems"
                | "end"
                | "keys"
                | "values"
                | "pairs"
                | "sort"
                | "reverse"
                | "rotate"
                | "join"
                | "map"
                | "grep"
                | "roundrobin"
                | "push"
                | "pop"
                | "shift"
                | "unshift"
                | "dir"
                | "chdir"
                | "QX"
                | "qx"
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

    fn callable_produce_arity(&self, callable: &Value) -> usize {
        let (params, param_defs) = self.callable_signature(callable);
        if !param_defs.is_empty() {
            let mut total = 0usize;
            let mut required = 0usize;
            for pd in &param_defs {
                if pd.named
                    || pd.slurpy
                    || pd.double_slurpy
                    || pd.traits.iter().any(|t| t == "invocant")
                {
                    continue;
                }
                total += 1;
                let is_required = pd.required || (!pd.optional_marker && pd.default.is_none());
                if is_required {
                    required += 1;
                }
            }
            if required >= 2 {
                return required;
            }
            if total >= 2 {
                return total;
            }
        }
        params.len().max(2)
    }

    fn callable_produce_assoc(&self, callable: &Value) -> OpAssoc {
        let callable_name: Option<String> = match callable {
            Value::Sub(data) => Some(data.name.resolve()),
            Value::Routine { name, .. } => Some(name.resolve()),
            _ => None,
        };
        if let Some(name) = &callable_name
            && let Some(assoc) = self.infix_associativity(name)
        {
            return match assoc.as_str() {
                "right" => OpAssoc::Right,
                "chain" => OpAssoc::Chain,
                _ => OpAssoc::Left,
            };
        }
        Self::op_associativity(callable)
    }

    fn eval_produce_over_items(
        &mut self,
        callable: Value,
        items: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        if items.is_empty() {
            return Ok(Vec::new());
        }
        if items.len() == 1 {
            return Ok(vec![items[0].clone()]);
        }

        let arity = self.callable_produce_arity(&callable);
        let step = arity.saturating_sub(1).max(1);
        let assoc = self.callable_produce_assoc(&callable);

        match assoc {
            OpAssoc::Right => {
                let mut out = Vec::new();
                let mut acc = items.last().cloned().unwrap_or(Value::Nil);
                out.push(acc.clone());
                let mut right_edge = items.len().saturating_sub(1);
                while right_edge >= step {
                    let start = right_edge - step;
                    let mut call_args = items[start..right_edge].to_vec();
                    call_args.push(acc);
                    acc = self.call_sub_value(callable.clone(), call_args, true)?;
                    out.push(acc.clone());
                    right_edge = start;
                }
                Ok(out)
            }
            _ => {
                let mut out = Vec::new();
                let mut acc = items[0].clone();
                out.push(acc.clone());
                let mut idx = 1usize;
                while idx + step <= items.len() {
                    let mut call_args = vec![acc];
                    call_args.extend(items[idx..idx + step].iter().cloned());
                    acc = self.call_sub_value(callable.clone(), call_args, true)?;
                    out.push(acc.clone());
                    idx += step;
                }
                Ok(out)
            }
        }
    }

    fn builtin_produce(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let callable = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("produce expects a callable as first argument"))?;
        if !matches!(
            callable,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            return Err(RuntimeError::new(
                "produce expects a callable as first argument",
            ));
        }

        let mut items = Vec::new();
        for arg in args.iter().skip(1) {
            if matches!(arg, Value::Hash(_)) {
                items.push(arg.clone());
            } else {
                items.extend(crate::runtime::value_to_list(arg));
            }
        }
        let out = self.eval_produce_over_items(callable, items)?;
        Ok(Value::array(out))
    }

    /// zip:with — zip lists using a custom combining function.
    pub(super) fn builtin_zip_with(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
        let name_str = match func {
            Value::Routine { name, .. } => name.resolve(),
            _ => return OpAssoc::Left,
        };
        // Extract the operator from "infix:<op>"
        let op = name_str
            .strip_prefix("infix:<")
            .and_then(|s: &str| s.strip_suffix('>'))
            .map(|s| s.to_string())
            .unwrap_or_else(|| name_str.clone());
        match op.as_str() {
            "**" => OpAssoc::Right,
            "=" | ":=" | "=>" | "x" | "xx" => OpAssoc::Right,
            "eqv" | "===" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "eq" | "ne" | "lt" | "gt"
            | "le" | "ge" | "~~" | "=~=" | "=:=" => OpAssoc::Chain,
            _ => OpAssoc::Left,
        }
    }
}
