use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn builtin_index_var_meta(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let source_name = args.first().map(Value::to_string_value).unwrap_or_default();
        let index_key = args.get(1);

        // For Map containers, .VAR returns the value itself (no Scalar container)
        // since Map decontainerizes all values.
        if let Some(container) = self.env.get(&source_name)
            && let Value::Hash(map) = container
        {
            let is_map = self
                .container_type_metadata(container)
                .and_then(|info| info.declared_type)
                .is_some_and(|dt| dt == "Map");
            if is_map {
                if let Some(key) = index_key {
                    let key_str = key.to_string_value();
                    return Ok(map.get(&key_str).cloned().unwrap_or(Value::Nil));
                }
                return Ok(Value::Nil);
            }
        }

        let mut attributes = std::collections::HashMap::new();
        attributes.insert("name".to_string(), Value::str(format!("{source_name}[]")));
        attributes.insert(
            "__mutsu_var_target".to_string(),
            Value::str(source_name.clone()),
        );
        attributes.insert(
            "dynamic".to_string(),
            Value::Bool(self.is_var_dynamic(&source_name)),
        );
        let default_val = if let Some(tc) = self.var_type_constraint(&source_name) {
            Value::Package(Symbol::intern(&tc))
        } else {
            Value::Package(Symbol::intern("Any"))
        };
        attributes.insert("default".to_string(), default_val);
        Ok(Value::make_instance(Symbol::intern("Scalar"), attributes))
    }

    pub(super) fn has_invalid_anonymous_rw_trait(code: &str) -> bool {
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
        variants: &[(String, EnumValue)],
        value: Value,
    ) -> Option<Value> {
        let by_index = |idx: usize| -> Option<Value> {
            variants.get(idx).map(|(key, val)| Value::Enum {
                enum_type: Symbol::intern(enum_name),
                key: Symbol::intern(key),
                value: val.clone(),
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
                .find(|(_, (_, v))| *v == EnumValue::Int(int_value))
                .and_then(|(idx, _)| by_index(idx)),
            Value::Num(num_value) => {
                if num_value.fract() == 0.0 {
                    let int_value = num_value as i64;
                    variants
                        .iter()
                        .enumerate()
                        .find(|(_, (_, v))| *v == EnumValue::Int(int_value))
                        .and_then(|(idx, _)| by_index(idx))
                } else {
                    None
                }
            }
            Value::Str(name) => variants
                .iter()
                .enumerate()
                .find(|(_, (key, _))| key.as_str() == name.as_str())
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
        if let Value::Junction { kind, values } = target_val {
            let mut results = Vec::with_capacity(values.len());
            for callable in values.iter() {
                results.push(self.eval_call_on_value(callable.clone(), args.clone())?);
            }
            return Ok(Value::junction(kind, results));
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
                Value::Sub(_) => return self.call_sub_value(inner.as_ref().clone(), args, true),
                Value::Routine { .. } => {
                    return self.call_sub_value(inner.as_ref().clone(), args, false);
                }
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
        let (mut args, callsite_line) = self.sanitize_call_args(&args);
        self.test_pending_callsite_line = callsite_line;
        crate::trace::trace_log!("call", "call_function: {} ({} args)", name, args.len());
        match name {
            // Error / control flow
            "die" => self.builtin_die(&args),
            "fail" => self.builtin_fail(&args),
            "succeed" => self.builtin_succeed(&args),
            "leave" => self.builtin_leave(&args),
            "return-rw" => self.builtin_return_rw(&args),
            "__mutsu_assign_method_lvalue" => self.builtin_assign_method_lvalue(&args),
            "__mutsu_index_assign_method_lvalue" => self.builtin_index_assign_method_lvalue(&args),
            "__mutsu_assign_named_sub_lvalue" => self.builtin_assign_named_sub_lvalue(&args),
            "__mutsu_assign_callable_lvalue" => self.builtin_assign_callable_lvalue(&args),
            "__mutsu_assignment_ro" => self.builtin_assignment_ro(&args),
            "__mutsu_star_lvalue_rhs" => self.builtin_star_lvalue_rhs(&args),
            "__mutsu_record_bound_array_len" => self.builtin_record_bound_array_len(&args),
            "__mutsu_record_shaped_array_dims" => self.builtin_record_shaped_array_dims(&args),
            "__mutsu_feed_whatever" => self.builtin_feed_whatever(&args),
            "__mutsu_feed_append" => self.builtin_feed_append(&args),
            "__mutsu_feed_append_whatever" => self.builtin_feed_append_whatever(&args),
            "__mutsu_feed_array_assign" => self.builtin_feed_array_assign(&args),
            "__mutsu_reverse_xx" => self.builtin_reverse_xx(&args),
            "__mutsu_reverse_andthen" => self.builtin_reverse_andthen(&args),
            "__mutsu_andthen_finalize" => self.builtin_andthen_finalize(&args),
            "__mutsu_cross_shortcircuit" => self.builtin_cross_shortcircuit(&args),
            "__mutsu_zip_shortcircuit" => self.builtin_zip_shortcircuit(&args),
            "__mutsu_zip_xx" => self.builtin_zip_xx(&args),
            "__mutsu_bind_index_value" => Ok(Value::Pair(
                "__mutsu_bind_index_value".to_string(),
                Box::new(Value::Array(
                    std::sync::Arc::new(vec![
                        args.first().cloned().unwrap_or(Value::Nil),
                        args.get(1).cloned().unwrap_or(Value::Array(
                            std::sync::Arc::new(Vec::new()),
                            crate::value::ArrayKind::List,
                        )),
                    ]),
                    crate::value::ArrayKind::List,
                )),
            )),
            "__mutsu_subscript_adverb" => self.builtin_subscript_adverb(&args),
            "__mutsu_multidim_adverb" => self.builtin_multidim_adverb(&args),
            "__mutsu_multidim_subscript_adverb" => self.builtin_multidim_subscript_adverb(&args),
            "__mutsu_multidim_exists_adverb" => self.builtin_multidim_exists_adverb(&args),
            "__mutsu_multidim_delete" => self.builtin_multidim_delete(&mut args),
            "__mutsu_multidim_dynamic_adverb" => self.builtin_multidim_dynamic_adverb(&mut args),
            "__mutsu_multidim_subscript_adverb_dyn" => {
                self.builtin_multidim_subscript_adverb_dyn(&mut args)
            }
            "__mutsu_multidim_exists_adverb_dyn" => {
                self.builtin_multidim_exists_adverb_dyn(&mut args)
            }
            "__mutsu_stub_die" => self.builtin_stub_die(&args),
            "__mutsu_stub_warn" => self.builtin_stub_warn(&args),
            "__mutsu_incdec_nomatch" => self.builtin_incdec_nomatch(&args),
            "__mutsu_index_var_meta" => self.builtin_index_var_meta(&args),
            "exit" => self.builtin_exit(&args),
            "__PROTO_DISPATCH__" => self.call_proto_dispatch(),
            // Multi dispatch control flow
            "callsame" => self.builtin_callsame(),
            "nextsame" => self.builtin_nextsame(),
            "callwith" => self.builtin_callwith(&args),
            "nextwith" => self.builtin_nextwith(&args),
            "samewith" => self.builtin_samewith(&args),
            "nextcallee" => self.builtin_nextcallee(),
            // Type coercion
            "Int" | "Num" | "Str" | "Bool" | "Uni" => self.builtin_coerce(name, &args),
            "UNBASE" => self.builtin_unbase(&args),
            "RADIX_LIST" => self.builtin_radix_list(&args),
            // Grammar helpers
            "make" => self.builtin_make(&args),
            "made" => self.builtin_made(),
            "__mutsu_make_format" => self.builtin_make_format(&args),
            "__mutsu_quotewords_atom" => self.builtin_quotewords_atom(&args),
            "__mutsu_words_atom" => self.builtin_words_atom(&args),
            "__mutsu_qw_result" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "__mutsu_unknown_backslash_escape" => self.builtin_unknown_backslash_escape(&args),
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
            "push" | "unshift" | "append" | "prepend" => {
                if args.is_empty() {
                    let msg = format!(
                        "X::TypeCheck::Argument: Calling {name}(Any) will never work with declared signature ($, |)"
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                Ok(Value::Nil)
            }
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
            "Set" if !args.is_empty() => self.builtin_set(&args),
            "Bag" if !args.is_empty() => self.builtin_bag(&args),
            "Mix" if !args.is_empty() => self.builtin_mix(&args),
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
            "sign" => self.builtin_sign(&args),
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
            "cache" => self.builtin_cache(&args),
            "circumfix:<[ ]>" => Ok(Value::real_array(args.clone())),
            "lol" => Ok(Value::array(args.clone())),
            "flat" => self.builtin_flat(&args),
            "duckmap" => self.builtin_duckmap(&args),
            "deepmap" => self.builtin_deepmap(&args),
            "slip" | "Slip" => self.builtin_slip(&args),
            "take" => {
                let value = if args.len() <= 1 {
                    args.first().cloned().unwrap_or(Value::Nil)
                } else {
                    // take with multiple args creates a single list element
                    Value::Array(
                        std::sync::Arc::new(args.clone()),
                        crate::value::ArrayKind::List,
                    )
                };
                self.take_value(value.clone())?;
                Ok(value)
            }
            "return" => {
                let mut err = RuntimeError::new("return");
                err.return_value = Some(args.first().cloned().unwrap_or(Value::Nil));
                Err(err)
            }
            "reverse" => self.builtin_reverse(&args),
            "sort" => self.builtin_sort(&args),
            "unique" => self.builtin_unique(&args),
            "repeated" => self.builtin_repeated(&args),
            "squish" => self.builtin_squish(&args),
            "reduce" => self.builtin_reduce(&args),
            "produce" => self.builtin_produce(&args),
            // Higher-order functions
            "map" => self.builtin_map(&args),
            "grep" => self.builtin_grep(&args),
            "snip" => self.builtin_snip(&args),
            "first" => self.builtin_first(&args),
            "tail" => {
                if args.len() < 2 {
                    return Err(RuntimeError::new("Too few positionals passed to 'tail'"));
                }
                let tail_spec = args[0].clone();
                let arg_sources = self.pending_call_arg_sources.clone().unwrap_or_default();
                let mut values = Vec::new();
                for (offset, value) in args[1..].iter().enumerate() {
                    let source_name = arg_sources
                        .get(offset + 1)
                        .and_then(|entry| entry.as_ref())
                        .map(String::as_str);
                    if source_name.is_some_and(|name| {
                        !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&')
                    }) {
                        values.push(value.clone());
                        continue;
                    }
                    match value {
                        Value::Array(items, ..) => {
                            values.extend(items.iter().cloned());
                        }
                        Value::Seq(items) | Value::Slip(items) => {
                            values.extend(items.iter().cloned());
                        }
                        Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                        | Value::GenericRange { .. } => {
                            values.extend(crate::runtime::value_to_list(value));
                        }
                        other => values.push(other.clone()),
                    }
                }
                let list = Value::array(values);
                self.call_method_with_values(list, "tail", vec![tail_spec])
            }
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
            "rindex" => {
                if args.is_empty() {
                    return Err(RuntimeError::new("Too few positionals passed to 'rindex'"));
                }
                let target = args[0].clone();
                let method_args = args[1..].to_vec();
                self.call_method_with_values(target, "rindex", method_args)
            }
            "chrs" => self.builtin_chrs(&args),
            "chr" => self.builtin_chr(&args),
            "ord" => self.builtin_ord(&args),
            "ords" => self.builtin_ords(&args),
            "unival" => self.builtin_unival(&args),
            "univals" => self.builtin_univals(&args),
            "flip" => self.builtin_flip(&args),
            "lc" => self.builtin_lc(&args),
            "uc" => self.builtin_uc(&args),
            "tc" => self.builtin_tc(&args),
            "trim" => self.builtin_trim(&args),
            "chars" => self.builtin_chars(&args),
            "sprintf" | "zprintf" => {
                // If the first arg is a Junction, thread through it:
                // call .Str on each element and concatenate.
                if let Some(Value::Junction { kind: _, values }) = args.first() {
                    let mut content = String::new();
                    for v in values.iter() {
                        content.push_str(&self.render_str_value(v));
                    }
                    Ok(Value::str(content))
                } else {
                    self.builtin_sprintf(&args)
                }
            }
            "printf" => {
                // If the first arg is a Junction, thread through it:
                // call .Str on each element and print the result.
                if let Some(Value::Junction { kind: _, values }) = args.first() {
                    let mut content = String::new();
                    for v in values.iter() {
                        content.push_str(&self.render_str_value(v));
                    }
                    self.write_to_named_handle("$*OUT", &content, false)?;
                    Ok(Value::Bool(true))
                } else {
                    let formatted = self.builtin_sprintf(&args)?;
                    self.write_to_named_handle("$*OUT", &formatted.to_string_value(), false)?;
                    Ok(Value::Bool(true))
                }
            }
            "split" => self.handle_split_function(args),
            // File I/O
            "slurp" => self.builtin_slurp(&args),
            "spurt" => self.builtin_spurt(&args),
            "unlink" => self.builtin_unlink(&args),
            "open" => self.builtin_open(&args),
            "close" => self.builtin_close(&args),
            "dir" => self.builtin_dir(&args),
            "copy" => self.builtin_copy(&args),
            "rename" | "move" => self.builtin_rename(name, &args),
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
                        let result = self.call_sub_value(first, Vec::new(), true);
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
            "getlogin" => Ok(Value::str(Self::get_login_name().unwrap_or_default())),
            "gethost" => self.builtin_gethost(&args),
            "chroot" => self.builtin_chroot(&args),
            "run" => self.builtin_run(&args),
            "shell" => self.builtin_shell(&args),
            "QX" | "qx" => self.builtin_qx(&args),
            "kill" => self.builtin_kill(&args),
            "syscall" => self.builtin_syscall(&args),
            "sleep" => self.builtin_sleep(&args),
            "sleep-timer" => self.builtin_sleep_timer(&args),
            "sleep-till" | "sleep-until" => self.builtin_sleep_till(&args),
            // Concurrency (single-threaded simulation)
            "start" => self.builtin_start(args),
            "await" => self.builtin_await(&args),
            "full-barrier" => Ok(Value::Nil),
            "atomic-fetch" => Ok(args.first().cloned().unwrap_or(Value::Nil)),
            "__mutsu_atomic_fetch_var" => self.builtin_atomic_fetch_var(&args),
            "__mutsu_atomic_store_var" => self.builtin_atomic_store_var(&args),
            "__mutsu_atomic_add_var" => self.builtin_atomic_add_var(&args),
            "__mutsu_atomic_post_inc_var" => self.builtin_atomic_post_inc_var(&args),
            "__mutsu_atomic_pre_inc_var" => self.builtin_atomic_pre_inc_var(&args),
            "__mutsu_atomic_post_dec_var" => self.builtin_atomic_post_dec_var(&args),
            "__mutsu_cas_var" => self.builtin_cas_var(args),
            "__mutsu_cas_array_elem" => self.builtin_cas_array_elem(args),
            "__mutsu_cas_array_multidim" => self.builtin_cas_array_multidim(args),
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
                | "succeed"
                | "warn"
                | "sink"
                | "quietly"
                | "exit"
                | "abs"
                | "sign"
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
                | "univals"
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
                | "repeated"
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
                | "roots"
                | "substr"
                | "substr-rw"
                | "words"
                | "rand"
                | "sprintf"
                | "zprintf"
                | "printf"
                | "uniname"
                | "uniprop"
                | "unimatch"
                | "uniparse"
                | "parse-names"
                | "symlink"
                | "link"
                | "spurt"
                | "slurp"
                | "open"
                | "close"
                | "unlink"
                | "mkdir"
                | "rmdir"
                | "rename"
                | "copy"
                | "move"
                | "chmod"
                | "lines"
                | "get"
                | "prompt"
        )
    }

    // Reduce/produce/zip operations are in builtins_reduce.rs
}
