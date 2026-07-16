use super::builtins_collection::format_first_result;
use super::*;

impl Interpreter {
    pub(super) fn builtin_map(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let arg_sources = self.pending_call_arg_sources.clone().unwrap_or_default();
        // Check if the source is a single named array variable for rw writeback
        let source_var = if args.len() == 2 {
            arg_sources
                .get(1)
                .and_then(|entry| entry.as_ref())
                .filter(|name| name.starts_with('@'))
                .cloned()
        } else {
            None
        };
        if args.len() == 2 && Self::is_lazy_pipe_source(&args[1]) {
            let method_args: Vec<Value> = func.into_iter().collect();
            return self.call_method_with_values(args[1].clone(), "map", method_args);
        }
        let mut list_items = Vec::new();
        for (idx, arg) in args.iter().skip(1).enumerate() {
            // Check if this argument came from a $ variable (itemized container).
            // Scalar variables don't have a sigil prefix in arg_sources, while
            // @array and %hash variables start with '@' and '%' respectively.
            let is_itemized = arg_sources
                .get(idx + 1)
                .and_then(|entry| entry.as_ref())
                .is_some_and(|name| {
                    !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&')
                });
            if !is_itemized && crate::runtime::utils::is_shaped_array(arg) {
                list_items.extend(crate::runtime::utils::shaped_array_leaves(arg));
            } else if is_itemized {
                // Itemized containers (from $ variables) are NOT flattened
                list_items.push(arg.clone());
            } else {
                match arg.view() {
                    // An itemized array/list value (`$[...]` / `$(...)`, e.g. a
                    // sub-call result) is a single item regardless of arg source.
                    ValueView::Array(_, kind) if kind.is_itemized() => list_items.push(arg.clone()),
                    ValueView::Array(items, ..) => list_items.extend(items.iter().cloned()),
                    ValueView::Seq(items) => list_items.extend(items.iter().cloned()),
                    ValueView::Hash(map) => {
                        // Hashes in list context flatten to key-value Pairs
                        for (k, v) in map.iter() {
                            list_items.push(Value::pair(k.clone(), v.clone()));
                        }
                    }
                    _ if arg.is_range() => {
                        // Route ranges through the unified pull iterator so an
                        // open-ended range (`1..Inf` == `Range(1, i64::MAX)`) is
                        // truncated at the cap instead of panicking with
                        // `capacity overflow` (ANALYSIS §8.2).
                        list_items.extend(crate::runtime::value_iterator::materialize_capped(
                            arg,
                            crate::runtime::utils::MAX_RANGE_EXPAND as usize,
                        ));
                    }
                    _ => list_items.push(arg.clone()),
                }
            }
        }
        if let Some(var_name) = source_var {
            let result = self.eval_map_over_items_rw(func, &mut list_items)?;
            self.env.insert(var_name, Value::real_array(list_items));
            Ok(result)
        } else {
            // Same deferral as dispatch_map_method: a callback containing
            // `return` or a stub (`...`) must not run until the Seq is forced.
            if let Some(ValueView::Sub(sub_data)) = func.as_ref().map(Value::view)
                && (Self::body_contains_return(&sub_data.body)
                    || Self::is_stub_routine_body(&sub_data.body))
            {
                return Ok(self.create_lazy_map_list(list_items, &sub_data));
            }
            self.eval_map_over_items(func, list_items)
        }
    }

    pub(super) fn builtin_grep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Parse named adverbs (:k, :v, :kv, :p) from args
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        let mut positional_args: Vec<Value> = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key == "k" => has_k = value.truthy(),
                ValueView::Pair(key, value) if key == "kv" => has_kv = value.truthy(),
                ValueView::Pair(key, value) if key == "p" => has_p = value.truthy(),
                ValueView::Pair(key, value) if key == "v" => {
                    if !value.truthy() {
                        return Err(RuntimeError::new(
                            "X::Adverb: Unexpected adverb 'v' passed to grep",
                        ));
                    }
                }
                _ => positional_args.push(arg.clone()),
            }
        }
        let args = &positional_args;
        let func = args.first().cloned();
        // Check if the matcher is a Bool — this is always an error.
        // `grep $_ == 1, 1,2,3` passes a Bool as the matcher.
        if let Some(ValueView::Bool(_)) = func.as_ref().map(Value::view) {
            return Err(RuntimeError::typed_msg(
                "X::Match::Bool",
                "Cannot use Bool as Matcher with '.match'. Did you mean to use $_ ~~ ... instead?",
            ));
        }
        if args.len() == 1
            && matches!(
                args[0].view(),
                ValueView::Int(_) | ValueView::Num(_) | ValueView::Str(_)
            )
        {
            return Err(RuntimeError::typed_msg(
                "X::Match::Bool",
                "Cannot use Bool as Matcher with '.match'. Did you mean to use $_ ~~ ... instead?",
            ));
        }
        if args.len() == 2 && Self::is_lazy_pipe_source(&args[1]) {
            let mut method_args: Vec<Value> = func.into_iter().collect();
            if has_k {
                method_args.push(Value::pair("k".to_string(), Value::TRUE));
            } else if has_kv {
                method_args.push(Value::pair("kv".to_string(), Value::TRUE));
            } else if has_p {
                method_args.push(Value::pair("p".to_string(), Value::TRUE));
            }
            return self.call_method_with_values(args[1].clone(), "grep", method_args);
        }
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg.view() {
                // Only flatten List-kind arrays (from @-sigiled variables).
                // Array-kind (from [...] literals) are kept as individual items,
                // and itemized (`$(...)` / `$[...]`) values are a single item.
                ValueView::Array(items, kind)
                    if matches!(kind, crate::value::ArrayKind::List)
                        || (matches!(kind, crate::value::ArrayKind::Array) && args.len() == 2) =>
                {
                    list_items.extend(items.iter().cloned());
                }
                // A Seq (e.g. the result of `@a.sort`) is an iterable sequence and
                // is flattened into grep's list, mirroring `map` (which already
                // handles `Seq`). Without this, `grep { ... }, @a.sort`
                // greps over a single one-element list.
                ValueView::Seq(items) => {
                    list_items.extend(items.iter().cloned());
                }
                _ if arg.is_range() => {
                    // Route ranges through the unified pull iterator so an
                    // open-ended range (`1..Inf` == `Range(1, i64::MAX)`) is
                    // truncated at the cap instead of panicking with
                    // `capacity overflow` (ANALYSIS §8.2).
                    list_items.extend(crate::runtime::value_iterator::materialize_capped(
                        arg,
                        crate::runtime::utils::MAX_RANGE_EXPAND as usize,
                    ));
                }
                _ => list_items.push(arg.clone()),
            }
        }
        if has_k || has_kv || has_p {
            let original_items = list_items.clone();
            let filtered = self.eval_grep_over_items(func, list_items)?;
            let indices = crate::runtime::methods_collection_ops::compute_grep_indices(
                &original_items,
                &filtered,
            );
            if has_k {
                let idx_vals: Vec<Value> = indices.iter().map(|&i| Value::int(i as i64)).collect();
                Ok(Value::array(idx_vals))
            } else if has_kv {
                let items = if let ValueView::Array(items, ..) = filtered.view() {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    result.push(Value::int(*i as i64));
                    result.push(item.clone());
                }
                Ok(Value::array(result))
            } else {
                // :p
                let items = if let ValueView::Array(items, ..) = filtered.view() {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    // The key is the Int index (`3 => v`), not a Str ("3" => v).
                    result.push(Value::value_pair(Value::int(*i as i64), item.clone()));
                }
                Ok(Value::array(result))
            }
        } else {
            self.eval_grep_over_items(func, list_items)
        }
    }

    /// `snip(matcher, +values)` — split a list at positions where the matcher stops matching.
    pub(super) fn builtin_snip(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new("Too few positionals passed to 'snip'"));
        }
        let matcher = args[0].clone();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg.view() {
                ValueView::Array(items, ..) => list_items.extend(items.iter().cloned()),
                _ => list_items.push(arg.clone()),
            }
        }
        self.eval_snip(matcher, list_items)
    }

    /// Core snip implementation shared by both sub and method forms.
    pub(super) fn eval_snip(
        &mut self,
        matcher: Value,
        items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Extract the list of matchers: if matcher is a list/array of callables/types,
        // use them in sequence; otherwise treat as a single matcher.
        let matchers: Vec<Value> = match matcher.view() {
            ValueView::Array(elems, ..) => elems.iter().cloned().collect(),
            ValueView::Seq(elems) | ValueView::Slip(elems) => elems.iter().cloned().collect(),
            _ => vec![matcher.clone()],
        };

        let mut result_groups: Vec<Value> = Vec::new();
        let mut current_group: Vec<Value> = Vec::new();
        let mut matcher_idx: usize = 0;

        for item in items {
            let current_matcher = matchers.get(matcher_idx);
            let matched = if let Some(m) = current_matcher {
                self.snip_matches(&item, m)?
            } else {
                // No more matchers — everything goes into the last group
                true
            };

            if matched {
                current_group.push(item);
            } else {
                // Snip here: save current group and start a new one
                result_groups.push(Value::array(current_group));
                current_group = vec![item];
                matcher_idx += 1;
            }
        }
        // Push the final group
        if !current_group.is_empty() {
            result_groups.push(Value::array(current_group));
        }

        Ok(Value::array(result_groups))
    }

    /// Check if a value matches a snip matcher (Callable or type object).
    fn snip_matches(&mut self, item: &Value, matcher: &Value) -> Result<bool, RuntimeError> {
        if matches!(matcher.view(), ValueView::Sub(_)) {
            Ok(self
                .call_sub_value(matcher.clone(), vec![item.clone()], true)?
                .truthy())
        } else {
            Ok(self.smart_match(item, matcher))
        }
    }

    /// If `v` is a Buf/Blob, return its element bytes as a list of Int values.
    /// Raku iterates a Blob as its bytes, so list methods (`map`/`grep`/`first`)
    /// must expand it rather than treating the whole Blob as a single element.
    /// (Note: list-*assignment* keeps a Blob as one element — `my @a = $buf` —
    /// so this is used only by the iterating methods, not `value_to_list`.)
    pub(crate) fn buf_as_byte_items(v: &Value) -> Option<Vec<Value>> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = v.view()
        {
            let cn = class_name.resolve();
            let is_blob = cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn == "utf32"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob");
            if is_blob
                && let Some(ValueView::Array(bytes, ..)) =
                    attributes.as_map().get("bytes").map(Value::view)
            {
                return Some(bytes.iter().cloned().collect());
            }
        }
        None
    }

    pub(super) fn builtin_first(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Separate named args (Pairs) from positional args
        let mut positional = Vec::new();
        let mut has_v = false;
        let mut has_neg_v = false;
        let mut has_end = false;
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key == "v" => {
                    if value.truthy() {
                        has_v = true;
                    } else {
                        has_neg_v = true;
                    }
                }
                ValueView::Pair(key, value) if key == "end" => {
                    if value.truthy() {
                        has_end = true;
                    }
                }
                ValueView::Pair(key, value) if key == "k" => {
                    has_k = value.truthy();
                }
                ValueView::Pair(key, value) if key == "kv" => {
                    has_kv = value.truthy();
                }
                ValueView::Pair(key, value) if key == "p" => {
                    has_p = value.truthy();
                }
                _ => positional.push(arg.clone()),
            }
        }
        if has_neg_v {
            return Err(RuntimeError::new(
                "Throwing `:!v` on first is not supported",
            ));
        }
        let _ = has_v; // :v is the default behavior
        // Check for conflicting adverbs (X::Adverb)
        {
            let adverb_count = has_k as u8 + has_kv as u8 + has_p as u8;
            if adverb_count > 1 {
                let mut names = Vec::new();
                if has_k {
                    names.push("k");
                }
                if has_kv {
                    names.push("kv");
                }
                if has_p {
                    names.push("p");
                }
                let adverb_list = names
                    .iter()
                    .map(|n| format!("'{}'", n))
                    .collect::<Vec<_>>()
                    .join(", ");
                let mut err = RuntimeError::new(format!(
                    "Unsupported combination of adverbs ({}) passed to first on\n'List'.",
                    adverb_list
                ));
                err.exception = Some(Box::new(Value::make_instance(
                    crate::symbol::Symbol::intern("X::Adverb"),
                    std::collections::HashMap::new(),
                )));
                return Err(err);
            }
        }
        // Check for Bool matcher (X::Match::Bool)
        if matches!(
            positional.first().map(Value::view),
            Some(ValueView::Bool(_))
        ) {
            let mut err = RuntimeError::new("Cannot use Bool as a matcher");
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Match::Bool"),
                std::collections::HashMap::new(),
            )));
            return Err(err);
        }
        let func = positional.first().cloned();
        let mut list_items = Vec::new();
        for arg in positional.iter().skip(1) {
            match arg.view() {
                ValueView::Array(items, ..) => list_items.extend(items.iter().cloned()),
                _ if arg.is_range() => {
                    // Route ranges through the unified pull iterator so an
                    // open-ended range (`1..Inf` == `Range(1, i64::MAX)`) is
                    // truncated at the cap instead of panicking with
                    // `capacity overflow` (ANALYSIS §8.2).
                    list_items.extend(crate::runtime::value_iterator::materialize_capped(
                        arg,
                        crate::runtime::utils::MAX_RANGE_EXPAND as usize,
                    ));
                }
                _ => list_items.push(arg.clone()),
            }
        }
        if let Some((idx, value)) = self.find_first_match_over_items(func, &list_items, has_end)? {
            return Ok(format_first_result(idx, value, has_k, has_kv, has_p));
        }
        Ok(Value::NIL)
    }

    /// Find a variable name in the current environment whose value is identical
    /// (by Arc pointer) to the given value. Used for :into writeback.
    pub(super) fn find_var_by_identity(&self, value: &Value) -> Option<String> {
        match value.view() {
            ValueView::Hash(target_arc) => {
                for (name, env_val) in self.env().iter() {
                    if let ValueView::Hash(env_arc) = env_val.view()
                        && crate::gc::Gc::ptr_eq(&target_arc, &env_arc)
                    {
                        return Some(name.resolve());
                    }
                }
                None
            }
            ValueView::Bag(target_arc, _) => {
                for (name, env_val) in self.env().iter() {
                    if let ValueView::Bag(env_arc, _) = env_val.view()
                        && crate::gc::Gc::ptr_eq(&target_arc, &env_arc)
                    {
                        return Some(name.resolve());
                    }
                }
                None
            }
            ValueView::Mix(target_arc, _) => {
                for (name, env_val) in self.env().iter() {
                    if let ValueView::Mix(env_arc, _) = env_val.view()
                        && crate::gc::Gc::ptr_eq(&target_arc, &env_arc)
                    {
                        return Some(name.resolve());
                    }
                }
                None
            }
            _ => None,
        }
    }
}
