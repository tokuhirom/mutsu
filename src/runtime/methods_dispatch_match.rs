use super::*;

impl Interpreter {
    /// Whether `v` is an `IO::CatHandle` instance. Used to keep the generic
    /// 0-arg `.say`/`.print`/`.put`/`.printf` "stringify the invocant" behavior
    /// from shadowing the cat's own (X::NYI) write methods.
    fn is_io_cathandle(v: &Value) -> bool {
        matches!(v.view(), ValueView::Instance { class_name, .. } if class_name == "IO::CatHandle")
    }

    /// Dispatch methods by name - first group (string, IO, coercion, misc).
    /// Returns Some(result) if the method was handled, None to fall through.
    #[allow(clippy::too_many_lines)]
    pub(super) fn dispatch_method_by_name_1(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // A quoted MOP pseudo-method call (`$obj."WHAT"()`) must dispatch a
        // user-defined method of that name instead of the reflection macro. The
        // CallMethod opcode records the quoted pseudo name in
        // `skip_pseudo_method_native`; consume it here (once per dispatch) so the
        // WHAT/HOW/WHO/WHY macro arms below fall through to user resolution.
        let quoted_pseudo = self
            .skip_pseudo_method_native
            .as_deref()
            .is_some_and(|m| m == method);
        if quoted_pseudo {
            self.skip_pseudo_method_native = None;
        }
        match method {
            "are" => Some(self.dispatch_are(target, &args)),
            "classify" | "categorize" if matches!(target.view(), ValueView::Package(name) if name == "Supply") =>
            {
                // Supply.classify / Supply.categorize are instance methods
                // (declared `Supply:D:`); calling them on the Supply type
                // object is an error in Rakudo.
                Some(Err(RuntimeError::new(format!(
                    "Cannot call '{method}' as a class method on Supply (requires a defined invocant)"
                ))))
            }
            "classify" | "categorize" if !matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply") =>
            {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.extend(args.iter().cloned());
                call_args.push(target);
                Some(self.builtin_classify(method, &call_args))
            }
            "classify-list" | "categorize-list" => {
                // Immutable Bag and Mix cannot be classified into
                let type_name = match target.view() {
                    ValueView::Bag(_, false) => Some("Bag"),
                    ValueView::Mix(_, false) => Some("Mix"),
                    _ => None,
                };
                if let Some(tname) = type_name {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("typename".to_string(), Value::str_from(tname));
                    attrs.insert("method".to_string(), Value::str_from(method));
                    let exception = Value::make_instance(Symbol::intern("X::Immutable"), attrs);
                    let mut err = RuntimeError::new(format!(
                        "Cannot call '{}' on an immutable '{}'",
                        method, tname
                    ));
                    err.exception = Some(Box::new(exception));
                    return Some(Err(err));
                }
                let classify_name = if method == "classify-list" {
                    "classify"
                } else {
                    "categorize"
                };
                // A mutable QuantHash invocant (BagHash/MixHash/SetHash) yields a
                // result of the same mutable type; builtin_classify builds an
                // immutable Bag/Mix, so re-overlay the invocant's mutability.
                let result_mutable = set_result_mutability(&target);
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.extend(args.iter().cloned());
                call_args.push(Value::pair("into".to_string(), target));
                Some(
                    self.builtin_classify(classify_name, &call_args)
                        .map(|r| with_set_mutability(r, result_mutable)),
                )
            }
            "from-loop" | "from_loop" if matches!(target.view(), ValueView::Package(name) if name == "Seq") => {
                Some(self.dispatch_seq_from_loop(args))
            }
            // `$obj.say`/`.print`/`.put`/`.printf` (no args) stringify and print the
            // invocant — EXCEPT on an IO::CatHandle, where these are write methods
            // that route to the native handler (which raises X::NYI on a read-only
            // cat). Without this guard the generic form would silently print the
            // cat's gist instead of throwing.
            // Cost: O(n), n = chars of the gist of the invocant (rendered, then written).
            "say" if args.is_empty() && !Self::is_io_cathandle(&target) => {
                Some(self.dispatch_say(&target))
            }
            // Cost: O(n), n = chars of the stringified invocant (rendered, then written).
            "print" if args.is_empty() && !Self::is_io_cathandle(&target) => {
                Some(self.dispatch_print(&target))
            }
            // Cost: O(n), n = chars of the stringified invocant (rendered, then written).
            "put" if args.is_empty() && !Self::is_io_cathandle(&target) => {
                Some(self.dispatch_put(&target))
            }
            "printf"
                if args.is_empty()
                    && !Self::is_io_cathandle(&target)
                    && !matches!(target.view(), ValueView::Junction { .. }) =>
            {
                Some(self.dispatch_printf(&target))
            }
            // Method form `$format.printf(*@args)` == `printf($format, @args)`,
            // the same `Cool` convention `.sprintf` already follows (documented
            // in `Type/independent-routines.rakudoc`). Restricted to non-`Instance`
            // receivers so `$*OUT.printf(...)` / `IO::CatHandle.printf(...)` keep
            // their own handle-writing dispatch. Note this is `Cool.printf`, which
            // has no `Junction:D` candidate — a Junction *argument* is a directive
            // error here, exactly as in Rakudo, so it must NOT autothread; a
            // Junction *invocant* does autothread (it is the `Str(Cool)` format),
            // which `call_function` already handles, so route those through it.
            "printf" if !matches!(target.view(), ValueView::Instance { .. }) => {
                let mut full = Vec::with_capacity(args.len() + 1);
                full.push(target.clone());
                full.extend(args.iter().cloned());
                if matches!(target.view(), ValueView::Junction { .. }) {
                    return Some(self.call_function("printf", full));
                }
                Some((|| {
                    let formatted = self.builtin_sprintf(&full, false)?;
                    self.write_to_named_handle("$*OUT", &formatted.to_string_value(), false)?;
                    Ok(Value::TRUE)
                })())
            }
            "sprintf"
                if args.is_empty() && !matches!(target.view(), ValueView::Junction { .. }) =>
            {
                Some(self.dispatch_sprintf(&target))
            }
            "sprintf" => {
                // Method form `$format.sprintf(*@args)` == `sprintf($format, @args)`.
                // Delegate to `builtin_sprintf` (the single source of truth) so a
                // bare type object arg gets the same string-context warning and
                // "" coercion as the sub form. Reached for 2+ args, and for the
                // 1-arg type-object case the native fast-path defers here (the
                // 0-arg arm above is a no-directive passthrough).
                let mut full = Vec::with_capacity(args.len() + 1);
                full.push(target.clone());
                full.extend(args.iter().cloned());
                // A Junction invocant is the `Str(Cool) $format`, which
                // autothreads: `call_function` owns that threading.
                if matches!(target.view(), ValueView::Junction { .. }) {
                    return Some(self.call_function("sprintf", full));
                }
                Some(self.builtin_sprintf(&full, false))
            }
            "zprintf" if !args.is_empty() => {
                // Method form `$format.zprintf(*@args)`: mirrors the `.sprintf`
                // arm above (interpreter-aware coercion via `builtin_sprintf`),
                // but with zprintf semantics.
                let mut full = Vec::with_capacity(args.len() + 1);
                full.push(target.clone());
                full.extend(args.iter().cloned());
                Some(self.builtin_sprintf(&full, true))
            }
            "shape" if args.is_empty() => self.dispatch_shape(&target),
            "default" if args.is_empty() => Self::dispatch_default(&target),
            "note" if args.is_empty() => Some(self.dispatch_note(&target)),
            "snitch" if args.len() <= 1 => self.dispatch_snitch(&target, &args),
            "return-rw" if args.is_empty() => Some(Ok(target)),
            "encode" if !matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply") => {
                Some(self.dispatch_encode(&target, &args))
            }
            "decode" => self.dispatch_decode(&target, &args),
            "unpack" if matches!(target.view(), ValueView::Instance { .. }) => {
                // $blob.unpack($template) — experimental, mirrors the sub form.
                let bytes = Self::extract_buf_bytes(&target);
                let template = args.first().map(Value::to_string_value).unwrap_or_default();
                Some(crate::builtins::pack::unpack(&bytes, &template))
            }
            "subbuf" => self.dispatch_subbuf(&target, &args),
            "polymod" => Some(self.method_polymod(&target, &args)),
            "VAR" if args.is_empty() => {
                // Proxy .VAR returns a decontainerized copy
                if matches!(target.view(), ValueView::Proxy { .. }) {
                    return Some(Ok(Value::proxy_var_object(target, String::new())));
                }
                Some(Ok(target))
            }
            "can" if args.len() == 1 => {
                let method_name = args[0].to_string_value();
                let results = self.collect_can_methods(&target, &method_name);
                Some(Ok(Value::array(results)))
            }
            "does" if args.len() == 1 => {
                let type_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => args[0].to_string_value(),
                };
                Some(Ok(Value::truth(
                    self.type_matches_value(&type_name, &target),
                )))
            }
            "start" => self.dispatch_promise_start(&target, &args),
            "is-initial-thread" => {
                if matches!(target.view(), ValueView::Package(cn) if cn == "Thread")
                    || matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Thread")
                {
                    Some(Ok(Value::truth(
                        super::methods_collection_ops::is_initial_thread(),
                    )))
                } else {
                    None
                }
            }
            "in" => self.dispatch_promise_in(&target, &args),
            "THREAD" => {
                if let ValueView::Junction { values, .. } = target.view() {
                    let code = args.first().cloned().unwrap_or(Value::NIL);
                    for value in values.iter() {
                        match self.call_sub_value(code.clone(), vec![value.clone()], false) {
                            Ok(_) => {}
                            // A matching `when` inside the block leaves that
                            // block with `succeed`. That ends the *eigenstate's*
                            // iteration, not the THREAD loop — every loop
                            // construct absorbs it the same way (see the `for`
                            // body in vm_for_loop_body.rs). Propagating it
                            // aborted the remaining eigenstates and unwound the
                            // enclosing routine: Test::Util's
                            // `is-deeply-junction` guts only the first
                            // eigenstate of `any(all(1,2), 3)` without this.
                            Err(e) if e.is_succeed() => {}
                            Err(e) => return Some(Err(e)),
                        }
                    }
                    return Some(Ok(Value::NIL));
                }
                None
            }
            "at" => self.dispatch_promise_at(&target, &args),
            "kept" => self.dispatch_promise_kept(&target, &args),
            "broken" => self.dispatch_promise_broken(&target, &args),
            // `X::AdHoc.from-slurpy(3, False, "Not here")` — the documented
            // class method that builds an `X::AdHoc` out of a slurpy argument
            // list. Sits alongside the other native class methods on builtin
            // type objects (`Promise.allof`, `Promise.in`, ...) rather than in
            // the arity-keyed native tables, because it is variadic.
            "from-slurpy" if matches!(target.view(), ValueView::Package(n) if n == "X::AdHoc") => {
                Some(self.dispatch_adhoc_from_slurpy(&args))
            }
            "allof" => self.dispatch_promise_allof(&target, &args),
            "anyof" => self.dispatch_promise_anyof(&target, &args),
            "WHAT" if args.is_empty() && !quoted_pseudo => Some(self.dispatch_what(&target, args)),
            "HOW" if !quoted_pseudo => Some(self.dispatch_how(&target, &args)),
            "WHO" if args.is_empty() && !quoted_pseudo => Some(self.dispatch_who(&target)),
            "WHY" if args.is_empty() && !quoted_pseudo => Some(self.dispatch_why(&target)),
            "^name" if args.is_empty() => Some(self.dispatch_caret_name(&target)),
            "^enum_value_list" | "enum_value_list" => self.dispatch_enum_value_list(&target),
            "enums" => self.dispatch_enums(&target),
            "invert" => self.dispatch_invert_enum(&target),
            "subparse" | "parse" | "parsefile" => {
                match target.view() {
                    ValueView::Package(package_name)
                        if !self.grammar_has_user_method(&package_name.resolve(), method) =>
                    {
                        Some(self.dispatch_package_parse(&package_name.resolve(), method, &args))
                    }
                    // A grammar *instance* (`G.new`) dispatches `.parse` just like
                    // the type object; grammars are stateless so the instance name
                    // is its grammar package name.
                    ValueView::Instance { class_name, .. }
                        if self.class_is_grammar(&class_name.resolve())
                            && !self.grammar_has_user_method(&class_name.resolve(), method) =>
                    {
                        Some(self.dispatch_instance_parse(
                            target.clone(),
                            &class_name.resolve(),
                            method,
                            &args,
                        ))
                    }
                    _ => None,
                }
            }
            // A regex argument may be a closure over its defining scope; install
            // it around the match the same way `~~` does.
            "match" => Some(self.with_regex_closure_scope(args.first().cloned(), |me| {
                me.dispatch_match_method(target, &args)
            })),
            "subst" => Some(self.with_regex_closure_scope(args.first().cloned(), |me| {
                me.dispatch_subst(target, &args)
            })),
            // Cost: see `dispatch_wordcase` (src/runtime/methods_string.rs).
            "wordcase" if !args.is_empty() => Some(self.dispatch_wordcase(target, &args)),
            "comb" if !args.is_empty() => {
                if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
                {
                    Some(self.dispatch_supply_transform(target, "comb", &args))
                } else if matches!(target.view(), ValueView::Instance { class_name, .. }
                    if matches!(class_name.resolve().as_str(),
                        "IO::Handle" | "IO::Path" | "IO::Pipe" | "IO::CatHandle"))
                {
                    // Defer to native IO dispatch so file content is read first.
                    None
                } else {
                    // The matcher argument may be a stored regex closed over
                    // its defining scope; install it around the comb the same
                    // way `~~`/`.match`/`.subst` do.
                    let matcher = args.first().cloned();
                    self.with_regex_closure_scope(matcher, |me| {
                        me.dispatch_comb_with_args(target, &args)
                    })
                }
            }
            // `.IO` takes only named adverbs (`:CWD`, `:SPEC`); accept and IGNORE
            // them. Per Raku, `'.'.IO(:CWD($x))` does NOT forward `:CWD` to the
            // resulting IO::Path (unlike `IO::Path.new('.', :CWD($x))`), so the
            // path keeps the current `$*CWD`/`$*SPEC`.
            "IO" if args.is_empty()
                || args.iter().all(|a| {
                    matches!(a.view(), ValueView::Pair(k, _) if k == "CWD" || k == "SPEC")
                        || matches!(a.view(), ValueView::ValuePair(k, _)
                                if k.to_string_value() == "CWD" || k.to_string_value() == "SPEC")
                }) =>
            {
                // IO handle objects have their own `.IO` (returning the path of
                // the open file); never coerce them via stringification.
                if matches!(target.view(), ValueView::Instance { class_name, .. }
                    if matches!(class_name.resolve().as_str(),
                        "IO::Handle" | "IO::Pipe" | "IO::CatHandle"))
                {
                    return None;
                }
                // ADR-0051 P4: an `Instance` receiver whose ancestry does not
                // actually provide `.IO` (no `Cool` anywhere in its dispatch
                // chain) must not be answered by this generic interceptor --
                // it is receiver-class-blind by construction (unconditional
                // `target.to_string_value()`), which is exactly the leak the
                // ADR closes. `should_bypass_native_fastpath`/`shadows_builtin`
                // already route this call past this function entirely for a
                // plain (non-wildcard-handles) class; this is the direct
                // guard the ADR also calls for, so this interceptor stays
                // correct even if reached by a future call path that skips
                // those two gates.
                if matches!(target.view(), ValueView::Instance { .. })
                    && !self.e2_native_method_exists(&target, "IO")
                {
                    return None;
                }
                // `.IO` on an IO::Path (sub)class type object returns the type
                // object itself, so `IO::Path::Unix === IO::Path::Unix.IO`.
                if let ValueView::Package(name) = target.view() {
                    let n = name.resolve();
                    if n == "IO::Path" || n.starts_with("IO::Path::") {
                        return Some(Ok(target.clone()));
                    }
                    if let Some(err) = crate::runtime::utils::dateish_io_concreteness_error(&n) {
                        return Some(Err(err));
                    }
                }
                let s = target.to_string_value();
                if s.contains('\0') {
                    return Some(Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    )));
                }
                Some(Ok(self.make_io_path_instance(&s)))
            }
            "contains" => Some(self.dispatch_contains(target, &args)),
            "starts-with" => Some(self.dispatch_starts_with(target, &args)),
            "ends-with" => Some(self.dispatch_ends_with(target, &args)),
            "index" => Some(self.dispatch_index(target, &args)),
            "indices" => Some(self.dispatch_indices(target, &args)),
            "rindex" => Some(self.dispatch_rindex(target, &args)),
            "substr-eq" => Some(self.dispatch_substr_eq(target, &args)),
            "substr" => Some(self.dispatch_substr(target, &args)),
            "substr-rw" => Some(self.dispatch_substr_rw(target, &args)),
            // Cost: see `dispatch_trans` (src/runtime/methods_trans.rs).
            "trans" => Some(self.dispatch_trans(target, &args)),
            _ => None,
        }
    }

    /// Helper for .comb with arguments.
    /// Handles: .comb($matcher), .comb($matcher, $limit), .comb($matcher, :match)
    pub(super) fn dispatch_comb_with_args(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let text = target.string_value_cow();

        // Separate positional args from named ones. `.comb` declares only
        // `:match`; every other named is swallowed by the implicit `*%_` that
        // each Raku method carries, so it must NOT fall into `positional` (where
        // it used to be read as the matcher, turning `"abc".comb(:nonsense)` into
        // a regex search for ":nonsense"). A *positional* `Pair` -- the
        // `ValuePair` flavour, ADR-0021 -- is a real matcher argument.
        let mut positional: Vec<&Value> = Vec::new();
        let mut return_match = false;
        for arg in args {
            if let ValueView::Pair(key, val) = arg.view() {
                if key == "match" {
                    return_match = val.truthy();
                }
                continue;
            }
            positional.push(arg);
        }

        // Extract limit from second positional arg (default: unlimited)
        let limit: Option<i64> = if positional.len() >= 2 {
            Some(positional[1].to_f64() as i64)
        } else {
            None
        };

        // If limit is 0 or negative, return empty
        if let Some(lim) = limit
            && lim <= 0
        {
            return Some(Ok(Value::seq(Vec::new())));
        }

        let matcher = positional.first().copied();

        let make_seq = |items: Vec<Value>| Value::seq(items);

        match matcher.map(Value::view) {
            // Pure Int-chunk / Str-fixed split: single shared impl in
            // `builtins::comb` (also driving the native fast path). The regex
            // engine cases below stay here because they need the interpreter.
            Some(ValueView::Int(_) | ValueView::Str(_)) => {
                let m = matcher.expect("matcher is Some in this arm");
                let items = crate::builtins::comb::comb_pure(&text, Some(m), limit)
                    .expect("comb_pure always handles Int/Str matchers");
                Some(Ok(make_seq(items)))
            }
            // Cost: O(n + k) plus the engine's per-match cost, n = chars of the invocant,
            // k = matches; with `$limit` the search stops at the k-th match. With
            // `:match` every Match shares one `MatchTarget`.
            Some(ValueView::Regex(pat)) => {
                let max = limit.map_or(usize::MAX, |lim| lim as usize);
                // Use the capturing path only when the regex contains code
                // blocks whose side effects must fire (e.g. `{ take $/.Str }`).
                // For regular regexes, use the faster non-capturing path.
                let spans: Vec<(usize, usize)> = if self.has_code_block_in_prefix(&pat) {
                    let mut matches = self.regex_find_all_with_caps_limited(&pat, &text, max);
                    for (_, _, caps) in &mut matches {
                        if caps.named.values().any(|slot| !slot.nodes.is_empty()) {
                            let ct = caps.target_or_new(&text);
                            self.reduce_regex_captures_made(caps, Some(&ct));
                        }
                    }
                    matches.into_iter().map(|(s, e, _)| (s, e)).collect()
                } else {
                    self.regex_find_all_limited(&pat, &text, max)
                };
                let result: Vec<Value> = if return_match {
                    let mt = crate::runtime::MatchTarget::new(&text);
                    spans
                        .iter()
                        .map(|(start, end)| Self::create_match_object(&mt, *start, *end))
                        .collect()
                } else {
                    Self::char_span_strs(&text, &spans)
                };
                Some(Ok(make_seq(result)))
            }
            Some(ValueView::Sub(_) | ValueView::WeakSub(_)) => Some(Err(RuntimeError::new(
                "none of these signatures match: comb does not accept a Code argument",
            ))),
            _ => {
                if let Some(m) = matcher {
                    let pattern = m.to_string_value();
                    let max = limit.map_or(usize::MAX, |lim| lim as usize);
                    let spans = self.regex_find_all_limited(&pattern, &text, max);
                    Some(Ok(make_seq(Self::char_span_strs(&text, &spans))))
                } else {
                    None
                }
            }
        }
    }

    /// The substrings of `text` at the ascending, non-overlapping char-index
    /// `spans` a regex search returned.
    // Cost: O(p + t), p = chars up to the end of the last span (one forward
    // walk), t = total chars of the result.
    fn char_span_strs(text: &str, spans: &[(usize, usize)]) -> Vec<Value> {
        let mut out = Vec::with_capacity(spans.len());
        // `(char index, byte offset)` of the walk's current position; `spans`
        // are ascending, so each lookup resumes where the last one stopped.
        let mut cur = (0usize, 0usize);
        let mut byte_at = |target: usize| -> usize {
            while cur.0 < target {
                match text[cur.1..].chars().next() {
                    Some(c) => cur = (cur.0 + 1, cur.1 + c.len_utf8()),
                    None => break,
                }
            }
            cur.1
        };
        for &(start, end) in spans {
            let b_start = byte_at(start);
            let b_end = byte_at(end);
            out.push(Value::str(text[b_start..b_end].to_string()));
        }
        out
    }

    /// Create a Match object from regex match positions, sharing the call's
    /// subject `target`.
    // Cost: O(1): the target is shared by refcount, not copied per Match.
    fn create_match_object(
        target: &crate::runtime::MatchTarget,
        start: usize,
        end: usize,
    ) -> Value {
        Value::make_match_object_full(
            start as i64,
            end as i64,
            &[],
            &Default::default(),
            target.clone(),
        )
    }

    /// Dispatch trig methods on Instance values via Numeric/Bridge coercion.
    pub(super) fn dispatch_trig_instance_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "sin"
                | "cos"
                | "tan"
                | "asin"
                | "acos"
                | "atan"
                | "atan2"
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
        ) {
            return None;
        }

        if matches!(target.view(), ValueView::Instance { .. }) {
            let coerced =
                if let Ok(v) = self.call_method_with_values(target.clone(), "Numeric", vec![]) {
                    v
                } else if let Ok(v) = self.call_method_with_values(target, "Bridge", vec![]) {
                    v
                } else {
                    return Some(Err(RuntimeError::new(format!(
                        "Cannot coerce to numeric for {}",
                        method
                    ))));
                };
            return Some(self.call_method_with_values(coerced, method, args));
        }

        // .atan2(Instance) — coerce Instance arg
        if method == "atan2"
            && args.len() == 1
            && matches!(args[0].view(), ValueView::Instance { .. })
        {
            let coerced_arg = match self
                .call_method_with_values(args[0].clone(), "Numeric", vec![])
                .or_else(|_| self.call_method_with_values(args[0].clone(), "Bridge", vec![]))
            {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            return Some(self.call_method_with_values(target, "atan2", vec![coerced_arg]));
        }

        None
    }
}
