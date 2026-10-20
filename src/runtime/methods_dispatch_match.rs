use super::*;

/// The text methods [`Interpreter::text_method_by_mro`] answers.
const TEXT_METHODS_BY_MRO: [&str; 15] = [
    "match",
    "subst",
    "wordcase",
    "comb",
    "IO",
    "contains",
    "starts-with",
    "ends-with",
    "index",
    "indices",
    "rindex",
    "substr-eq",
    "substr",
    "trans",
    "split",
];

impl Interpreter {
    /// Whether `v` is an `IO::CatHandle` instance. Used to keep the generic
    /// 0-arg `.say`/`.print`/`.put`/`.printf` "stringify the invocant" behavior
    /// from shadowing the cat's own (X::NYI) write methods.
    pub(crate) fn is_io_cathandle(v: &Value) -> bool {
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
            .dispatch
            .skip_pseudo_method_native
            .as_deref()
            .is_some_and(|m| m == method);
        if quoted_pseudo {
            self.dispatch.skip_pseudo_method_native = None;
        }
        // The class methods of the built-in types (`Promise.start`,
        // `Seq.from-loop`, `Thread.start`, `Supply.from-list`,
        // `List.from-iterator`, ...) are rows of the method table, found for
        // the type object of the owner or of a subclass of it. Asked ahead of
        // the generic paths: `Promise.start` and `Thread.start` would
        // otherwise be taken by `Any`'s or `Mu`'s rows.
        if let Some(result) = self.type_object_class_method(&target, method, &args) {
            return Some(result);
        }
        // Supply.classify / Supply.categorize are instance methods (declared
        // `Supply:D:`); calling them on the Supply type object is an error in
        // Rakudo, not `Any`'s method.
        if matches!(method, "classify" | "categorize")
            && matches!(target.view(), ValueView::Package(name) if name == "Supply")
        {
            return Some(Err(RuntimeError::new(format!(
                "Cannot call '{method}' as a class method on Supply (requires a defined invocant)"
            ))));
        }
        // `Any`'s iteration methods are rows, found by the receiver's MRO
        // (ADR-11276 §9.134).
        if let Some(result) = self.any_list_method_by_mro(&target, method, &args) {
            return Some(result);
        }
        // `Mu.say`/`put` with arguments for a class acting as `$*OUT`,
        // `Junction.THREAD`, `Cool.encode`, `Blob.unpack`,
        // `Enumeration.enums`, `Mu.set_why` and the methods revision 6.e adds
        // (`snip`, `snitch`, `nomark`, `stem`, `Callable`, and `Grammar`'s
        // `locprepost`, `typed_exception`, `SETFAIL`) are late rows of the receiver's
        // MRO and roles (§9.142, §9.143, §9.145, §9.146); `Mu`'s other late
        // rows are the cascade's last resort.
        if matches!(
            method,
            "THREAD"
                | "encode"
                | "unpack"
                | "enums"
                | "set_why"
                | "snip"
                | "snitch"
                | "nomark"
                | "stem"
                | "Callable"
                | "locprepost"
                | "typed_exception"
                | "SETFAIL"
                | "head"
                | "tail"
                | "fmt"
                | "Capture"
        )
            // A collection's `gist` twin (§9.188); every other receiver's
            // `gist` is `Mu`'s, answered by the cascades.
            || (method == "gist"
                && matches!(
                    target.view(),
                    ValueView::Array(..)
                        | ValueView::Seq(..)
                        | ValueView::HyperSeq(..)
                        | ValueView::RaceSeq(..)
                        | ValueView::Slip(..)
                        | ValueView::Hash(..)
                        | ValueView::Pair(..)
                        | ValueView::ValuePair(..)
                ))
            || (matches!(method, "say" | "put") && !args.is_empty())
            // A collection's `raku` twin (§9.194), for a collection holding a
            // value whose rendering is user code.
            || (method == "raku"
                && args.is_empty()
                && crate::runtime::container_needs_raku_dispatch(&target))
            // A string that spells a `Complex` converts as it (§9.190).
            || (matches!(target.view(), ValueView::Str(_))
                && crate::builtins::method_table::scalars::str_complex_real::NAMES
                    .contains(&method))
            // An allomorph matched against an object (§9.191).
            || (method == "ACCEPTS"
                && matches!(target.view(), ValueView::Mixin(_, m) if m.contains_key("Str")))
        {
            let owner_names = self.late_walk_owners(&target);
            let owners: Vec<&str> = owner_names.iter().map(String::as_str).collect();
            if let Some(result) = crate::builtins::method_table::invoke_mro_late(
                self, &owners, method, &args, &target,
            ) {
                return Some(result);
            }
        }
        match method {
            // ADR-0051 §3 (#9948): this interceptor is receiver-blind (it
            // reads any value as a number), so a receiver whose ancestry
            // declares no `polymod` (`Str`, `Complex`, ...) falls through to
            // normal resolution and `X::Method::NotFound`, as in Rakudo.
            "VAR" if args.is_empty() => {
                // Proxy .VAR returns a decontainerized copy
                if matches!(target.view(), ValueView::Proxy { .. }) {
                    return Some(Ok(Value::proxy_var_object(target, String::new())));
                }
                Some(Ok(target))
            }
            "WHAT" if args.is_empty() && !quoted_pseudo => Some(self.dispatch_what(&target, args)),
            "HOW" if !quoted_pseudo => Some(self.dispatch_how(&target, &args)),
            "WHO" if args.is_empty() && !quoted_pseudo => Some(self.dispatch_who(&target)),
            "WHY" if args.is_empty() && !quoted_pseudo => Some(self.dispatch_why(&target)),
            "^name" if args.is_empty() => Some(self.dispatch_caret_name(&target)),
            // A grammar's `parse`/`subparse`/`parsefile` are `Grammar`'s rows
            // (ADR-11276 §9.46): a type object and an instance both reach
            // them, unless the grammar declares its own.
            "subparse" | "parse" | "parsefile" => {
                let grammar = match target.view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => return None,
                };
                if self.grammar_has_user_method(&grammar, method) {
                    return None;
                }
                let mut full = Vec::with_capacity(args.len() + 1);
                full.push(target);
                full.extend(args);
                crate::builtins::method_table::invoke_owner_raw(self, &["Grammar"], method, &full)
            }
            _ => self.text_method_by_mro(target, method, &args),
        }
    }

    /// `Str`'s, `Cool`'s and `Any`'s text methods for a call no
    /// receiver-specific answer took: the first row of the receiver's MRO, as
    /// `.^can` walks it, that binds the call (ADR-11276 §9.129). An instance
    /// with a `Str` of its own whose method is `Cool`'s or `Any`'s is read
    /// through it, as Rakudo's `self.Str.METHOD` does.
    /// A junction autothreads elsewhere; a receiver no owner of which declares
    /// the name is `X::Method::NotFound`.
    // Cost: O(m) for the MRO, m = its length, plus `invoke_mro`'s.
    fn text_method_by_mro(
        &mut self,
        target: Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !TEXT_METHODS_BY_MRO.contains(&method)
            || matches!(target.view(), ValueView::Junction { .. })
        {
            return None;
        }
        // An enum member is a value of its base type (§9.125); `.^mro` does
        // not list the base yet, so the member's roles' base is named here.
        let mro = match target.view() {
            ValueView::Enum { value, .. } => match value {
                crate::value::EnumValue::Str(_) => vec!["Str", "Cool", "Any", "Mu"],
                _ => vec!["Int", "Cool", "Any", "Mu"],
            }
            .into_iter()
            .map(String::from)
            .collect(),
            _ => self.classhow_mro_unhidden_names(&target),
        };
        let owners: Vec<&str> = mro.iter().map(String::as_str).collect();
        let method_sym = Symbol::intern(method);
        let first = owners
            .iter()
            .find(|owner| crate::builtins::method_table::owner_declares(owner, method_sym));
        if matches!(first, Some(&"Cool" | &"Any"))
            && let ValueView::Instance { class_name, .. } = target.view()
            && self.has_user_method(&class_name.resolve(), "Str")
        {
            let text = match self.call_method_with_values(target, "Str", Vec::new()) {
                Ok(text) => text,
                Err(err) => return Some(Err(err)),
            };
            return crate::builtins::method_table::invoke_mro(
                self,
                &["Str", "Cool", "Any"],
                method,
                args,
                &text,
            );
        }
        crate::builtins::method_table::invoke_mro(self, &owners, method, args, &target)
    }

    /// Dispatch "classify" / "categorize": the `Any` rows' handler and the
    /// cascade's arm. A `Supply` receiver is not served here.
    // Cost: O(e) plus one test call per element, e = elements of the invocant.
    pub(crate) fn dispatch_classify_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
        {
            return None;
        }
        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.extend(args.iter().cloned());
        call_args.push(target);
        Some(self.builtin_classify(method, &call_args))
    }

    /// Dispatch "classify-list" / "categorize-list": the `Hash` rows' handler
    /// and the cascade's arm.
    // Cost: O(e) plus one test call per element, e = elements of the list.
    pub(crate) fn dispatch_classify_list_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
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
            return Err(err);
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
        self.builtin_classify(classify_name, &call_args)
            .map(|r| with_set_mutability(r, result_mutable))
    }

    /// Dispatch "match": the `Any.match` row's handler and the cascade's arm.
    /// A regex argument may be a closure over its defining scope; install it
    /// around the match the same way `~~` does.
    // Cost: O(n) in the haystack text for a plain pattern.
    pub(crate) fn dispatch_match_row(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.with_regex_closure_scope(args.first().cloned(), |me| {
            me.dispatch_match_method(target, &args)
        })
    }

    /// Helper for .comb with arguments.
    /// Handles: .comb($matcher), .comb($matcher, $limit), .comb($matcher, :match)
    pub(crate) fn dispatch_comb_with_args(
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
                let seq = crate::builtins::comb::comb_pure(&target, Some(m), limit)
                    .expect("comb_pure always handles Int/Str matchers");
                Some(Ok(seq))
            }
            // Cost: O(n + k) plus the engine's per-match cost, n = chars of the invocant,
            // k = matches; with `$limit` the search stops at the k-th match. With
            // `:match` every Match shares one `MatchTarget`.
            // An `rx//` / `rx:i//` literal is a `RegexWithAdverbs` whose
            // pattern already carries its inline adverbs (`":i A"`); it combs
            // exactly like a bare `/.../`, not as its stringified form.
            Some(view @ (ValueView::Regex(_) | ValueView::RegexWithAdverbs(_))) => {
                let pat: String = match view {
                    ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
                    ValueView::Regex(p) => p.to_string(),
                    _ => String::new(),
                };
                let max = limit.map_or(usize::MAX, |lim| lim as usize);
                // Use the capturing path only when the regex contains code
                // blocks whose side effects must fire (e.g. `{ take $/.Str }`).
                // For regular regexes, use the faster non-capturing path.
                // The capture markers `<(` / `)>` also need the capturing path:
                // `.comb` returns each match's `.Str`, which they narrow.
                let has_markers = pat.contains("<(") || pat.contains(")>");
                // `:match` hands back each Match whole, captures included
                // (`$path.comb($rx, :match)` reading `$<var>`; Path::Map), so it
                // takes the capturing path and builds the Matches from it.
                if return_match {
                    let mut matches = self.regex_find_all_with_caps_limited(&pat, &text, max);
                    let result = matches
                        .iter_mut()
                        .map(|(_, _, caps)| {
                            if caps.named.values().any(|slot| !slot.nodes.is_empty()) {
                                let ct = caps.target_or_new(&text);
                                self.reduce_regex_captures_made(caps, Some(&ct));
                            }
                            Value::make_match_object_full(
                                caps.from as i64,
                                caps.to as i64,
                                &caps.positional,
                                &caps.named,
                                caps.target_or_new(&text),
                            )
                            .with_match_cursor_pos(caps.narrowed_pos())
                        })
                        .collect::<Vec<_>>();
                    return Some(Ok(make_seq(result)));
                }
                let spans: Vec<(usize, usize)> =
                    if self.has_code_block_in_prefix(&pat) || has_markers {
                        let mut matches = self.regex_find_all_with_caps_limited(&pat, &text, max);
                        for (_, _, caps) in &mut matches {
                            if caps.named.values().any(|slot| !slot.nodes.is_empty()) {
                                let ct = caps.target_or_new(&text);
                                self.reduce_regex_captures_made(caps, Some(&ct));
                            }
                        }
                        matches
                            .into_iter()
                            .map(|(s, e, caps)| {
                                if has_markers && caps.to >= caps.from {
                                    (caps.from, caps.to)
                                } else {
                                    (s, e)
                                }
                            })
                            .collect()
                    } else {
                        self.regex_find_all_limited(&pat, &text, max)
                    };
                Some(Ok(make_seq(Self::char_span_strs(&text, &spans))))
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
                    // No positional matcher (only nameds, e.g. a handle's
                    // `:close`): graphemes, exactly as `Str.comb` with no args.
                    // Returning `None` here made every caller invent its own
                    // answer, and the IO::Handle one answered `().Seq` (#9255).
                    crate::builtins::comb::comb_pure(&target, None, limit).map(Ok)
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

        if let ValueView::Instance { class_name, .. } = target.view() {
            // A class that declares its own `cos` (a grammar's actions class
            // with `method cos($/)`, PDF::Grammar) is not asking for the
            // numeric coercion: leave it to ordinary method dispatch.
            if self.has_visible_user_method(class_name.as_str(), method) {
                return None;
            }
            // An `Int` subclass's `.Numeric` is the instance itself, so it
            // takes its `Int` payload instead of re-entering this dispatch.
            let coerced = if let Some(payload) =
                crate::builtins::native_base::numeric_subclass_payload(&target)
            {
                payload
            } else if let Ok(v) = self.call_method_with_values(target.clone(), "Numeric", vec![]) {
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

impl Interpreter {
    /// True when the instance's class (or an ancestor) declares its own `print`.
    fn instance_has_own_print(&mut self, target: &Value) -> bool {
        let ValueView::Instance { class_name, .. } = target.view() else {
            return false;
        };
        let cls = class_name.resolve();
        self.class_has_user_method(&cls, "print")
    }

    /// `Mu.say(\x)` / `Mu.say(|)` (and `put`): the fallback for a class that
    /// acts as `$*OUT`/`$*ERR` by declaring its own `print` but no `say`/`put`
    /// -- the arguments are gisted (`say`) or stringified (`put`), joined,
    /// followed by `nl-out`, and handed to that `print`. `None` for a
    /// receiver with no `print` of its own.
    // TODO: compile to bytecode -- this re-enters user code through the generic
    // method-call path like the other `Mu` fallbacks.
    // Cost: O(n), n = total chars of the rendered arguments.
    pub(crate) fn mu_say_put_via_print(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.is_empty()
            || !matches!(target.view(), ValueView::Instance { .. })
            || !self.instance_has_own_print(target)
        {
            return None;
        }
        Some((|| {
            let mut text = String::new();
            for a in args {
                if method == "say" {
                    text.push_str(&self.render_gist_value(a)?);
                } else {
                    text.push_str(
                        &self
                            .call_method_with_values(a.clone(), "Str", vec![])?
                            .to_string_value(),
                    );
                }
            }
            let nl = self.call_method_with_values(target.clone(), "nl-out", vec![])?;
            text.push_str(&nl.to_string_value());
            self.call_method_with_values(target.clone(), "print", vec![Value::str(text)])
        })())
    }

    /// `Junction.THREAD(&code)`: call the code once per eigenstate. `None` for
    /// a receiver that is no junction.
    // Cost: O(v) calls, v = the junction's values.
    pub(crate) fn junction_thread(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Junction { values, .. } = target.view() else {
            return None;
        };
        let code = args.first().cloned().unwrap_or(Value::NIL);
        for value in values.iter() {
            match self.call_sub_value(code.clone(), vec![value.clone()], false) {
                Ok(_) => {}
                // A matching `when` inside the block leaves that block with
                // `succeed`. That ends the *eigenstate's* iteration, not the
                // THREAD loop -- every loop construct absorbs it the same way
                // (see the `for` body in vm_for_loop_body.rs). Propagating it
                // aborted the remaining eigenstates and unwound the enclosing
                // routine: Test::Util's `is-deeply-junction` guts only the first
                // eigenstate of `any(all(1,2), 3)` without this.
                Err(e) if e.is_succeed() => {}
                Err(e) => return Some(Err(e)),
            }
        }
        Some(Ok(Value::NIL))
    }
}
