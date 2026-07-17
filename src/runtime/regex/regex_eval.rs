use super::super::*;

impl Interpreter {
    /// Copy the declaration registry (functions / proto_functions / token_defs)
    /// from `self` into a freshly-built sub-interpreter used for regex/grammar
    /// evaluation. `self` and `target` have distinct registry `Arc`s, so this is
    /// a snapshot copy (matches the prior per-field clone in the struct literal).
    pub(crate) fn copy_decl_registry_into(&self, target: &mut Interpreter) {
        // During a `Grammar.parse(:actions(...))`, a sub-interpreter spawned for
        // subrule/proto-regex matching may evaluate a `<?{ $<x>.made ... }>`
        // assertion, which has to dispatch the action class's methods (in
        // `Registry::classes`). Copy the *full* registry in that case so those
        // methods are reachable; otherwise keep the lean three-field copy that the
        // common (action-less) regex/closure eval path relies on.
        if self.current_grammar_actions.is_some() {
            self.copy_full_registry_into(target);
        } else {
            let (functions, proto_functions, token_defs, enum_types) = {
                let src = self.registry();
                (
                    src.functions.clone(),
                    src.proto_functions.clone(),
                    src.token_defs.clone(),
                    src.enum_types.clone(),
                )
            };
            let mut dst = target.registry_mut();
            dst.functions = functions;
            dst.proto_functions = proto_functions;
            dst.token_defs = token_defs;
            // Inherit the parent's enum types (built-in Order/Signal/... plus any
            // user-declared enums). The scratch interpreter is built via
            // `new_regex_scratch`, which skips seeding the built-in enums into its
            // own registry, so copy them from the parent here — this also makes a
            // regex closure see user-defined enums it previously could not.
            dst.enum_types = enum_types;
        }
        // Propagate the in-progress `Grammar.parse(:actions(...))` object so the
        // assertion's sub-interpreter can still run the action method mid-parse.
        // None outside a parse, so this is a no-op there.
        target.current_grammar_actions = self.current_grammar_actions.clone();
    }

    /// Snapshot the *entire* declaration registry (classes, roles, methods,
    /// proto-methods, ... in addition to functions/tokens) into `target`. Needed
    /// when the sub-interpreter must dispatch user class methods — e.g. running
    /// grammar action methods on the `:actions` object during an in-parse
    /// `<?{ $<x>.made ... }>` assertion (see `run_named_capture_actions`), where
    /// the action class's methods live in `Registry::classes`, which the leaner
    /// `copy_decl_registry_into` omits.
    pub(crate) fn copy_full_registry_into(&self, target: &mut Interpreter) {
        // The sub-interpreter only READS the registry during regex/grammar
        // evaluation (dispatching methods, resolving tokens/actions); it never
        // declares new classes into it. So rather than deep-cloning the entire
        // registry (all classes/roles/methods) on every call — catastrophic in a
        // hot loop matching thousands of grammar tokens, each triggering a full
        // clone — reuse a single cached snapshot behind an `Arc<RwLock>`, rebuilt
        // only when the registry actually changed (tracked by `registry_write_gen`).
        //
        // Isolation is preserved: the snapshot is a SEPARATE `Arc<RwLock>` from
        // `self.registry`, so any (rare) write a sub-interpreter makes lands on the
        // shared snapshot, never leaking back into the parent's registry. Each
        // thread has its own `Interpreter` (and thus its own cache), so the shared
        // snapshot lock is never contended across threads.
        let cur_gen = self
            .registry_write_gen
            .load(std::sync::atomic::Ordering::Relaxed);
        {
            let cache = self.regex_registry_snapshot.lock().unwrap();
            if let Some((cached_gen, snapshot)) = &*cache
                && *cached_gen == cur_gen
            {
                target.registry = Arc::clone(snapshot);
                return;
            }
        }
        let snapshot = Arc::new(RwLock::new(self.registry().clone()));
        *self.regex_registry_snapshot.lock().unwrap() = Some((cur_gen, Arc::clone(&snapshot)));
        target.registry = snapshot;
    }

    /// Evaluate a closure interpolation `<{ code }>` inside a regex.
    /// Returns the regex pattern string to match against.
    pub(super) fn eval_regex_closure_interpolation(
        &mut self,
        code: &str,
        caps: &RegexCaptures,
        target: &str,
    ) -> Option<String> {
        let mut env = self.make_regex_eval_env(caps);
        // Set $_ to the match target string
        env.insert("_".to_string(), Value::str(target.to_string()));
        // Add variables declared via :my inside the regex
        for (k, v) in &caps.regex_vars {
            env.insert(k.clone(), v.clone());
        }
        let (stmts, _) = crate::parse_dispatch::parse_source(code).ok()?;
        let mut interp = Interpreter {
            env,
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Self::new_regex_scratch()
        };
        self.copy_decl_registry_into(&mut interp);
        let val = match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(e) => e.return_value?,
        };
        match val.view() {
            ValueView::Regex(pat) => Some(pat.to_string()),
            ValueView::RegexWithAdverbs(a) => Some(a.pattern.to_string()),
            ValueView::Routine {
                is_regex: true,
                name,
                package,
            } => {
                let full_name = if package.resolve().is_empty() {
                    name.resolve()
                } else {
                    format!("{}::{}", package, name)
                };
                Some(format!("<{}>", full_name))
            }
            ValueView::Array(elems, ..) => {
                // Array/List -> alternation of escaped literals
                let alts: Vec<String> = elems
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Regex(pat) => pat.to_string(),
                        ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
                        _ => {
                            let s = v.to_string_value();
                            // Quote as regex literal using single quotes
                            format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'"))
                        }
                    })
                    .collect();
                if alts.is_empty() {
                    return None;
                }
                Some(format!("[ {} ]", alts.join(" | ")))
            }
            ValueView::Seq(elems) => {
                // Array/List -> alternation of escaped literals
                let alts: Vec<String> = elems
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Regex(pat) => pat.to_string(),
                        ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
                        _ => {
                            let s = v.to_string_value();
                            // Quote as regex literal using single quotes
                            format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'"))
                        }
                    })
                    .collect();
                if alts.is_empty() {
                    return None;
                }
                Some(format!("[ {} ]", alts.join(" | ")))
            }
            _ => {
                let s = val.to_string_value();
                Some(s)
            }
        }
    }

    /// Evaluate a code assertion inside a regex, **on this interpreter**.
    ///
    /// Raku semantics: the assertion runs inline, once, where the cursor reaches
    /// it, and its side effects are real — visible to later assertions in the same
    /// match, and surviving a match that ultimately fails. `advent2013-day18` needs
    /// all three (`%*PLAYED{$card}++` must be visible to the next card's assertion;
    /// `@dups.push` must survive the failing parses). A scratch interpreter cannot
    /// provide any of them, so this runs in `self` (ADR-0009 part B).
    ///
    /// Only the *regex-internal* bindings this sets up (`$/`, `$0`…, `$<name>`) and
    /// the assertion's own `my` declarations are scoped: they are saved and
    /// restored around the body. Every other write is a genuine side effect and is
    /// left in place — that is the whole point.
    pub(super) fn eval_regex_code_assertion(
        &mut self,
        code: &str,
        caps: &RegexCaptures,
        matched_so_far: &str,
    ) -> bool {
        let (stmts, _) = match crate::parse_dispatch::parse_source(code) {
            Ok(result) => result,
            Err(_) => return false,
        };
        // The bindings to install for the body, and to restore afterwards.
        let mut env: Vec<(String, Value)> = Vec::new();
        // Set positional capture variables ($0, $1, etc.)
        for (i, val) in caps.positional.iter().enumerate() {
            env.push((i.to_string(), Value::str(val.clone())));
        }
        // Build `$/` as a proper Match object so `$/.Str`/`$/.lc`/`~$/` yield the
        // matched-so-far text (not just an array of positional captures). A
        // `<?{ … $/.lc … }>` assertion inside a `token` relies on this (the card
        // grammar's dup check does `%*PLAYED{$/.lc}++`). `$/[n]` still indexes the
        // positional captures on the Match object.
        env.push((
            "/".to_string(),
            Value::make_match_object_with_captures(
                matched_so_far.to_string(),
                caps.match_from as i64,
                (caps.match_from + matched_so_far.chars().count()) as i64,
                &caps.positional,
                &caps.named,
            ),
        ));
        // When the assertion references `.made` AND we are inside a
        // `Grammar.parse(:actions(...))`, run the relevant action method on each
        // just-matched named capture so `$<x>.made` is available *during* the
        // parse. raku runs actions incrementally at reduce time; mutsu otherwise
        // only runs them post-parse, which leaves `.made` undefined here (e.g.
        // Template::Mustache's standalone-line rule:
        // `token linetag { ^^ (\h*) <tag> <?{ $<tag>.made<type> ~~ none(...) }> ... }`).
        // The actions run in a scratch interpreter so the assertion's own
        // `$/`/`$0` env below is not clobbered by the action dispatch.
        let made_named: HashMap<String, Value> = if code.contains(".made")
            && let Some(actions0) = self.current_grammar_actions.clone()
        {
            self.run_named_capture_actions(caps, actions0)
        } else {
            HashMap::new()
        };

        // Set named captures
        for (k, v) in &caps.named {
            if let Some(m) = made_named.get(k) {
                env.push((format!("<{}>", k), m.clone()));
                continue;
            }
            let value = if v.len() == 1 {
                Value::str(v[0].clone())
            } else {
                Value::array(v.iter().cloned().map(Value::str).collect())
            };
            env.push((format!("<{}>", k), value));
        }
        // The assertion's own `my` declarations are lexical to it, so scope them
        // alongside the regex bindings. `eval_block_value` does not scope plain
        // lexicals (mutsu's env is flat), and without this an assertion's
        // `my $card = …` would clobber a same-named variable in the enclosing
        // scope — day18's assertion declares exactly that.
        let mut scoped: Vec<String> = env.iter().map(|(k, _)| k.clone()).collect();
        for stmt in &stmts {
            if let Stmt::VarDecl { name, .. } = stmt
                && !scoped.contains(name)
            {
                scoped.push(name.clone());
            }
        }
        let saved: Vec<(String, Option<Value>)> = scoped
            .iter()
            .map(|k| (k.clone(), self.env.get(k).cloned()))
            .collect();
        for (k, v) in env {
            self.env.insert(k, v);
        }
        let result = self.eval_block_value(&stmts);
        for (k, orig) in saved {
            match orig {
                Some(v) => self.env.insert(k, v),
                None => self.env.remove(&k),
            };
        }
        match result {
            Ok(val) => val.truthy(),
            Err(_) => false,
        }
    }

    /// Build a Match object for each named capture in `caps` and run its grammar
    /// action method (proto-regex `:sym<>` variant aware) so the resulting Match
    /// carries `.made`. Used by `eval_regex_code_assertion` to support
    /// `$<x>.made` inside `<?{ ... }>` assertions during parsing. Runs in a
    /// scratch interpreter to avoid mutating the caller's env. Best-effort: a
    /// capture whose action errors or is absent maps to its un-actioned Match.
    fn run_named_capture_actions(
        &mut self,
        caps: &RegexCaptures,
        mut actions: Value,
    ) -> HashMap<String, Value> {
        let mut out = HashMap::new();
        let full = Value::make_match_object_full_q(
            caps.matched.clone(),
            caps.from as i64,
            caps.to as i64,
            &caps.positional,
            &caps.named,
            &caps.named_subcaps,
            &caps.positional_subcaps,
            &caps.positional_quantified,
            &caps.positional_nil,
            None,
            &caps.named_quantified,
        );
        let ValueView::Instance { attributes, .. } = full.view() else {
            return out;
        };
        let attr_map = attributes.as_map();
        let Some(ValueView::Hash(named)) = attr_map.get("named").map(Value::view) else {
            return out;
        };
        let mut scratch = Interpreter {
            env: self.env.clone(),
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Self::new_regex_scratch()
        };
        self.copy_full_registry_into(&mut scratch);
        for (k, child) in named.iter() {
            let ran = match child.view() {
                ValueView::Array(items, meta) => {
                    let mut acc = Vec::with_capacity(items.len());
                    for it in items.as_ref() {
                        let dn = Interpreter::get_action_name(it).unwrap_or_else(|| k.clone());
                        let m = scratch
                            .invoke_grammar_actions(it.clone(), &mut actions, &dn)
                            .unwrap_or_else(|_| it.clone());
                        acc.push(m);
                    }
                    Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(acc)),
                        meta,
                    )
                }
                _ => {
                    let dn = Interpreter::get_action_name(child).unwrap_or_else(|| k.clone());
                    scratch
                        .invoke_grammar_actions(child.clone(), &mut actions, &dn)
                        .unwrap_or_else(|_| child.clone())
                }
            };
            out.insert(k.clone(), ran);
        }
        out
    }

    /// Reduce-time grammar action hook for a `<subrule>` quantifier iteration.
    /// Called from the quantifier loop after each iteration commits. When the
    /// live parse is action-driven AND its matching already depends on a `$*`
    /// dynamic var (the SEEN gate), run THIS iteration's subrule action so any
    /// dyn-var write it performs (e.g. Template::Mustache's delimiter finalizer
    /// `($*LEFT,$*RIGHT)=@delim`) is published to the overlay and thus visible to
    /// the next iteration's pattern interpolation. No-op for ordinary grammars.
    pub(super) fn maybe_run_reduce_time_dynvar_action(
        &mut self,
        token: &RegexToken,
        new_caps: &RegexCaptures,
    ) {
        if !super::regex_helpers::dynvar_overlay_active() || !super::regex_helpers::dynvar_seen() {
            return;
        }
        let Some(actions) = self.current_grammar_actions.clone() else {
            return;
        };
        let rule_name = match &token.atom {
            RegexAtom::Named(n) => n.trim().to_string(),
            _ => return,
        };
        // The sub-capture this iteration stored under (explicit `$<alias>=` wins).
        let cap_name = token
            .named_capture
            .clone()
            .unwrap_or_else(|| rule_name.clone());
        let Some(sub) = new_caps
            .named_subcaps
            .get(&cap_name)
            .and_then(|v| v.last())
            .cloned()
        else {
            return;
        };
        self.run_reduce_time_action(&sub, &rule_name, actions);
    }

    /// Run a single subrule's grammar action in a scratch interpreter and publish
    /// any `$*` dynamic-var writes it makes into the reduce-time overlay. The
    /// scratch is seeded with the overlay's current values so the action sees the
    /// latest dynamic state; only vars whose value actually changes are published.
    fn run_reduce_time_action(&self, sub: &RegexCaptures, rule_name: &str, actions: Value) {
        // Run on an INDEPENDENT deep copy of the actions object so any `self`
        // attribute the action mutates does not leak into the real actions
        // object — the authoritative post-parse action pass will run again and is
        // the one whose `make`/`self` effects count. The reduce-time pass exists
        // ONLY to extract `$*` dynamic-var writes (which flow through the scratch
        // env into the overlay, not through actions state). `InstanceAttrs::clone`
        // is a deep, fresh-cell copy.
        let mut actions = match actions.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } => Value::instance_parts(class_name, crate::gc::Gc::new((**attributes).clone()), id),
            _ => actions.clone(),
        };
        let match_obj = Value::make_match_object_full_q(
            sub.matched.clone(),
            sub.from as i64,
            sub.to as i64,
            &sub.positional,
            &sub.named,
            &sub.named_subcaps,
            &sub.positional_subcaps,
            &sub.positional_quantified,
            &sub.positional_nil,
            None,
            &sub.named_quantified,
        );
        let mut scratch = Interpreter {
            env: self.env.clone(),
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Self::new_regex_scratch()
        };
        self.copy_full_registry_into(&mut scratch);
        // Seed the scratch's `$*` vars from the overlay (latest delimiters etc.).
        for (k, v) in super::regex_helpers::dynvar_overlay_snapshot() {
            scratch.env.insert(k, v);
        }
        // Baseline of `$*` vars visible to the action, to diff after it runs.
        let baseline: HashMap<String, Value> = scratch
            .env
            .iter()
            .filter(|(k, _)| k.starts_with("*"))
            .map(|(k, v)| (k.with_str(|s| s.to_string()), v.clone()))
            .collect();
        let _ = scratch.invoke_grammar_actions(match_obj, &mut actions, rule_name);
        // Publish changed `$*` vars into the overlay for subsequent matching.
        let changed: Vec<(String, Value)> = scratch
            .env
            .iter()
            .filter(|(k, _)| k.starts_with("*"))
            .filter_map(|(k, v)| {
                let name = k.with_str(|s| s.to_string());
                match baseline.get(&name) {
                    Some(old) if old == v => None,
                    _ => Some((name, v.clone())),
                }
            })
            .collect();
        for (name, val) in changed {
            super::regex_helpers::dynvar_overlay_put(&name, val);
        }
    }

    /// Create an X::Syntax::Regex::QuantifierValue exception with the given flag attribute set to True.
    pub(super) fn make_quantifier_value_error(flag: &str, message: &str) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.to_string()));
        attrs.insert(flag.to_string(), Value::TRUE);
        let ex = Value::make_instance(
            crate::symbol::Symbol::intern("X::Syntax::Regex::QuantifierValue"),
            attrs,
        );
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        err
    }

    /// Set a pending quantifier-value error in the thread-local error store.
    pub(super) fn set_quantifier_value_error(flag: &str, message: &str) {
        let err = Self::make_quantifier_value_error(flag, message);
        crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
            *e.borrow_mut() = Some(err);
        });
    }
}
