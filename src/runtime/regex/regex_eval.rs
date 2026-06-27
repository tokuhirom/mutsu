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
            let (functions, proto_functions, token_defs) = {
                let src = self.registry();
                (
                    src.functions.clone(),
                    src.proto_functions.clone(),
                    src.token_defs.clone(),
                )
            };
            let mut dst = target.registry_mut();
            dst.functions = functions;
            dst.proto_functions = proto_functions;
            dst.token_defs = token_defs;
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
        let snapshot = self.registry().clone();
        *target.registry_mut() = snapshot;
    }

    /// Evaluate a closure interpolation `<{ code }>` inside a regex.
    /// Returns the regex pattern string to match against.
    pub(super) fn eval_regex_closure_interpolation(
        &self,
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
            ..Default::default()
        };
        self.copy_decl_registry_into(&mut interp);
        let val = match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(e) => e.return_value?,
        };
        match val {
            Value::Regex(pat) => Some(pat.to_string()),
            Value::RegexWithAdverbs(a) => Some(a.pattern.to_string()),
            Value::Routine {
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
            Value::Array(ref elems, ..) => {
                // Array/List -> alternation of escaped literals
                let alts: Vec<String> = elems
                    .iter()
                    .map(|v| match v {
                        Value::Regex(pat) => pat.to_string(),
                        Value::RegexWithAdverbs(a) => a.pattern.to_string(),
                        other => {
                            let s = other.to_string_value();
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
            Value::Seq(ref elems) => {
                // Array/List -> alternation of escaped literals
                let alts: Vec<String> = elems
                    .iter()
                    .map(|v| match v {
                        Value::Regex(pat) => pat.to_string(),
                        Value::RegexWithAdverbs(a) => a.pattern.to_string(),
                        other => {
                            let s = other.to_string_value();
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
            other => {
                let s = other.to_string_value();
                Some(s)
            }
        }
    }

    /// Evaluate a code assertion inside a regex.
    /// Sets up $0, $1, etc. from current captures, evaluates the code,
    /// and returns whether the result is truthy.
    pub(super) fn eval_regex_code_assertion(&self, code: &str, caps: &RegexCaptures) -> bool {
        // We need to evaluate in a mutable context but `self` is &self.
        // Use unsafe interior mutability pattern via a clone-and-eval approach.
        // Parse the code first.
        let (stmts, _) = match crate::parse_dispatch::parse_source(code) {
            Ok(result) => result,
            Err(_) => return false,
        };
        // Create a minimal interpreter with env set up
        let mut env = self.env.clone();
        // Set positional capture variables ($0, $1, etc.)
        for (i, val) in caps.positional.iter().enumerate() {
            env.insert(i.to_string(), Value::str(val.clone()));
        }
        // Build $/ as an array for $/[n] access
        let match_list: Vec<Value> = caps
            .positional
            .iter()
            .map(|s| Value::str(s.clone()))
            .collect();
        env.insert("/".to_string(), Value::array(match_list));
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
                env.insert(format!("<{}>", k), m.clone());
                continue;
            }
            let value = if v.len() == 1 {
                Value::str(v[0].clone())
            } else {
                Value::array(v.iter().cloned().map(Value::str).collect())
            };
            env.insert(format!("<{}>", k), value);
        }
        // Evaluate the code in a fresh interpreter with this env
        let mut interp = Interpreter {
            env,
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Default::default()
        };
        self.copy_decl_registry_into(&mut interp);
        match interp.eval_block_value(&stmts) {
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
        &self,
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
        let Value::Instance { attributes, .. } = &full else {
            return out;
        };
        let attr_map = attributes.as_map();
        let Some(Value::Hash(named)) = attr_map.get("named") else {
            return out;
        };
        let mut scratch = Interpreter {
            env: self.env.clone(),
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Default::default()
        };
        self.copy_full_registry_into(&mut scratch);
        for (k, child) in named.iter() {
            let ran = match child {
                Value::Array(items, meta) => {
                    let mut acc = Vec::with_capacity(items.len());
                    for it in items.as_ref() {
                        let dn = Interpreter::get_action_name(it).unwrap_or_else(|| k.clone());
                        let m = scratch
                            .invoke_grammar_actions(it.clone(), &mut actions, &dn)
                            .unwrap_or_else(|_| it.clone());
                        acc.push(m);
                    }
                    Value::Array(Arc::new(crate::value::ArrayData::new(acc)), *meta)
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
        &self,
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
        let mut actions = match &actions {
            Value::Instance {
                class_name,
                attributes,
                id,
            } => Value::Instance {
                class_name: *class_name,
                attributes: Arc::new((**attributes).clone()),
                id: *id,
            },
            other => other.clone(),
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
            ..Default::default()
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
        attrs.insert(flag.to_string(), Value::Bool(true));
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
