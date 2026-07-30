use super::super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Evaluate a `** {code}` quantifier code block and return (min, max).
    /// The code should return either a numeric value (exact count) or a Range.
    /// Returns None if the code fails to evaluate or produces an invalid/infinite value;
    /// in invalid cases a pending error is set via PENDING_REGEX_ERROR for the caller to propagate.
    pub(super) fn eval_regex_repeat_code(
        &mut self,
        code: &str,
        caps: &RegexCaptures,
    ) -> Option<(usize, Option<usize>)> {
        let stmts = self.parse_regex_code_cached(code)?;
        let env = self.make_regex_eval_env(caps);
        let mut interp = Interpreter {
            env,
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Self::new_regex_scratch()
        };
        self.copy_decl_registry_into(&mut interp);
        let val = match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(_) => return None,
        };

        /// Helper: check if a Value is a non-numeric type (Str/Bool/etc.) for range endpoints.
        fn is_non_numeric_value(v: &Value) -> bool {
            matches!(v.view(), ValueView::Str(_) | ValueView::Bool(_))
        }

        /// Helper: extract f64 from a Value, treating NaN/Inf specially.
        fn endpoint_to_f64(v: &Value) -> f64 {
            match v.view() {
                ValueView::Num(n) => n,
                ValueView::Int(i) => i as f64,
                ValueView::Rat(n, d) => n as f64 / d as f64,
                _ => v.to_f64(),
            }
        }

        match val.view() {
            ValueView::Range(start, end) => {
                let min = start.max(0) as usize;
                let max = if end == i64::MAX {
                    None
                } else {
                    Some(end.max(0) as usize)
                };
                // Check for empty range (min > max)
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            ValueView::RangeExcl(start, end) => {
                let min = start.max(0) as usize;
                let max = if end == i64::MAX {
                    None
                } else {
                    Some((end - 1).max(0) as usize)
                };
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            ValueView::RangeExclStart(start, end) => {
                let min = (start + 1).max(0) as usize;
                let max = if end == i64::MAX {
                    None
                } else {
                    Some(end.max(0) as usize)
                };
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            ValueView::RangeExclBoth(start, end) => {
                let min = (start + 1).max(0) as usize;
                let max = if end == i64::MAX {
                    None
                } else {
                    Some((end - 1).max(0) as usize)
                };
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // Check for non-numeric range endpoints (e.g., strings, NaN endpoints)
                if is_non_numeric_value(start.as_ref()) || is_non_numeric_value(end.as_ref()) {
                    Self::set_quantifier_value_error(
                        "non-numeric-range",
                        "Quantifier range has non-numeric endpoint",
                    );
                    return None;
                }
                let start_f = endpoint_to_f64(start.as_ref());
                let end_f = endpoint_to_f64(end.as_ref());
                // NaN in either endpoint → non-numeric-range
                if start_f.is_nan() || end_f.is_nan() {
                    Self::set_quantifier_value_error(
                        "non-numeric-range",
                        "Quantifier range has non-numeric (NaN) endpoint",
                    );
                    return None;
                }
                // Inf as start → error (infinite lower bound)
                if start_f.is_infinite() && start_f > 0.0 {
                    Self::set_quantifier_value_error("inf", "Quantifier lower bound is Inf");
                    return None;
                }
                // Compute min and max.
                // For float ranges, Raku uses floor for the inclusive bound
                // and floor+1 for the exclusive bound.
                let min_f = if start_f.is_infinite() && start_f < 0.0 {
                    // -Inf start: effective min is 0
                    0.0
                } else if excl_start {
                    start_f.floor() + 1.0
                } else {
                    start_f.floor()
                };
                let min = if min_f < 0.0 { 0 } else { min_f as usize };

                let max = if end_f.is_infinite() && end_f > 0.0 {
                    None // +Inf end → unbounded
                } else {
                    let max_f = if excl_end {
                        end_f.ceil() - 1.0
                    } else {
                        end_f.floor()
                    };
                    let max_val = if max_f < 0.0 { 0 } else { max_f as usize };
                    Some(max_val)
                };
                // Empty range check
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            ValueView::Str(s) => {
                // String values that cannot parse as a number are non-numeric
                match s.trim().parse::<f64>() {
                    Ok(n) if n.is_nan() => {
                        Self::set_quantifier_value_error(
                            "non-numeric",
                            "Quantifier value is not numeric",
                        );
                        None
                    }
                    Ok(n) if n.is_infinite() && n > 0.0 => {
                        Self::set_quantifier_value_error("inf", "Quantifier value is Inf");
                        None
                    }
                    Ok(n) => {
                        let n = n.max(0.0) as usize;
                        Some((n, Some(n)))
                    }
                    Err(_) => {
                        // Non-parseable string like "meow"
                        Self::set_quantifier_value_error(
                            "non-numeric",
                            "Quantifier value is not numeric",
                        );
                        None
                    }
                }
            }
            _ => {
                let n = val.to_f64();
                // Non-numeric value (NaN)
                if n.is_nan() {
                    Self::set_quantifier_value_error(
                        "non-numeric",
                        "Quantifier value is not numeric",
                    );
                    return None;
                }
                // Positive Inf is an error; negative Inf is treated as 0.
                if n.is_infinite() && n > 0.0 {
                    Self::set_quantifier_value_error("inf", "Quantifier value is Inf");
                    return None;
                }
                let n = n.max(0.0) as usize;
                Some((n, Some(n)))
            }
        }
    }

    /// Enable eager collection of plain code blocks during regex matching.
    /// When enabled, code blocks are recorded even if the overall match fails.
    pub(in crate::runtime) fn enable_eager_code_blocks(&self) {
        super::regex_helpers::EAGER_CODE_BLOCKS.with(|slot| *slot.borrow_mut() = Some(Vec::new()));
    }

    /// Drain and return eagerly-collected code blocks, disabling collection.
    pub(in crate::runtime) fn drain_eager_code_blocks(&self) -> Vec<CodeBlockContext> {
        super::regex_helpers::EAGER_CODE_BLOCKS
            .with(|slot| slot.borrow_mut().take().unwrap_or_default())
    }

    /// Execute code blocks collected during regex matching for side effects.
    pub(in crate::runtime) fn execute_regex_code_blocks(
        &mut self,
        code_blocks: &[CodeBlockContext],
    ) {
        for ctx in code_blocks {
            let Some(stmts) = self.parse_regex_code_cached(&ctx.code) else {
                continue;
            };
            self.setup_regex_code_block_env(ctx);
            self.eval_regex_code_block_body(&stmts);
        }
    }

    /// Install `$/` (matched-so-far), `$¢`, `$0…` and `$<name>` in the env from a
    /// single code-block context, i.e. the capture state as it stood at the block's
    /// textual position during matching (so a mid-rule `{ … $/ … }` sees the
    /// prefix matched so far, not the whole rule).
    fn setup_regex_code_block_env(&mut self, ctx: &CodeBlockContext) {
        let ast_hint = self.env.get("made").cloned().unwrap_or(Value::NIL);
        let to_match_with_ast = |text: &str, ast: &Value| -> Value {
            let match_obj = Value::make_match_object_with_captures(
                text.to_string(),
                0,
                text.chars().count() as i64,
                &[],
                &HashMap::new(),
            );
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = match_obj.view()
            {
                let attrs = attributes.as_ref().clone();
                attrs.insert("ast".to_string(), ast.clone());
                Value::make_instance(class_name, (attrs).to_map())
            } else {
                match_obj
            }
        };
        // Build the named-capture submatches once, so they can be installed BOTH
        // as `$<name>` variables AND into the `$/` match object's `named`
        // attribute — otherwise `$/.hash` / `$/.values` (and hence
        // `{ make $/.values[0].ast }`, as YAMLish's `Schema::JSON` TOP does) see
        // an empty match at reduce time while `$<name>` alone worked.
        let mut named_map: HashMap<String, Value> = HashMap::new();
        for (k, v) in &ctx.named {
            let value = if v.len() == 1 {
                to_match_with_ast(&v[0], &ast_hint)
            } else {
                Value::array(
                    v.iter()
                        .map(|s| to_match_with_ast(s, &ast_hint))
                        .collect::<Vec<_>>(),
                )
            };
            self.env.insert(format!("<{}>", k), value.clone());
            named_map.insert(k.clone(), value);
        }
        // Positional submatches ($0, $1, ...), also folded into `$/.list`.
        let mut pos_list: Vec<Value> = Vec::with_capacity(ctx.positional.len());
        for (i, val) in ctx.positional.iter().enumerate() {
            let pos_match = Value::make_match_object_with_captures(
                val.clone(),
                0,
                val.chars().count() as i64,
                &[],
                &HashMap::new(),
            );
            self.env.insert(i.to_string(), pos_match.clone());
            pos_list.push(pos_match);
        }
        // Set up $/ as a match object for the matched-so-far text, carrying the
        // named/positional captures so `$/.hash`/`$/.values`/`$/<name>` all work.
        let match_obj = Value::make_match_object_with_captures(
            ctx.matched_so_far.clone(),
            0,
            ctx.matched_so_far.chars().count() as i64,
            &[],
            &HashMap::new(),
        );
        let match_obj = if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = match_obj.view()
        {
            let attrs = attributes.as_ref().clone();
            attrs.insert("named".to_string(), Value::hash(named_map));
            if !pos_list.is_empty() {
                attrs.insert("list".to_string(), Value::array(pos_list));
            }
            Value::make_instance(class_name, attrs.to_map())
        } else {
            match_obj
        };
        self.env.insert("/".to_string(), match_obj.clone());
        // Set up $¢ (current match cursor) — same as $/ for in-progress match
        self.env.insert("\u{00A2}".to_string(), match_obj);
    }

    /// Evaluate one already-parsed regex `{ … }` code block body, snapshotting
    /// the env before and recording any changed variable as a pending local
    /// update for the outer VM. Assumes the block's `$/`, `$<name>`, `$0…` etc.
    /// have already been installed in `self.env` by the caller.
    pub(in crate::runtime) fn eval_regex_code_block_body(&mut self, stmts: &[crate::ast::Stmt]) {
        // Snapshot the env by *binding identity* — the cloned `Value` is an Arc
        // bump, and holding it also keeps the old allocation alive so a freed
        // address cannot be recycled into a false "unchanged".
        //
        // This used to snapshot `format!("{:?}", v)` for every name and compare
        // the strings, which made `Debug` formatting of the whole env the single
        // largest cost of a grammar parse (~20% of a `load-yaml` profile — a YAML
        // grammar runs a `{ … }` block per line, per backtrack). Identity is also
        // the *right* question: what has to be written back to the caller's local
        // is a name that was **rebound**. A container mutated in place keeps its
        // allocation, which the caller's local already shares, so it needs no
        // writeback — while a rebinding to an equal-looking value (which the
        // string comparison missed) now is reported.
        let snapshot: HashMap<Symbol, Value> =
            self.env.iter().map(|(k, v)| (*k, v.clone())).collect();
        let saved_in_block = self.in_regex_code_block;
        self.in_regex_code_block = true;
        let _ = self.eval_block_value(stmts);
        self.in_regex_code_block = saved_in_block;
        // Record changed env variables as pending local updates for the outer VM
        for (k, v) in &self.env {
            let rebound = snapshot.get(k).is_none_or(|old| !old.same_binding(v));
            if rebound {
                let name = k.resolve();
                // Slice C' (docs/vm-single-store.md, open-question #2): an
                // embedded regex `{ ... }` / `:my`/`:let` block writes a caller
                // lexical *directly* into `env`, bypassing
                // `set_env_with_main_alias`. If a carrier is active (the regex
                // ran inside an EVAL / interpreter fallback), log the name into
                // the carrier write set too, so the carrier-return writeback
                // reconciles it precisely and the blanket `env_dirty` net can be
                // dropped for non-EVAL carriers as well. Logging a superset is
                // safe — the writeback filters by the caller's compiled slots.
                if let Some(set) = self.carrier_writes.as_mut() {
                    set.insert(name.clone());
                }
                self.pending_local_updates.push((name, v.clone()));
            }
        }
    }

    /// Reduce-time bottom-up walk over the winning capture tree that runs each
    /// node's inline `{ … }` code blocks exactly once — children first — and
    /// commits the produced `make` value to that node's `ast`. Binding `$<child>`
    /// to the already-reduced child Matches (so `$<child>.made` / `$<child>».made`
    /// resolve to the children's produced values) is what makes an inline grammar
    /// action's `make [+] $<time>».made` work. The block's `$/` still comes from
    /// its own matched-so-far context, so a mid-rule `{ … $/ … }` is unaffected.
    /// Leaves the top node's `make` in `env["made"]` for the call site to read as
    /// the whole-parse `.made`.
    ///
    /// Subrule code blocks are NOT bubbled into the parent (see
    /// `build_named_candidates_from_inner`), so each `caps.code_blocks` list holds
    /// only that node's own blocks and every block runs once here.
    pub(in crate::runtime) fn reduce_regex_captures_made(
        &mut self,
        caps: &mut RegexCaptures,
        orig: Option<&str>,
    ) {
        self.reduce_regex_captures_made_for_rule(caps, orig, None);
    }

    /// [`Self::reduce_regex_captures_made`] with the name of the rule this node
    /// matched, which the parent knows (it is the capture key the node is stored
    /// under). A rule that declares `:my $*x` gets a **fresh binding per match**
    /// here: installed before its subtree reduces, read back onto the node
    /// afterwards, and carried to that node's action. Without it every match
    /// shares the one parse-wide slot `establish_grammar_dynamic_vars` set up, so
    /// the last code block to write wins for every reader
    /// (`t/grammar-per-match-dynvar-action.t`).
    pub(in crate::runtime) fn reduce_regex_captures_made_for_rule(
        &mut self,
        caps: &mut RegexCaptures,
        orig: Option<&str>,
        rule_name: Option<&str>,
    ) {
        // A fresh binding for THIS match, installed before the subtree reduces so
        // a child's code block accumulates into this match's binding (the
        // `:my %*PLAYED = (); <card>+` shape) rather than a sibling match's.
        let declared_keys = self.install_fresh_rule_dynvars(rule_name);
        // The walk mutates a node only to take/run its code blocks (writing
        // `ast`) or to record per-rule dynvar bindings. A subtree with no code
        // blocks anywhere — the overwhelmingly common case for a leaf-heavy
        // grammar parse — is left byte-identical, so descending into it through
        // `Arc::make_mut` would only deep-copy shared nodes (each is already in
        // `REDUCED_SUBRULES` and/or a `snapshot()`) for nothing. Skip those
        // subtrees outright. Only sound when no rule declares `:my $*x` dynvars
        // (a declaring rule must run even without blocks); such grammars are
        // rare and keep the full walk.
        let skip_untouched = self.grammar_rule_dynvar_decls.is_empty();
        // Children first so a parent block reading a child's `.made` sees it.
        for (key, scs) in caps.named_subcaps.iter_mut() {
            let child_rule = Self::reduce_child_rule_name(key);
            for sc in scs.iter_mut() {
                if skip_untouched && !Self::subtree_has_code_blocks(sc) {
                    continue;
                }
                crate::vm::vm_stats::record_regex_cap_makemut(Arc::strong_count(sc) > 1);
                let sc = Arc::make_mut(sc);
                let child_rule = sc.action_name.clone().unwrap_or(child_rule.clone());
                self.reduce_regex_captures_made_for_rule(sc, orig, Some(&child_rule));
            }
        }
        for sc in caps.positional_subcaps.iter_mut().flatten() {
            if skip_untouched && !Self::subtree_has_code_blocks(sc) {
                continue;
            }
            crate::vm::vm_stats::record_regex_cap_makemut(Arc::strong_count(sc) > 1);
            self.reduce_regex_captures_made(Arc::make_mut(sc), orig);
        }
        for pq in caps.positional_quantified.iter_mut().flatten() {
            for entry in pq.iter_mut() {
                if let Some(sc) = entry.3.as_mut() {
                    if skip_untouched && !Self::subtree_has_code_blocks(sc) {
                        continue;
                    }
                    crate::vm::vm_stats::record_regex_cap_makemut(Arc::strong_count(sc) > 1);
                    self.reduce_regex_captures_made(Arc::make_mut(sc), orig);
                }
            }
        }
        if caps.code_blocks.is_empty() {
            // Even with no code blocks of its own, a declaring match must record
            // what its binding holds — its action still reads it, and a sibling
            // that DOES have a block would otherwise decide the value.
            self.record_rule_dynvars(caps, &declared_keys);
            return;
        }
        // Build this node's Match so `$<name>` can carry the children's asts. The
        // block's `$/` still comes from its own matched-so-far context (below), so
        // a mid-rule `{ … $/ … }` sees the prefix — only the child `.made` values
        // read via `$<name>` / `$<name>».made` come from here.
        let node_match = Value::make_match_object_full_q(
            caps.matched.clone(),
            caps.from as i64,
            caps.to as i64,
            &caps.positional,
            &caps.named,
            &caps.named_subcaps,
            &caps.positional_subcaps,
            &caps.positional_quantified,
            &caps.positional_nil,
            orig,
            &caps.named_quantified,
        );
        // Named captures carrying a child `.made` (from the recursion above).
        let ast_named: Vec<(String, Value)> =
            if let ValueView::Instance { attributes, .. } = node_match.view() {
                match attributes.as_map().get("named").map(Value::view) {
                    Some(ValueView::Hash(named_hash)) => named_hash
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect(),
                    _ => Vec::new(),
                }
            } else {
                Vec::new()
            };
        let blocks = std::mem::take(&mut caps.code_blocks);
        let saved_match = self.env.get("/").cloned();
        // Fresh `make` slot for this node (do not inherit a sibling's value).
        self.env.remove("made");
        for ctx in &blocks {
            let Some(stmts) = self.parse_regex_code_cached(&ctx.code) else {
                continue;
            };
            self.setup_regex_code_block_env(ctx);
            // Override `$<name>` with the reduced (ast-carrying) child Matches so
            // `$<sub>.made` / `$<sub>».made` resolve to the produced values.
            for (k, v) in &ast_named {
                self.env.insert(format!("<{}>", k), v.clone());
            }
            // Fold the same ast-carrying children into `$/`'s `named` attribute so
            // `$/.hash` / `$/.values` (e.g. `{ make $/.values[0].ast }`) also see
            // the produced values — `setup_regex_code_block_env` only had the raw
            // matched text and a single `made` hint. `$/.str`/from/to still come
            // from the block's matched-so-far context (mid-rule `{ … $/ … }`).
            if !ast_named.is_empty()
                && let Some(cur) = self.env.get("/").cloned()
                && let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = cur.view()
            {
                let attrs = attributes.as_ref().clone();
                let named_hash: HashMap<String, Value> = ast_named.iter().cloned().collect();
                attrs.insert("named".to_string(), Value::hash(named_hash));
                let updated = Value::make_instance(class_name, attrs.to_map());
                self.env.insert("/".to_string(), updated.clone());
                self.env.insert("\u{00A2}".to_string(), updated);
            }
            self.eval_regex_code_block_body(&stmts);
        }
        caps.ast = self.env.get("made").cloned();
        self.record_rule_dynvars(caps, &declared_keys);
        if let Some(m) = saved_match {
            self.env.insert("/".to_string(), m);
        }
    }

    /// Read-only pre-check for the reduce walk: does this stored capture
    /// subtree contain any inline `{ … }` code blocks? When it does not (and no
    /// rule declares dynvars — checked by the caller), the walk would leave the
    /// subtree byte-identical, so the caller skips it instead of deep-copying
    /// shared `Arc` nodes via `make_mut`.
    fn subtree_has_code_blocks(caps: &RegexCaptures) -> bool {
        if !caps.code_blocks.is_empty() {
            return true;
        }
        caps.named_subcaps
            .values()
            .flatten()
            .any(|sc| Self::subtree_has_code_blocks(sc))
            || caps
                .positional_subcaps
                .iter()
                .flatten()
                .any(|sc| Self::subtree_has_code_blocks(sc))
            || caps
                .positional_quantified
                .iter()
                .flatten()
                .flatten()
                .any(|e| e.3.as_deref().is_some_and(Self::subtree_has_code_blocks))
    }

    /// The rule name a `named_subcaps` key stands for. A silent-action capture
    /// (`<.foo>`) is stored under a marker-prefixed key; everything else is
    /// stored under the capture name, which for a plain `<foo>` IS the rule.
    fn reduce_child_rule_name(key: &str) -> String {
        key.strip_prefix(crate::runtime::SILENT_ACTION_MARKER_PREFIX)
            .unwrap_or(key)
            .to_string()
    }

    /// Re-evaluate `rule_name`'s own `:my $*x = …;` declarations into the env, so
    /// this match starts from the declared value rather than from whatever an
    /// earlier match of the same rule left behind. Returns the env keys it bound
    /// (empty — and free — for the overwhelmingly common rule that declares
    /// nothing). The previous values are deliberately NOT restored afterwards:
    /// a rule that declares nothing still reads the parse-wide slot in its own
    /// action, and that behaviour is load-bearing (t/grammar-reduce-time-dynvar.t).
    fn install_fresh_rule_dynvars(&mut self, rule_name: Option<&str>) -> Vec<String> {
        if self.grammar_rule_dynvar_decls.is_empty() {
            return Vec::new();
        }
        let Some(decls) = rule_name
            .and_then(|r| self.grammar_rule_dynvar_decls.get(r))
            .cloned()
        else {
            return Vec::new();
        };
        let mut keys = Vec::new();
        for decl in &decls {
            let Some(key) = Self::dynamic_decl_var_key(decl) else {
                continue;
            };
            if let Some(stmts) = self.parse_regex_code_cached(&format!("{decl};")) {
                let _ = self.eval_block_value(&stmts);
            }
            // A `$`-sigil dynamic variable lives in env WITHOUT its sigil
            // (`$*S` -> `*S`), while `@*A` / `%*H` keep theirs — match what a
            // `my $*x` declaration actually stores, or the install below writes
            // a key nothing reads.
            keys.push(key.strip_prefix('$').unwrap_or(&key).to_string());
        }
        keys
    }

    /// Copy the current value of each key bound by
    /// [`Self::install_fresh_rule_dynvars`] onto this node, for the action walk
    /// (a separate, later pass) to re-install around this node's action.
    fn record_rule_dynvars(&mut self, caps: &mut RegexCaptures, keys: &[String]) {
        for key in keys {
            if let Some(v) = self.env.get(key).cloned() {
                caps.regex_vars.insert(key.clone(), v);
            }
        }
    }
}
