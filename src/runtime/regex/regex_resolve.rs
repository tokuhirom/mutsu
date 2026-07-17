use super::super::*;
use super::regex_helpers::{NamedRegexLookupSpec, PENDING_REGEX_GOAL_FAILURE};
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    pub(in crate::runtime) fn clear_pending_goal_failure() {
        PENDING_REGEX_GOAL_FAILURE.with(|slot| {
            *slot.borrow_mut() = None;
        });
    }

    pub(in crate::runtime) fn take_pending_goal_failure() -> Option<(String, usize)> {
        PENDING_REGEX_GOAL_FAILURE.with(|slot| slot.borrow_mut().take())
    }

    pub(super) fn record_goal_failure(goal: &str, pos: usize) {
        PENDING_REGEX_GOAL_FAILURE.with(|slot| {
            let mut slot = slot.borrow_mut();
            let replace = slot
                .as_ref()
                .map(|(_, best_pos)| pos >= *best_pos)
                .unwrap_or(true);
            if replace {
                *slot = Some((goal.to_string(), pos));
            }
        });
    }

    pub(super) fn regex_escape_literal(text: &str) -> String {
        let mut out = String::new();
        for ch in text.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                out.push(ch);
            } else {
                out.push('\\');
                out.push(ch);
            }
        }
        out
    }

    pub(super) fn extract_sym_adverb(name: &str) -> Option<String> {
        // Try «» delimiters first (handles values containing '>')
        let french_marker = ":sym\u{ab}";
        if let Some(start_idx) = name.find(french_marker) {
            let start = start_idx + french_marker.len();
            let rest = &name[start..];
            let end = rest.find('\u{bb}')?;
            return Some(rest[..end].to_string());
        }
        let marker = ":sym<";
        let start = name.find(marker)? + marker.len();
        let rest = &name[start..];
        let end = rest.find('>')?;
        Some(rest[..end].to_string())
    }

    /// Check if a regex pattern's own token list has a bare code block `{}`.
    ///
    /// Gates the eager-code-block buffer (`enable_eager_code_blocks`), which makes a
    /// plain `{ … }` block's side effects survive a failed parse. LTM no longer uses
    /// this: `declarative_prefix_match_len` applies the terminate/skip rules inside
    /// the matcher, which — unlike this predicate — also sees through subrules.
    pub(crate) fn has_code_block_in_prefix(&self, pattern: &str) -> bool {
        let Some(parsed) = self.parse_regex(pattern) else {
            return false;
        };
        parsed.tokens.iter().any(|t| {
            matches!(
                &t.atom,
                RegexAtom::CodeAssertion {
                    is_assertion: false,
                    ..
                }
            )
        })
    }

    /// Compute the declarative prefix match length against actual input text:
    /// how far the candidate matches using only its *declarative* atoms.
    ///
    /// Rakudo builds its LTM NFA from the declarative prefix and never *executes*
    /// anything while doing so. Measuring a length must therefore never run a code
    /// atom, or every candidate measurement duplicates that candidate's side
    /// effects (ADR-0009).
    ///
    /// The two kinds of code atom differ, per `roast/S05-grammar/protoregex.t`:
    /// `<?{ … }>` / `<!{ … }>` do **not** terminate the prefix (the NFA treats them
    /// as a zero-width pass and keeps measuring past them, so `a <?{1}> .+` has
    /// prefix `a .+`), while a plain `{ … }` block **does** (`a {} .+` has prefix
    /// `a`). Neither is run.
    ///
    /// This runs the ordinary matcher under `LTM_DECLARATIVE_MODE`, which applies
    /// both rules without executing (see the `CodeAssertion` arm of
    /// `regex_match_atom_with_capture_in_pkg` and the `LTM_PREFIX_TERMINATED` check
    /// in `walk_tokens`). Going through the real matcher — rather than truncating
    /// the token list — is what lets the prefix descend *into* a subrule and handle
    /// a code atom nested inside it (`token TOP { <item> }` where `item` holds the
    /// assertion): the flags are thread-locals, so they survive the nested
    /// sub-interpreter dispatch a subrule match goes through.
    ///
    /// A pattern with no code atom anywhere therefore measures as a full match,
    /// which executes nothing.
    ///
    /// Returns `(len, stopped_at_code_block)`. The flag matters because a `None`
    /// length means two different things. With `false`, the measurement ran to the
    /// end and `None` is real evidence the candidate cannot match — the caller may
    /// drop it. With `true`, a plain `{ }` block cut the measurement short, so a
    /// `None` proves nothing about whether the real match would succeed and the
    /// candidate must be kept for the real match to judge.
    pub(crate) fn declarative_prefix_match_len(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> (Option<usize>, bool) {
        // Saved/restored rather than simply cleared: a subrule's own pattern can be
        // measured while an outer measurement is still live.
        let saved_mode = super::regex_helpers::LTM_DECLARATIVE_MODE.replace(true);
        let saved_terminated = super::regex_helpers::LTM_PREFIX_TERMINATED.replace(false);
        let result = self.regex_match_len_at_start(pattern, text);
        let stopped_at_code_atom = super::regex_helpers::LTM_PREFIX_TERMINATED.get();
        super::regex_helpers::LTM_DECLARATIVE_MODE.set(saved_mode);
        super::regex_helpers::LTM_PREFIX_TERMINATED.set(saved_terminated);
        (result, stopped_at_code_atom)
    }

    pub(in crate::runtime) fn instantiate_token_pattern(
        def: &FunctionDef,
        pattern: &str,
    ) -> String {
        let Some(sym) = Self::extract_sym_adverb(&def.name.resolve()) else {
            return pattern.to_string();
        };
        let escaped = Self::regex_escape_literal(&sym);
        pattern
            .replace("<.sym>", &escaped)
            .replace("<sym>", &escaped)
    }

    pub(crate) fn token_pattern_from_def(def: &FunctionDef) -> Option<String> {
        let expr = match def.body.last() {
            Some(Stmt::Expr(e)) | Some(Stmt::Return(e)) => e,
            _ => return None,
        };
        let Expr::Literal(v) = expr else {
            return None;
        };
        match v.view() {
            ValueView::Regex(p) => Some(Self::instantiate_token_pattern(def, &p)),
            ValueView::Str(s) => Some(Self::instantiate_token_pattern(def, &s)),
            _ => None,
        }
    }

    /// Like [`Self::collect_token_patterns_for_scope`], but deduplicating by
    /// candidate identity (`None` for the exact token, `Some(sym)` for a proto
    /// candidate) across successive calls via `seen`, so an MRO walk merges
    /// proto candidates from every class while a same-identity redefinition in
    /// a more-derived class overrides its ancestor's.
    fn collect_token_patterns_for_scope_dedup(
        &self,
        scope: &str,
        name: &str,
        out: &mut Vec<(String, String, Option<String>)>,
        seen: &mut std::collections::HashSet<Option<String>>,
    ) {
        // Dedupe against ancestors only: multi candidates within ONE scope
        // legitimately share a sym key and must all be kept.
        let seen_before = seen.clone();
        let before = out.len();
        self.collect_token_patterns_for_scope(scope, name, out);
        let mut idx = before;
        while idx < out.len() {
            if seen_before.contains(&out[idx].2) {
                out.remove(idx);
            } else {
                seen.insert(out[idx].2.clone());
                idx += 1;
            }
        }
    }

    /// Collect static token patterns for a given scope.
    /// Returns (pattern, package, sym_key) tuples. sym_key is Some for :sym<> variants.
    pub(super) fn collect_token_patterns_for_scope(
        &self,
        scope: &str,
        name: &str,
        out: &mut Vec<(String, String, Option<String>)>,
    ) {
        let exact_key = format!("{scope}::{name}");
        if let Some(defs) = self.registry().token_defs.get(&Symbol::intern(&exact_key)) {
            for def in defs {
                if let Some(p) = Self::token_pattern_from_def(def) {
                    out.push((p, def.package.resolve(), None));
                }
            }
        }
        let sym_prefix_angle = format!("{scope}::{name}:sym<");
        let sym_prefix_french = format!("{scope}::{name}:sym\u{ab}");
        let mut sym_keys: Vec<String> = self
            .registry()
            .token_defs
            .keys()
            .map(|key| key.resolve())
            .filter(|key| key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french))
            .collect();
        sym_keys.sort();
        for key in &sym_keys {
            let sym_val = Self::extract_sym_adverb(key);
            if let Some(defs) = self.registry().token_defs.get(&Symbol::intern(key)) {
                for def in defs {
                    if let Some(p) = Self::token_pattern_from_def(def) {
                        out.push((p, def.package.resolve(), sym_val.clone()));
                    }
                }
            }
        }
    }

    pub(crate) fn resolve_token_patterns_static_in_pkg(
        &self,
        name: &str,
        pkg: &str,
    ) -> Vec<(String, String, Option<String>)> {
        let mut out = Vec::new();
        if name.contains("::") {
            self.collect_token_patterns_for_scope(
                &name[..name.rfind("::").unwrap()],
                &name[name.rfind("::").unwrap() + 2..],
                &mut out,
            );
            // Walk the MRO for qualified names, merging proto candidates from
            // every ancestor (dedup by sym identity, derived-first).
            if let Some(pos) = name.rfind("::") {
                let qual_pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                let mut seen: std::collections::HashSet<Option<String>> =
                    out.iter().map(|e| e.2.clone()).collect();
                let own = out.len();
                for ancestor in self.mro_readonly(qual_pkg) {
                    if ancestor == qual_pkg {
                        continue;
                    }
                    self.collect_token_patterns_for_scope_dedup(
                        &ancestor, token_name, &mut out, &mut seen,
                    );
                }
                // Rewrite ancestor entries' dispatch package to the original
                // qualified package so nested subrule lookups dispatch
                // virtually through the receiver's MRO (Liskov substitution).
                for entry in out.iter_mut().skip(own) {
                    entry.1 = qual_pkg.to_string();
                }
            }
            return out;
        }
        if !pkg.is_empty() {
            // Walk the MRO of pkg, merging proto candidates from every class:
            // a derived grammar adding `rule statement:sym<repeat>` keeps the
            // base grammar's candidates (advent2009-day24).
            let mut seen: std::collections::HashSet<Option<String>> =
                std::collections::HashSet::new();
            let mut own = None;
            for scope in self.mro_readonly(pkg) {
                if scope != pkg && own.is_none() {
                    own = Some(out.len());
                }
                self.collect_token_patterns_for_scope_dedup(&scope, name, &mut out, &mut seen);
            }
            // Ancestor entries dispatch virtually through the receiver package.
            let own = own.unwrap_or(out.len());
            for entry in out.iter_mut().skip(own) {
                entry.1 = pkg.to_string();
            }
            if !out.is_empty() {
                return out;
            }
        }
        self.collect_token_patterns_for_scope("GLOBAL", name, &mut out);
        out
    }

    pub(super) fn split_regex_arg_list(args: &str) -> Vec<String> {
        let mut parts = Vec::new();
        let mut start = 0usize;
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (i, ch) in args.char_indices() {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren_depth += 1,
                ')' => paren_depth = paren_depth.saturating_sub(1),
                '[' => bracket_depth += 1,
                ']' => bracket_depth = bracket_depth.saturating_sub(1),
                '{' => brace_depth += 1,
                '}' => brace_depth = brace_depth.saturating_sub(1),
                ',' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                    let part = args[start..i].trim();
                    if !part.is_empty() {
                        parts.push(part.to_string());
                    }
                    start = i + 1;
                }
                _ => {}
            }
        }
        let tail = args[start..].trim();
        if !tail.is_empty() {
            parts.push(tail.to_string());
        }
        parts
    }

    pub(super) fn find_top_level_call_colon(text: &str) -> Option<usize> {
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (i, ch) in text.char_indices() {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren_depth += 1,
                ')' => paren_depth = paren_depth.saturating_sub(1),
                '[' => bracket_depth += 1,
                ']' => bracket_depth = bracket_depth.saturating_sub(1),
                '{' => brace_depth += 1,
                '}' => brace_depth = brace_depth.saturating_sub(1),
                ':' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                    let prev_is_colon = i > 0 && text[..i].ends_with(':');
                    let next_is_colon = text[i + 1..].starts_with(':');
                    if !prev_is_colon && !next_is_colon {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }

    pub(super) fn parse_regex_lookup_target(text: &str) -> (String, Vec<String>) {
        let trimmed = text.trim();
        if let Some(open) = trimmed.find('(')
            && trimmed.ends_with(')')
            && open < trimmed.len() - 1
        {
            let name = trimmed[..open].trim().to_string();
            let args = Self::split_regex_arg_list(&trimmed[open + 1..trimmed.len() - 1]);
            return (name, args);
        }
        if let Some(colon_idx) = Self::find_top_level_call_colon(trimmed) {
            // Don't split on colon that's part of a Unicode property prefix
            // (e.g., ":Letter", ":!Letter", "-:Letter"), not a method call colon.
            let is_unicode_prop_colon = colon_idx == 0
                || (colon_idx == 1 && trimmed.starts_with('-'))
                || (colon_idx == 1 && trimmed.starts_with('!'));
            if !is_unicode_prop_colon {
                let name = trimmed[..colon_idx].trim().to_string();
                let arg_src = trimmed[colon_idx + 1..].trim();
                let args = Self::split_regex_arg_list(arg_src);
                return (name, args);
            }
        }
        (trimmed.to_string(), Vec::new())
    }

    pub(super) fn parse_named_regex_lookup_spec(name: &str) -> NamedRegexLookupSpec {
        let mut raw = name.trim();
        let mut silent = false;
        if let Some(stripped) = raw.strip_prefix('.') {
            silent = true;
            raw = stripped.trim();
        }
        let mut token_lookup = false;
        let mut capture_name = None;
        let mut alias_replaces_original = false;

        // Look for alias syntax <name=.subrule>, <name=&subrule>, <name=subrule>.
        // Only match `=` that is NOT part of `=>` (fat-arrow pair syntax in args)
        // and where the LHS is a simple identifier (no parens, which would indicate
        // we're inside an argument list like `<.foo(a => 1)>`).
        if let Some(eq_pos) = raw.find('=')
            && !raw[eq_pos + 1..].starts_with('>')
        {
            let lhs = raw[..eq_pos].trim();
            let rhs = raw[eq_pos + 1..].trim();
            // LHS must be a simple identifier (no parens, commas, colons, etc.)
            if !lhs.is_empty() && !lhs.contains('(') && !lhs.contains(',') && !lhs.contains(':') {
                if let Some(stripped) = rhs.strip_prefix('&') {
                    // <name=&subrule> — call &subrule, capture under name
                    capture_name = Some(lhs.to_string());
                    raw = stripped.trim();
                    token_lookup = true;
                    silent = false;
                    alias_replaces_original = true;
                } else if let Some(stripped) = rhs.strip_prefix('.') {
                    // <name=.subrule> — call .subrule (non-capturing), capture under name
                    capture_name = Some(lhs.to_string());
                    raw = stripped.trim();
                    silent = false;
                    alias_replaces_original = true;
                } else {
                    // <name=subrule> — call subrule, capture under name AND original
                    capture_name = Some(lhs.to_string());
                    raw = rhs;
                    silent = false;
                    alias_replaces_original = false;
                }
            }
        }
        if !token_lookup && let Some(stripped) = raw.strip_prefix('&') {
            raw = stripped.trim();
            token_lookup = true;
            silent = true;
        }

        let (lookup_name, arg_exprs) = Self::parse_regex_lookup_target(raw);
        NamedRegexLookupSpec {
            silent,
            token_lookup,
            lookup_name,
            capture_name,
            arg_exprs,
            alias_replaces_original,
        }
    }

    pub(super) fn make_regex_eval_env(&self, caps: &RegexCaptures) -> Env {
        let mut env = self.env.clone();
        for (i, val) in caps.positional.iter().enumerate() {
            env.insert(i.to_string(), Value::str(val.clone()));
        }
        let match_list: Vec<Value> = caps
            .positional
            .iter()
            .map(|s| Value::str(s.clone()))
            .collect();
        env.insert("/".to_string(), Value::array(match_list));
        for (k, v) in &caps.named {
            let value = if v.len() == 1 {
                Value::str(v[0].clone())
            } else {
                Value::array(v.iter().cloned().map(Value::str).collect())
            };
            env.insert(format!("<{}>", k), value);
        }
        env
    }

    /// Evaluate a list of regex argument expressions, flattening any Slip
    /// values produced by the `|` prefix. Returns `None` if any argument
    /// fails to evaluate.
    pub(in crate::runtime) fn eval_regex_arg_list(
        &self,
        exprs: &[String],
        caps: &RegexCaptures,
    ) -> Option<Vec<Value>> {
        let mut out = Vec::new();
        for arg in exprs {
            let v = self.eval_regex_expr_value(arg, caps)?;
            if let ValueView::Slip(items) = v.view() {
                for item in items.iter() {
                    out.push(Self::normalize_pair_for_binding(item.clone()));
                }
            } else {
                out.push(Self::normalize_pair_for_binding(v));
            }
        }
        Some(out)
    }

    fn normalize_pair_for_binding(v: Value) -> Value {
        if let ValueView::ValuePair(key, val) = v.view()
            && let ValueView::Str(name) = key.view()
        {
            return Value::pair(name.to_string(), val.clone());
        }
        v
    }

    pub(in crate::runtime) fn eval_regex_expr_value(
        &self,
        expr_src: &str,
        caps: &RegexCaptures,
    ) -> Option<Value> {
        let trimmed = expr_src.trim();
        if let Some(name) = trimmed.strip_prefix('$')
            && !name.is_empty()
            && {
                // A bare variable reference only — `$p-1` (kebab `-` followed
                // by a digit is NOT part of a raku identifier) must fall to
                // the general expression evaluator below.
                let bytes: Vec<char> = name.chars().collect();
                let mut ok = bytes[0].is_alphabetic() || bytes[0] == '_';
                let mut k = 1;
                while ok && k < bytes.len() {
                    let c = bytes[k];
                    let kebab = c == '-'
                        && bytes
                            .get(k + 1)
                            .is_some_and(|n| n.is_alphabetic() || *n == '_');
                    if c.is_alphanumeric() || c == '_' || kebab {
                        k += 1;
                    } else {
                        ok = false;
                    }
                }
                ok
            }
        {
            let env = self.make_regex_eval_env(caps);
            return env
                .get(name)
                .cloned()
                .or_else(|| env.get(trimmed).cloned())
                .or(Some(Value::NIL));
        }
        let source = format!("({expr_src});");
        let (stmts, _) = crate::parse_dispatch::parse_source(&source).ok()?;
        let mut interp = Interpreter {
            env: self.make_regex_eval_env(caps),
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Self::new_regex_scratch()
        };
        self.copy_decl_registry_into(&mut interp);
        match interp.eval_block_value(&stmts) {
            Ok(v) => Some(v),
            Err(e) => e.return_value,
        }
    }
}
