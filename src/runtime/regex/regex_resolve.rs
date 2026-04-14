use super::super::*;
use super::regex_helpers::{NamedRegexLookupSpec, PENDING_REGEX_GOAL_FAILURE};
use crate::symbol::Symbol;

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

    /// Check if a regex pattern has a bare code block `{}` that terminates
    /// the declarative prefix for LTM purposes.
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

    /// Compute the declarative prefix match length against actual input text.
    /// For patterns without code blocks, returns the full match length.
    /// For patterns with code blocks, returns the length matched by the
    /// prefix (atoms before the first code block).
    pub(crate) fn declarative_prefix_match_len(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> Option<usize> {
        if !self.has_code_block_in_prefix(pattern) {
            // No code block — entire pattern is the declarative prefix
            return self.regex_match_len_at_start(pattern, text);
        }
        // Has code block — match only the prefix atoms
        let parsed = self.parse_regex(pattern)?;
        let mut prefix_tokens: Vec<RegexToken> = Vec::new();
        for token in &parsed.tokens {
            if matches!(
                &token.atom,
                RegexAtom::CodeAssertion {
                    is_assertion: false,
                    ..
                }
            ) {
                break;
            }
            prefix_tokens.push(token.clone());
        }
        let prefix_pattern = RegexPattern {
            tokens: prefix_tokens,
            anchor_start: parsed.anchor_start,
            anchor_end: false,
            ignore_case: parsed.ignore_case,
            ignore_mark: parsed.ignore_mark,
        };
        let chars: Vec<char> = text.chars().collect();
        let pkg = self.current_package.clone();
        self.regex_match_end_from_caps_in_pkg(&prefix_pattern, &chars, 0, &pkg)
            .map(|(end, _)| end)
    }

    pub(super) fn instantiate_token_pattern(def: &FunctionDef, pattern: &str) -> String {
        let Some(sym) = Self::extract_sym_adverb(&def.name.resolve()) else {
            return pattern.to_string();
        };
        let escaped = Self::regex_escape_literal(&sym);
        pattern
            .replace("<.sym>", &escaped)
            .replace("<sym>", &escaped)
    }

    pub(super) fn token_pattern_from_def(def: &FunctionDef) -> Option<String> {
        match def.body.last() {
            Some(Stmt::Expr(Expr::Literal(Value::Regex(p)))) => {
                Some(Self::instantiate_token_pattern(def, p))
            }
            Some(Stmt::Expr(Expr::Literal(Value::Str(s)))) => {
                Some(Self::instantiate_token_pattern(def, s))
            }
            Some(Stmt::Return(Expr::Literal(Value::Regex(p)))) => {
                Some(Self::instantiate_token_pattern(def, p))
            }
            Some(Stmt::Return(Expr::Literal(Value::Str(s)))) => {
                Some(Self::instantiate_token_pattern(def, s))
            }
            _ => None,
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
        if let Some(defs) = self.token_defs.get(&Symbol::intern(&exact_key)) {
            for def in defs {
                if let Some(p) = Self::token_pattern_from_def(def) {
                    out.push((p, def.package.resolve(), None));
                }
            }
        }
        let sym_prefix_angle = format!("{scope}::{name}:sym<");
        let sym_prefix_french = format!("{scope}::{name}:sym\u{ab}");
        let mut sym_keys: Vec<String> = self
            .token_defs
            .keys()
            .map(|key| key.resolve())
            .filter(|key| key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french))
            .collect();
        sym_keys.sort();
        for key in &sym_keys {
            let sym_val = Self::extract_sym_adverb(key);
            if let Some(defs) = self.token_defs.get(&Symbol::intern(key)) {
                for def in defs {
                    if let Some(p) = Self::token_pattern_from_def(def) {
                        out.push((p, def.package.resolve(), sym_val.clone()));
                    }
                }
            }
        }
    }

    pub(super) fn resolve_token_patterns_static_in_pkg(
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
            // Walk MRO for qualified names
            if out.is_empty()
                && let Some(pos) = name.rfind("::")
            {
                let qual_pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                for ancestor in self.mro_readonly(qual_pkg) {
                    if ancestor == qual_pkg {
                        continue;
                    }
                    self.collect_token_patterns_for_scope(&ancestor, token_name, &mut out);
                    if !out.is_empty() {
                        break;
                    }
                }
            }
            return out;
        }
        if !pkg.is_empty() {
            // Walk MRO of pkg
            for scope in self.mro_readonly(pkg) {
                self.collect_token_patterns_for_scope(&scope, name, &mut out);
                if !out.is_empty() {
                    return out;
                }
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
            let name = trimmed[..colon_idx].trim().to_string();
            let arg_src = trimmed[colon_idx + 1..].trim();
            let args = Self::split_regex_arg_list(arg_src);
            return (name, args);
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

        if let Some((lhs, rhs)) = raw.split_once('=') {
            let lhs = lhs.trim();
            let rhs = rhs.trim();
            if !lhs.is_empty()
                && let Some(stripped) = rhs.strip_prefix('&')
            {
                capture_name = Some(lhs.to_string());
                raw = stripped.trim();
                token_lookup = true;
                silent = false;
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
            match v {
                Value::Slip(items) => {
                    for item in items.iter() {
                        out.push(Self::normalize_pair_for_binding(item.clone()));
                    }
                }
                other => out.push(Self::normalize_pair_for_binding(other)),
            }
        }
        Some(out)
    }

    fn normalize_pair_for_binding(v: Value) -> Value {
        if let Value::ValuePair(key, val) = &v
            && let Value::Str(name) = key.as_ref()
        {
            return Value::Pair(name.to_string(), val.clone());
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
            && name
                .chars()
                .all(|ch| ch.is_alphanumeric() || ch == '_' || ch == '-')
        {
            let env = self.make_regex_eval_env(caps);
            return env
                .get(name)
                .cloned()
                .or_else(|| env.get(trimmed).cloned())
                .or(Some(Value::Nil));
        }
        let source = format!("({expr_src});");
        let (stmts, _) = crate::parse_dispatch::parse_source(&source).ok()?;
        let mut interp = Interpreter {
            env: self.make_regex_eval_env(caps),
            functions: self.functions.clone(),
            proto_functions: self.proto_functions.clone(),
            token_defs: self.token_defs.clone(),
            current_package: self.current_package.clone(),
            ..Default::default()
        };
        match interp.eval_block_value(&stmts) {
            Ok(v) => Some(v),
            Err(e) => e.return_value,
        }
    }
}
