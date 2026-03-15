use super::*;
use crate::symbol::Symbol;
use std::cell::RefCell;

thread_local! {
    static PENDING_REGEX_GOAL_FAILURE: RefCell<Option<(String, usize)>> = const { RefCell::new(None) };
}

struct NamedRegexLookupSpec {
    silent: bool,
    token_lookup: bool,
    lookup_name: String,
    capture_name: Option<String>,
    arg_exprs: Vec<String>,
}

/// Check if a character is a "word" character for word boundary purposes.
/// In Raku, word characters are alphanumeric or underscore.
fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Check if an atom is "simple" — it only advances position without producing
/// any captures. Used to enable a fast path in ratcheted quantifier loops
/// that avoids cloning RegexCaptures on every iteration.
fn is_simple_atom(atom: &RegexAtom) -> bool {
    matches!(
        atom,
        RegexAtom::Literal(_)
            | RegexAtom::CharClass(_)
            | RegexAtom::Any
            | RegexAtom::Newline
            | RegexAtom::NotNewline
            | RegexAtom::UnicodeProp { .. }
            | RegexAtom::CompositeClass { .. }
    )
}

fn merge_regex_captures(mut dst: RegexCaptures, mut src: RegexCaptures) -> RegexCaptures {
    for (k, v) in src.named.drain() {
        dst.named.entry(k).or_default().extend(v);
    }
    dst.positional.append(&mut src.positional);
    dst.positional_subcaps.append(&mut src.positional_subcaps);
    dst.code_blocks.append(&mut src.code_blocks);
    dst
}

impl Interpreter {
    pub(super) fn clear_pending_goal_failure() {
        PENDING_REGEX_GOAL_FAILURE.with(|slot| {
            *slot.borrow_mut() = None;
        });
    }

    pub(super) fn take_pending_goal_failure() -> Option<(String, usize)> {
        PENDING_REGEX_GOAL_FAILURE.with(|slot| slot.borrow_mut().take())
    }

    fn record_goal_failure(goal: &str, pos: usize) {
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

    fn regex_escape_literal(text: &str) -> String {
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

    fn extract_sym_adverb(name: &str) -> Option<String> {
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

    fn instantiate_token_pattern(def: &FunctionDef, pattern: &str) -> String {
        let Some(sym) = Self::extract_sym_adverb(&def.name.resolve()) else {
            return pattern.to_string();
        };
        let escaped = Self::regex_escape_literal(&sym);
        pattern
            .replace("<.sym>", &escaped)
            .replace("<sym>", &escaped)
    }

    fn token_pattern_from_def(def: &FunctionDef) -> Option<String> {
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
    fn collect_token_patterns_for_scope(
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

    fn resolve_token_patterns_static_in_pkg(
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

    fn split_regex_arg_list(args: &str) -> Vec<String> {
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

    fn find_top_level_call_colon(text: &str) -> Option<usize> {
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

    fn parse_regex_lookup_target(text: &str) -> (String, Vec<String>) {
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

    fn parse_named_regex_lookup_spec(name: &str) -> NamedRegexLookupSpec {
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

    fn make_regex_eval_env(&self, caps: &RegexCaptures) -> Env {
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

    pub(super) fn eval_regex_expr_value(
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

    fn resolve_token_defs_in_pkg(&self, name: &str, pkg: &str) -> Vec<FunctionDef> {
        let mut out = Vec::new();
        if name.contains("::") {
            if let Some(defs) = self.token_defs.get(&Symbol::intern(name)) {
                out.extend(defs.clone());
            }
            let sym_prefix_angle = format!("{name}:sym<");
            let sym_prefix_french = format!("{name}:sym\u{ab}");
            let mut sym_keys: Vec<String> = self
                .token_defs
                .keys()
                .map(|key| key.resolve())
                .filter(|key| {
                    key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french)
                })
                .collect();
            sym_keys.sort();
            for key in &sym_keys {
                if let Some(defs) = self.token_defs.get(&Symbol::intern(key)) {
                    out.extend(defs.clone());
                }
            }
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
                    self.collect_token_defs_for_scope(&ancestor, token_name, &mut out);
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
                self.collect_token_defs_for_scope(&scope, name, &mut out);
                if !out.is_empty() {
                    return out;
                }
            }
        }
        self.collect_token_defs_for_scope("GLOBAL", name, &mut out);
        out
    }

    fn resolve_token_patterns_with_args_in_pkg(
        &self,
        name: &str,
        pkg: &str,
        arg_values: &[Value],
    ) -> Vec<(String, String, Option<String>)> {
        let mut out = Vec::new();
        for def in self.resolve_token_defs_in_pkg(name, pkg) {
            let mut interp = Interpreter {
                env: self.env.clone(),
                functions: self.functions.clone(),
                proto_functions: self.proto_functions.clone(),
                token_defs: self.token_defs.clone(),
                current_package: def.package.resolve(),
                ..Default::default()
            };
            let saved_env = interp.env.clone();
            if interp
                .bind_function_args_values(&def.param_defs, &def.params, arg_values)
                .is_ok()
            {
                interp
                    .routine_stack
                    .push((def.package.resolve(), def.name.resolve()));
                let result = interp.eval_block_value(&def.body);
                interp.routine_stack.pop();
                let value = match result {
                    Ok(v) => Some(v),
                    Err(e) if e.return_value.is_some() => e.return_value,
                    Err(_) => None,
                };
                if let Some(value) = value {
                    let pattern = match value {
                        Value::Regex(pat) => pat.to_string(),
                        Value::Str(s) => s.to_string(),
                        Value::Nil => String::new(),
                        other => other.to_string_value(),
                    };
                    let pattern = interp.interpolate_bound_regex_scalars(&pattern);
                    if let Ok(instantiated) = interp.instantiate_named_regex_arg_calls(&pattern) {
                        let sym_val = Self::extract_sym_adverb(&def.name.resolve());
                        out.push((instantiated, def.package.resolve(), sym_val));
                    }
                }
            }
            interp.env = saved_env;
        }
        out
    }

    fn resolve_named_regex_candidates_in_pkg(
        &self,
        spec: &NamedRegexLookupSpec,
        pkg: &str,
        arg_values: &[Value],
    ) -> Vec<(String, String, Option<String>)> {
        if arg_values.is_empty() {
            self.resolve_token_patterns_static_in_pkg(&spec.lookup_name, pkg)
        } else {
            self.resolve_token_patterns_with_args_in_pkg(&spec.lookup_name, pkg, arg_values)
        }
    }

    fn format_named_regex_arg_value(value: &Value) -> String {
        match value {
            Value::Str(s) => {
                let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
                format!("\"{escaped}\"")
            }
            Value::Bool(true) => "True".to_string(),
            Value::Bool(false) => "False".to_string(),
            Value::Nil => "Nil".to_string(),
            _ => value.to_string_value(),
        }
    }

    pub(super) fn interpolate_bound_regex_scalars(&self, pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            if ch == '{' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '{' {
                        depth += 1;
                    } else if c == '}' {
                        depth -= 1;
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            if ch == '<' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '<' {
                        depth += 1;
                    } else if c == '>' {
                        depth -= 1;
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            if ch == '\\' {
                out.push(ch);
                i += 1;
                if i < chars.len() {
                    out.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            if ch == '$' {
                let start = i;
                let mut j = i + 1;
                let parsed = if j < chars.len() && chars[j] == '{' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len() && chars[j] != '}' {
                        j += 1;
                    }
                    if j < chars.len() && j > name_start {
                        let name: String = chars[name_start..j].iter().collect();
                        j += 1;
                        Some((name, j))
                    } else {
                        None
                    }
                } else if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    Some((chars[name_start..j].iter().collect::<String>(), j))
                } else {
                    None
                };
                if let Some((name, end)) = parsed {
                    if let Some(value) = self
                        .env
                        .get(&name)
                        .cloned()
                        .or_else(|| self.env.get(&format!("${name}")).cloned())
                    {
                        match value {
                            Value::Regex(pat) => out.push_str(&pat),
                            other => {
                                out.push_str(&Self::regex_escape_literal(&other.to_string_value()))
                            }
                        }
                    } else {
                        out.extend(chars[start..end].iter());
                    }
                    i = end;
                    continue;
                }
            }
            out.push(ch);
            i += 1;
        }
        out
    }

    pub(super) fn instantiate_named_regex_arg_calls(
        &self,
        pattern: &str,
    ) -> Result<String, RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let default_caps = RegexCaptures::default();
        let mut i = 0usize;
        while i < chars.len() {
            if chars[i] != '<' {
                out.push(chars[i]);
                i += 1;
                continue;
            }

            let mut depth = 1usize;
            let start = i + 1;
            i += 1;
            while i < chars.len() && depth > 0 {
                match chars[i] {
                    '<' => depth += 1,
                    '>' => depth -= 1,
                    _ => {}
                }
                i += 1;
            }
            if depth != 0 {
                out.push('<');
                out.extend(chars[start..].iter());
                break;
            }

            let inner_end = i - 1;
            let inner: String = chars[start..inner_end].iter().collect();
            let spec = Self::parse_named_regex_lookup_spec(&inner);
            if spec.arg_exprs.is_empty() {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            }

            let Some(open_idx) = inner.find('(') else {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            };
            let Some(close_idx) = inner.rfind(')') else {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            };

            let mut rendered_args = Vec::new();
            for arg in &spec.arg_exprs {
                let Some(value) = self.eval_regex_expr_value(arg, &default_caps) else {
                    return Err(RuntimeError::new(format!(
                        "Failed to evaluate regex argument expression: {arg}"
                    )));
                };
                rendered_args.push(Self::format_named_regex_arg_value(&value));
            }

            out.push('<');
            out.push_str(&inner[..open_idx + 1]);
            out.push_str(&rendered_args.join(", "));
            out.push_str(&inner[close_idx..]);
            out.push('>');
        }
        Ok(out)
    }

    fn restore_env_entries(&mut self, restore: HashMap<String, Option<Value>>) {
        for (key, value) in restore {
            match value {
                Some(v) => {
                    self.env.insert(key, v);
                }
                None => {
                    self.env.remove(&key);
                }
            }
        }
    }

    fn find_top_level_semicolon(text: &str) -> Option<usize> {
        let mut paren = 0usize;
        let mut bracket = 0usize;
        let mut brace = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (idx, ch) in text.char_indices() {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    continue;
                }
                if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren += 1,
                ')' => paren = paren.saturating_sub(1),
                '[' => bracket += 1,
                ']' => bracket = bracket.saturating_sub(1),
                '{' => brace += 1,
                '}' => brace = brace.saturating_sub(1),
                ';' if paren == 0 && bracket == 0 && brace == 0 => return Some(idx),
                _ => {}
            }
        }
        None
    }

    fn find_matching_brace_end(text: &str, open_idx: usize) -> Option<usize> {
        let mut depth = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (idx, ch) in text.char_indices().skip_while(|(i, _)| *i < open_idx) {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    continue;
                }
                if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '{' => depth += 1,
                '}' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return Some(idx);
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn parse_regex_declarative_prefix(pattern: &str) -> (Vec<(String, String)>, String) {
        let mut decls = Vec::new();
        let mut rest = pattern;
        let mut preserved_adverbs: Vec<String> = Vec::new();
        loop {
            let trimmed = rest.trim_start();
            rest = trimmed;
            let Some(after_colon) = rest.strip_prefix(':') else {
                break;
            };
            let name_len = after_colon
                .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '-'))
                .unwrap_or(after_colon.len());
            if name_len == 0 {
                break;
            }
            let name = &after_colon[..name_len];
            if !matches!(
                name,
                "ratchet"
                    | "ignorecase"
                    | "ignoremark"
                    | "sigspace"
                    | "i"
                    | "m"
                    | "s"
                    | "r"
                    | "x"
                    | "p5"
            ) {
                break;
            }
            preserved_adverbs.push(format!(":{name}"));
            rest = &after_colon[name_len..];
        }
        loop {
            let trimmed = rest.trim_start();
            rest = trimmed;
            let Some(after_colon) = rest.strip_prefix(':') else {
                break;
            };
            let name_len = after_colon
                .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '-'))
                .unwrap_or(after_colon.len());
            if name_len == 0 {
                break;
            }
            let decl_name = &after_colon[..name_len];
            if !matches!(
                decl_name,
                "my" | "our" | "state" | "constant" | "temp" | "let"
            ) {
                break;
            }
            let after_name = &after_colon[name_len..];
            let body = after_name.trim_start();
            let stmt_text;
            let consumed_len;
            let scoped_token_decl = matches!(decl_name, "my" | "our" | "state")
                && (body.starts_with("token ")
                    || body.starts_with("regex ")
                    || body.starts_with("rule "));
            if scoped_token_decl {
                let Some(open_idx) = body.find('{') else {
                    break;
                };
                let Some(close_idx) = Self::find_matching_brace_end(body, open_idx) else {
                    break;
                };
                let mut end_idx = close_idx + 1;
                if body[end_idx..].starts_with(';') {
                    end_idx += 1;
                }
                stmt_text = format!("{decl_name} {}", body[..end_idx].trim());
                consumed_len =
                    (rest.len() - after_name.len()) + (after_name.len() - body.len()) + end_idx;
            } else {
                let Some(semi_idx) = Self::find_top_level_semicolon(body) else {
                    break;
                };
                stmt_text = format!("{decl_name} {}", body[..semi_idx].trim());
                consumed_len = (rest.len() - after_name.len())
                    + (after_name.len() - body.len())
                    + semi_idx
                    + 1;
            }
            decls.push((decl_name.to_string(), stmt_text));
            rest = &rest[consumed_len..];
        }
        let mut remaining = String::new();
        if !preserved_adverbs.is_empty() {
            remaining.push_str(&preserved_adverbs.join(" "));
            if !rest.trim_start().is_empty() {
                remaining.push(' ');
            }
        }
        remaining.push_str(rest.trim_start());
        (decls, remaining)
    }

    /// Try to resolve a Named regex atom to a pre-parsed pattern for fast-path
    /// matching. Returns Some((parsed_pattern, package)) if the Named atom
    /// resolves to exactly one token candidate with no arguments.
    fn try_resolve_named_to_pattern(
        &self,
        atom: &RegexAtom,
        pkg: &str,
    ) -> Option<(RegexPattern, String)> {
        let RegexAtom::Named(name) = atom else {
            return None;
        };
        let spec = Self::parse_named_regex_lookup_spec(name);
        if !spec.arg_exprs.is_empty() {
            return None;
        }
        let candidates = self.resolve_token_patterns_static_in_pkg(&spec.lookup_name, pkg);
        if candidates.len() != 1 {
            return None;
        }
        let (sub_pat, sub_pkg, _sym_key) = &candidates[0];
        let parsed = self.parse_regex(sub_pat)?;
        Some((parsed, sub_pkg.clone()))
    }

    pub(super) fn regex_match_with_captures_core(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_caps_in_pkg(&parsed, &chars, 0, &pkg)
                .map(|(end, mut caps)| {
                    caps.from = caps.capture_start.unwrap_or(0);
                    caps.to = caps.capture_end.unwrap_or(end);
                    caps.matched = chars[caps.from..caps.to].iter().collect();
                    caps
                });
        }
        for start in 0..=chars.len() {
            if let Some((end, mut caps)) =
                self.regex_match_end_from_caps_in_pkg(&parsed, &chars, start, &pkg)
            {
                caps.from = caps.capture_start.unwrap_or(start);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.matched = chars[caps.from..caps.to].iter().collect();
                return Some(caps);
            }
        }
        None
    }

    fn parse_anchored_single_subrule(pattern: &str) -> Option<String> {
        let compact: String = pattern.chars().filter(|c| !c.is_whitespace()).collect();
        let inner = compact.strip_prefix("^<")?.strip_suffix(">$")?;
        if inner.is_empty() || inner.contains('<') || inner.contains('>') || inner.contains("::") {
            return None;
        }
        Some(inner.to_string())
    }

    #[allow(dead_code)]
    pub(super) fn regex_is_match(&mut self, pattern: &str, text: &str) -> bool {
        self.regex_match_with_captures(pattern, text).is_some()
    }

    pub(super) fn regex_match_with_captures(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        if let Some(raw_name) = Self::parse_anchored_single_subrule(pattern) {
            let spec = Self::parse_named_regex_lookup_spec(&raw_name);
            let arg_values = if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                let default_caps = RegexCaptures::default();
                let mut values = Vec::new();
                for arg in &spec.arg_exprs {
                    let v = self.eval_regex_expr_value(arg, &default_caps)?;
                    values.push(v);
                }
                values
            };
            let candidates = self.resolve_named_regex_candidates_in_pkg(
                &spec,
                &self.current_package.clone(),
                &arg_values,
            );
            // Use LTM: compute declarative prefix match length for each candidate
            let filtered: Vec<(String, String, Option<String>)> = candidates
                .into_iter()
                .filter(|(sub_pat, _, _)| *sub_pat != pattern)
                .collect();

            let mut best: Option<RegexCaptures> = None;
            let mut best_sym: Option<String> = None;
            let mut best_prefix_match: usize = 0;
            for (sub_pat, sub_pkg, sym_key) in filtered {
                let prefix_match_len = self
                    .declarative_prefix_match_len(&sub_pat, text)
                    .unwrap_or(0);
                let saved_pkg = self.current_package.clone();
                self.current_package = sub_pkg;
                let mut caps = self.regex_match_with_captures(&sub_pat, text);
                self.current_package = saved_pkg;
                if let Some(mut caps) = caps.take() {
                    if caps.from != 0 || caps.to != text.chars().count() {
                        continue;
                    }
                    if !spec.silent {
                        caps.named
                            .entry(spec.lookup_name.clone())
                            .or_default()
                            .push(caps.matched.clone());
                    }
                    // LTM: prefer longer declarative prefix match;
                    // if equal, prefer longer overall match
                    let better = best.is_none()
                        || prefix_match_len > best_prefix_match
                        || (prefix_match_len == best_prefix_match
                            && caps.to > best.as_ref().unwrap().to);
                    if better {
                        best = Some(caps);
                        best_sym = sym_key;
                        best_prefix_match = prefix_match_len;
                    }
                }
            }
            if let Some(mut best) = best {
                // Store the winning :sym<> variant name
                if best_sym.is_some() {
                    best.sym = best_sym.clone();
                }
                // Ensure subcapture exists for the subrule so sym_variant
                // propagates to the child Match object via make_subcap_match
                if !spec.silent {
                    let mut subcap = best.clone();
                    subcap.sym = best_sym;
                    best.named_subcaps
                        .entry(spec.lookup_name.clone())
                        .or_default()
                        .push(subcap);
                }
                return Some(best);
            }
        }

        let (declarators, remaining_pattern) = Self::parse_regex_declarative_prefix(pattern);
        if declarators.is_empty() {
            return self.regex_match_with_captures_core(pattern, text);
        }

        let mut restore_always: HashMap<String, Option<Value>> = HashMap::new();
        let mut restore_on_fail: HashMap<String, Option<Value>> = HashMap::new();
        let saved_token_defs = self.token_defs.clone();

        for (decl_name, stmt_src) in declarators {
            let before_env = self.env.clone();
            let mut handled_state_postfix = false;
            let mut handled_direct_assign = false;
            if decl_name == "state" {
                let state_src = stmt_src.trim_start_matches("state").trim();
                if let Some(name) = state_src
                    .strip_prefix('$')
                    .and_then(|s| s.strip_suffix("++"))
                    .map(str::trim)
                {
                    let cur = match self.env.get(name) {
                        Some(Value::Int(i)) => *i,
                        _ => 0,
                    };
                    self.env.insert(name.to_string(), Value::Int(cur + 1));
                    handled_state_postfix = true;
                } else if let Some(name) = state_src
                    .strip_prefix('$')
                    .and_then(|s| s.strip_suffix("--"))
                    .map(str::trim)
                {
                    let cur = match self.env.get(name) {
                        Some(Value::Int(i)) => *i,
                        _ => 0,
                    };
                    self.env.insert(name.to_string(), Value::Int(cur - 1));
                    handled_state_postfix = true;
                }
            }
            if !handled_state_postfix && (decl_name == "temp" || decl_name == "let") {
                let assign_src = if decl_name == "temp" {
                    stmt_src.trim_start_matches("temp").trim()
                } else {
                    stmt_src.trim_start_matches("let").trim()
                };
                if let Some((lhs, rhs)) = assign_src.split_once('=') {
                    let lhs = lhs.trim();
                    if let Some(name) = lhs.strip_prefix('$').map(str::trim) {
                        let caps = RegexCaptures::default();
                        if let Some(value) = self.eval_regex_expr_value(rhs.trim(), &caps) {
                            self.env.insert(name.to_string(), value);
                            handled_direct_assign = true;
                        }
                    }
                }
            }
            if !handled_state_postfix && !handled_direct_assign {
                let eval_src = stmt_src.clone();
                let Ok((stmts, _)) = crate::parse_dispatch::parse_source(&eval_src) else {
                    self.token_defs = saved_token_defs;
                    self.restore_env_entries(restore_always);
                    self.restore_env_entries(restore_on_fail);
                    return None;
                };
                if self.eval_block_value(&stmts).is_err() {
                    self.token_defs = saved_token_defs;
                    self.restore_env_entries(restore_always);
                    self.restore_env_entries(restore_on_fail);
                    return None;
                }
            }

            let mut changed = HashSet::new();
            for key in before_env.keys() {
                if before_env.get(key) != self.env.get(key) {
                    changed.insert(key.clone());
                }
            }
            for key in self.env.keys() {
                if !before_env.contains_key(key) {
                    changed.insert(key.clone());
                }
            }
            if matches!(decl_name.as_str(), "my" | "constant" | "temp") {
                for key in changed {
                    restore_always
                        .entry(key.clone())
                        .or_insert_with(|| before_env.get(&key).cloned());
                }
            } else if decl_name == "let" {
                for key in changed {
                    restore_on_fail
                        .entry(key.clone())
                        .or_insert_with(|| before_env.get(&key).cloned());
                }
            }
        }

        let result = self.regex_match_with_captures_core(&remaining_pattern, text);
        let matched = result.is_some();
        self.token_defs = saved_token_defs;
        self.restore_env_entries(restore_always);
        if !matched {
            self.restore_env_entries(restore_on_fail);
        }
        result
    }

    pub(super) fn regex_match_with_captures_full_from_start(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        let mut matches = self.regex_match_ends_from_caps_in_pkg(&parsed, &chars, 0, &pkg);
        if matches.is_empty() {
            return None;
        }
        matches.sort_by_key(|(end, caps)| (*end, caps.positional.len(), caps.named.len()));
        let (end, mut caps) = matches
            .into_iter()
            .rev()
            .find(|(end, _)| *end == chars.len())?;
        caps.from = caps.capture_start.unwrap_or(0);
        caps.to = caps.capture_end.unwrap_or(end);
        caps.matched = chars[caps.from..caps.to].iter().collect();
        Some(caps)
    }

    /// Match regex anchored at a specific character position.
    /// Returns captures only if the match starts exactly at `pos`.
    pub(crate) fn regex_match_with_captures_at(
        &mut self,
        pattern: &str,
        text: &str,
        pos: usize,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        if pos > chars.len() {
            return None;
        }
        // If pattern has ^ anchor, it can only match at position 0
        if parsed.anchor_start && pos != 0 {
            return None;
        }
        self.regex_match_end_from_caps_in_pkg(&parsed, &chars, pos, &pkg)
            .map(|(end, mut caps)| {
                caps.from = caps.capture_start.unwrap_or(pos);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.matched = chars[caps.from..caps.to].iter().collect();
                caps
            })
    }

    pub(super) fn regex_match_all_with_captures(
        &self,
        pattern: &str,
        text: &str,
    ) -> Vec<RegexCaptures> {
        let Some(parsed) = self.parse_regex(pattern) else {
            return Vec::new();
        };
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        let mut out = Vec::new();
        let mut starts = Vec::new();
        if parsed.anchor_start {
            starts.push(0usize);
        } else {
            starts.extend(0..=chars.len());
        }
        for start in starts {
            for (end, mut caps) in
                self.regex_match_ends_from_caps_in_pkg(&parsed, &chars, start, &pkg)
            {
                caps.from = caps.capture_start.unwrap_or(start);
                caps.to = caps.capture_end.unwrap_or(end);
                caps.matched = chars[caps.from..caps.to].iter().collect();
                out.push(caps);
            }
        }
        out.sort_by_key(|caps| (caps.from, caps.to, caps.positional.len(), caps.named.len()));
        out
    }

    pub(super) fn regex_find_first(&self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();

        // When :m (ignoremark) is set, strip combining marks from input and match,
        // then map positions back to the original text.
        if parsed.ignore_mark {
            use unicode_normalization::UnicodeNormalization;
            // Build stripped text and position map (stripped char index -> original char index)
            let orig_chars: Vec<char> = text.chars().collect();
            let mut stripped_chars: Vec<char> = Vec::new();
            let mut pos_map: Vec<usize> = Vec::new(); // stripped idx -> original idx
            for (orig_idx, ch) in orig_chars.iter().enumerate() {
                // Decompose char and check if it has combining marks
                let decomposed: Vec<char> = ch.to_string().nfd().collect();
                for dch in &decomposed {
                    if !unicode_normalization::char::is_combining_mark(*dch) {
                        stripped_chars.push(*dch);
                        pos_map.push(orig_idx);
                    }
                }
            }
            // Also map end position (one past last char)
            pos_map.push(orig_chars.len());

            if parsed.anchor_start {
                return self
                    .regex_match_end_from_in_pkg(&parsed, &stripped_chars, 0, &pkg)
                    .map(|end| (pos_map[0], pos_map[end]));
            }
            for start in 0..=stripped_chars.len() {
                if let Some(end) =
                    self.regex_match_end_from_in_pkg(&parsed, &stripped_chars, start, &pkg)
                {
                    let orig_start = pos_map[start];
                    let orig_end = if end < pos_map.len() {
                        pos_map[end]
                    } else {
                        orig_chars.len()
                    };
                    return Some((orig_start, orig_end));
                }
            }
            return None;
        }

        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
                .map(|end| (0, end));
        }
        for start in 0..=chars.len() {
            if let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, start, &pkg) {
                return Some((start, end));
            }
        }
        None
    }

    pub(super) fn regex_match_len_at_start(&mut self, pattern: &str, text: &str) -> Option<usize> {
        let captures = self.regex_match_with_captures(pattern, text)?;
        if captures.from == 0 {
            Some(captures.to)
        } else {
            None
        }
    }

    fn regex_match_len_at_start_in_pkg(
        &self,
        pattern: &str,
        text: &str,
        pkg: &str,
    ) -> Option<usize> {
        let mut interp = Interpreter {
            env: self.env.clone(),
            functions: self.functions.clone(),
            proto_functions: self.proto_functions.clone(),
            token_defs: self.token_defs.clone(),
            current_package: pkg.to_string(),
            var_dynamic_flags: self.var_dynamic_flags.clone(),
            var_type_constraints: self.var_type_constraints.clone(),
            state_vars: self.state_vars.clone(),
            ..Default::default()
        };
        interp.regex_match_len_at_start(pattern, text)
    }

    fn regex_match_end_from_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Option<usize> {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return Some(pos);
                    }
                } else {
                    return Some(pos);
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    stack.push((idx + 1, pos));
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        None
    }

    fn regex_match_end_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Option<(usize, RegexCaptures)> {
        self.regex_match_ends_from_caps_in_pkg(pattern, chars, start, pkg)
            .into_iter()
            .next()
    }

    fn regex_match_ends_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Vec<(usize, RegexCaptures)> {
        let apply_named_capture =
            |token: &RegexToken, from: usize, to: usize, caps: RegexCaptures| -> RegexCaptures {
                let Some(name) = token.named_capture.as_ref() else {
                    return caps;
                };
                let mut updated = caps;
                let captured: String = chars[from..to].iter().collect();
                updated
                    .named
                    .entry(name.clone())
                    .or_default()
                    .push(captured);
                updated
            };
        let mut stack = Vec::new();
        stack.push((0usize, start, RegexCaptures::default()));
        let mut matches = Vec::new();
        while let Some((idx, pos, caps)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if !pattern.anchor_end || pos == chars.len() {
                    matches.push((pos, caps));
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    );
                    if token.ratchet {
                        candidates
                            .sort_by_key(|(next, c)| (*next, c.positional.len(), c.named.len()));
                        if let Some(best) = candidates.pop() {
                            candidates = vec![best];
                        }
                    }
                    for (next, new_caps) in candidates {
                        stack.push((
                            idx + 1,
                            next,
                            apply_named_capture(token, pos, next, new_caps),
                        ));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    );
                    if token.ratchet {
                        candidates
                            .sort_by_key(|(next, c)| (*next, c.positional.len(), c.named.len()));
                        if let Some(best) = candidates.pop() {
                            // Atom matched — commit to the match (no backtracking)
                            candidates = vec![best];
                        } else {
                            // Atom didn't match — commit to "zero" (no match)
                            stack.push((idx + 1, pos, caps.clone()));
                            candidates.clear();
                        }
                    } else {
                        stack.push((idx + 1, pos, caps.clone()));
                    }
                    for (next, new_caps) in candidates {
                        stack.push((
                            idx + 1,
                            next,
                            apply_named_capture(token, pos, next, new_caps),
                        ));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    // Fast path: ratcheted simple atom with no named capture
                    // avoids all RegexCaptures cloning by only tracking position.
                    if token.ratchet && token.named_capture.is_none() && is_simple_atom(&token.atom)
                    {
                        let mut current = pos;
                        while let Some(next) = self.regex_match_atom_in_pkg(
                            &token.atom,
                            chars,
                            current,
                            pkg,
                            pattern.ignore_case,
                        ) {
                            if next == current {
                                break;
                            }
                            current = next;
                        }
                        stack.push((idx + 1, current, caps));
                    } else if token.ratchet
                        && token.named_capture.is_none()
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted Named token: resolve pattern once,
                        // match directly without creating new Interpreter per iteration
                        let mut current = pos;
                        while current < chars.len() {
                            if let Some(end) = self.regex_match_end_from_in_pkg(
                                &resolved,
                                chars,
                                current,
                                &resolved_pkg,
                            ) {
                                if end == current {
                                    break;
                                }
                                current = end;
                            } else {
                                break;
                            }
                        }
                        stack.push((idx + 1, current, caps));
                    } else {
                        let mut positions = Vec::new();
                        positions.push((pos, caps.clone()));
                        let mut current = pos;
                        let mut current_caps = caps.clone();
                        while let Some((next, new_caps)) = self
                            .regex_match_atom_with_capture_in_pkg(
                                &token.atom,
                                chars,
                                current,
                                &current_caps,
                                pkg,
                                pattern.ignore_case,
                            )
                        {
                            if next == current {
                                break;
                            }
                            let new_caps = apply_named_capture(token, current, next, new_caps);
                            current_caps = new_caps.clone();
                            positions.push((next, new_caps));
                            current = next;
                        }
                        if token.ratchet
                            && let Some(last) = positions.last().cloned()
                        {
                            positions = vec![last];
                        }
                        for (p, c) in positions {
                            stack.push((idx + 1, p, c));
                        }
                    }
                }
                RegexQuant::OneOrMore => {
                    // Fast path: ratcheted simple atom with no named capture
                    if token.ratchet && token.named_capture.is_none() && is_simple_atom(&token.atom)
                    {
                        let Some(mut current) = self.regex_match_atom_in_pkg(
                            &token.atom,
                            chars,
                            pos,
                            pkg,
                            pattern.ignore_case,
                        ) else {
                            continue;
                        };
                        while let Some(next) = self.regex_match_atom_in_pkg(
                            &token.atom,
                            chars,
                            current,
                            pkg,
                            pattern.ignore_case,
                        ) {
                            if next == current {
                                break;
                            }
                            current = next;
                        }
                        stack.push((idx + 1, current, caps));
                    } else if token.ratchet
                        && token.named_capture.is_none()
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted Named token
                        let Some(mut current) =
                            self.regex_match_end_from_in_pkg(&resolved, chars, pos, &resolved_pkg)
                        else {
                            continue;
                        };
                        while current < chars.len() {
                            if let Some(end) = self.regex_match_end_from_in_pkg(
                                &resolved,
                                chars,
                                current,
                                &resolved_pkg,
                            ) {
                                if end == current {
                                    break;
                                }
                                current = end;
                            } else {
                                break;
                            }
                        }
                        stack.push((idx + 1, current, caps));
                    } else {
                        let (mut current, mut current_caps) = match self
                            .regex_match_atom_with_capture_in_pkg(
                                &token.atom,
                                chars,
                                pos,
                                &caps,
                                pkg,
                                pattern.ignore_case,
                            ) {
                            Some((next, new_caps)) => {
                                let new_caps = apply_named_capture(token, pos, next, new_caps);
                                (next, new_caps)
                            }
                            None => continue,
                        };
                        let mut positions = Vec::new();
                        positions.push((current, current_caps.clone()));
                        while let Some((next, new_caps)) = self
                            .regex_match_atom_with_capture_in_pkg(
                                &token.atom,
                                chars,
                                current,
                                &current_caps,
                                pkg,
                                pattern.ignore_case,
                            )
                        {
                            if next == current {
                                break;
                            }
                            let new_caps = apply_named_capture(token, current, next, new_caps);
                            current_caps = new_caps.clone();
                            positions.push((next, new_caps));
                            current = next;
                        }
                        if token.ratchet
                            && let Some(last) = positions.last().cloned()
                        {
                            positions = vec![last];
                        }
                        for (p, c) in positions {
                            stack.push((idx + 1, p, c));
                        }
                    }
                }
            }
        }
        matches
    }

    fn regex_match_atom_all_with_capture_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        if let RegexAtom::Alternation(alternatives) = atom {
            let mut out = Vec::new();
            for alt in alternatives {
                if let Some((next, mut inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg)
                {
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps
                        .positional_subcaps
                        .append(&mut inner_caps.positional_subcaps);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    out.push((next, new_caps));
                }
            }
            // Stack matching is LIFO; reverse so the first alternative is explored first.
            out.reverse();
            return out;
        }
        if let RegexAtom::Group(pattern) = atom {
            let mut out = Vec::new();
            for (end, mut inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
            {
                let mut new_caps = current_caps.clone();
                for (k, v) in inner_caps.named.drain() {
                    new_caps.named.entry(k).or_default().extend(v);
                }
                for v in inner_caps.positional.drain(..) {
                    new_caps.positional.push(v);
                }
                new_caps
                    .positional_subcaps
                    .append(&mut inner_caps.positional_subcaps);
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                out.push((end, new_caps));
            }
            return out;
        }
        if let RegexAtom::GoalMatch {
            goal,
            inner,
            goal_text,
        } = atom
        {
            let mut out = Vec::new();
            for (inner_end, inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(inner, chars, pos, pkg)
            {
                let goal_matches =
                    self.regex_match_ends_from_caps_in_pkg(goal, chars, inner_end, pkg);
                if goal_matches.is_empty() {
                    Self::record_goal_failure(goal_text, inner_end);
                    continue;
                }
                for (goal_end, goal_caps) in goal_matches {
                    let new_caps = merge_regex_captures(
                        current_caps.clone(),
                        merge_regex_captures(goal_caps, inner_caps.clone()),
                    );
                    out.push((goal_end, new_caps));
                }
            }
            return out;
        }
        if let RegexAtom::CaptureGroup(pattern) = atom {
            let mut out = Vec::new();
            for (end, inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
            {
                let captured: String = chars[pos..end].iter().collect();
                let mut new_caps = current_caps.clone();
                let mut inner_caps = inner_caps;
                // Merge inner named captures into parent
                for (k, v) in inner_caps.named.drain() {
                    new_caps.named.entry(k).or_default().extend(v);
                }
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                // Store inner captures as subcaptures of this group
                let mut subcap = inner_caps;
                subcap.matched = captured.clone();
                subcap.from = pos;
                subcap.to = end;
                new_caps.positional.push(captured);
                new_caps.positional_subcaps.push(Some(subcap));
                out.push((end, new_caps));
            }
            out.sort_by_key(|(end, caps)| {
                (
                    *end,
                    caps.code_blocks.len(),
                    caps.positional.len(),
                    caps.named.len(),
                )
            });
            let mut deduped: Vec<(usize, RegexCaptures)> = Vec::new();
            for (end, caps) in out {
                if deduped.last().is_some_and(|(last_end, _)| *last_end == end) {
                    deduped.pop();
                }
                deduped.push((end, caps));
            }
            return deduped;
        }
        if let RegexAtom::Alternation(alternatives) = atom {
            let mut out = Vec::new();
            for alt in alternatives {
                for (end, mut inner_caps) in
                    self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg)
                {
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
                    for v in inner_caps.positional.drain(..) {
                        new_caps.positional.push(v);
                    }
                    new_caps
                        .positional_subcaps
                        .append(&mut inner_caps.positional_subcaps);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    out.push((end, new_caps));
                }
            }
            out
        } else if let RegexAtom::Named(name) = atom {
            let spec = Self::parse_named_regex_lookup_spec(name);
            let arg_values = if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                let mut values = Vec::new();
                for arg in &spec.arg_exprs {
                    let Some(v) = self.eval_regex_expr_value(arg, current_caps) else {
                        return Vec::new();
                    };
                    values.push(v);
                }
                values
            };
            let candidates = self.resolve_named_regex_candidates_in_pkg(&spec, pkg, &arg_values);
            if !candidates.is_empty() {
                let tail: Vec<char> = chars[pos..].to_vec();
                let mut out = Vec::new();
                for (sub_pat, sub_pkg, sym_key) in candidates {
                    if let Some(parsed) = self.parse_regex(&sub_pat) {
                        for (inner_end, inner_caps) in
                            self.regex_match_ends_from_caps_in_pkg(&parsed, &tail, 0, &sub_pkg)
                        {
                            let end = pos + inner_end;
                            let mut new_caps = current_caps.clone();
                            let capture_name = spec
                                .capture_name
                                .as_deref()
                                .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
                            if let Some(capture_name) = capture_name {
                                let captured: String = chars[pos..end].iter().collect();
                                // Store inner captures as subcaptures (nested)
                                let mut subcap = inner_caps;
                                subcap.matched = captured.clone();
                                subcap.from = pos;
                                subcap.to = end;
                                // Store :sym<> variant in subcapture
                                if sym_key.is_some() {
                                    subcap.sym = sym_key.clone();
                                }
                                new_caps.code_blocks.extend(subcap.code_blocks.clone());
                                new_caps
                                    .named_subcaps
                                    .entry(capture_name.to_string())
                                    .or_default()
                                    .push(subcap);
                                new_caps
                                    .named
                                    .entry(capture_name.to_string())
                                    .or_default()
                                    .push(captured);
                            } else {
                                // Silent subrule — merge named captures only (not positional).
                                // Inner positional captures must not leak into the parent's
                                // positional array, otherwise they consume indices that belong
                                // to subsequent capturing groups in the outer regex.
                                let mut inner_caps = inner_caps;
                                for (k, v) in inner_caps.named.drain() {
                                    new_caps.named.entry(k).or_default().extend(v);
                                }
                                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                            }
                            out.push((end, new_caps));
                        }
                    }
                }
                out.sort_by_key(|(end, caps)| {
                    (
                        *end,
                        caps.code_blocks.len(),
                        caps.positional.len(),
                        caps.named.len(),
                    )
                });
                let mut deduped: Vec<(usize, RegexCaptures)> = Vec::new();
                for (end, caps) in out {
                    if deduped.last().is_some_and(|(last_end, _)| *last_end == end) {
                        deduped.pop();
                    }
                    deduped.push((end, caps));
                }
                return deduped;
            }
            self.regex_match_atom_with_capture_in_pkg(
                atom,
                chars,
                pos,
                current_caps,
                pkg,
                ignore_case,
            )
            .into_iter()
            .collect()
        } else {
            self.regex_match_atom_with_capture_in_pkg(
                atom,
                chars,
                pos,
                current_caps,
                pkg,
                ignore_case,
            )
            .into_iter()
            .collect()
        }
    }

    #[allow(dead_code)]
    fn regex_match_from_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> bool {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return true;
                    }
                } else {
                    return true;
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    stack.push((idx + 1, pos));
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        false
    }

    fn regex_match_atom_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        pkg: &str,
        ignore_case: bool,
    ) -> Option<usize> {
        // Group, CaptureGroup, Alternation, ZeroWidth, CodeAssertion can match zero-width
        match atom {
            RegexAtom::Group(pattern) => {
                return self.regex_match_end_from_in_pkg(pattern, chars, pos, pkg);
            }
            RegexAtom::CaptureGroup(pattern) => {
                return self.regex_match_end_from_in_pkg(pattern, chars, pos, pkg);
            }
            RegexAtom::GoalMatch {
                goal,
                inner,
                goal_text,
            } => {
                if let Some(inner_end) = self.regex_match_end_from_in_pkg(inner, chars, pos, pkg) {
                    if let Some(goal_end) =
                        self.regex_match_end_from_in_pkg(goal, chars, inner_end, pkg)
                    {
                        return Some(goal_end);
                    }
                    Self::record_goal_failure(goal_text, inner_end);
                }
                return None;
            }
            RegexAtom::Alternation(alternatives) => {
                for alt in alternatives {
                    if let Some(end) = self.regex_match_end_from_in_pkg(alt, chars, pos, pkg) {
                        return Some(end);
                    }
                }
                return None;
            }
            RegexAtom::ZeroWidth => {
                return Some(pos); // Matches at any position without consuming
            }
            RegexAtom::LeftWordBoundary => {
                // << / « — left word boundary: position is at the start of a word.
                // The char before pos is NOT a word char (or pos == 0),
                // AND the char at pos IS a word char.
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let at_is_word = pos < chars.len() && is_word_char(chars[pos]);
                return if !before_is_word && at_is_word {
                    Some(pos)
                } else {
                    None
                };
            }
            RegexAtom::RightWordBoundary => {
                // >> / » — right word boundary: position is at the end of a word.
                // The char before pos IS a word char,
                // AND the char at pos is NOT a word char (or pos == chars.len()).
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let at_is_word = pos < chars.len() && is_word_char(chars[pos]);
                return if before_is_word && !at_is_word {
                    Some(pos)
                } else {
                    None
                };
            }
            RegexAtom::StartOfLine => {
                // ^^ — matches at the start of any logical line:
                // pos == 0 OR the char before pos is \n
                return if pos == 0 || chars[pos - 1] == '\n' {
                    Some(pos)
                } else {
                    None
                };
            }
            RegexAtom::EndOfLine => {
                // $$ — matches at the end of any logical line:
                // pos == chars.len() OR the char at pos is \n
                // But NOT between \r and \n (i.e., if chars[pos-1] == '\r' and chars[pos] == '\n', no match)
                if pos == chars.len() {
                    return Some(pos);
                }
                if chars[pos] == '\n' {
                    // Don't match between \r and \n
                    if pos > 0 && chars[pos - 1] == '\r' {
                        return None;
                    }
                    return Some(pos);
                }
                if chars[pos] == '\r' && (pos + 1 >= chars.len() || chars[pos + 1] != '\n') {
                    return Some(pos);
                }
                return None;
            }
            RegexAtom::CodeAssertion { .. } => {
                // In non-capture mode, code assertions always succeed
                // (we can't evaluate them without capture context)
                return Some(pos);
            }
            RegexAtom::CaptureStartMarker | RegexAtom::CaptureEndMarker => {
                return Some(pos);
            }
            RegexAtom::Backref(_) => {
                // Backreferences need capture context; can't match without it
                return None;
            }
            RegexAtom::Lookaround {
                pattern,
                negated,
                is_behind,
            } => {
                let matched = if *is_behind {
                    let mut found = false;
                    for start in 0..=pos {
                        if self
                            .regex_match_end_from_caps_in_pkg(pattern, chars, start, pkg)
                            .is_some_and(|(end, _)| end == pos)
                        {
                            found = true;
                            break;
                        }
                    }
                    found
                } else {
                    self.regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                        .is_some()
                };
                let pass = if *negated { !matched } else { matched };
                return if pass { Some(pos) } else { None };
            }
            RegexAtom::UnicodePropAssert { name, negated } => {
                // Zero-width assertion: check next char but don't consume
                if pos >= chars.len() {
                    // At end of string, negated assertion succeeds
                    return if *negated { Some(pos) } else { None };
                }
                let c = chars[pos];
                let prop_match = check_unicode_property(name, c);
                let result = if *negated { !prop_match } else { prop_match };
                return if result { Some(pos) } else { None };
            }
            _ => {}
        }
        if let RegexAtom::Named(name) = atom {
            let spec = Self::parse_named_regex_lookup_spec(name);
            let default_caps = RegexCaptures::default();
            let arg_values = if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                let mut values = Vec::new();
                for arg in &spec.arg_exprs {
                    let v = self.eval_regex_expr_value(arg, &default_caps)?;
                    values.push(v);
                }
                values
            };
            let candidates = self.resolve_named_regex_candidates_in_pkg(&spec, pkg, &arg_values);
            if !candidates.is_empty() {
                let remaining: String = chars[pos..].iter().collect();
                let mut best_len: Option<usize> = None;
                for (sub_pat, sub_pkg, _sym_key) in candidates {
                    if let Some(len) =
                        self.regex_match_len_at_start_in_pkg(&sub_pat, &remaining, &sub_pkg)
                    {
                        let better = best_len.map(|current| len > current).unwrap_or(true);
                        if better {
                            best_len = Some(len);
                        }
                    }
                }
                return best_len.map(|len| pos + len);
            }
            if spec.lookup_name == "ws" && !spec.token_lookup {
                let mut next = pos;
                while next < chars.len() && chars[next].is_whitespace() {
                    next += 1;
                }
                // Between two word characters, require at least one whitespace
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let after_is_word = next < chars.len() && is_word_char(chars[next]);
                if before_is_word && after_is_word && next == pos {
                    return None;
                }
                return Some(next);
            }
        }
        if pos >= chars.len() {
            return None;
        }
        let c = chars[pos];
        // Newline needs special handling: \r\n is a single logical newline (2 chars)
        match atom {
            RegexAtom::Newline => {
                // Raku \n matches: \r\n (CR+LF), \n (LF), \r (CR), \x85 (NEL), \x2028 (LINE SEP)
                if c == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
                    return Some(pos + 2); // CR/LF
                }
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return Some(pos + 1);
                }
                return None;
            }
            RegexAtom::NotNewline => {
                // Matches anything that is NOT a newline character
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return None;
                }
                return Some(pos + 1);
            }
            _ => {}
        }
        let matched = match atom {
            RegexAtom::Literal(ch) => {
                if ignore_case {
                    ch.to_lowercase().to_string() == c.to_lowercase().to_string()
                } else {
                    *ch == c
                }
            }
            RegexAtom::Named(name) => {
                let spec = Self::parse_named_regex_lookup_spec(name);
                if spec.token_lookup || !spec.arg_exprs.is_empty() {
                    return None;
                }
                let literal = spec.lookup_name;
                let name_chars: Vec<char> = literal.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    false
                } else {
                    let slice = &chars[pos..pos + name_chars.len()];
                    if ignore_case {
                        slice.iter().collect::<String>().to_lowercase()
                            == name_chars.iter().collect::<String>().to_lowercase()
                    } else {
                        *slice == name_chars[..]
                    }
                }
            }
            RegexAtom::Any => true,
            RegexAtom::CharClass(class) => self.regex_match_class(class, c),
            RegexAtom::UnicodeProp {
                name,
                negated,
                args,
            } => {
                let prop_match = if let Some(arg_str) = args {
                    check_unicode_property_with_args(name, arg_str, c)
                } else {
                    check_unicode_property(name, c)
                };
                if *negated { !prop_match } else { prop_match }
            }
            RegexAtom::CompositeClass { positive, negative } => {
                let pos_match = positive.iter().any(|item| match item {
                    ClassItem::NamedBuiltin(n) => matches_named_builtin(n, c),
                    ClassItem::UnicodePropItem { name, negated } => {
                        let m = check_unicode_property(name, c);
                        if *negated { !m } else { m }
                    }
                    _ => self.regex_match_class(
                        &CharClass {
                            items: vec![item.clone()],
                            negated: false,
                        },
                        c,
                    ),
                });
                let neg_match = negative.iter().any(|item| match item {
                    ClassItem::NamedBuiltin(n) => matches_named_builtin(n, c),
                    ClassItem::UnicodePropItem { name, negated } => {
                        let m = check_unicode_property(name, c);
                        if *negated { !m } else { m }
                    }
                    _ => self.regex_match_class(
                        &CharClass {
                            items: vec![item.clone()],
                            negated: false,
                        },
                        c,
                    ),
                });
                pos_match && !neg_match
            }
            RegexAtom::Group(_)
            | RegexAtom::CaptureGroup(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::GoalMatch { .. }
            | RegexAtom::Newline
            | RegexAtom::NotNewline
            | RegexAtom::ZeroWidth
            | RegexAtom::CodeAssertion { .. }
            | RegexAtom::UnicodePropAssert { .. }
            | RegexAtom::Lookaround { .. }
            | RegexAtom::CaptureStartMarker
            | RegexAtom::CaptureEndMarker
            | RegexAtom::Backref(_)
            | RegexAtom::VarDecl { .. }
            | RegexAtom::ClosureInterpolation { .. }
            | RegexAtom::LeftWordBoundary
            | RegexAtom::RightWordBoundary
            | RegexAtom::StartOfLine
            | RegexAtom::TildeMarker
            | RegexAtom::EndOfLine => unreachable!(),
        };
        if matched {
            match atom {
                RegexAtom::Named(name) => {
                    let spec = Self::parse_named_regex_lookup_spec(name);
                    Some(pos + spec.lookup_name.chars().count())
                }
                _ => Some(pos + 1),
            }
        } else {
            None
        }
    }

    fn regex_match_atom_with_capture_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
    ) -> Option<(usize, RegexCaptures)> {
        // Handle zero-width and group atoms before the length check
        match atom {
            RegexAtom::Group(pattern) => {
                // Use capture-aware matching for groups to propagate inner named captures
                return self
                    .regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                    .map(|(next, mut inner_caps)| {
                        let mut new_caps = current_caps.clone();
                        for (k, v) in inner_caps.named.drain() {
                            new_caps.named.entry(k).or_default().extend(v);
                        }
                        new_caps.positional.extend(inner_caps.positional);
                        new_caps
                            .positional_subcaps
                            .append(&mut inner_caps.positional_subcaps);
                        (next, new_caps)
                    });
            }
            RegexAtom::GoalMatch {
                goal,
                inner,
                goal_text,
            } => {
                if let Some((inner_end, inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(inner, chars, pos, pkg)
                {
                    if let Some((goal_end, goal_caps)) =
                        self.regex_match_end_from_caps_in_pkg(goal, chars, inner_end, pkg)
                    {
                        let new_caps = merge_regex_captures(
                            current_caps.clone(),
                            merge_regex_captures(goal_caps, inner_caps),
                        );
                        return Some((goal_end, new_caps));
                    }
                    Self::record_goal_failure(goal_text, inner_end);
                }
                return None;
            }
            RegexAtom::Alternation(alternatives) => {
                // Explore all alternatives and keep the longest successful one.
                // This allows parse-time backtracking into a longer branch when
                // shorter branches cannot satisfy a surrounding full-match check.
                let mut best: Option<(usize, RegexCaptures)> = None;
                for alt in alternatives {
                    if let Some((next, mut inner_caps)) =
                        self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg)
                    {
                        let mut new_caps = current_caps.clone();
                        for (k, v) in inner_caps.named.drain() {
                            new_caps.named.entry(k).or_default().extend(v);
                        }
                        new_caps.positional.append(&mut inner_caps.positional);
                        new_caps
                            .positional_subcaps
                            .append(&mut inner_caps.positional_subcaps);
                        new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                        let replace = best
                            .as_ref()
                            .map(|(best_next, _)| next > *best_next)
                            .unwrap_or(true);
                        if replace {
                            best = Some((next, new_caps));
                        }
                    }
                }
                return best;
            }
            RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. }
            | RegexAtom::LeftWordBoundary
            | RegexAtom::RightWordBoundary
            | RegexAtom::StartOfLine
            | RegexAtom::EndOfLine => {
                return self
                    .regex_match_atom_in_pkg(atom, chars, pos, pkg, ignore_case)
                    .map(|next| (next, current_caps.clone()));
            }
            RegexAtom::Lookaround {
                pattern,
                negated,
                is_behind,
            } => {
                let matched = if *is_behind {
                    // Lookbehind: try matching the pattern at each possible start
                    // position before current pos, checking if it ends at pos
                    let mut found = false;
                    for start in 0..=pos {
                        if self
                            .regex_match_end_from_caps_in_pkg(pattern, chars, start, pkg)
                            .is_some_and(|(end, _)| end == pos)
                        {
                            found = true;
                            break;
                        }
                    }
                    found
                } else {
                    // Lookahead: try matching the pattern at current position
                    self.regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                        .is_some()
                };
                let pass = if *negated { !matched } else { matched };
                return if pass {
                    Some((pos, current_caps.clone()))
                } else {
                    None
                };
            }
            RegexAtom::CaptureGroup(pattern) => {
                // Match the inner pattern and capture the matched text
                if let Some((end, inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                {
                    let captured: String = chars[pos..end].iter().collect();
                    let mut new_caps = current_caps.clone();
                    // Merge inner named captures into parent
                    for (k, v) in &inner_caps.named {
                        new_caps
                            .named
                            .entry(k.clone())
                            .or_default()
                            .extend(v.clone());
                    }
                    // Store inner captures as subcaptures of this group
                    // (don't flatten into parent positional list)
                    let mut subcap = inner_caps.clone();
                    subcap.matched = captured.clone();
                    subcap.from = pos;
                    subcap.to = end;
                    new_caps.positional.push(captured);
                    new_caps.positional_subcaps.push(Some(subcap));
                    return Some((end, new_caps));
                }
                return None;
            }
            RegexAtom::CodeAssertion {
                code,
                negated,
                is_assertion,
            } => {
                if *is_assertion {
                    // <?{ expr }> or <!{ expr }> — assertion based on truthiness
                    let result = self.eval_regex_code_assertion(code, current_caps);
                    let pass = if *negated { !result } else { result };
                    return if pass {
                        Some((pos, current_caps.clone()))
                    } else {
                        None
                    };
                }
                // Plain { code } block — always succeeds, record for side effects
                let mut new_caps = current_caps.clone();
                new_caps
                    .code_blocks
                    .push((code.clone(), current_caps.named.clone()));
                return Some((pos, new_caps));
            }
            RegexAtom::ClosureInterpolation { code } => {
                // <{ code }> — closure interpolation: evaluate code and use
                // the result as a regex pattern to match at current position
                let target: String = chars.iter().collect();
                let pattern_str =
                    self.eval_regex_closure_interpolation(code, current_caps, &target);
                if let Some(ref pat_str) = pattern_str
                    && Interpreter::contains_dangerous_regex_code(pat_str)
                {
                    regex_parse::PENDING_REGEX_ERROR.with(|e| {
                        *e.borrow_mut() = Some(Interpreter::make_security_policy_error());
                    });
                    return None;
                }
                if let Some(pat_str) = pattern_str
                    && let Some(parsed) = self.parse_regex(&pat_str)
                {
                    let pkg = self.current_package.clone();
                    if let Some((end, inner_caps)) =
                        self.regex_match_end_from_caps_in_pkg(&parsed, chars, pos, &pkg)
                    {
                        let mut new_caps = current_caps.clone();
                        for v in &inner_caps.positional {
                            new_caps.positional.push(v.clone());
                        }
                        new_caps
                            .positional_subcaps
                            .extend(inner_caps.positional_subcaps.clone());
                        for (k, v) in &inner_caps.named {
                            new_caps
                                .named
                                .entry(k.clone())
                                .or_default()
                                .extend(v.clone());
                        }
                        return Some((end, new_caps));
                    }
                }
                return None;
            }
            RegexAtom::CaptureStartMarker => {
                let mut new_caps = current_caps.clone();
                new_caps.capture_start = Some(pos);
                return Some((pos, new_caps));
            }
            RegexAtom::CaptureEndMarker => {
                let mut new_caps = current_caps.clone();
                new_caps.capture_end = Some(pos);
                return Some((pos, new_caps));
            }
            RegexAtom::Backref(idx) => {
                // Match the text previously captured by positional group $idx.
                // TODO: With quantified captures, $0 should reference the most
                // recent capture of the idx-th group. Currently, this uses a
                // simple positional index which doesn't track group identity
                // across multiple iterations of a quantified capture group.
                let captured = current_caps.positional.get(*idx).cloned();
                if let Some(ref_text) = captured {
                    let ref_chars: Vec<char> = ref_text.chars().collect();
                    if pos + ref_chars.len() <= chars.len()
                        && chars[pos..pos + ref_chars.len()] == ref_chars[..]
                    {
                        return Some((pos + ref_chars.len(), current_caps.clone()));
                    }
                }
                return None;
            }
            RegexAtom::VarDecl { code } => {
                // Evaluate the variable declaration and store in env
                // so subsequent <{ }> closures can access the variables.
                let source = format!("{};", code);
                if let Ok((stmts, _)) = crate::parse_dispatch::parse_source(&source) {
                    let mut interp = Interpreter {
                        env: self.env.clone(),
                        functions: self.functions.clone(),
                        current_package: self.current_package.clone(),
                        ..Default::default()
                    };
                    let _ = interp.eval_block_value(&stmts);
                    // Copy any new/changed variables back into self.env
                    // We use unsafe interior mutability pattern since self is &self
                    // and the regex engine needs to be &self for recursive matching.
                    // Store declarations in captures for later propagation.
                    let mut new_caps = current_caps.clone();
                    for (k, v) in &interp.env {
                        if !self.env.contains_key(k) || self.env.get(k) != Some(v) {
                            new_caps.regex_vars.insert(k.clone(), v.clone());
                        }
                    }
                    return Some((pos, new_caps));
                }
                return Some((pos, current_caps.clone()));
            }
            _ => {}
        }
        if let RegexAtom::Named(name) = atom {
            let spec = Self::parse_named_regex_lookup_spec(name);
            let arg_values = if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                let mut values = Vec::new();
                for arg in &spec.arg_exprs {
                    let v = self.eval_regex_expr_value(arg, current_caps)?;
                    values.push(v);
                }
                values
            };
            let candidates = self.resolve_named_regex_candidates_in_pkg(&spec, pkg, &arg_values);
            if !candidates.is_empty() {
                let tail: Vec<char> = chars[pos..].to_vec();
                let mut best: Option<(usize, RegexCaptures)> = None;
                let mut best_sym: Option<String> = None;
                for (sub_pat, sub_pkg, sym_key) in candidates {
                    let tail_text: String = tail.iter().collect();
                    let mut interp = Interpreter {
                        env: self.env.clone(),
                        functions: self.functions.clone(),
                        proto_functions: self.proto_functions.clone(),
                        token_defs: self.token_defs.clone(),
                        current_package: sub_pkg.clone(),
                        var_dynamic_flags: self.var_dynamic_flags.clone(),
                        var_type_constraints: self.var_type_constraints.clone(),
                        state_vars: self.state_vars.clone(),
                        ..Default::default()
                    };
                    if let Some(mut inner_caps) =
                        interp.regex_match_with_captures(&sub_pat, &tail_text)
                    {
                        if inner_caps.from != 0 {
                            continue;
                        }
                        let inner_end = inner_caps.to;
                        let better = best
                            .as_ref()
                            .map(|(best_end, _)| inner_end > *best_end)
                            .unwrap_or(true);
                        if better {
                            inner_caps.from = 0;
                            inner_caps.to = inner_end;
                            best = Some((inner_end, inner_caps));
                            best_sym = sym_key;
                        }
                    }
                }
                if let Some((inner_end, inner_caps)) = best {
                    let end = pos + inner_end;
                    let mut new_caps = current_caps.clone();
                    let capture_name = spec
                        .capture_name
                        .as_deref()
                        .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
                    if let Some(capture_name) = capture_name {
                        let captured: String = chars[pos..end].iter().collect();
                        // Store inner captures as subcaptures (nested)
                        let mut subcap = inner_caps;
                        subcap.matched = captured.clone();
                        subcap.from = pos;
                        subcap.to = end;
                        // Store :sym<> variant in subcapture
                        if best_sym.is_some() {
                            subcap.sym = best_sym;
                        }
                        new_caps.code_blocks.extend(subcap.code_blocks.clone());
                        new_caps
                            .named_subcaps
                            .entry(capture_name.to_string())
                            .or_default()
                            .push(subcap);
                        new_caps
                            .named
                            .entry(capture_name.to_string())
                            .or_default()
                            .push(captured);
                    } else {
                        // No capture name — merge inner captures flat
                        let mut inner_caps = inner_caps;
                        for (k, v) in inner_caps.named.drain() {
                            new_caps.named.entry(k).or_default().extend(v);
                        }
                        for v in inner_caps.positional.drain(..) {
                            new_caps.positional.push(v);
                        }
                        new_caps
                            .positional_subcaps
                            .append(&mut inner_caps.positional_subcaps);
                        new_caps.code_blocks.extend(inner_caps.code_blocks);
                    }
                    return Some((end, new_caps));
                }
                return None;
            }
            if spec.lookup_name == "ws" && !spec.token_lookup {
                let mut end = pos;
                while end < chars.len() && chars[end].is_whitespace() {
                    end += 1;
                }
                // Between two word characters, require at least one whitespace
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let after_is_word = end < chars.len() && is_word_char(chars[end]);
                if before_is_word && after_is_word && end == pos {
                    return None;
                }
                let mut new_caps = current_caps.clone();
                if !spec.silent {
                    let captured: String = chars[pos..end].iter().collect();
                    new_caps
                        .named
                        .entry(spec.lookup_name.to_string())
                        .or_default()
                        .push(captured);
                }
                return Some((end, new_caps));
            }
        }
        if pos >= chars.len() {
            return None;
        }
        match atom {
            RegexAtom::Named(name) => {
                let spec = Self::parse_named_regex_lookup_spec(name);
                if spec.token_lookup {
                    return None;
                }
                let literal = spec.lookup_name;
                let name_chars: Vec<char> = literal.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    return None;
                }
                if chars[pos..pos + name_chars.len()] == name_chars[..] {
                    let captured: String = name_chars.iter().collect();
                    let mut new_caps = current_caps.clone();
                    let capture_name = spec.capture_name.unwrap_or(literal);
                    new_caps
                        .named
                        .entry(capture_name)
                        .or_default()
                        .push(captured);
                    return Some((pos + name_chars.len(), new_caps));
                }
                None
            }
            _ => self
                .regex_match_atom_in_pkg(atom, chars, pos, pkg, ignore_case)
                .map(|next| (next, current_caps.clone())),
        }
    }

    /// Evaluate a closure interpolation `<{ code }>` inside a regex.
    /// Returns the regex pattern string to match against.
    fn eval_regex_closure_interpolation(
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
            functions: self.functions.clone(),
            token_defs: self.token_defs.clone(),
            current_package: self.current_package.clone(),
            ..Default::default()
        };
        let val = match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(e) => e.return_value?,
        };
        match val {
            Value::Regex(pat) => Some(pat.to_string()),
            Value::RegexWithAdverbs { pattern, .. } => Some(pattern.to_string()),
            Value::Routine {
                is_regex: true,
                name,
                package,
            } => {
                let full_name = if package == "" {
                    name.resolve()
                } else {
                    format!("{}::{}", package, name)
                };
                Some(format!("<{}>", full_name))
            }
            Value::Array(ref elems, ..) | Value::Seq(ref elems) => {
                // Array/List → alternation of escaped literals
                let alts: Vec<String> = elems
                    .iter()
                    .map(|v| match v {
                        Value::Regex(pat) => pat.to_string(),
                        Value::RegexWithAdverbs { pattern, .. } => pattern.to_string(),
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
    fn eval_regex_code_assertion(&self, code: &str, caps: &RegexCaptures) -> bool {
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
        // Set named captures
        for (k, v) in &caps.named {
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
            functions: self.functions.clone(),
            current_package: self.current_package.clone(),
            ..Default::default()
        };
        match interp.eval_block_value(&stmts) {
            Ok(val) => val.truthy(),
            Err(_) => false,
        }
    }

    /// Execute code blocks collected during regex matching for side effects.
    pub(super) fn execute_regex_code_blocks(
        &mut self,
        code_blocks: &[(String, HashMap<String, Vec<String>>)],
    ) {
        for (code, named_at_point) in code_blocks {
            let Ok((stmts, _)) = crate::parse_dispatch::parse_source(code) else {
                continue;
            };
            // Set up named captures as $<name> variables
            let ast_hint = self.env.get("made").cloned().unwrap_or(Value::Nil);
            for (k, v) in named_at_point {
                let to_match_with_ast = |text: &str, ast: &Value| -> Value {
                    let match_obj = Value::make_match_object_with_captures(
                        text.to_string(),
                        0,
                        text.chars().count() as i64,
                        &[],
                        &HashMap::new(),
                    );
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = match_obj
                    {
                        let mut attrs = attributes.as_ref().clone();
                        attrs.insert("ast".to_string(), ast.clone());
                        Value::make_instance(class_name, attrs)
                    } else {
                        match_obj
                    }
                };
                let value = if v.len() == 1 {
                    to_match_with_ast(&v[0], &ast_hint)
                } else {
                    Value::array(
                        v.iter()
                            .map(|s| to_match_with_ast(s, &ast_hint))
                            .collect::<Vec<_>>(),
                    )
                };
                self.env.insert(format!("<{}>", k), value);
            }
            // Snapshot env keys and values before execution
            let snapshot: HashMap<String, String> = self
                .env
                .iter()
                .map(|(k, v)| (k.clone(), format!("{:?}", v)))
                .collect();
            let _ = self.eval_block_value(&stmts);
            // Record changed env variables as pending local updates for the outer VM
            for (k, v) in &self.env {
                let old_repr = snapshot.get(k).map(|s| s.as_str()).unwrap_or("");
                let new_repr = format!("{:?}", v);
                if old_repr != new_repr {
                    self.pending_local_updates.push((k.clone(), v.clone()));
                }
            }
        }
    }

    pub(super) fn regex_match_class(&self, class: &CharClass, c: char) -> bool {
        let mut matched = false;
        for item in &class.items {
            match item {
                ClassItem::Range(a, b) => {
                    if *a <= c && c <= *b {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Char(ch) => {
                    if *ch == c {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Digit => {
                    if c.is_ascii_digit() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Word => {
                    if c.is_alphanumeric() || c == '_' {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Space => {
                    if c.is_whitespace() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NamedBuiltin(name) => {
                    if matches_named_builtin(name, c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::UnicodePropItem { name, negated } => {
                    let prop_match = check_unicode_property(name, c);
                    if if *negated { !prop_match } else { prop_match } {
                        matched = true;
                        break;
                    }
                }
            }
        }
        if class.negated { !matched } else { matched }
    }

    /// Find all non-overlapping regex matches, returning (start, end) char-index pairs.
    pub(super) fn regex_find_all(&self, pattern: &str, text: &str) -> Vec<(usize, usize)> {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        let mut results = Vec::new();
        let mut pos = 0;
        while pos <= chars.len() {
            let search_start = if parsed.anchor_start { 0 } else { pos };
            let mut found = None;
            if parsed.anchor_start {
                if pos == 0
                    && let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
                {
                    found = Some((0, end));
                }
            } else {
                for start in search_start..=chars.len() {
                    if let Some(end) =
                        self.regex_match_end_from_in_pkg(&parsed, &chars, start, &pkg)
                    {
                        found = Some((start, end));
                        break;
                    }
                }
            }
            match found {
                Some((start, end)) => {
                    results.push((start, end));
                    // Advance past the match (at least 1 to avoid infinite loop)
                    pos = if end > start { end } else { start + 1 };
                }
                None => break,
            }
            if parsed.anchor_start {
                break;
            }
        }
        results
    }
}

/// Check if a character matches a named builtin character class.
pub(super) fn matches_named_builtin(name: &str, c: char) -> bool {
    match name {
        "alpha" => c.is_alphabetic() || c == '_',
        "upper" => check_unicode_property("Uppercase_Letter", c),
        "lower" => check_unicode_property("Lowercase_Letter", c),
        "digit" => c.is_ascii_digit(),
        "xdigit" => c.is_ascii_hexdigit(),
        "space" | "ws" => c.is_whitespace(),
        "alnum" => c.is_alphabetic() || c == '_' || c.is_ascii_digit(),
        "blank" => c == '\t' || c == ' ' || c == '\u{A0}',
        "cntrl" => c.is_control(),
        "punct" => check_unicode_property("Punctuation", c),
        _ => false,
    }
}
