use super::super::*;
use super::regex_helpers::{map_pos, strip_marks_pattern, strip_marks_text};

impl Interpreter {
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
    pub(super) fn try_resolve_named_to_pattern(
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

    pub(in crate::runtime) fn regex_match_with_captures_core(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let orig_chars: Vec<char> = text.chars().collect();

        // When :m (ignoremark) is set, strip combining marks from both text and
        // pattern literals, match on stripped forms, then map positions back.
        if parsed.ignore_mark {
            let (stripped_chars, pos_map) = strip_marks_text(&orig_chars);
            let stripped_parsed = strip_marks_pattern(&parsed);
            let orig_len = orig_chars.len();
            if stripped_parsed.anchor_start {
                return self
                    .regex_match_end_from_caps_in_pkg(&stripped_parsed, &stripped_chars, 0, &pkg)
                    .map(|(end, mut caps)| {
                        let from = map_pos(caps.capture_start.unwrap_or(0), &pos_map, orig_len);
                        let to = map_pos(caps.capture_end.unwrap_or(end), &pos_map, orig_len);
                        caps.from = from;
                        caps.to = to;
                        caps.matched = orig_chars[from..to].iter().collect();
                        caps
                    });
            }
            for start in 0..=stripped_chars.len() {
                if let Some((end, mut caps)) = self.regex_match_end_from_caps_in_pkg(
                    &stripped_parsed,
                    &stripped_chars,
                    start,
                    &pkg,
                ) {
                    let from = map_pos(caps.capture_start.unwrap_or(start), &pos_map, orig_len);
                    let to = map_pos(caps.capture_end.unwrap_or(end), &pos_map, orig_len);
                    caps.from = from;
                    caps.to = to;
                    caps.matched = orig_chars[from..to].iter().collect();
                    return Some(caps);
                }
            }
            return None;
        }

        let chars = orig_chars;
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
    pub(in crate::runtime) fn regex_is_match(&mut self, pattern: &str, text: &str) -> bool {
        self.regex_match_with_captures(pattern, text).is_some()
    }

    pub(in crate::runtime) fn regex_match_with_captures(
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
                self.eval_regex_arg_list(&spec.arg_exprs, &default_caps)?
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
}
