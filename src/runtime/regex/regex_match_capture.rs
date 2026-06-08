use super::super::unicode::check_unicode_property;
use super::super::*;
use super::regex_helpers::{is_word_char, matches_named_builtin, merge_regex_captures};

impl Interpreter {
    pub(super) fn regex_match_atom_with_capture_in_pkg(
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
                        for (k, v) in inner_caps.named_subcaps.drain() {
                            new_caps.named_subcaps.entry(k).or_default().extend(v);
                        }
                        new_caps
                            .named_quantified
                            .extend(inner_caps.named_quantified.drain());
                        for (k, v) in inner_caps.capture_alias_map.drain() {
                            new_caps.capture_alias_map.insert(k, v);
                        }
                        new_caps.positional.extend(inner_caps.positional);
                        new_caps
                            .positional_subcaps
                            .append(&mut inner_caps.positional_subcaps);
                        new_caps
                            .positional_quantified
                            .append(&mut inner_caps.positional_quantified);
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
                let mut best: Option<(usize, RegexCaptures)> = None;
                for alt in alternatives {
                    if let Some((next, mut inner_caps)) =
                        self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg)
                    {
                        let mut new_caps = current_caps.clone();
                        for (k, v) in inner_caps.named.drain() {
                            new_caps.named.entry(k).or_default().extend(v);
                        }
                        for (k, v) in inner_caps.named_subcaps.drain() {
                            new_caps.named_subcaps.entry(k).or_default().extend(v);
                        }
                        new_caps
                            .named_quantified
                            .extend(inner_caps.named_quantified.drain());
                        for (k, v) in inner_caps.capture_alias_map.drain() {
                            new_caps.capture_alias_map.insert(k, v);
                        }
                        new_caps.positional.append(&mut inner_caps.positional);
                        new_caps
                            .positional_subcaps
                            .append(&mut inner_caps.positional_subcaps);
                        new_caps
                            .positional_quantified
                            .append(&mut inner_caps.positional_quantified);
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
            RegexAtom::Conjunction(_) => {
                // ALL branches must match the SAME substring (end at the same
                // position). Captures from EVERY branch are merged (Raku keeps
                // all captures from each side of `&` / `&&`), in written order.
                // Delegate to the multi-end variant and take the highest-priority
                // (longest) result.
                return self
                    .regex_match_atom_all_with_capture_in_pkg(
                        atom,
                        chars,
                        pos,
                        current_caps,
                        pkg,
                        ignore_case,
                    )
                    .into_iter()
                    .next_back();
            }
            RegexAtom::SequentialAlternation(alternatives) => {
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
                        new_caps
                            .positional_quantified
                            .append(&mut inner_caps.positional_quantified);
                        new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                        return Some((next, new_caps));
                    }
                }
                return None;
            }
            RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. }
            | RegexAtom::LeftWordBoundary
            | RegexAtom::RightWordBoundary
            | RegexAtom::StartOfLine
            | RegexAtom::EndOfLine
            | RegexAtom::WsRule
            | RegexAtom::SameAssertion { .. }
            | RegexAtom::AtPosition(_) => {
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
                    // Named captures appearing inside a positional capture group
                    // belong to that group's sub-Match (`$/[0]<name>`), NOT to the
                    // parent Match's top-level named captures (`$/<name>`). They are
                    // therefore preserved only in `positional_subcaps` below, and
                    // are intentionally NOT merged into the parent `named` map.
                    // Store inner captures as subcaptures of this group
                    let mut subcap = inner_caps.clone();
                    subcap.matched = captured.clone();
                    subcap.from = pos;
                    subcap.to = end;
                    new_caps.positional.push(captured);
                    new_caps.positional_subcaps.push(Some(subcap));
                    new_caps.positional_quantified.push(None);
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
                    let result = self.eval_regex_code_assertion(code, current_caps);
                    let pass = if *negated { !result } else { result };
                    if pass {
                        let mut new_caps = current_caps.clone();
                        let matched_so_far: String =
                            chars[current_caps.match_from..pos].iter().collect();
                        new_caps.code_blocks.push(CodeBlockContext {
                            code: code.clone(),
                            named: current_caps.named.clone(),
                            matched_so_far,
                            positional: current_caps.positional.clone(),
                        });
                        return Some((pos, new_caps));
                    } else {
                        return None;
                    }
                }
                // Plain { code } block — always succeeds, record for side effects
                let mut new_caps = current_caps.clone();
                let matched_so_far: String = chars[current_caps.match_from..pos].iter().collect();
                let ctx = CodeBlockContext {
                    code: code.clone(),
                    named: current_caps.named.clone(),
                    matched_so_far,
                    positional: current_caps.positional.clone(),
                };
                // If eager code block collection is enabled, push immediately
                // so the block is captured even if the overall match fails later.
                super::regex_helpers::EAGER_CODE_BLOCKS.with(|slot| {
                    if let Some(ref mut vec) = *slot.borrow_mut() {
                        vec.push(ctx.clone());
                    }
                });
                new_caps.code_blocks.push(ctx);
                return Some((pos, new_caps));
            }
            RegexAtom::ClosureInterpolation { code } => {
                let target: String = chars.iter().collect();
                let pattern_str =
                    self.eval_regex_closure_interpolation(code, current_caps, &target);
                if let Some(ref pat_str) = pattern_str
                    && Interpreter::contains_dangerous_regex_code(pat_str)
                {
                    super::super::regex_parse::PENDING_REGEX_ERROR.with(|e| {
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
                        new_caps
                            .positional_quantified
                            .extend(inner_caps.positional_quantified.clone());
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
                let ref_text =
                    if let Some(Some(qlist)) = current_caps.positional_quantified.get(*idx) {
                        let mut s = String::new();
                        for (text, _, _, _) in qlist {
                            s.push_str(text);
                        }
                        Some(s)
                    } else {
                        current_caps.positional.get(*idx).cloned()
                    };
                if let Some(ref_text) = ref_text {
                    let ref_chars: Vec<char> = ref_text.chars().collect();
                    if pos + ref_chars.len() <= chars.len()
                        && chars[pos..pos + ref_chars.len()] == ref_chars[..]
                    {
                        return Some((pos + ref_chars.len(), current_caps.clone()));
                    }
                }
                return None;
            }
            RegexAtom::NamedBackref(name) => {
                let ref_text = current_caps
                    .named
                    .get(name.as_str())
                    .and_then(|vals| vals.last())
                    .cloned();
                if let Some(ref_text) = ref_text {
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
                let source = format!("{};", code);
                if let Ok((stmts, _)) = crate::parse_dispatch::parse_source(&source) {
                    let mut interp = Interpreter {
                        env: self.env.clone(),
                        current_package: self.current_package.clone(),
                        ..Default::default()
                    };
                    self.copy_decl_registry_into(&mut interp);
                    let _ = interp.eval_block_value(&stmts);
                    let mut new_caps = current_caps.clone();
                    for (k, v) in &interp.env {
                        if !self.env.contains_key_sym(*k) || self.env.get_sym(*k) != Some(v) {
                            new_caps.regex_vars.insert(k.resolve(), v.clone());
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
            // Symbolic indirect subrule `<::(EXPR)>`: evaluate EXPR to obtain the
            // dynamic rule name, then dispatch as if it were `<NAME>` so that
            // builtin character classes and user-defined tokens both resolve.
            if spec.lookup_name == "::" && spec.arg_exprs.len() == 1 {
                let val = self.eval_regex_expr_value(&spec.arg_exprs[0], current_caps)?;
                let dyn_atom = RegexAtom::Named(val.to_string_value());
                return self
                    .regex_match_atom_all_with_capture_in_pkg(
                        &dyn_atom,
                        chars,
                        pos,
                        current_caps,
                        pkg,
                        ignore_case,
                    )
                    .into_iter()
                    .last();
            }
            let arg_values = if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                self.eval_regex_arg_list(&spec.arg_exprs, current_caps)?
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
                        current_package: sub_pkg.clone(),
                        var_dynamic_flags: self.var_dynamic_flags.clone(),
                        var_type_constraints: self.var_type_constraints.clone(),
                        state_vars: self.state_vars.clone(),
                        ..Default::default()
                    };
                    self.copy_decl_registry_into(&mut interp);
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
                        if spec.capture_name.is_some() && capture_name != spec.lookup_name {
                            new_caps
                                .capture_alias_map
                                .insert(capture_name.to_string(), spec.lookup_name.clone());
                            if let Some(subcaps) = new_caps.named_subcaps.get_mut(capture_name)
                                && let Some(last) = subcaps.last_mut()
                            {
                                last.action_name = Some(spec.lookup_name.clone());
                            }
                        }
                    } else {
                        // No capture name — merge inner captures flat
                        let mut inner_caps = inner_caps;
                        for (k, v) in inner_caps.named.drain() {
                            new_caps.named.entry(k).or_default().extend(v);
                        }
                        new_caps
                            .named_quantified
                            .extend(inner_caps.named_quantified.drain());
                        for v in inner_caps.positional.drain(..) {
                            new_caps.positional.push(v);
                        }
                        new_caps
                            .positional_subcaps
                            .append(&mut inner_caps.positional_subcaps);
                        new_caps
                            .positional_quantified
                            .append(&mut inner_caps.positional_quantified);
                        new_caps.code_blocks.extend(inner_caps.code_blocks);
                    }
                    return Some((end, new_caps));
                }
                return None;
            }
            if spec.lookup_name == "wb" && !spec.token_lookup {
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let after_is_word = pos < chars.len() && is_word_char(chars[pos]);
                let at_boundary = before_is_word != after_is_word
                    || (pos == 0 && after_is_word)
                    || (pos == chars.len() && before_is_word);
                if !at_boundary {
                    return None;
                }
                return Some((pos, current_caps.clone()));
            }
            if spec.lookup_name == "ws" && !spec.token_lookup {
                let mut end = pos;
                while end < chars.len() && chars[end].is_whitespace() {
                    end += 1;
                }
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
            // Fallback: check if lookup_name is a builtin character class
            // (e.g., alpha, digit, etc.). This handles <foo=alpha> aliases
            // where "alpha" is not a user-defined token but a builtin.
            let is_builtin_class = matches!(
                spec.lookup_name.as_str(),
                "alpha"
                    | "upper"
                    | "lower"
                    | "digit"
                    | "xdigit"
                    | "space"
                    | "alnum"
                    | "blank"
                    | "cntrl"
                    | "punct"
                    | "graph"
                    | "print"
            );
            if is_builtin_class
                && (pos >= chars.len() || !matches_named_builtin(&spec.lookup_name, chars[pos]))
            {
                return None; // builtin class doesn't match here — no error, just no match
            }
            if is_builtin_class
                && pos < chars.len()
                && matches_named_builtin(&spec.lookup_name, chars[pos])
            {
                let end = pos + 1;
                let mut new_caps = current_caps.clone();
                let captured: String = chars[pos..end].iter().collect();
                let capture_name = spec
                    .capture_name
                    .as_deref()
                    .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
                if let Some(capture_name) = capture_name {
                    let subcap = RegexCaptures {
                        matched: captured.clone(),
                        from: pos,
                        to: end,
                        ..Default::default()
                    };
                    new_caps
                        .named_subcaps
                        .entry(capture_name.to_string())
                        .or_default()
                        .push(subcap);
                    new_caps
                        .named
                        .entry(capture_name.to_string())
                        .or_default()
                        .push(captured.clone());
                    // For <foo=alpha>, also capture under the original name.
                    // For <foo=.alpha>, the dot suppresses the original name.
                    if spec.capture_name.is_some()
                        && capture_name != spec.lookup_name
                        && !spec.alias_replaces_original
                    {
                        new_caps
                            .capture_alias_map
                            .insert(capture_name.to_string(), spec.lookup_name.clone());
                        let subcap2 = RegexCaptures {
                            matched: captured.clone(),
                            from: pos,
                            to: end,
                            ..Default::default()
                        };
                        new_caps
                            .named_subcaps
                            .entry(spec.lookup_name.to_string())
                            .or_default()
                            .push(subcap2);
                        new_caps
                            .named
                            .entry(spec.lookup_name.to_string())
                            .or_default()
                            .push(captured);
                    }
                }
                return Some((end, new_caps));
            }
            // Fallback: check if lookup_name is a Unicode property assertion
            // (e.g., :Letter, :!Letter, -:Letter). Handles <foo=:Letter> aliases.
            let (uni_prop, uni_negated) = if let Some(prop) = spec.lookup_name.strip_prefix(":!") {
                (Some(prop), true)
            } else if let Some(prop) = spec.lookup_name.strip_prefix("-:") {
                (Some(prop), true)
            } else if let Some(prop) = spec.lookup_name.strip_prefix(':') {
                (Some(prop), false)
            } else {
                (None, false)
            };
            if let Some(prop_name) = uni_prop {
                if pos >= chars.len() {
                    return None;
                }
                let matches = check_unicode_property(prop_name, chars[pos]);
                let matches = if uni_negated { !matches } else { matches };
                if !matches {
                    return None;
                }
                let end = pos + 1;
                let mut new_caps = current_caps.clone();
                let captured: String = chars[pos..end].iter().collect();
                let capture_name = spec
                    .capture_name
                    .as_deref()
                    .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
                if let Some(capture_name) = capture_name {
                    let subcap = RegexCaptures {
                        matched: captured.clone(),
                        from: pos,
                        to: end,
                        ..Default::default()
                    };
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
                }
                return Some((end, new_caps));
            }
            // Named rule not found — report error for valid identifier names.
            // Skip error for names containing special chars (likely parser
            // artifacts from character class syntax like `<[...]>`).
            //
            // When the first character after the identifier is whitespace, the
            // remainder is a regex argument (`<test hat>`), so the method name is
            // just the leading identifier. Validate and report against that name.
            let method_name = spec
                .lookup_name
                .split_whitespace()
                .next()
                .unwrap_or("")
                .to_string();
            let is_plain_ident = !method_name.is_empty()
                && method_name
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == ':' || c == '.');
            // If the leading identifier resolves to a known rule/token (e.g. the
            // space form `<lit 'a'>` passes `'a'` as an argument to the defined
            // `lit` token), it is not an unknown-method error — just a subrule
            // call form we do not fully support yet, so fall through.
            let leading_resolves = is_plain_ident
                && !self
                    .resolve_token_patterns_static_in_pkg(&method_name, pkg)
                    .is_empty();
            if !spec.silent && is_plain_ident && !leading_resolves {
                super::super::regex_parse::PENDING_REGEX_ERROR.with(|e| {
                    let msg = format!(
                        "No such method '{}' for invocant of type 'Match'",
                        method_name
                    );
                    let mut err = RuntimeError::new(msg.clone());
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg));
                    attrs.insert("method".to_string(), Value::str(method_name.clone()));
                    attrs.insert("typename".to_string(), Value::str("Match".to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::Method::NotFound"), attrs);
                    err.exception = Some(Box::new(ex));
                    *e.borrow_mut() = Some(err);
                });
                return None;
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
}
