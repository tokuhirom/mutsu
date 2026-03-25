use super::super::*;
use crate::symbol::Symbol;
use crate::value::signature::{extract_sig_info, signature_smartmatch};

impl Interpreter {
    /// Extract path and CWD from IO::Path attributes for ACCEPTS comparison.
    fn io_path_attrs_for_accepts(
        attrs: &std::sync::Arc<crate::value::InstanceAttrs>,
    ) -> (String, String) {
        let path = attrs
            .get("path")
            .map(|v: &Value| v.to_string_value())
            .unwrap_or_default();
        let cwd = attrs
            .get("cwd")
            .map(|v: &Value| v.to_string_value())
            .unwrap_or_else(|| {
                std::env::current_dir()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|_| ".".to_string())
            });
        (path, cwd)
    }

    /// Normalize an IO path: cleanup + absolute.
    fn io_path_cleanup_absolute(path: &str, cwd: &str) -> String {
        let cleaned = Self::cleanup_io_path_lexical(path);
        if std::path::Path::new(&cleaned).is_absolute() {
            cleaned
        } else {
            let abs = std::path::PathBuf::from(cwd).join(&cleaned);
            Self::cleanup_io_path_lexical(abs.to_string_lossy().as_ref())
        }
    }

    pub(in crate::runtime) fn smart_match(&mut self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            // Whatever on RHS always matches (ACCEPTS returns True for any value)
            (_, Value::Whatever) => true,
            (
                Value::Instance {
                    class_name: left_class,
                    attributes: left_attrs,
                    ..
                },
                Value::Instance {
                    class_name: right_class,
                    attributes: right_attrs,
                    ..
                },
            ) if left_class == "DateTime" && right_class == "Date" => {
                let (y, m, d, _, _, _, _) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(left_attrs);
                let (ry, rm, rd) = crate::builtins::methods_0arg::temporal::date_attrs(right_attrs);
                y == ry && m == rm && d == rd
            }
            (Value::Version { .. }, Value::Version { parts, plus, minus }) => {
                Self::version_smart_match(left, parts, *plus, *minus)
            }
            // When RHS is a callable (Sub), invoke it with LHS as argument and
            // return truthiness of the result.  If the sub accepts no parameters,
            // call it with no arguments (simple closure truth).
            (_, Value::Sub(data)) => {
                let func = right.clone();
                let _ = data; // keep pattern match shape explicit for callable RHS
                if let Ok(result) = self.call_sub_value(func.clone(), vec![left.clone()], false) {
                    return result.truthy();
                }
                match self.call_sub_value(func, vec![], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // Junction ~~ Junction type: a Junction IS a Junction, don't autothread
            (Value::Junction { .. }, Value::Package(name))
                if matches!(name.resolve().as_str(), "Junction" | "Mu") =>
            {
                true
            }
            (Value::Junction { kind, values }, _) => match kind {
                crate::value::JunctionKind::Any => {
                    values.iter().any(|v| self.smart_match(v, right))
                }
                crate::value::JunctionKind::All => {
                    values.iter().all(|v| self.smart_match(v, right))
                }
                crate::value::JunctionKind::One => {
                    values.iter().filter(|v| self.smart_match(v, right)).count() == 1
                }
                crate::value::JunctionKind::None => {
                    values.iter().all(|v| !self.smart_match(v, right))
                }
            },
            // Smartmatch against a flip-flop matcher object produced by ff/fff
            // in SmartMatchExpr RHS context.
            (_, Value::Hash(map))
                if matches!(map.get("__mutsu_ff_matcher"), Some(Value::Bool(true))) =>
            {
                let key = map
                    .get("key")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "__mutsu_ff_matcher::default".to_string());
                let lhs_pat = map.get("lhs").cloned().unwrap_or(Value::Nil);
                let rhs_pat = map.get("rhs").cloned().unwrap_or(Value::Nil);
                let exclude_start = matches!(map.get("exclude_start"), Some(Value::Bool(true)));
                let exclude_end = matches!(map.get("exclude_end"), Some(Value::Bool(true)));
                let is_fff = matches!(map.get("is_fff"), Some(Value::Bool(true)));

                let seq = self
                    .get_state_var(&key)
                    .and_then(|v| match v {
                        Value::Int(i) if *i > 0 => Some(*i),
                        _ => None,
                    })
                    .unwrap_or(0);

                let (lhs_hit, rhs_hit) = if seq > 0 {
                    (false, self.smart_match(left, &rhs_pat))
                } else {
                    let lhs_match = self.smart_match(left, &lhs_pat);
                    if !lhs_match {
                        (false, false)
                    } else if is_fff {
                        (true, false)
                    } else {
                        (true, self.smart_match(left, &rhs_pat))
                    }
                };

                let out = if seq > 0 {
                    let current = seq;
                    if rhs_hit {
                        self.set_state_var(key.clone(), Value::Int(0));
                        if exclude_end {
                            Value::Nil
                        } else {
                            Value::Int(current)
                        }
                    } else {
                        self.set_state_var(key.clone(), Value::Int(current + 1));
                        Value::Int(current)
                    }
                } else if lhs_hit {
                    if !is_fff && rhs_hit {
                        self.set_state_var(key.clone(), Value::Int(0));
                        if exclude_start || exclude_end {
                            Value::Nil
                        } else {
                            Value::Int(1)
                        }
                    } else {
                        self.set_state_var(key.clone(), Value::Int(2));
                        if exclude_start {
                            Value::Nil
                        } else {
                            Value::Int(1)
                        }
                    }
                } else {
                    Value::Nil
                };
                out.truthy()
            }
            // Named regex/token used as smartmatch RHS -- perform regex match
            (
                _,
                Value::Routine {
                    is_regex: true,
                    name,
                    package,
                },
            ) => {
                // Look up the token def and extract the regex pattern from its body
                let qualified = format!("{}::{}", package, name);
                if let Some(pat) = self
                    .extract_token_regex_pattern(&qualified)
                    .or_else(|| self.extract_token_regex_pattern(&name.resolve()))
                {
                    let text = left.to_string_value();
                    if let Some(captures) = self.regex_match_with_captures(&pat, &text) {
                        let match_obj = Value::make_match_object_with_captures(
                            captures.matched.clone(),
                            captures.from as i64,
                            captures.to as i64,
                            &captures.positional,
                            &captures.named,
                        );
                        self.env.insert("/".to_string(), match_obj);
                        for (i, v) in captures.positional.iter().enumerate() {
                            self.env.insert(i.to_string(), Value::str(v.clone()));
                        }
                        for (k, v) in &captures.named {
                            let value = if v.len() == 1 {
                                Value::str(v[0].clone())
                            } else {
                                Value::array(v.iter().cloned().map(Value::str).collect())
                            };
                            self.env.insert(format!("<{}>", k), value);
                        }
                        return true;
                    }
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
                // Fallback: call as sub
                let func = right.clone();
                match self.call_sub_value(func, vec![left.clone()], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // Built-in routines used as callables in smartmatch
            (_, Value::Routine { .. }) => {
                let func = right.clone();
                match self.call_sub_value(func, vec![left.clone()], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // :nth(...) and ordinal forms (:1st, :2nd, :3rd, :4th, ...)
            (
                _,
                Value::RegexWithAdverbs {
                    pattern,
                    nth: Some(raw_nth),
                    perl5,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pattern = if *perl5 {
                    self.interpolate_regex_pattern(pattern)
                } else {
                    pattern.to_string()
                };
                let all = if *perl5 {
                    #[cfg(feature = "pcre2")]
                    {
                        self.regex_match_all_with_captures_p5(&pattern, &text)
                    }
                    #[cfg(not(feature = "pcre2"))]
                    {
                        self.regex_match_all_with_captures(&pattern, &text)
                    }
                } else {
                    self.regex_match_all_with_captures(&pattern, &text)
                };
                let non_overlapping = self.select_non_overlapping_matches(all);
                let resolved = match self.resolve_nth_indices(raw_nth, non_overlapping.len()) {
                    Ok(v) => v,
                    Err(message) => {
                        Self::set_pending_nth_error(message);
                        self.env.insert("/".to_string(), Value::Nil);
                        return false;
                    }
                };
                if resolved.is_empty() {
                    Self::set_pending_nth_error("Invalid :nth index".to_string());
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
                let mut selected = Vec::new();
                for idx in resolved {
                    if idx == 0 || idx > non_overlapping.len() {
                        self.env.insert("/".to_string(), Value::Nil);
                        return false;
                    }
                    selected.push(non_overlapping[idx - 1].clone());
                }
                if selected.len() == 1 {
                    self.apply_single_regex_captures(&selected[0]);
                } else {
                    self.apply_multi_regex_captures(&selected);
                }
                true
            }
            // :c/:continue -- search from $/.to (or 0) onwards (non-anchored)
            (
                _,
                Value::RegexWithAdverbs {
                    pattern: pat,
                    continue_: true,
                    global: false,
                    exhaustive: false,
                    overlap: false,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pat = pat.to_string();
                let start_pos = self.get_match_to_position();
                if let Some(captures) = self.regex_match_with_captures_from(&pat, &text, start_pos)
                {
                    self.apply_single_regex_captures(&captures);
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            // :pos/:p anchored match (non-exhaustive/non-global) -- match at $/.to (or 0)
            (
                _,
                Value::RegexWithAdverbs {
                    pattern: pat,
                    pos: true,
                    global: false,
                    exhaustive: false,
                    overlap: false,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pat = pat.to_string();
                let start_pos = self.get_match_to_position();
                if let Some(captures) = self.regex_match_with_captures_at(&pat, &text, start_pos) {
                    self.apply_single_regex_captures(&captures);
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            // :x(N) without :g/:ex/:ov -- require at least N non-overlapping matches,
            // return first N in $/.
            (
                _,
                Value::RegexWithAdverbs {
                    pattern,
                    global: false,
                    exhaustive: false,
                    overlap: false,
                    repeat: Some(needed),
                    perl5,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pattern = if *perl5 {
                    self.interpolate_regex_pattern(pattern)
                } else {
                    pattern.to_string()
                };
                let all = if *perl5 {
                    #[cfg(feature = "pcre2")]
                    {
                        self.regex_match_all_with_captures_p5(&pattern, &text)
                    }
                    #[cfg(not(feature = "pcre2"))]
                    {
                        self.regex_match_all_with_captures(&pattern, &text)
                    }
                } else {
                    self.regex_match_all_with_captures(&pattern, &text)
                };
                let non_overlapping = self.select_non_overlapping_matches(all);
                let Some(selected) =
                    Self::select_matches_by_repeat_bounds(non_overlapping, *needed, Some(*needed))
                else {
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                };
                self.apply_multi_regex_captures(&selected);
                true
            }
            // :g (global) -- find all non-overlapping matches
            (
                _,
                Value::RegexWithAdverbs {
                    pattern,
                    global: true,
                    overlap: false,
                    exhaustive: false,
                    repeat,
                    perl5,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pattern = if *perl5 {
                    self.interpolate_regex_pattern(pattern)
                } else {
                    pattern.to_string()
                };
                let all = if *perl5 {
                    #[cfg(feature = "pcre2")]
                    {
                        self.regex_match_all_with_captures_p5(&pattern, &text)
                    }
                    #[cfg(not(feature = "pcre2"))]
                    {
                        self.regex_match_all_with_captures(&pattern, &text)
                    }
                } else {
                    self.regex_match_all_with_captures(&pattern, &text)
                };
                // Filter to non-overlapping: take longest match at each position,
                // then skip matches that overlap with already-selected ones
                let non_overlapping = self.select_non_overlapping_matches(all);
                if non_overlapping.is_empty() {
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
                let selected = if let Some(needed) = *repeat {
                    if non_overlapping.len() < needed {
                        self.env.insert("/".to_string(), Value::Nil);
                        return false;
                    }
                    non_overlapping.into_iter().take(needed).collect::<Vec<_>>()
                } else {
                    non_overlapping
                };
                self.apply_multi_regex_captures(&selected);
                true
            }
            // :ov (overlap) -- find longest match at each starting position
            (
                _,
                Value::RegexWithAdverbs {
                    pattern,
                    overlap: true,
                    perl5,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pattern = if *perl5 {
                    self.interpolate_regex_pattern(pattern)
                } else {
                    pattern.to_string()
                };
                let all = if *perl5 {
                    #[cfg(feature = "pcre2")]
                    {
                        self.regex_match_all_with_captures_p5(&pattern, &text)
                    }
                    #[cfg(not(feature = "pcre2"))]
                    {
                        self.regex_match_all_with_captures(&pattern, &text)
                    }
                } else {
                    self.regex_match_all_with_captures(&pattern, &text)
                };
                if all.is_empty() {
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
                // Keep longest match at each starting position
                let mut best_by_start: std::collections::BTreeMap<usize, RegexCaptures> =
                    std::collections::BTreeMap::new();
                for capture in all {
                    let key = capture.from;
                    match best_by_start.get(&key) {
                        Some(existing) if capture.to <= existing.to => {}
                        _ => {
                            best_by_start.insert(key, capture);
                        }
                    }
                }
                let selected: Vec<_> = best_by_start.into_values().collect();
                self.apply_multi_regex_captures(&selected);
                true
            }
            // :ex (exhaustive) -- find ALL possible matches
            (
                _,
                Value::RegexWithAdverbs {
                    pattern,
                    exhaustive: true,
                    repeat,
                    perl5,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pattern = if *perl5 {
                    self.interpolate_regex_pattern(pattern)
                } else {
                    pattern.to_string()
                };
                let mut all = if *perl5 {
                    #[cfg(feature = "pcre2")]
                    {
                        self.regex_match_all_with_captures_p5(&pattern, &text)
                    }
                    #[cfg(not(feature = "pcre2"))]
                    {
                        self.regex_match_all_with_captures(&pattern, &text)
                    }
                } else {
                    self.regex_match_all_with_captures(&pattern, &text)
                };
                if all.is_empty() {
                    self.env.insert("/".to_string(), Value::Nil);
                    return false;
                }
                let selected = if let Some(needed) = *repeat {
                    let earliest = all.iter().map(|c| c.from).min().unwrap_or(0);
                    all.retain(|c| c.from == earliest);
                    if all.len() < needed {
                        self.env.insert("/".to_string(), Value::Nil);
                        return false;
                    }
                    all.into_iter().take(needed).collect::<Vec<_>>()
                } else {
                    all
                };
                self.apply_multi_regex_captures(&selected);
                true
            }
            // Array/List ~~ Regex: iterate elements, match each individually
            (
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items),
                Value::Regex(_) | Value::RegexWithAdverbs { .. },
            ) => {
                for item in items.iter() {
                    if self.smart_match(item, right) {
                        return true;
                    }
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            // Hash ~~ Regex: iterate keys, match each individually
            (Value::Hash(map), Value::Regex(_) | Value::RegexWithAdverbs { .. }) => {
                for key in map.keys() {
                    let key_val = Value::str(key.clone());
                    if self.smart_match(&key_val, right) {
                        return true;
                    }
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            // Single match: plain Regex or RegexWithAdverbs without multi-match flags
            (_, Value::Regex(pat))
            | (
                _,
                Value::RegexWithAdverbs {
                    pattern: pat,
                    global: false,
                    exhaustive: false,
                    overlap: false,
                    perl5: false,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pat = pat.to_string();
                if let Some(captures) = self.regex_match_with_captures(&pat, &text) {
                    // Set positional captures as strings first (needed by code blocks)
                    for (i, v) in captures.positional.iter().enumerate() {
                        self.env.insert(i.to_string(), Value::str(v.clone()));
                    }
                    // Clear any previous `made` value before executing code blocks
                    self.env.remove("made");
                    // Execute code blocks from regex for side effects
                    self.execute_regex_code_blocks(&captures.code_blocks);
                    let mut match_obj = Value::make_match_object_full(
                        captures.matched.clone(),
                        captures.from as i64,
                        captures.to as i64,
                        &captures.positional,
                        &captures.named,
                        &captures.named_subcaps,
                        &captures.positional_subcaps,
                        &captures.positional_quantified,
                        Some(&text),
                    );
                    // If the original value is not a Str, store the original value
                    // as the `orig` attribute so .orig preserves the type
                    if !matches!(left, Value::Str(_))
                        && let Value::Instance {
                            ref mut attributes, ..
                        } = match_obj
                    {
                        let attrs = std::sync::Arc::make_mut(attributes);
                        attrs.insert("orig".to_string(), left.clone());
                    }
                    // If `make` was called in a code block, set the ast attribute
                    if let Some(made_val) = self.env.get("made").cloned()
                        && let Value::Instance {
                            ref mut attributes, ..
                        } = match_obj
                    {
                        let attrs = std::sync::Arc::make_mut(attributes);
                        attrs.insert("ast".to_string(), made_val);
                    }
                    // Upgrade positional capture env vars ($0, $1, ...) to Match objects
                    if let Value::Instance { ref attributes, .. } = match_obj
                        && let Some(Value::Array(list, _)) = attributes.get("list")
                    {
                        for (i, v) in list.iter().enumerate() {
                            self.env.insert(i.to_string(), v.clone());
                        }
                    }
                    // Set named capture env vars from the match object's named hash
                    // so subcapture-aware Match objects are used (not plain strings)
                    if let Value::Instance { ref attributes, .. } = match_obj
                        && let Some(Value::Hash(named_hash)) = attributes.get("named")
                    {
                        for (k, v) in named_hash.iter() {
                            self.env.insert(format!("<{}>", k), v.clone());
                        }
                    }
                    self.env.insert("/".to_string(), match_obj);
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            // P5 regex single match
            (
                _,
                Value::RegexWithAdverbs {
                    pattern: pat,
                    global: false,
                    exhaustive: false,
                    overlap: false,
                    perl5: true,
                    ..
                },
            ) => {
                let text = left.to_string_value();
                let pat = self.interpolate_regex_pattern(pat);
                #[cfg(feature = "pcre2")]
                let result = self.regex_match_with_captures_p5(&pat, &text);
                #[cfg(not(feature = "pcre2"))]
                let result = self.regex_match_with_captures(&pat, &text);
                if let Some(captures) = result {
                    self.apply_single_regex_captures(&captures);
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            (_, Value::Junction { kind, values }) => match kind {
                crate::value::JunctionKind::Any => values.iter().any(|v| self.smart_match(left, v)),
                crate::value::JunctionKind::All => values.iter().all(|v| self.smart_match(left, v)),
                crate::value::JunctionKind::One => {
                    values.iter().filter(|v| self.smart_match(left, v)).count() == 1
                }
                crate::value::JunctionKind::None => {
                    values.iter().all(|v| !self.smart_match(left, v))
                }
            },
            // IO::Path/Str ~~ Pair(:e), :d, :f, :r, :w, :x file tests
            // Also handles negated forms: :!e, :!d, :!f, :!r, :!w, :!x, :!s, :!z
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Pair(key, val),
            ) if class_name == "IO::Path"
                && matches!(val.as_ref(), Value::Bool(_))
                && matches!(
                    key.as_str(),
                    "e" | "d" | "f" | "l" | "r" | "w" | "x" | "rw" | "rwx" | "s" | "z"
                ) =>
            {
                let negated = matches!(val.as_ref(), Value::Bool(false));
                let path_str = attributes.get("path").map(|v| v.to_string_value());
                if let Some(p) = path_str {
                    let path = std::path::Path::new(&p);
                    let result = match key.as_str() {
                        "e" => path.exists(),
                        "d" => path.is_dir(),
                        "f" => path.is_file(),
                        "l" => std::fs::symlink_metadata(&p)
                            .map(|m| m.file_type().is_symlink())
                            .unwrap_or(false),
                        "r" => {
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::PermissionsExt;
                                std::fs::metadata(&p)
                                    .map(|m| m.permissions().mode() & 0o444 != 0)
                                    .unwrap_or(false)
                            }
                            #[cfg(not(unix))]
                            {
                                path.exists()
                            }
                        }
                        "w" => {
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::PermissionsExt;
                                std::fs::metadata(&p)
                                    .map(|m| m.permissions().mode() & 0o222 != 0)
                                    .unwrap_or(false)
                            }
                            #[cfg(not(unix))]
                            {
                                path.exists()
                            }
                        }
                        "x" => {
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::PermissionsExt;
                                std::fs::metadata(&p)
                                    .map(|m| m.permissions().mode() & 0o111 != 0)
                                    .unwrap_or(false)
                            }
                            #[cfg(not(unix))]
                            {
                                false
                            }
                        }
                        "rw" => {
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::PermissionsExt;
                                std::fs::metadata(&p)
                                    .map(|m| {
                                        let mode = m.permissions().mode();
                                        mode & 0o444 != 0 && mode & 0o222 != 0
                                    })
                                    .unwrap_or(false)
                            }
                            #[cfg(not(unix))]
                            {
                                std::fs::metadata(&p)
                                    .map(|m| !m.permissions().readonly())
                                    .unwrap_or(false)
                            }
                        }
                        "rwx" => {
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::PermissionsExt;
                                std::fs::metadata(&p)
                                    .map(|m| {
                                        let mode = m.permissions().mode();
                                        mode & 0o444 != 0 && mode & 0o222 != 0 && mode & 0o111 != 0
                                    })
                                    .unwrap_or(false)
                            }
                            #[cfg(not(unix))]
                            {
                                false
                            }
                        }
                        "s" => std::fs::metadata(&p).map(|m| m.len() > 0).unwrap_or(false),
                        "z" => std::fs::metadata(&p).map(|m| m.len() == 0).unwrap_or(false),
                        _ => false,
                    };
                    if negated { !result } else { result }
                } else {
                    negated
                }
            }
            // Hash ~~ Pair: check that key exists in hash and value smartmatches
            (Value::Hash(map), Value::Pair(key, val)) => {
                if let Some(hash_val) = map.get(key.as_str()) {
                    self.smart_match(hash_val, val)
                } else {
                    // Key not in hash: compare against an undefined type object.
                    self.smart_match(&Value::Package(Symbol::intern("Mu")), val)
                }
            }
            // Hash ~~ Hash: structural equality (eqv)
            (Value::Hash(lmap), Value::Hash(rmap)) => {
                if lmap.len() != rmap.len() {
                    return false;
                }
                for (k, lv) in lmap.iter() {
                    match rmap.get(k) {
                        Some(rv) => {
                            if !self.smart_match(lv, rv) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }
                true
            }
            // Set ~~ Mix: all set elements must exist in the Mix with unit weights.
            (Value::Set(set), Value::Mix(mix)) => {
                set.len() == mix.len()
                    && set.iter().all(|key| {
                        mix.get(key)
                            .is_some_and(|weight| weight.is_finite() && *weight == 1.0)
                    })
            }
            // Mix ~~ Set: all mix elements must have unit weights and exist in the set.
            (Value::Mix(mix), Value::Set(set)) => {
                mix.len() == set.len()
                    && mix.iter().all(|(key, weight)| {
                        weight.is_finite() && *weight == 1.0 && set.contains(key)
                    })
            }
            // Array ~~ Hash: check if any element exists as a key in the hash
            (Value::Array(items, ..), Value::Hash(map)) => items.iter().any(|item| {
                let key = item.to_string_value();
                map.contains_key(&key)
            }),
            // Regex ~~ Hash: check if any key matches the regex
            (
                Value::Regex(pat) | Value::RegexWithAdverbs { pattern: pat, .. },
                Value::Hash(map),
            ) => {
                for key in map.keys() {
                    if self.regex_find_first(pat, key).is_some() {
                        return true;
                    }
                }
                false
            }
            // Scalar ~~ Hash: check key existence
            (_, Value::Hash(map)) => {
                let key = left.to_string_value();
                map.contains_key(&key)
            }
            // List/Array ~~ List/Array: element-wise smartmatch with ** support
            (_, r) if Self::is_list_like(r) => {
                // Non-iterable LHS: return False (don't treat scalars as a list)
                if !Self::is_iterable(left) {
                    return false;
                }
                // Lazy LHS or lazy RHS: return False (unless same object, handled by PartialEq)
                if Self::is_lazy(left) || Self::is_lazy(right) {
                    return Self::same_object(left, right);
                }
                let lhs = Self::extract_list_items(left);
                let rhs = Self::extract_list_items(right);
                self.list_smartmatch(&lhs, &rhs)
            }
            // Parametric role smartmatch: R1[C2] ~~ R1[C1] (subtyping)
            (
                Value::ParametricRole {
                    base_name: lhs_base,
                    type_args: lhs_args,
                },
                Value::ParametricRole {
                    base_name: rhs_base,
                    type_args: rhs_args,
                },
            ) => {
                let comparable_lhs_args: Vec<Value> = if lhs_base == rhs_base {
                    lhs_args.to_vec()
                } else if let Some(parent_args) =
                    self.role_parent_args_for(&lhs_base.resolve(), lhs_args, &rhs_base.resolve())
                {
                    parent_args
                } else if self.role_is_subtype(&lhs_base.resolve(), &rhs_base.resolve()) {
                    lhs_args.to_vec()
                } else {
                    return false;
                };
                if comparable_lhs_args.len() != rhs_args.len() {
                    return false;
                }
                for (l_arg, r_arg) in comparable_lhs_args.iter().zip(rhs_args.iter()) {
                    if !self.parametric_arg_subtypes(l_arg, r_arg) {
                        return false;
                    }
                }
                true
            }
            // Parametric role ~~ base role/class: R1[C1] ~~ R1, or R1[T] ~~ ParentClass
            (
                Value::ParametricRole {
                    base_name: lhs_base,
                    type_args: lhs_args,
                },
                Value::Package(rhs_name),
            ) => {
                let rhs_resolved = rhs_name.resolve();
                let lhs_base_resolved = lhs_base.resolve();
                if lhs_base_resolved == rhs_resolved
                    || self
                        .role_parent_args_for(&lhs_base_resolved, lhs_args, &rhs_resolved)
                        .is_some()
                    || self.role_is_subtype(&lhs_base_resolved, &rhs_resolved)
                {
                    return true;
                }
                // Check if the parametric candidate's `is` parents include the RHS class.
                // For role groups, look up the parametric candidate's parents.
                if let Some(candidates) = self.role_candidates.get(&lhs_base_resolved)
                    && let Some(candidate) = candidates.iter().find(|c| !c.type_params.is_empty())
                {
                    let mut stack: Vec<String> = candidate.parents.clone();
                    let mut seen = HashSet::new();
                    while let Some(parent) = stack.pop() {
                        if !seen.insert(parent.clone()) {
                            continue;
                        }
                        if Self::type_matches(&rhs_resolved, &parent) {
                            return true;
                        }
                        // Walk parent's MRO for transitive matching
                        let parent_mro = self.class_mro(&parent);
                        if parent_mro
                            .iter()
                            .any(|p| Self::type_matches(&rhs_resolved, p))
                        {
                            return true;
                        }
                        // Check composed roles of parent class
                        if let Some(composed) = self.class_composed_roles.get(&parent) {
                            for cr in composed {
                                let cr_base =
                                    cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                                if Self::type_matches(&rhs_resolved, cr_base) {
                                    return true;
                                }
                            }
                        }
                    }
                }
                false
            }
            // Value instance/mixin ~~ parametric role: check composed role + type arguments.
            (
                left_value,
                Value::ParametricRole {
                    base_name: rhs_base,
                    type_args: rhs_args,
                },
            ) => {
                // For Package values (type objects), check class hierarchy
                if let Value::Package(_) = left_value {
                    let args_str = rhs_args
                        .iter()
                        .map(|v| match v {
                            Value::Package(n) => n.resolve(),
                            other => other.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    let full_name = format!("{}[{}]", rhs_base, args_str);
                    return self.type_matches_value(&full_name, left_value);
                }
                if !left_value.does_check(&rhs_base.resolve()) {
                    return false;
                }
                let lhs_args = if let Value::Mixin(_, mixins) = left_value {
                    match mixins.get(&format!("__mutsu_role_typeargs__{}", rhs_base)) {
                        Some(Value::Array(items, ..)) => Some(items.as_ref().clone()),
                        _ => None,
                    }
                } else {
                    None
                };
                let Some(lhs_args) = lhs_args else {
                    return false;
                };
                if lhs_args.len() != rhs_args.len() {
                    return false;
                }
                lhs_args
                    .iter()
                    .zip(rhs_args.iter())
                    .all(|(lhs, rhs)| self.parametric_arg_subtypes(lhs, rhs))
            }
            // When RHS is a CustomType, use Raku type checking protocol
            (_, Value::CustomType { id, how, .. }) => self.custom_type_check(left, *id, how),
            // When LHS is a CustomType (type object), check type cache or HOW.type_check
            (Value::CustomType { how, id, .. }, _) => {
                if let Value::Package(_) = right {
                    // After compose: check the type check cache
                    let data = self.custom_type_data.get(id).cloned();
                    if let Some(ref data) = data
                        && let Some(ref cache) = data.type_check_cache
                    {
                        // Check if the RHS type is in our cache
                        for cached_type in cache {
                            if right == cached_type {
                                return true;
                            }
                        }
                        if data.authoritative {
                            return false;
                        }
                    }
                    // Before compose or no cache: call HOW.type_check
                    if let Ok(result) = self.call_method_with_values(
                        *how.clone(),
                        "type_check",
                        vec![Value::Nil, right.clone()],
                    ) {
                        result.truthy()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            // When RHS is a type/Package, check type membership
            (_, Value::Package(type_name)) => {
                // Handle type smileys (:U, :D, :_)
                let type_name_resolved = type_name.resolve();
                let (base_type, smiley) =
                    super::super::types::strip_type_smiley(&type_name_resolved);

                // Enum type object smartmatch against enum values.
                if self.enum_types.contains_key(base_type) {
                    let enum_match =
                        matches!(left, Value::Enum { enum_type, .. } if enum_type == base_type);
                    return match smiley {
                        Some(":U") => false,
                        Some(":D") => enum_match,
                        _ => enum_match,
                    };
                }

                // A Package on the LHS is a type object - check type hierarchy
                // LHS ~~ RHS checks: is LHS a subtype of RHS?
                if let Value::Package(lhs_name) = left {
                    let lhs_resolved = lhs_name.resolve();
                    let mut type_ok = self.type_matches_value(base_type, left);
                    // type_matches treats "Any" as a universal match, but a
                    // user-defined class that explicitly inherits from Mu only
                    // (e.g., `class Foo is Mu {}`) should NOT match Any.
                    // Check the actual MRO to correct this.
                    if type_ok && base_type == "Any" && self.classes.contains_key(&*lhs_resolved) {
                        let mro = self.class_mro(&lhs_resolved);
                        if !mro.iter().any(|c| c == "Any") {
                            type_ok = false;
                        }
                    }
                    if !type_ok {
                        return false;
                    }
                    // Check definedness constraint
                    return match smiley {
                        Some(":U") => true,  // Package is undefined
                        Some(":D") => false, // Package is not defined
                        _ => true,
                    };
                }
                self.type_matches_value(&type_name_resolved, left)
            }
            // Backward-compatibility: enum type objects may still arrive as Str values.
            (_, Value::Str(type_name)) if self.enum_types.contains_key(type_name.as_str()) => {
                matches!(left, Value::Enum { enum_type, .. } if enum_type.resolve() == **type_name)
            }
            // Mu instances smartmatch only the Mu type object (Mu ~~ Mu.new is True).
            (
                Value::Package(lhs_type),
                Value::Instance {
                    class_name: rhs_class,
                    ..
                },
            ) if rhs_class == "Mu" => lhs_type == "Mu",
            // When LHS is a type object (Package), only match same type or type hierarchy
            (Value::Package(_), _) => false,
            // When RHS is NaN, check if LHS is also NaN
            (_, Value::Num(b)) if b.is_nan() => Self::value_is_nan(left),
            (Value::Num(a), _) if a.is_nan() => Self::value_is_nan(right),
            // Complex comparison (NaN-aware: any NaN component means NaN smartmatch)
            (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
                let a_nan = ar.is_nan() || ai.is_nan();
                let b_nan = br.is_nan() || bi.is_nan();
                if a_nan && b_nan {
                    true
                } else if a_nan || b_nan {
                    false
                } else {
                    ar == br && ai == bi
                }
            }
            (Value::Int(a), Value::Complex(br, bi)) => (*a as f64) == *br && *bi == 0.0,
            (Value::Complex(ar, ai), Value::Int(b)) => *ar == (*b as f64) && *ai == 0.0,
            (Value::Num(a), Value::Complex(br, bi)) => {
                if a.is_nan() && (br.is_nan() || bi.is_nan()) {
                    true
                } else {
                    *a == *br && *bi == 0.0
                }
            }
            (Value::Complex(ar, ai), Value::Num(b)) => {
                if b.is_nan() && (ar.is_nan() || ai.is_nan()) {
                    true
                } else {
                    *ar == *b && *ai == 0.0
                }
            }
            (Value::Complex(ar, ai), Value::Rat(n, d)) => {
                if *d != 0 {
                    *ar == (*n as f64 / *d as f64) && *ai == 0.0
                } else {
                    false
                }
            }
            (Value::Rat(n, d), Value::Complex(br, bi)) => {
                if *d != 0 {
                    (*n as f64 / *d as f64) == *br && *bi == 0.0
                } else {
                    false
                }
            }
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Int(a), Value::Num(b)) => (*a as f64) == *b,
            (Value::Num(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Rat(an, ad), Value::Rat(bn, bd)) => an * bd == bn * ad,
            (Value::Int(a), Value::Rat(n, d)) => *a * d == *n,
            (Value::Rat(n, d), Value::Int(b)) => *n == *b * d,
            (Value::Str(a), Value::Str(b)) => a == b,
            // Str ~~ Numeric: numify LHS and compare
            (Value::Str(a), Value::Int(b)) => a.trim().parse::<f64>() == Ok(*b as f64),
            (Value::Str(a), Value::Num(b)) => {
                let trimmed = a.trim().replace('\u{2212}', "-");
                trimmed.parse::<f64>().is_ok_and(|v| {
                    if v.is_nan() && b.is_nan() {
                        true
                    } else {
                        v == *b
                    }
                })
            }
            (Value::Str(a), Value::Rat(n, d)) => {
                if *d != 0 {
                    a.trim()
                        .parse::<f64>()
                        .is_ok_and(|v| v == *n as f64 / *d as f64)
                } else {
                    false
                }
            }
            (Value::Int(a), Value::Str(b)) => b.trim().parse::<f64>() == Ok(*a as f64),
            (Value::Nil, Value::Str(s)) => s.is_empty(),
            // IO::Path ~~ IO::Path: compare by cleanup.absolute
            (
                Value::Instance {
                    class_name: cn_a,
                    attributes: attrs_a,
                    ..
                },
                Value::Instance {
                    class_name: cn_b,
                    attributes: attrs_b,
                    ..
                },
            ) if cn_a == "IO::Path" && cn_b == "IO::Path" => {
                let (path_a, cwd_a) = Self::io_path_attrs_for_accepts(attrs_a);
                let (path_b, cwd_b) = Self::io_path_attrs_for_accepts(attrs_b);
                Self::io_path_cleanup_absolute(&path_a, &cwd_a)
                    == Self::io_path_cleanup_absolute(&path_b, &cwd_b)
            }
            // Cool ~~ IO::Path: convert LHS to string, compare by cleanup.absolute
            (
                _,
                Value::Instance {
                    class_name: cn_b,
                    attributes: attrs_b,
                    ..
                },
            ) if cn_b == "IO::Path"
                && !matches!(
                    left,
                    Value::Junction { .. }
                        | Value::Sub(_)
                        | Value::Regex(_)
                        | Value::RegexWithAdverbs { .. }
                        | Value::Routine { .. }
                        | Value::Package(_)
                        | Value::CustomType { .. }
                        | Value::ParametricRole { .. }
                ) =>
            {
                let lhs_str = left.to_string_value();
                let (path_b, cwd_b) = Self::io_path_attrs_for_accepts(attrs_b);
                let cwd = std::env::current_dir()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|_| ".".to_string());
                Self::io_path_cleanup_absolute(&lhs_str, &cwd)
                    == Self::io_path_cleanup_absolute(&path_b, &cwd_b)
            }
            // Signature ~~ Signature: s1 ACCEPTS s2
            (
                Value::Instance {
                    class_name: cn_a,
                    id: id_a,
                    ..
                },
                Value::Instance {
                    class_name: cn_b,
                    id: id_b,
                    ..
                },
            ) if cn_a == "Signature" && cn_b == "Signature" => {
                if let (Some(info_a), Some(info_b)) =
                    (extract_sig_info(left), extract_sig_info(right))
                {
                    signature_smartmatch(&info_b, &info_a)
                } else {
                    id_a == id_b
                }
            }
            // Value ~~ Signature: signature ACCEPTS value
            (_, Value::Instance { class_name: cn, .. }) if cn == "Signature" => {
                if let Some(info) = extract_sig_info(right) {
                    self.signature_accepts_value(left, &info)
                } else {
                    false
                }
            }
            // Buf/Blob ~~ Buf/Blob: compare byte contents
            (
                Value::Instance {
                    class_name: cn_a, ..
                },
                Value::Instance {
                    class_name: cn_b, ..
                },
            ) if {
                let a = cn_a.resolve();
                let b = cn_b.resolve();
                let is_buf = |cn: &str| {
                    cn == "Buf"
                        || cn == "Blob"
                        || cn == "utf8"
                        || cn == "utf16"
                        || cn.starts_with("Buf[")
                        || cn.starts_with("Blob[")
                        || cn.starts_with("buf")
                        || cn.starts_with("blob")
                };
                is_buf(&a) && is_buf(&b)
            } =>
            {
                let lb = crate::vm::VM::extract_buf_bytes(left);
                let rb = crate::vm::VM::extract_buf_bytes(right);
                lb == rb
            }
            // Instance ~~ Instance: value types (Date, DateTime) use eqv,
            // other classes use identity comparison
            (
                Value::Instance {
                    class_name: lc,
                    id: id_a,
                    ..
                },
                Value::Instance {
                    class_name: rc,
                    id: id_b,
                    ..
                },
            ) => {
                if lc == rc
                    && matches!(
                        lc.resolve().as_ref(),
                        "Date" | "DateTime" | "Duration" | "Instant"
                    )
                {
                    left.eqv(right)
                } else {
                    id_a == id_b
                }
            }
            // When RHS is a Bool, result is that Bool
            (_, Value::Bool(b)) => *b,
            // X::AdHoc smartmatch: delegate to payload
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                _,
            ) if class_name == "X::AdHoc" => {
                if let Some(payload) = attributes.get("payload") {
                    self.smart_match(payload, right)
                } else {
                    false
                }
            }
            // Instance ~~ Type or other: identity check (false)
            (Value::Instance { .. }, _) | (_, Value::Instance { .. }) => false,
            // Range ~~ Range: LHS is subset of RHS.
            // Uses raw bound values. Exclusivity is compared pairwise:
            // if RHS excludes a bound, LHS must either also exclude it
            // or have a strictly interior value.
            (l, r) if l.is_range() && r.is_range() => {
                let r_str = Self::range_has_string_endpoints(r);
                let (_, _, l_es, l_ee) = Self::range_exclusivity(l);
                let (_, _, r_es, r_ee) = Self::range_exclusivity(r);
                if r_str {
                    let (l_min_s, l_max_s) = Self::range_raw_string_bounds(l);
                    let (r_min_s, r_max_s) = Self::range_raw_string_bounds(r);
                    let min_ok = if r_es {
                        l_min_s > r_min_s || (l_min_s == r_min_s && l_es)
                    } else {
                        l_min_s >= r_min_s
                    };
                    let max_ok = if r_ee {
                        l_max_s < r_max_s || (l_max_s == r_max_s && l_ee)
                    } else {
                        l_max_s <= r_max_s
                    };
                    min_ok && max_ok
                } else {
                    let (l_min, l_max) = Self::range_raw_bounds_f64(l);
                    let (r_min, r_max) = Self::range_raw_bounds_f64(r);
                    let min_ok = if r_es {
                        l_min > r_min || (l_min == r_min && l_es)
                    } else {
                        l_min >= r_min
                            || (l_min.is_nan() && r_min.is_nan())
                            || (l_min.is_nan() && r_min.is_infinite() && r_min < 0.0)
                    };
                    let max_ok = if r_ee {
                        l_max < r_max || (l_max == r_max && l_ee)
                    } else {
                        l_max <= r_max
                            || (l_max.is_nan() && r_max.is_nan())
                            || (l_max.is_nan() && r_max.is_infinite() && r_max > 0.0)
                    };
                    min_ok && max_ok
                }
            }
            // Range ~~ Numeric: numify range (element count) and compare with ==
            (l, r) if l.is_range() && r.is_numeric() => {
                let elems = Self::range_elems_f64(l);
                let rval = r.to_f64();
                elems == rval
            }
            // Value ~~ Range: check if value is contained in the range
            (l, r) if r.is_range() => Self::value_in_range(l, r),
            // Default: compare equality
            _ => left.to_string_value() == right.to_string_value(),
        }
    }

    /// Check if a value is list-like (Array, Seq, Slip).
    fn is_list_like(v: &Value) -> bool {
        matches!(
            v,
            Value::Array(..) | Value::Seq(_) | Value::Slip(_) | Value::LazyList(_)
        )
    }

    /// Check if a value is iterable (can be treated as a list in smartmatch).
    fn is_iterable(v: &Value) -> bool {
        matches!(
            v,
            Value::Array(..) | Value::Seq(_) | Value::Slip(_) | Value::LazyList(_)
        ) || v.is_range()
    }

    /// Check if a value is lazy.
    fn is_lazy(v: &Value) -> bool {
        matches!(v, Value::LazyList(_))
    }

    /// Check if two values are the same object (pointer equality).
    fn same_object(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Array(a, _), Value::Array(b, _)) => Arc::ptr_eq(a, b),
            (Value::Seq(a), Value::Seq(b)) => Arc::ptr_eq(a, b),
            (Value::Slip(a), Value::Slip(b)) => Arc::ptr_eq(a, b),
            (Value::LazyList(a), Value::LazyList(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }

    /// Extract list items from a value, expanding ranges.
    fn extract_list_items(v: &Value) -> Vec<Value> {
        if let Some(items) = v.as_list_items() {
            items.as_ref().clone()
        } else if v.is_range() {
            Self::value_to_list(v)
        } else {
            vec![v.clone()]
        }
    }

    /// Perform list smartmatch with ** (HyperWhatever) support.
    /// Each RHS element is smartmatched against the corresponding LHS element.
    /// ** matches 0 or more elements. Consecutive **s are collapsed.
    fn list_smartmatch(&mut self, lhs: &[Value], rhs: &[Value]) -> bool {
        // Collapse consecutive HyperWhatevers in rhs
        let rhs_collapsed: Vec<&Value> = {
            let mut result = Vec::new();
            let mut prev_was_hw = false;
            for v in rhs {
                if matches!(v, Value::HyperWhatever) {
                    if !prev_was_hw {
                        result.push(v);
                    }
                    prev_was_hw = true;
                } else {
                    prev_was_hw = false;
                    result.push(v);
                }
            }
            result
        };
        self.list_smartmatch_recursive(lhs, 0, &rhs_collapsed, 0)
    }

    fn list_smartmatch_recursive(
        &mut self,
        lhs: &[Value],
        li: usize,
        rhs: &[&Value],
        ri: usize,
    ) -> bool {
        // Both exhausted -- match
        if li == lhs.len() && ri == rhs.len() {
            return true;
        }
        // RHS exhausted but LHS has more -- no match
        if ri == rhs.len() {
            return false;
        }
        // RHS has ** -- try matching 0..n elements from LHS
        if matches!(rhs[ri], Value::HyperWhatever) {
            // Try consuming 0, 1, 2, ... elements from LHS
            for skip in 0..=(lhs.len() - li) {
                if self.list_smartmatch_recursive(lhs, li + skip, rhs, ri + 1) {
                    return true;
                }
            }
            return false;
        }
        // LHS exhausted but RHS has more non-** elements -- no match
        if li == lhs.len() {
            return false;
        }
        // Match current element using smartmatch
        if self.element_smartmatch(&lhs[li], rhs[ri]) {
            self.list_smartmatch_recursive(lhs, li + 1, rhs, ri + 1)
        } else {
            false
        }
    }

    /// Smartmatch a single element: delegate to full smartmatch.
    fn element_smartmatch(&mut self, lhs_elem: &Value, rhs_elem: &Value) -> bool {
        self.smart_match(lhs_elem, rhs_elem)
    }
}
