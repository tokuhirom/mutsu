use super::vm_string_regex_ops::*;
use super::*;

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_idx: Option<u32>,
        perl5: bool,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let raw_replacement = normalize_subst_replacement(Self::const_str(code, replacement_idx));
        // A replacement containing a `{...}` code block (e.g. from `s[...] = $0 x 2`)
        // must be evaluated *per match*, with `$/`, `$0`, ... bound to that match.
        // For replacements without code blocks, interpolate once up front.
        let has_code_block = raw_replacement.contains('{');
        let replacement = if has_code_block {
            raw_replacement.clone()
        } else {
            self.interpolate_subst_replacement_with_closures(&raw_replacement)
        };

        let nth_spec = nth_idx.map(|idx| Self::const_str(code, idx).to_string());
        let x_spec = x_idx.map(|idx| Self::const_str(code, idx).to_string());
        let target = self.env().get("_").cloned().unwrap_or(Value::Nil);
        let text = target.to_string_value();

        if nth_spec.is_none() && x_spec.is_none() && !global {
            if perl5 {
                let found = self.regex_find_first_p5(&pattern, &text);
                if let Some((start, end)) = found {
                    let out = Self::apply_substitutions(
                        &text,
                        &[(start, end)],
                        &replacement,
                        samecase,
                        sigspace,
                        samemark,
                        samespace,
                    );
                    let result = Value::str(out);
                    self.write_subst_topic_checked(code, result)?;
                    // Create Match object and set $/
                    let match_obj = Self::make_subst_match(&text, start, end);
                    self.env_mut().insert("/".to_string(), match_obj.clone());
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(match_obj);
                } else {
                    self.env_mut().insert("/".to_string(), Value::Nil);
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(Value::Bool(false));
                }
            } else {
                let found = loan_env!(
                    self,
                    regex_find_first_from_with_captures(&pattern, &text, 0)
                );
                if let Some((start, end, captures)) = found {
                    let out = if has_code_block {
                        self.apply_substitutions_dynamic(
                            &text,
                            &[(start, end)],
                            &raw_replacement,
                            std::slice::from_ref(&captures),
                            samecase,
                            sigspace,
                            samemark,
                            samespace,
                        )
                    } else {
                        // Expand $0, $1, ... in the replacement with captured groups
                        let expanded = expand_capture_refs(&replacement, &captures);
                        Self::apply_substitutions(
                            &text,
                            &[(start, end)],
                            &expanded,
                            samecase,
                            sigspace,
                            samemark,
                            samespace,
                        )
                    };
                    let result = Value::str(out);
                    self.write_subst_topic_checked(code, result)?;
                    // Create Match object and set $/
                    let match_obj = Self::make_subst_match(&text, start, end);
                    self.env_mut().insert("/".to_string(), match_obj.clone());
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(match_obj);
                } else {
                    self.env_mut().insert("/".to_string(), Value::Nil);
                    self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                    self.stack.push(Value::Bool(false));
                }
            }
            return Ok(());
        }

        // For Raku regex with capture references, collect per-match captures
        // so each match can expand $0, $1 etc. independently.
        let (ranges, per_match_captures) = if perl5 {
            let all_matches = loan_env!(self, regex_find_all_p5(&pattern, &text));
            let selected = if global && nth_spec.is_none() && x_spec.is_none() {
                all_matches
            } else {
                Self::select_substitution_ranges(
                    &all_matches,
                    nth_spec.as_deref(),
                    x_spec.as_deref(),
                )?
            };
            (selected, Vec::new())
        } else {
            // Find all matches with captures using iterative find_first_from
            let mut matches_with_caps: Vec<(usize, usize, Vec<String>)> = Vec::new();
            let mut pos = 0;
            while let Some((start, end, caps)) = loan_env!(
                self,
                regex_find_first_from_with_captures(&pattern, &text, pos)
            ) {
                matches_with_caps.push((start, end, caps));
                pos = if end > start { end } else { start + 1 };
            }
            let all_ranges: Vec<(usize, usize)> =
                matches_with_caps.iter().map(|(s, e, _)| (*s, *e)).collect();
            let selected = if global && nth_spec.is_none() && x_spec.is_none() {
                all_ranges
            } else {
                Self::select_substitution_ranges(
                    &all_ranges,
                    nth_spec.as_deref(),
                    x_spec.as_deref(),
                )?
            };
            // Extract captures for selected ranges
            let captures: Vec<Vec<String>> = selected
                .iter()
                .filter_map(|r| {
                    matches_with_caps
                        .iter()
                        .find(|(s, e, _)| *s == r.0 && *e == r.1)
                        .map(|(_, _, c)| c.clone())
                })
                .collect();
            (selected, captures)
        };
        // When :g, :x, or a multi-value :nth is used, the substitution result
        // (and $/) is a List of Match objects rather than a single Match. A bare
        // substitution yields a single Match. A *single* :nth(N) forces a single
        // Match even when combined with :g (e.g. `s:2nd:g/./Z/` yields a Match).
        let single_nth = nth_spec.as_deref().is_some_and(|s| !s.contains(','));
        let nth_is_multi = nth_spec.as_deref().is_some_and(|s| s.contains(','));
        let result_is_list = !single_nth && (global || x_spec.is_some() || nth_is_multi);
        if ranges.is_empty() {
            if result_is_list {
                // :g / :x with no match: result is an empty List (falsy).
                let empty = Value::array(Vec::new());
                self.env_mut().insert("/".to_string(), empty.clone());
                self.substitution_in_smartmatch = self.in_smartmatch_rhs;
                self.stack.push(empty);
                return Ok(());
            }
            self.env_mut().insert("/".to_string(), Value::Nil);
            self.substitution_in_smartmatch = self.in_smartmatch_rhs;
            self.stack.push(Value::Bool(false));
            return Ok(());
        }

        let out = if has_code_block {
            // Replacement has `{...}` code block(s): interpolate per match.
            self.apply_substitutions_dynamic(
                &text,
                &ranges,
                &raw_replacement,
                &per_match_captures,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        } else if !per_match_captures.is_empty() {
            // Build output with per-match capture expansion
            Self::apply_substitutions_with_captures(
                &text,
                &ranges,
                &replacement,
                &per_match_captures,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        } else {
            Self::apply_substitutions(
                &text,
                &ranges,
                &replacement,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        };
        let result = Value::str(out);
        self.write_subst_topic_checked(code, result)?;
        // For :g / :x, $/ and the substitution result are a List of Match
        // objects; otherwise a single Match for the first (and only) range.
        if result_is_list {
            let matches: Vec<Value> = ranges
                .iter()
                .map(|(s, e)| Self::make_subst_match(&text, *s, *e))
                .collect();
            let list = Value::array(matches);
            self.env_mut().insert("/".to_string(), list.clone());
            self.substitution_in_smartmatch = self.in_smartmatch_rhs;
            self.stack.push(list);
            return Ok(());
        }
        // Create Match object from first match range and set $/
        let (first_start, first_end) = ranges[0];
        let match_obj = Self::make_subst_match(&text, first_start, first_end);
        self.env_mut().insert("/".to_string(), match_obj.clone());
        self.substitution_in_smartmatch = self.in_smartmatch_rhs;
        self.stack.push(match_obj);
        Ok(())
    }

    /// Write a destructive `s///` result back to the topic, throwing
    /// `X::Assignment::RO` when the topic is bound read-only (e.g. a
    /// `method ro($_) {...}` / `sub ($x is readonly) {...}` parameter). Only
    /// reached after a substitution actually occurred, so a non-matching
    /// `s///` against a read-only topic stays a no-op.
    fn write_subst_topic_checked(
        &mut self,
        code: &CompiledCode,
        result: Value,
    ) -> Result<(), RuntimeError> {
        if self.readonly_vars().contains("_") {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str("Cannot modify an immutable Str".to_string()),
            );
            attrs.insert("value".to_string(), result);
            return Err(RuntimeError::typed("X::Assignment::RO", attrs));
        }
        self.env_mut().insert("_".to_string(), result.clone());
        self.env_mut().insert("$_".to_string(), result.clone());
        self.env_mut()
            .insert("__mutsu_rw_map_topic__".to_string(), result.clone());
        // Slice F (regex carrier): a bare `s///` modifies the topic `$_` by name
        // in `env`. When `my $_` makes `_` a compiled local slot, write the new
        // value through to that slot so it stays coherent without the reverse
        // `sync_locals_from_env` pull. If `$_` aliases a `given`/`for` source
        // scalar (`topic_source_var`), mirror the modified topic back to it too,
        // matching the `$x ~~ s///` smartmatch writeback path. Both `_` and the
        // source name are flagged for the carrier (env_dirty) so an enclosing
        // EVAL/carrier dropping its blanket net still reconciles the slot.
        self.update_local_if_exists(code, "_", &result);
        self.note_caller_env_write("_");
        if let Some(source_var) = self.topic_source_var.clone()
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            self.set_env_with_main_alias(&source_var, result.clone());
            self.update_local_if_exists(code, &source_var, &result);
            self.note_caller_env_write(&source_var);
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_non_destructive_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth_idx: Option<u32>,
        x_idx: Option<u32>,
        perl5: bool,
    ) -> Result<(), RuntimeError> {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let raw_replacement = normalize_subst_replacement(Self::const_str(code, replacement_idx));
        // A replacement containing a `{...}` code block (e.g. from `s[...] = $0 x 2`)
        // must be evaluated *per match*, with `$/`, `$0`, ... bound to that match.
        // For replacements without code blocks, interpolate once up front.
        let has_code_block = raw_replacement.contains('{');
        let replacement = if has_code_block {
            raw_replacement.clone()
        } else {
            self.interpolate_subst_replacement_with_closures(&raw_replacement)
        };

        let nth_spec = nth_idx.map(|idx| Self::const_str(code, idx).to_string());
        let x_spec = x_idx.map(|idx| Self::const_str(code, idx).to_string());
        let target = self.env().get("_").cloned().unwrap_or(Value::Nil);
        let text = target.to_string_value();

        if nth_spec.is_none() && x_spec.is_none() && !global {
            if perl5 {
                let found = self.regex_find_first_p5(&pattern, &text);
                if let Some((start, end)) = found {
                    let out = Self::apply_substitutions(
                        &text,
                        &[(start, end)],
                        &replacement,
                        samecase,
                        sigspace,
                        samemark,
                        samespace,
                    );
                    // S/// sets $/ to the match (without mutating $_).
                    let match_obj = Self::make_subst_match(&text, start, end);
                    self.env_mut().insert("/".to_string(), match_obj);
                    self.stack.push(Value::str(out));
                } else {
                    self.env_mut().insert("/".to_string(), Value::Nil);
                    self.stack.push(Value::str(text));
                }
            } else {
                let found = loan_env!(
                    self,
                    regex_find_first_from_with_captures(&pattern, &text, 0)
                );
                if let Some((start, end, captures)) = found {
                    let out = if has_code_block {
                        self.apply_substitutions_dynamic(
                            &text,
                            &[(start, end)],
                            &raw_replacement,
                            std::slice::from_ref(&captures),
                            samecase,
                            sigspace,
                            samemark,
                            samespace,
                        )
                    } else {
                        let expanded = expand_capture_refs(&replacement, &captures);
                        Self::apply_substitutions(
                            &text,
                            &[(start, end)],
                            &expanded,
                            samecase,
                            sigspace,
                            samemark,
                            samespace,
                        )
                    };
                    let match_obj = Self::make_subst_match(&text, start, end);
                    self.env_mut().insert("/".to_string(), match_obj);
                    self.stack.push(Value::str(out));
                } else {
                    self.env_mut().insert("/".to_string(), Value::Nil);
                    self.stack.push(Value::str(text));
                }
            }
            return Ok(());
        }

        let (ranges, per_match_captures) = if perl5 {
            let all_matches = loan_env!(self, regex_find_all_p5(&pattern, &text));
            let selected = if global && nth_spec.is_none() && x_spec.is_none() {
                all_matches
            } else {
                Self::select_substitution_ranges(
                    &all_matches,
                    nth_spec.as_deref(),
                    x_spec.as_deref(),
                )?
            };
            (selected, Vec::new())
        } else {
            let mut matches_with_caps: Vec<(usize, usize, Vec<String>)> = Vec::new();
            let mut pos = 0;
            while let Some((start, end, caps)) = loan_env!(
                self,
                regex_find_first_from_with_captures(&pattern, &text, pos)
            ) {
                matches_with_caps.push((start, end, caps));
                pos = if end > start { end } else { start + 1 };
            }
            let all_ranges: Vec<(usize, usize)> =
                matches_with_caps.iter().map(|(s, e, _)| (*s, *e)).collect();
            let selected = if global && nth_spec.is_none() && x_spec.is_none() {
                all_ranges
            } else {
                Self::select_substitution_ranges(
                    &all_ranges,
                    nth_spec.as_deref(),
                    x_spec.as_deref(),
                )?
            };
            let captures: Vec<Vec<String>> = selected
                .iter()
                .filter_map(|r| {
                    matches_with_caps
                        .iter()
                        .find(|(s, e, _)| *s == r.0 && *e == r.1)
                        .map(|(_, _, c)| c.clone())
                })
                .collect();
            (selected, captures)
        };
        // Mirror the destructive path: a single :nth(N) yields a Match; :g, :x,
        // or a multi-value :nth yields a List of Matches in $/.
        let single_nth = nth_spec.as_deref().is_some_and(|s| !s.contains(','));
        let nth_is_multi = nth_spec.as_deref().is_some_and(|s| s.contains(','));
        let result_is_list = !single_nth && (global || x_spec.is_some() || nth_is_multi);
        if ranges.is_empty() {
            let slash = if result_is_list {
                Value::array(Vec::new())
            } else {
                Value::Nil
            };
            self.env_mut().insert("/".to_string(), slash);
            self.stack.push(Value::str(text));
            return Ok(());
        }
        let out = if has_code_block {
            self.apply_substitutions_dynamic(
                &text,
                &ranges,
                &raw_replacement,
                &per_match_captures,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        } else if !per_match_captures.is_empty() {
            Self::apply_substitutions_with_captures(
                &text,
                &ranges,
                &replacement,
                &per_match_captures,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        } else {
            Self::apply_substitutions(
                &text,
                &ranges,
                &replacement,
                samecase,
                sigspace,
                samemark,
                samespace,
            )
        };
        // Set $/ to the match result (List or single Match).
        if result_is_list {
            let matches: Vec<Value> = ranges
                .iter()
                .map(|(s, e)| Self::make_subst_match(&text, *s, *e))
                .collect();
            self.env_mut()
                .insert("/".to_string(), Value::array(matches));
        } else {
            let (s, e) = ranges[0];
            let match_obj = Self::make_subst_match(&text, s, e);
            self.env_mut().insert("/".to_string(), match_obj);
        }
        self.stack.push(Value::str(out));
        Ok(())
    }
}
