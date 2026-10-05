use super::methods_string_subst_repl::SubstCaseTransforms;
use super::*;

impl Interpreter {
    /// Validate an `:nth` index list: every index must be at least 1 and the
    /// list must be monotonically increasing. A lazy match iterator cannot
    /// rewind, so a non-increasing list or a zero/negative index throws.
    pub(super) fn validate_subst_nth_list(nth_list: &[i64]) -> Result<(), RuntimeError> {
        let mut prev = 0;
        for &n in nth_list {
            if n < 1 {
                return Err(RuntimeError::new(format!(
                    "Attempt to retrieve before :1st match -- :nth({n})"
                )));
            }
            if n < prev {
                return Err(RuntimeError::new(format!(
                    "Attempt to fetch match #{n} after #{prev}"
                )));
            }
            prev = n;
        }
        Ok(())
    }

    /// Substitute a string matcher at grapheme boundaries and publish its
    /// selected spans as `$/` for `.subst-mutate`.
    // Cost: O(n + r) plus per-match replacement work, n = bytes of the
    // invocant, r = matches; all matches share one GraphemeIndex.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn dispatch_subst_string_pattern(
        &mut self,
        text: &str,
        pat: &str,
        global: bool,
        nth: &mut Option<Vec<i64>>,
        nth_deferred: &[Value],
        x_count: &Option<Value>,
        pos_start: Option<usize>,
        continue_from: Option<usize>,
        replacement_val: &Option<Value>,
        is_closure: bool,
        replacement_str: &str,
        transforms: SubstCaseTransforms,
        resolve_x_count: &impl Fn(&Option<Value>) -> Option<(usize, usize)>,
    ) -> Result<Value, RuntimeError> {
        let nth_len = nth.as_ref().map(Vec::len);
        let deferred_nth_is_range = nth_deferred
            .iter()
            .any(|v| Self::deferred_nth_range_end(v).is_some());
        let single_nth = nth_len == Some(1) && !deferred_nth_is_range;
        let nth_is_multi = nth_len.is_some_and(|n| n > 1) || deferred_nth_is_range;
        let result_is_list =
            !single_nth && (global || x_count.is_some() || nth_is_multi);
        let publish_str_matches = |me: &mut Self, captures: &[RegexCaptures]| {
            let match_var = Self::subst_match_var(captures, text, result_is_list);
            me.env.insert("/".to_string(), match_var.clone());
            me.publish_subst_capture_env(&match_var);
        };
        let has_adverbs = nth.is_some()
            || x_count.is_some()
            || pos_start.is_some()
            || continue_from.is_some();

        if has_adverbs || global {
            let index = crate::builtins::grapheme_index::GraphemeIndex::build(text);
            let mut str_matches: Vec<(usize, usize)> = Vec::new();
            let mut search_start = 0;
            while let Some(abs_pos) = crate::builtins::grapheme_index::find_graphemes(
                text,
                &index,
                search_start,
                pat,
            ) {
                str_matches.push((abs_pos, abs_pos + pat.len()));
                if pat.is_empty() {
                    if abs_pos == text.len() {
                        break;
                    }
                    search_start = abs_pos
                        + text[abs_pos..]
                            .chars()
                            .next()
                            .map_or(1, char::len_utf8);
                } else {
                    search_start = abs_pos + pat.len();
                }
            }

            // Matches are ascending, so one running count converts every byte
            // offset to a char offset.
            let mut counted = (0usize, 0usize); // (byte, char)
            let mut char_of = |b: usize| {
                counted.1 += text[counted.0..b].chars().count();
                counted.0 = b;
                counted.1
            };
            let char_indices: Vec<(usize, usize)> = str_matches
                .iter()
                .map(|&(start, end)| (char_of(start), char_of(end)))
                .collect();

            let mut keep: Vec<usize> = (0..str_matches.len()).collect();
            if let Some(c) = continue_from {
                keep.retain(|&i| char_indices[i].0 >= c);
            }
            if let Some(p) = pos_start {
                keep.retain(|&i| char_indices[i].0 == p);
            }
            if !global && nth.is_none() && x_count.is_none() {
                keep.truncate(1);
            }
            if !nth_deferred.is_empty() {
                let total = keep.len();
                let mut extra: Vec<i64> = Vec::new();
                for spec in nth_deferred {
                    let resolved = self.resolve_nth_value_indices(spec, total)?;
                    extra.extend(resolved.into_iter().map(|i| i as i64));
                }
                extra.sort_unstable();
                nth.get_or_insert_with(Vec::new).extend(extra);
            }
            if let Some(nth_list) = nth.as_ref() {
                Self::validate_subst_nth_list(nth_list)?;
                let total = keep.len();
                let mut selected: Vec<usize> = Vec::new();
                for &n in nth_list {
                    if (n as usize) <= total {
                        let chosen = keep[n as usize - 1];
                        if selected.last() != Some(&chosen) {
                            selected.push(chosen);
                        }
                    }
                }
                keep = selected;
            }
            if let Some((lo, hi)) = resolve_x_count(x_count) {
                let count = keep.len();
                if count < lo {
                    publish_str_matches(self, &[]);
                    return Ok(Value::str(text.to_string()));
                }
                if count > hi {
                    keep.truncate(hi);
                }
            }

            let selected_captures: Vec<RegexCaptures> = keep
                .iter()
                .map(|&idx| RegexCaptures {
                    from: char_indices[idx].0,
                    to: char_indices[idx].1,
                    ..Default::default()
                })
                .collect();
            publish_str_matches(self, &selected_captures);

            let target = is_closure.then(|| crate::runtime::MatchTarget::new(text));
            let mut result = String::with_capacity(text.len());
            let mut last_end = 0;
            for &idx in &keep {
                let (start, end) = str_matches[idx];
                result.push_str(&text[last_end..start]);
                let matched_text = &text[start..end];
                let captures = target.as_ref().map(|target| {
                    let mut captures = RegexCaptures {
                        from: char_indices[idx].0,
                        to: char_indices[idx].1,
                        ..Default::default()
                    };
                    captures.set_target(Some(target.clone()));
                    captures
                });
                let replacement = self.eval_subst_replacement_cased(
                    replacement_val,
                    is_closure,
                    replacement_str,
                    matched_text,
                    captures.as_ref(),
                    Some(text),
                    transforms,
                )?;
                result.push_str(&replacement);
                last_end = end;
            }
            result.push_str(&text[last_end..]);
            publish_str_matches(self, &selected_captures);
            return Ok(Value::str(result));
        }

        let index = crate::builtins::grapheme_index::GraphemeIndex::build(text);
        let Some(bpos) = crate::builtins::grapheme_index::find_graphemes(text, &index, 0, pat)
        else {
            publish_str_matches(self, &[]);
            return Ok(Value::str(text.to_string()));
        };
        let end = bpos + pat.len();
        let captures = RegexCaptures {
            from: text[..bpos].chars().count(),
            to: text[..end].chars().count(),
            ..Default::default()
        };
        publish_str_matches(self, std::slice::from_ref(&captures));
        let replacement = self.eval_subst_replacement_cased(
            replacement_val,
            is_closure,
            replacement_str,
            &text[bpos..end],
            is_closure.then_some(&captures),
            Some(text),
            transforms,
        )?;
        publish_str_matches(self, std::slice::from_ref(&captures));
        let mut result = String::with_capacity(text.len());
        result.push_str(&text[..bpos]);
        result.push_str(&replacement);
        result.push_str(&text[end..]);
        Ok(Value::str(result))
    }
}
