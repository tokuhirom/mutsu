use super::*;
use crate::value::ValueView;

/// The `:samecase`/`:samemark`/`:samespace` substitution adverbs, applied to a
/// computed replacement against the matched text (shared with the `s///`
/// operator via `apply_subst_case_transforms`).
#[derive(Clone, Copy, Default)]
pub(super) struct SubstCaseTransforms {
    pub samecase: bool,
    pub samemark: bool,
    pub sigspace: bool,
    pub samespace: bool,
}

impl SubstCaseTransforms {
    fn any(&self) -> bool {
        // `:sigspace`/`:samespace` imply `:samemark` (see
        // `apply_subst_case_transforms`), so a bare `:s` still needs the transform.
        self.samecase || self.samemark || self.samespace || self.sigspace
    }

    pub(super) fn apply(&self, replacement: &str, matched: &str) -> String {
        if !self.any() {
            return replacement.to_string();
        }
        crate::vm::vm_string_regex_ops::apply_subst_case_transforms(
            replacement,
            matched,
            self.samecase,
            self.samemark,
            self.sigspace,
            self.samespace,
        )
    }
}

impl Interpreter {
    /// Validate an `:nth` index list for substitution: every index must be at
    /// least 1 and the list must be monotonically increasing. Rakudo's lazy
    /// match iterator cannot rewind, so a non-increasing list (e.g.
    /// `:nth(2,4,1,6)`) or a zero/negative index throws.
    fn validate_subst_nth_list(nth_list: &[i64]) -> Result<(), RuntimeError> {
        let mut prev: i64 = 0;
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

    /// Expand a `:nth`/`:st`/`:nd`/`:rd`/`:th` adverb argument into a list of
    /// 1-based match indices. Accepts an Int, an Array of Ints, or any Range
    /// variant (`:nth(1..3)`).
    /// Is `value` a `WhateverCode` (e.g. `*-1`)? Those carry a
    /// `__mutsu_callable_type` marker in their captured environment.
    fn is_whatever_code_value(value: &Value) -> bool {
        matches!(
            value.view(),
            ValueView::Sub(data)
                if matches!(
                    data.env.get("__mutsu_callable_type").map(Value::view),
                    Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
                )
        )
    }

    fn subst_nth_indices(value: &Value) -> Vec<i64> {
        match value.view() {
            ValueView::Int(n) => vec![n],
            ValueView::Array(items, _) => items.iter().map(|v: &Value| v.to_f64() as i64).collect(),
            ValueView::Range(lo, hi) => (lo..=hi).collect(),
            ValueView::RangeExcl(lo, hi) => (lo..hi).collect(),
            ValueView::RangeExclStart(lo, hi) => ((lo + 1)..=hi).collect(),
            ValueView::RangeExclBoth(lo, hi) => ((lo + 1)..hi).collect(),
            _ => vec![value.to_f64() as i64],
        }
    }

    /// Build the `$/` value after a multi-match substitution. With a List-result
    /// adverb (`:g`/`:x`/multi-`:nth`) `$/` is a (possibly empty) List of Match
    /// objects; otherwise it is the single first Match, or Nil when nothing
    /// matched.
    fn subst_match_var(selected: &[RegexCaptures], text: &str, result_is_list: bool) -> Value {
        let to_match = |c: &RegexCaptures| {
            Value::make_match_object_full(
                c.matched.clone(),
                c.from as i64,
                c.to as i64,
                &c.positional,
                &c.named,
                &c.named_subcaps,
                &c.positional_subcaps,
                &c.positional_quantified,
                &c.positional_nil,
                Some(text),
            )
        };
        if result_is_list {
            Value::array(selected.iter().map(to_match).collect())
        } else if let Some(c) = selected.first() {
            to_match(c)
        } else {
            Value::NIL
        }
    }

    /// `.wordcase(:&filter = &tclc, :$where = True)`: title-case each word,
    /// but only words that smart-match `:where`, transformed by `:filter`.
    pub(crate) fn dispatch_wordcase(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();
        let mut filter: Option<Value> = None;
        let mut where_matcher: Option<Value> = None;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "filter" => filter = Some(value.clone()),
                    "where" => where_matcher = Some(value.clone()),
                    _ => {}
                }
            }
        }

        let mut result = String::new();
        for (segment, is_word) in crate::value::wordcase_segments(&text) {
            if !is_word {
                result.push_str(&segment);
                continue;
            }
            let word = Value::str(segment.clone());
            // `:where` selects which words are transformed (default: all).
            let selected = match &where_matcher {
                Some(m) => self.smart_match(&word, m),
                None => true,
            };
            if !selected {
                result.push_str(&segment);
                continue;
            }
            // `:filter` transforms the selected word (default: tclc).
            let transformed = match &filter {
                Some(f) => self
                    .call_sub_value(f.clone(), vec![word], false)?
                    .to_string_value(),
                None => crate::value::tclc_str(&segment),
            };
            result.push_str(&transformed);
        }
        Ok(Value::str(result))
    }

    pub(crate) fn dispatch_subst(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();
        let mut positional: Vec<Value> = Vec::new();
        let mut global = false;
        let mut nth: Option<Vec<i64>> = None;
        // `:nth(*)` / `:nth(*-1)` can only be resolved once the match count is
        // known, so keep the raw Whatever/WhateverCode for deferred resolution.
        let mut nth_deferred: Vec<Value> = Vec::new();
        let mut x_count: Option<Value> = None;
        let mut pos_start: Option<usize> = None;
        let mut continue_from: Option<usize> = None;
        let mut transforms = SubstCaseTransforms::default();
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "g" | "global" => global = value.truthy(),
                    "x" => x_count = Some(value.clone()),
                    // `:nth`, plus the ordinal aliases `:st`/`:nd`/`:rd`/`:th`, all
                    // select 1-based match indices. The argument may be an Int, a
                    // list of Ints, or a Range (`:nth(1..3)`).
                    "nth" | "st" | "nd" | "rd" | "th" => {
                        if matches!(value.view(), ValueView::Whatever)
                            || Self::is_whatever_code_value(value)
                        {
                            // Resolved later against the match count.
                            nth_deferred.push(value.clone());
                            nth.get_or_insert_with(Vec::new);
                        } else {
                            let idxs = Self::subst_nth_indices(value);
                            nth.get_or_insert_with(Vec::new).extend(idxs);
                        }
                    }
                    "1st" | "first" if value.truthy() => nth = Some(vec![1]),
                    "2nd" | "second" if value.truthy() => nth = Some(vec![2]),
                    "3rd" | "third" if value.truthy() => nth = Some(vec![3]),
                    "4th" | "fourth" if value.truthy() => nth = Some(vec![4]),
                    "p" | "pos" => pos_start = Some(value.to_f64() as usize),
                    "c" | "continue" => continue_from = Some(value.to_f64() as usize),
                    "samecase" | "ii" => transforms.samecase = value.truthy(),
                    "samemark" | "mm" => transforms.samemark = value.truthy(),
                    "samespace" | "ss" => transforms.samespace = value.truthy(),
                    "sigspace" | "s" => transforms.sigspace = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        // The `:x` adverb must be an Int/Range (or Whatever); anything else
        // (e.g. a class instance) is an X::Str::Match::x error, mirroring `.match`.
        if let Some(x) = &x_count
            && !Self::is_valid_match_x_arg(x)
        {
            return Err(Self::str_match_x_error("subst", x));
        }
        // `.subst` with no matcher at all resolves to no candidate — Rakudo
        // throws X::Multi::NoMatch (roast .../multi-no-match.t). (A bare pattern
        // with no replacement is still valid: replacement defaults to "".)
        let pattern = positional
            .first()
            .ok_or_else(|| super::methods_signature_errors::make_multi_no_match_error("subst"))?;
        let replacement_val = positional.get(1).cloned();
        let is_closure = matches!(
            replacement_val.as_ref().map(Value::view),
            Some(ValueView::Sub(_)) | Some(ValueView::WeakSub(_))
        );
        let replacement_str = if is_closure {
            String::new()
        } else {
            replacement_val
                .as_ref()
                .map(|v| v.to_string_value())
                .unwrap_or_default()
        };

        // Helper to determine how many matches to use based on :x
        let resolve_x_count = |x: &Option<Value>| -> Option<(usize, usize)> {
            let inner = x.as_ref()?;
            match inner.view() {
                ValueView::Int(n) => Some((n as usize, n as usize)),
                ValueView::Str(s) if s.as_str() == "Inf" || s.as_str() == "*" => {
                    Some((0, usize::MAX))
                }
                ValueView::Whatever | ValueView::Sub(_) | ValueView::WeakSub(_) => {
                    Some((0, usize::MAX))
                }
                ValueView::Range(lo, hi) => {
                    if lo > hi {
                        Some((usize::MAX, 0)) // Empty range: always fail
                    } else {
                        Some((lo as usize, hi as usize))
                    }
                }
                ValueView::RangeExcl(lo, hi) => {
                    Some((lo as usize, (hi as usize).saturating_sub(1)))
                }
                ValueView::RangeExclStart(lo, hi) => Some(((lo as usize) + 1, hi as usize)),
                ValueView::RangeExclBoth(lo, hi) => {
                    Some(((lo as usize) + 1, (hi as usize).saturating_sub(1)))
                }
                _ => Some((inner.to_f64() as usize, inner.to_f64() as usize)),
            }
        };

        match pattern.view() {
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => {
                let pat: String = match pattern.view() {
                    ValueView::Regex(p) => p.to_string(),
                    ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
                    _ => unreachable!(),
                };
                let is_p5 = matches!(pattern.view(), ValueView::RegexWithAdverbs(a) if a.perl5);
                let pat = if is_p5 {
                    self.interpolate_regex_pattern(&pat)
                } else {
                    pat
                };
                let pat_global =
                    matches!(pattern.view(), ValueView::RegexWithAdverbs(a) if a.global) || global;
                let all_captures = if is_p5 {
                    #[cfg(feature = "pcre2")]
                    {
                        self.regex_match_all_with_captures_p5(&pat, &text)
                    }
                    #[cfg(not(feature = "pcre2"))]
                    {
                        self.regex_match_all_with_captures(&pat, &text)
                    }
                } else {
                    self.regex_match_all_with_captures(&pat, &text)
                };
                // After a multi-match substitution `$/` is a List of Match objects
                // for `:g`/`:x`/multi-`:nth`, but a single Match for a single-index
                // `:nth` (even when combined with `:g`, e.g. `s:2nd:g/./Z/`).
                let nth_len = nth.as_ref().map(|v| v.len());
                let single_nth = nth_len == Some(1);
                let nth_is_multi = nth_len.is_some_and(|n| n > 1);
                let result_is_list =
                    !single_nth && (pat_global || x_count.is_some() || nth_is_multi);
                let empty_match_var = |me: &mut Self| {
                    let v = if result_is_list {
                        Value::array(Vec::new())
                    } else {
                        Value::NIL
                    };
                    me.env.insert("/".to_string(), v);
                };

                if all_captures.is_empty() {
                    empty_match_var(self);
                    return Ok(Value::str(text));
                }
                let chars: Vec<char> = text.chars().collect();

                let has_adverbs = nth.is_some()
                    || x_count.is_some()
                    || pos_start.is_some()
                    || continue_from.is_some();

                if has_adverbs || pat_global {
                    let mut selected = self.select_non_overlapping_matches(all_captures);

                    // Apply :c(N) - filter matches starting from character position N
                    if let Some(c) = continue_from {
                        selected.retain(|cap| cap.from >= c);
                    }

                    // Apply :p(N) - first match must start at exactly position N
                    // If it doesn't, return original string (failure)
                    if let Some(p) = pos_start {
                        // Filter to matches at or after position p
                        selected.retain(|cap| cap.from >= p);
                        // The first remaining match must be exactly at p
                        if selected.is_empty() || selected[0].from != p {
                            empty_match_var(self);
                            return Ok(Value::str(text));
                        }
                    }

                    // If no :g, :x, :nth - single match mode
                    if !pat_global && x_count.is_none() && nth.is_none() {
                        selected.truncate(1);
                    }

                    // Resolve any deferred `:nth(*)` / `:nth(*-1)` now that the
                    // match count is known.
                    if !nth_deferred.is_empty() {
                        let total = selected.len();
                        let mut extra: Vec<i64> = Vec::new();
                        for spec in &nth_deferred {
                            let resolved = self.resolve_nth_value_indices(spec, total)?;
                            extra.extend(resolved.into_iter().map(|i| i as i64));
                        }
                        extra.sort_unstable();
                        nth.get_or_insert_with(Vec::new).extend(extra);
                    }

                    // Apply :nth - select specific 1-based match indices
                    if let Some(ref nth_list) = nth {
                        Self::validate_subst_nth_list(nth_list)?;
                        let total = selected.len();
                        let mut indices: Vec<usize> = Vec::new();
                        for &n in nth_list {
                            if (n as usize) <= total && !indices.contains(&(n as usize - 1)) {
                                indices.push(n as usize - 1);
                            }
                        }
                        let new_selected: Vec<_> = indices
                            .iter()
                            .filter_map(|&i| selected.get(i).cloned())
                            .collect();
                        selected = new_selected;
                    }

                    // Apply :x(N) - select exactly N matches (or range)
                    if let Some((lo, hi)) = resolve_x_count(&x_count) {
                        let count = selected.len();
                        if count < lo {
                            empty_match_var(self);
                            return Ok(Value::str(text));
                        }
                        if count > hi {
                            selected.truncate(hi);
                        }
                    }

                    if selected.is_empty() {
                        empty_match_var(self);
                        return Ok(Value::str(text));
                    }

                    let mut result = String::new();
                    let mut last_end = 0;
                    for captures in &selected {
                        let prefix: String = chars[last_end..captures.from].iter().collect();
                        result.push_str(&prefix);
                        let repl = self.eval_subst_replacement_cased(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            &captures.matched,
                            Some(captures),
                            Some(&text),
                            transforms,
                        )?;
                        result.push_str(&repl);
                        last_end = captures.to;
                    }
                    let suffix: String = chars[last_end..].iter().collect();
                    result.push_str(&suffix);
                    let match_var = Self::subst_match_var(&selected, &text, result_is_list);
                    self.env.insert("/".to_string(), match_var);
                    Ok(Value::str(result))
                } else if let Some(captures) = {
                    if is_p5 {
                        #[cfg(feature = "pcre2")]
                        {
                            self.regex_match_with_captures_p5(&pat, &text)
                        }
                        #[cfg(not(feature = "pcre2"))]
                        {
                            self.regex_match_with_captures(&pat, &text)
                        }
                    } else {
                        self.regex_match_with_captures(&pat, &text)
                    }
                } {
                    // Set $/ to the match object
                    let match_obj = Value::make_match_object_full(
                        captures.matched.clone(),
                        captures.from as i64,
                        captures.to as i64,
                        &captures.positional,
                        &captures.named,
                        &captures.named_subcaps,
                        &captures.positional_subcaps,
                        &captures.positional_quantified,
                        &captures.positional_nil,
                        Some(&text),
                    );
                    self.env.insert("/".to_string(), match_obj);
                    let prefix: String = chars[..captures.from].iter().collect();
                    let suffix: String = chars[captures.to..].iter().collect();
                    let repl = self.eval_subst_replacement_cased(
                        &replacement_val,
                        is_closure,
                        &replacement_str,
                        &captures.matched,
                        Some(&captures),
                        Some(&text),
                        transforms,
                    )?;
                    Ok(Value::str(format!("{}{}{}", prefix, repl, suffix)))
                } else {
                    Ok(Value::str(text))
                }
            }
            ValueView::Str(pat) => {
                let has_adverbs = nth.is_some()
                    || x_count.is_some()
                    || pos_start.is_some()
                    || continue_from.is_some();
                if has_adverbs || global {
                    let pat_str = pat.as_str();
                    let mut str_matches: Vec<(usize, usize)> = Vec::new();
                    let mut search_start = 0;
                    while let Some(pos) = text[search_start..].find(pat_str) {
                        let abs_pos = search_start + pos;
                        str_matches.push((abs_pos, abs_pos + pat_str.len()));
                        search_start = abs_pos + pat_str.len().max(1);
                    }

                    let char_indices: Vec<(usize, usize)> = str_matches
                        .iter()
                        .map(|&(start, end)| {
                            let cs = text[..start].chars().count();
                            let ce = text[..end].chars().count();
                            (cs, ce)
                        })
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
                    if let Some(ref nth_list) = nth {
                        Self::validate_subst_nth_list(nth_list)?;
                        let total = keep.len();
                        let mut selected: Vec<usize> = Vec::new();
                        for &n in nth_list {
                            if (n as usize) <= total {
                                let chosen = keep[n as usize - 1];
                                if !selected.contains(&chosen) {
                                    selected.push(chosen);
                                }
                            }
                        }
                        keep = selected;
                    }
                    if let Some((lo, hi)) = resolve_x_count(&x_count) {
                        let count = keep.len();
                        if count < lo {
                            return Ok(Value::str(text));
                        }
                        if count > hi {
                            keep.truncate(hi);
                        }
                    }

                    let mut result = String::new();
                    let mut last_end = 0;
                    for &idx in &keep {
                        let (start, end) = str_matches[idx];
                        result.push_str(&text[last_end..start]);
                        let matched_text = &text[start..end];
                        let caps = is_closure.then(|| RegexCaptures {
                            matched: matched_text.to_string(),
                            from: char_indices[idx].0,
                            to: char_indices[idx].1,
                            ..Default::default()
                        });
                        let repl = self.eval_subst_replacement_cased(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            matched_text,
                            caps.as_ref(),
                            Some(&text),
                            transforms,
                        )?;
                        result.push_str(&repl);
                        last_end = end;
                    }
                    result.push_str(&text[last_end..]);
                    Ok(Value::str(result))
                } else if let Some(bpos) = text.find(pat.as_str()) {
                    // Single (first-match) replacement.
                    let end = bpos + pat.as_str().len();
                    let matched_text = &text[bpos..end];
                    let caps = is_closure.then(|| RegexCaptures {
                        matched: matched_text.to_string(),
                        from: text[..bpos].chars().count(),
                        to: text[..end].chars().count(),
                        ..Default::default()
                    });
                    let repl = self.eval_subst_replacement_cased(
                        &replacement_val,
                        is_closure,
                        &replacement_str,
                        matched_text,
                        caps.as_ref(),
                        Some(&text),
                        transforms,
                    )?;
                    let mut result = String::with_capacity(text.len());
                    result.push_str(&text[..bpos]);
                    result.push_str(&repl);
                    result.push_str(&text[end..]);
                    Ok(Value::str(result))
                } else {
                    Ok(Value::str(text))
                }
            }
            _ => {
                let pat_str = pattern.to_string_value();
                let repl = transforms.apply(&replacement_str, &pat_str);
                if global {
                    Ok(Value::str(text.replace(&pat_str, &repl)))
                } else {
                    Ok(Value::str(text.replacen(&pat_str, &repl, 1)))
                }
            }
        }
    }
}
