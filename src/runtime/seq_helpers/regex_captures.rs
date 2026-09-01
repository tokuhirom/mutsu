use super::super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;

impl Interpreter {
    /// Reset capture env vars left over from a previous match: numeric keys
    /// (`0`, `1`, ...) are set to Nil (`$0` is a plain env `Var` read with no
    /// fallback), and named-capture keys (`<name>`) are REMOVED so `$<name>`
    /// (`OpCode::GetCaptureVar`) falls through to the current `$/` AT-KEY
    /// lookup instead of seeing a stale entry. Removal, not Nil-ing, is
    /// load-bearing: a present-but-Nil entry would shadow the local-slot
    /// `$/` fallback action methods rely on (`t/capture-var-topic-slot.t`).
    pub(crate) fn reset_capture_env_vars(&mut self) {
        // A routine executes in a scoped env overlay. Capture vars inherited
        // from its caller therefore are not in `keys()` (which deliberately
        // exposes this frame's overlay only), but they must still be shadowed
        // here. Removing an inherited named capture records an overlay
        // tombstone, so it cannot reach the caller when this frame is dropped.
        let numeric_keys: Vec<Symbol> = self
            .env
            .visible_keys_where(|s| !s.is_empty() && s.chars().all(|ch| ch.is_ascii_digit()))
            .into_iter()
            .map(|key| Symbol::intern(&key))
            .collect();
        for key in numeric_keys {
            self.env.insert_sym(key, Value::NIL);
        }
        let angle_keys: Vec<Symbol> = self
            .env
            .visible_keys_where(|s| s.len() > 2 && s.starts_with('<') && s.ends_with('>'))
            .into_iter()
            .map(|key| Symbol::intern(&key))
            .collect();
        for key in angle_keys {
            self.env.remove_sym(key);
        }
    }

    /// Clear `$/` and all numeric capture variables (`$0`, `$1`, ...) after a failed match.
    pub(in crate::runtime) fn clear_match_state(&mut self) {
        self.env.insert("/".to_string(), Value::NIL);
        self.reset_capture_env_vars();
    }

    /// Clear match state after a failed *multi-match* (`:g` / `:ov` / `:ex`).
    /// Those adverbs make the match return a `List` of `Match`es, so a failure
    /// leaves `$/` an **empty List**, not `Nil` — `+@$/` must be 0. (A plain
    /// match returns a single `Match`, so its failure leaves `Nil`, which is
    /// what `clear_match_state` does.)
    pub(in crate::runtime) fn clear_multi_match_state(&mut self) {
        self.clear_match_state();
        self.env.insert(
            "/".to_string(),
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                crate::value::ArrayKind::List,
            ),
        );
    }

    pub(in crate::runtime) fn apply_single_regex_captures(&mut self, captures: &RegexCaptures) {
        let make_capture_match = |capture: &str, from: usize, to: usize| {
            let mut attrs = HashMap::new();
            attrs.insert("str".to_string(), Value::str(capture.to_string()));
            attrs.insert("from".to_string(), Value::int(from as i64));
            attrs.insert("to".to_string(), Value::int(to as i64));
            attrs.insert("list".to_string(), Value::array(Vec::new()));
            attrs.insert("named".to_string(), Value::hash_bare_values(HashMap::new()));
            Value::make_instance(Symbol::intern("Match"), attrs)
        };

        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::str(captures.matched_text()));
        attrs.insert("from".to_string(), Value::int(captures.from as i64));
        attrs.insert("to".to_string(), Value::int(captures.to as i64));
        let positional: Vec<Value> = if !captures.positional_slots.is_empty() {
            captures
                .positional_slots
                .iter()
                .map(|slot| match slot {
                    Some((from, to)) => {
                        make_capture_match(&captures.span_text(*from, *to), *from, *to)
                    }
                    None => Value::NIL,
                })
                .collect()
        } else {
            captures
                .positional
                .iter()
                .map(|slot| make_capture_match(&captures.slot_text(slot), slot.from, slot.to))
                .collect()
        };
        attrs.insert("list".to_string(), Value::array(positional));
        let mut named = HashMap::new();
        for (k, v) in &captures.named {
            if k.starts_with(crate::runtime::SILENT_ACTION_MARKER_PREFIX) {
                continue;
            }
            let vals: Vec<Value> = v
                .nodes
                .iter()
                .map(|n| make_capture_match(&captures.span_text(n.from, n.to), n.from, n.to))
                .collect();
            if vals.len() == 1 && !v.quantified {
                named.insert(k.resolve(), vals[0].clone());
            } else {
                named.insert(k.resolve(), Value::array(vals));
            }
        }
        // Add hash captures from %<name>=(...) aliasing
        for (hash_name, entries) in &captures.hash_captures {
            let mut hash_map: HashMap<String, Value> = HashMap::new();
            for (key, value) in entries {
                let val: Value = match value {
                    Some(v) => Value::str(v.clone()),
                    None => Value::NIL,
                };
                hash_map.insert(key.clone(), val);
            }
            named.insert(hash_name.clone(), Value::hash_bare_values(hash_map));
        }
        attrs.insert("named".to_string(), Value::hash_bare_values(named));
        let match_obj = Value::make_instance(Symbol::intern("Match"), attrs);
        self.env.insert("/".to_string(), match_obj.clone());

        // Reset stale numeric/named captures before applying new ones.
        self.reset_capture_env_vars();

        for (i, slot) in captures.positional_slots.iter().enumerate() {
            let value = match slot {
                Some((from, to)) => make_capture_match(&captures.span_text(*from, *to), *from, *to),
                None => Value::NIL,
            };
            self.env.insert(i.to_string(), value);
        }
        if captures.positional_slots.is_empty() {
            self.env.insert("0".to_string(), Value::NIL);
        }
        // Set named capture env vars from the match object's named hash
        let named_v = match_obj.match_named();
        if let Some(ValueView::Hash(named_hash)) = named_v.as_ref().map(Value::view) {
            for (k, v) in named_hash.iter() {
                self.env.insert(format!("<{}>", k), v.clone());
            }
        }
    }

    /// Select non-overlapping matches from all matches (for :g/global).
    /// Takes the longest match at each position, then greedily selects
    /// matches that don't overlap with previously selected ones.
    pub(in crate::runtime) fn select_non_overlapping_matches(
        &self,
        all: Vec<RegexCaptures>,
    ) -> Vec<RegexCaptures> {
        if all.is_empty() {
            return Vec::new();
        }
        // First, pick the longest match at each starting position
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
        // Then greedily select non-overlapping: skip if starts before previous end
        let mut result = Vec::new();
        let mut last_end = 0usize;
        for (_, capture) in best_by_start {
            if capture.from >= last_end {
                last_end = capture.to;
                result.push(capture);
            }
        }
        result
    }

    /// Parse :x(...) style repeat bounds.
    /// Returns (min_required, max_to_return). `max_to_return = None` means unbounded.
    /// The `:x` adverb to `.match` / `s///` must be a `Numeric` (which covers
    /// `Int`, `Bool`, the numeric allomorphs, `Rat` and `Num`), a `Range`, or
    /// `*`. Anything else — notably a plain `Str`, which is *not* `Numeric`
    /// even when it spells a number — is an `X::Str::Match::x` error, exactly
    /// as Rakudo reports it (`:x("2")` is rejected while `:x(<2>)` is not).
    pub(in crate::runtime) fn is_valid_match_x_arg(value: &Value) -> bool {
        // A numeric allomorph (`<2>`) is a `Mixin` wrapping the numeric value;
        // it is `Numeric`, so it is accepted while the bare `Str` "2" is not.
        if let ValueView::Mixin(inner, _) = value.view() {
            return Self::is_valid_match_x_arg(inner.as_ref());
        }
        matches!(
            value.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Bool(_)
                | ValueView::Num(_)
                | ValueView::Rat(_, _)
                | ValueView::BigRat(_, _)
                | ValueView::FatRat(_, _)
                | ValueView::Whatever
                | ValueView::Range(_, _)
                | ValueView::RangeExcl(_, _)
                | ValueView::RangeExclStart(_, _)
                | ValueView::RangeExclBoth(_, _)
                | ValueView::GenericRange { .. }
        )
    }

    /// Build the `X::Str::Match::x` exception value for an invalid `:x` adverb.
    /// Rakudo's message always names `Str.match`, even when the adverb reached
    /// it through `.subst`/`s///`.
    pub(in crate::runtime) fn str_match_x_exception(got: &Value) -> Value {
        let type_name = crate::value::types::what_type_name(got);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("got".to_string(), got.clone());
        let message = format!(
            "in Str.match, got invalid value of type {} for :x, must be Int or Range",
            type_name
        );
        attrs.insert("message".to_string(), Value::str(message));
        Value::make_instance(Symbol::intern("X::Str::Match::x"), attrs)
    }

    /// Build the X::Str::Match::x error for an invalid `:x` adverb value.
    pub(in crate::runtime) fn str_match_x_error(got: &Value) -> RuntimeError {
        RuntimeError::from_exception_value(Self::str_match_x_exception(got))
    }

    /// `Str.match` *returns* a `Failure` for an invalid `:x` (it does not throw
    /// eagerly), so `my $r = "ab".match("a", :x<z>)` yields a `Failure` and only
    /// blows up when the result is used. `.subst` throws instead.
    pub(in crate::runtime) fn str_match_x_failure(got: &Value) -> Value {
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), Self::str_match_x_exception(got));
        failure_attrs.insert("handled".to_string(), Value::FALSE);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(in crate::runtime) fn parse_match_repeat_bounds(
        value: &Value,
    ) -> Option<(usize, Option<usize>)> {
        fn parse_non_negative_int(v: &Value) -> Option<i64> {
            match v.view() {
                // Numeric allomorph (`<2>`): read the numeric value it wraps.
                ValueView::Mixin(inner, _) => parse_non_negative_int(inner.as_ref()),
                ValueView::Int(i) => Some(i.max(0)),
                ValueView::Bool(b) => Some(i64::from(b)),
                // A fractional bound truncates toward zero, like Rakudo's `.Int`
                // coercion of the adverb (`:x(1.5)` selects exactly one match).
                ValueView::Num(n) if n.is_finite() => Some((n as i64).max(0)),
                ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 => Some((n / d).max(0)),
                ValueView::Str(s) => s.trim().parse::<i64>().ok().map(|i| i.max(0)),
                ValueView::Whatever => Some(i64::MAX),
                _ => None,
            }
        }

        fn adjust_range_bounds(
            start: i64,
            end: i64,
            excl_start: bool,
            excl_end: bool,
        ) -> Option<(usize, Option<usize>)> {
            let mut min = start;
            let mut max = end;
            if excl_start {
                min = min.saturating_add(1);
            }
            if excl_end && max != i64::MAX {
                max = max.saturating_sub(1);
            }
            min = min.max(0);
            if max != i64::MAX {
                max = max.max(0);
                if max < min {
                    return None;
                }
                Some((min as usize, Some(max as usize)))
            } else {
                Some((min as usize, None))
            }
        }

        match value.view() {
            ValueView::Range(start, end) => adjust_range_bounds(start, end, false, false),
            ValueView::RangeExcl(start, end) => adjust_range_bounds(start, end, false, true),
            ValueView::RangeExclStart(start, end) => adjust_range_bounds(start, end, true, false),
            ValueView::RangeExclBoth(start, end) => adjust_range_bounds(start, end, true, true),
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let min = parse_non_negative_int(start.as_ref())?;
                let max = parse_non_negative_int(end.as_ref())?;
                adjust_range_bounds(min, max, excl_start, excl_end)
            }
            _ => {
                let n = parse_non_negative_int(value)?;
                if n == i64::MAX {
                    Some((0, None))
                } else {
                    Some((n as usize, Some(n as usize)))
                }
            }
        }
    }

    /// Apply :x bounds to already-ordered matches.
    pub(in crate::runtime) fn select_matches_by_repeat_bounds(
        matches: Vec<RegexCaptures>,
        min_required: usize,
        max_to_return: Option<usize>,
    ) -> Option<Vec<RegexCaptures>> {
        if matches.len() < min_required {
            return None;
        }
        let take_n = max_to_return.unwrap_or(matches.len()).min(matches.len());
        Some(matches.into_iter().take(take_n).collect())
    }

    /// Apply multiple regex captures (for :g, :ov, :ex) -- set $/ to list of Match objects.
    pub(in crate::runtime) fn apply_multi_regex_captures(
        &mut self,
        selected: &[RegexCaptures],
        orig: &str,
    ) {
        let slash_list = selected
            .iter()
            .map(|c| {
                // Use the full builder so quantified / nested positional captures
                // (e.g. `(\d) ** 4 % '.'` folding group 0 into a 4-element list)
                // survive into each `m:g` match object, matching the single-match
                // path. The short builder dropped `positional_quantified` etc.,
                // collapsing `$m[0]` to a bare string.
                //
                // `orig` is the whole subject string so each match's `.orig`
                // (and nested captures' `.orig`) reports the source text, matching
                // the single-match path and `.match(:g)` — the `~~ m:g/.../` path
                // previously passed `None`, leaving `.orig` empty.
                Value::make_match_object_full(
                    c.from as i64,
                    c.to as i64,
                    &c.positional,
                    &c.named,
                    c.target_or_new(orig),
                )
            })
            .collect::<Vec<_>>();
        self.reset_capture_env_vars();
        self.env.insert("/".to_string(), Value::array(slash_list));
        if let Some(first) = selected.first() {
            let t = first.target_or_new(orig);
            for (i, cap) in first.positional.iter().enumerate() {
                self.env
                    .insert(i.to_string(), Value::str(t.span_str(cap.from, cap.to)));
            }
        }
    }

    pub(in crate::runtime) fn set_pending_nth_error(message: String) {
        crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
            *e.borrow_mut() = Some(RuntimeError::new(message));
        });
    }

    pub(in crate::runtime) fn parse_nth_token(token: &str, total: usize) -> Result<usize, String> {
        let t = token.trim();
        if t.is_empty() {
            return Err("Invalid :nth index ()".to_string());
        }
        if t.eq_ignore_ascii_case("-Inf") {
            return Err("Invalid :nth index (-Inf)".to_string());
        }
        if t == "*" {
            if total == 0 {
                return Err("Invalid :nth index (*)".to_string());
            }
            return Ok(total);
        }
        if let Some(rest) = t.strip_prefix("*-") {
            let n = rest
                .trim()
                .parse::<usize>()
                .map_err(|_| format!("Invalid :nth index ({t})"))?;
            if n >= total {
                return Err(format!("Invalid :nth index ({t})"));
            }
            return Ok(total - n);
        }
        let n = t
            .parse::<i64>()
            .map_err(|_| format!("Invalid :nth index ({t})"))?;
        if n <= 0 {
            return Err(format!("Invalid :nth index ({t})"));
        }
        Ok(n as usize)
    }

    pub(in crate::runtime) fn collect_nth_indices_from_value(
        &self,
        value: &Value,
        total: usize,
        out: &mut Vec<usize>,
    ) -> Result<(), String> {
        match value.view() {
            ValueView::Int(i) => out.push(Self::parse_nth_token(&i.to_string(), total)?),
            ValueView::Num(n) => {
                if n.fract() != 0.0 {
                    return Err(format!("Invalid :nth index ({n})"));
                }
                out.push(Self::parse_nth_token(&format!("{}", n as i64), total)?);
            }
            ValueView::Str(s) => {
                for piece in s.split(',') {
                    out.push(Self::parse_nth_token(piece, total)?);
                }
            }
            ValueView::Whatever => out.push(Self::parse_nth_token("*", total)?),
            ValueView::Array(items, ..) => {
                for item in items.iter() {
                    self.collect_nth_indices_from_value(item, total, out)?;
                }
            }
            ValueView::Seq(items) => {
                for item in items.iter() {
                    self.collect_nth_indices_from_value(item, total, out)?;
                }
            }
            ValueView::Slip(items) => {
                for item in items.iter() {
                    self.collect_nth_indices_from_value(item, total, out)?;
                }
            }
            _ => {
                return Err(format!("Invalid :nth index ({})", value.to_string_value()));
            }
        }
        Ok(())
    }

    pub(in crate::runtime) fn resolve_nth_indices(
        &self,
        raw: &str,
        total: usize,
    ) -> Result<Vec<usize>, String> {
        let raw = raw.trim();
        if raw.starts_with('$') {
            let var_name = raw.trim_start_matches('$');
            let value = self.env.get(var_name).cloned().unwrap_or(Value::NIL);
            let mut out = Vec::new();
            self.collect_nth_indices_from_value(&value, total, &mut out)?;
            return Ok(out);
        }
        let mut out = Vec::new();
        for token in raw.split(',') {
            out.push(Self::parse_nth_token(token, total)?);
        }
        Ok(out)
    }

    /// Get the match continuation position from `$/.to`, defaulting to 0.
    pub(in crate::runtime) fn get_match_to_position(&self) -> usize {
        self.env
            .get("/")
            .and_then(Value::match_to)
            .map(|to| to as usize)
            .unwrap_or(0)
    }

    #[cfg(feature = "pcre2")]
    pub(in crate::runtime) fn regex_match_with_captures_p5(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        let re = self.compile_p5_regex(pattern)?;
        let mut locs = re.capture_locations();
        let m0 = re.captures_read(&mut locs, text.as_bytes()).ok()??;
        let names = re.capture_names();
        // pcre2 reports BYTE offsets; recorded spans are char indices into
        // the shared subject (ADR-0016 P3), so translate at the boundary.
        let to_char = |b: usize| text.get(..b).map_or(b, |p| p.chars().count());
        let mut out = RegexCaptures {
            from: to_char(m0.start()),
            to: to_char(m0.end()),
            target: Some(crate::runtime::MatchTarget::new(text)),
            ..RegexCaptures::default()
        };
        for idx in 1..locs.len() {
            if names.get(idx).is_some_and(Option::is_none) {
                if let Some((start, end)) = locs.get(idx) {
                    let (cs, ce) = (to_char(start), to_char(end));
                    out.positional.push(crate::runtime::PosSlot::span(cs, ce));
                    out.positional_slots.push(Some((cs, ce)));
                } else {
                    out.positional_slots.push(None);
                }
                continue;
            }
            if let (Some(Some(name)), Some((start, end))) = (names.get(idx), locs.get(idx)) {
                text.get(start..end)?;
                out.named
                    .entry(Symbol::intern(name))
                    .or_default()
                    .merge(NamedSlot::leaf(to_char(start), to_char(end)));
            }
        }
        Some(out)
    }

    #[cfg(feature = "pcre2")]
    pub(in crate::runtime) fn regex_match_all_with_captures_p5(
        &self,
        pattern: &str,
        text: &str,
    ) -> Vec<RegexCaptures> {
        let Some(re) = self.compile_p5_regex(pattern) else {
            return Vec::new();
        };
        let names = re.capture_names();
        let mut out = Vec::new();
        let mut start = 0usize;
        let bytes = text.as_bytes();
        let mut locs = re.capture_locations();
        // pcre2 reports BYTE offsets; recorded spans are char indices into
        // the shared subject (ADR-0016 P3), so translate at the boundary.
        let to_char = |b: usize| text.get(..b).map_or(b, |p| p.chars().count());
        let target = crate::runtime::MatchTarget::new(text);
        while start <= bytes.len() {
            let Ok(Some(m0)) = re.captures_read_at(&mut locs, bytes, start) else {
                break;
            };
            if text.get(m0.start()..m0.end()).is_none() {
                break;
            }
            let mut item = RegexCaptures {
                from: to_char(m0.start()),
                to: to_char(m0.end()),
                target: Some(target.clone()),
                ..RegexCaptures::default()
            };
            for idx in 1..locs.len() {
                if names.get(idx).is_some_and(Option::is_none) {
                    if let Some((c_start, c_end)) = locs.get(idx) {
                        if text.get(c_start..c_end).is_none() {
                            continue;
                        }
                        let (cs, ce) = (to_char(c_start), to_char(c_end));
                        item.positional.push(crate::runtime::PosSlot::span(cs, ce));
                        item.positional_slots.push(Some((cs, ce)));
                    } else {
                        item.positional_slots.push(None);
                    }
                    continue;
                }
                if let (Some(Some(name)), Some((c_start, c_end))) = (names.get(idx), locs.get(idx))
                {
                    if text.get(c_start..c_end).is_none() {
                        continue;
                    }
                    item.named
                        .entry(Symbol::intern(name))
                        .or_default()
                        .merge(NamedSlot::leaf(to_char(c_start), to_char(c_end)));
                }
            }
            // Advance past the match (at least 1 byte to avoid infinite loop)
            if m0.end() == start {
                start += 1;
            } else {
                start = m0.end();
            }
            out.push(item);
        }
        out
    }

    /// Every P5 match of `pattern` in `text` with its positional capture texts.
    ///
    /// Raku numbers a `:P5` substitution's captures the Raku way — P5 group 1 is
    /// `$0` — so the replacement (an ordinary `qq` quote, `:P5` or not) can
    /// interpolate `$0`, `$1`, ... exactly as it does for a Raku pattern.
    #[cfg(feature = "pcre2")]
    pub(crate) fn regex_find_all_p5_with_captures(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> Vec<(usize, usize, Vec<String>)> {
        let Some(re) = self.compile_p5_regex(pattern) else {
            return Vec::new();
        };
        let bytes = text.as_bytes();
        let mut results = Vec::new();
        let mut start = 0usize;
        let mut locs = re.capture_locations();
        while start <= bytes.len() {
            let Ok(Some(m0)) = re.captures_read_at(&mut locs, bytes, start) else {
                break;
            };
            let char_start = text[..m0.start()].chars().count();
            let char_end = text[..m0.end()].chars().count();
            // Group 0 is the whole match; Raku exposes group N as `$(N-1)`.
            let caps = (1..locs.len())
                .map(|i| {
                    locs.get(i)
                        .and_then(|(s, e)| text.get(s..e))
                        .unwrap_or("")
                        .to_string()
                })
                .collect();
            results.push((char_start, char_end, caps));
            if m0.end() == start {
                start += 1;
            } else {
                start = m0.end();
            }
        }
        results
    }

    #[cfg(not(feature = "pcre2"))]
    pub(crate) fn regex_find_all_p5_with_captures(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> Vec<(usize, usize, Vec<String>)> {
        let mut out = Vec::new();
        let mut pos = 0usize;
        while let Some((s, e, caps)) = self.regex_find_first_from_with_captures(pattern, text, pos)
        {
            out.push((s, e, caps));
            pos = if e > s { e } else { s + 1 };
        }
        out
    }

    /// Extract the regex pattern string from a named token/regex definition.
    /// Returns `Some(pattern)` if the token body contains a single regex literal.
    pub(in crate::runtime) fn extract_token_regex_pattern(&self, name: &str) -> Option<String> {
        let defs = self.resolve_token_defs(name)?;
        let def = defs.first()?;
        // Look for a body consisting of a single Expr(Literal(Regex(pat))),
        // skipping SetLine statements.
        let effective: Vec<_> = def
            .body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        if effective.len() == 1
            && let Stmt::Expr(Expr::Literal(lit)) = effective[0]
            && let ValueView::Regex(pat) = lit.view()
        {
            return Some(pat.to_string());
        }
        None
    }
}
