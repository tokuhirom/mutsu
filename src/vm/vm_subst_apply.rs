use super::vm_string_regex_ops::*;
use super::vm_subst_exec::SubstOp;
use super::vm_subst_repl::{ReplPart, SubstMatchCaps, expand_capture_parts};
use super::*;
use crate::ast::Stmt;

impl Interpreter {
    /// Create a Match object for a substitution match, including its positional
    /// (`$0`, `$1`, ...) and named (`$<name>`) captures, so the post-`s///` `$/`
    /// exposes them like a plain `m//` match does.
    pub(super) fn make_subst_match(
        text: &str,
        start: usize,
        end: usize,
        caps: &SubstMatchCaps,
    ) -> Value {
        Value::make_match_object_with_captures(
            start as i64,
            end as i64,
            &caps.positional,
            &caps.named,
            crate::runtime::MatchTarget::new(text),
        )
    }

    pub(super) fn select_substitution_ranges(
        all_matches: &[(usize, usize)],
        nth_spec: Option<&str>,
        x_spec: Option<&str>,
    ) -> Result<Vec<(usize, usize)>, RuntimeError> {
        // `:x(N)` requires exactly N matches; `:x(lo..hi)` requires at least
        // `lo` and keeps at most `hi`. Returns `(lo, hi)`.
        let x_bounds = x_spec.map(Self::parse_subst_x_spec);
        if let Some(raw) = nth_spec {
            // :nth may carry a comma-separated list of 1-based indices, e.g.
            // `:nth(1,3)`. Indices must be >= 1 and monotonically increasing.
            let nth_list = Self::parse_subst_nth_spec(raw, all_matches.len())?;
            let mut selected: Vec<(usize, usize)> = Vec::new();
            for &n in &nth_list {
                if n <= all_matches.len() {
                    let range = all_matches[n - 1];
                    if !selected.contains(&range) {
                        selected.push(range);
                    }
                }
            }
            // When combined with :x(lo..hi), require at least `lo` and keep up
            // to `hi` of the selected matches.
            if let Some((lo, hi)) = x_bounds {
                if selected.len() < lo {
                    return Ok(Vec::new());
                }
                selected.truncate(hi);
            }
            return Ok(selected);
        }
        if let Some((lo, hi)) = x_bounds {
            if hi == 0 || all_matches.len() < lo {
                return Ok(Vec::new());
            }
            return Ok(all_matches.iter().copied().take(hi).collect());
        }
        Ok(all_matches.first().copied().into_iter().collect())
    }

    /// Parse a `:x` adverb spec into `(lo, hi)` match-count bounds. A bare count
    /// `"3"` is `(3, 3)`; a range `"1..3"` / `"1..^3"` maps to its endpoints;
    /// `"*"`/`"Inf"` is unbounded `(0, usize::MAX)`.
    fn parse_subst_x_spec(raw: &str) -> (usize, usize) {
        let token = raw.trim();
        if token == "*" || token.eq_ignore_ascii_case("Inf") {
            return (0, usize::MAX);
        }
        let parse_bound = |s: &str| s.trim().parse::<i64>().ok();
        if let Some(idx) = token.find("..") {
            let lo_s = &token[..idx];
            let mut rest = &token[idx + 2..];
            let excl = rest.starts_with('^');
            if excl {
                rest = &rest[1..];
            }
            let lo = parse_bound(lo_s).unwrap_or(0).max(0) as usize;
            let hi = if rest.trim() == "*" || rest.trim().eq_ignore_ascii_case("Inf") {
                usize::MAX
            } else {
                match parse_bound(rest) {
                    Some(h) if h >= 0 => {
                        let h = h as usize;
                        if excl { h.saturating_sub(1) } else { h }
                    }
                    _ => usize::MAX,
                }
            };
            return (lo, hi);
        }
        match parse_bound(token) {
            Some(n) if n >= 0 => (n as usize, n as usize),
            _ => (0, usize::MAX),
        }
    }

    /// Parse an `:nth` spec into a list of 1-based indices. Accepts a single
    /// integer or a comma-separated list (e.g. `1,3`). Validates that every
    /// index is >= 1 and that the list is monotonically increasing.
    fn parse_subst_nth_spec(raw: &str, total: usize) -> Result<Vec<usize>, RuntimeError> {
        let token = raw.trim();
        if token.eq_ignore_ascii_case("-Inf") {
            return Err(RuntimeError::new("Invalid :nth index (-Inf)"));
        }
        let mut out: Vec<usize> = Vec::new();
        let mut prev: i64 = 0;
        for part in token.split(',') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            // `*` is the last match and `*-N` counts back from it, resolved
            // against the match count. A Whatever-derived index that falls out
            // of range selects nothing (rather than erroring like a literal).
            let (n, from_whatever) = if part == "*" {
                (total as i64, true)
            } else if let Some(rest) = part.strip_prefix("*-") {
                let sub = rest
                    .trim()
                    .parse::<i64>()
                    .map_err(|_| RuntimeError::new(format!("Invalid :nth index ({part})")))?;
                (total as i64 - sub, true)
            } else {
                let n = part
                    .parse::<i64>()
                    .map_err(|_| RuntimeError::new(format!("Invalid :nth index ({part})")))?;
                (n, false)
            };
            if from_whatever && (n < 1 || n as usize > total) {
                continue;
            }
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
            out.push(n as usize);
        }
        if out.is_empty() {
            return Err(RuntimeError::new(format!("Invalid :nth index ({token})")));
        }
        Ok(out)
    }

    pub(super) fn apply_substitutions(
        text: &str,
        ranges: &[(usize, usize)],
        replacement: &str,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
    ) -> String {
        let mut out = String::new();
        let mut prev_end_b = 0usize;
        for (start, end) in ranges {
            let start_b = runtime::char_idx_to_byte(text, *start);
            let end_b = runtime::char_idx_to_byte(text, *end);
            out.push_str(&text[prev_end_b..start_b]);
            let matched_text = &text[start_b..end_b];
            let repl = apply_subst_case_transforms(
                replacement,
                matched_text,
                samecase,
                samemark,
                sigspace,
                samespace,
            );
            out.push_str(&repl);
            prev_end_b = end_b;
        }
        out.push_str(&text[prev_end_b..]);
        out
    }

    /// Build a substitution output whose replacement interpolates. The
    /// replacement expression (parsed once under `qq` rules, see
    /// `vm_subst_repl`) is evaluated *per match*, with `$/` bound to that
    /// match -- which is what makes `$0`, `$<name>`, `%h{$/}` and `{ ... }`
    /// blocks see the right capture values.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn apply_substitutions_dynamic(
        &mut self,
        text: &str,
        ranges: &[(usize, usize)],
        body: &[Stmt],
        cache_id: u64,
        capture_parts: Option<&[ReplPart]>,
        per_match_captures: &[SubstMatchCaps],
        op: &SubstOp,
    ) -> Result<String, RuntimeError> {
        // Snapshot the env entries we overwrite so we can restore them after.
        let saved_slash = self.env().get("/").cloned();
        let cap_names = Self::subst_capture_env_names(per_match_captures);
        let saved_caps: Vec<(String, Option<Value>)> = cap_names
            .into_iter()
            .map(|n| {
                let old = self.env().get(&n).cloned();
                (n, old)
            })
            .collect();

        let mut out = String::new();
        let mut prev_end_b = 0usize;
        let empty = SubstMatchCaps::default();
        let mut result = Ok(());
        for (i, (start, end)) in ranges.iter().enumerate() {
            let start_b = runtime::char_idx_to_byte(text, *start);
            let end_b = runtime::char_idx_to_byte(text, *end);
            out.push_str(&text[prev_end_b..start_b]);
            let matched_text = &text[start_b..end_b];

            let caps = per_match_captures.get(i).unwrap_or(&empty);
            let spliced =
                capture_parts.and_then(|parts| expand_capture_parts(parts, matched_text, caps));
            let interpolated = match spliced {
                Some(text) => Ok(text),
                None => {
                    // Bind `$/` to this match. A `$0` / `$<name>` written in the
                    // replacement's *string* half reads through `$/`; one written
                    // inside an embedded `{ ... }` block is an ordinary variable
                    // lookup, so the numbered captures are also published by name.
                    let match_obj = Self::make_subst_match(text, *start, *end, caps);
                    self.env_mut().insert("/".to_string(), match_obj);
                    for (n, (name, _)) in saved_caps.iter().enumerate() {
                        match caps.positional.get(n) {
                            Some(cap) => {
                                self.env_mut().insert(name.clone(), Value::str(cap.clone()));
                            }
                            None => {
                                self.env_mut().remove(name);
                            }
                        }
                    }
                    self.eval_subst_replacement(body, cache_id)
                }
            };
            match interpolated {
                Ok(interpolated) => {
                    out.push_str(&apply_subst_case_transforms(
                        &interpolated,
                        matched_text,
                        op.samecase,
                        op.samemark,
                        op.sigspace,
                        op.samespace,
                    ));
                }
                Err(e) => {
                    result = Err(e);
                    break;
                }
            }
            prev_end_b = end_b;
        }
        if result.is_ok() {
            out.push_str(&text[prev_end_b..]);
        }

        // Restore overwritten env entries (`$/` is reset to the match list/object
        // by the caller after this returns).
        match saved_slash {
            Some(v) => {
                self.env_mut().insert("/".to_string(), v);
            }
            None => {
                self.env_mut().remove("/");
            }
        }
        for (name, saved) in saved_caps {
            match saved {
                Some(v) => {
                    self.env_mut().insert(name, v);
                }
                None => {
                    self.env_mut().remove(&name);
                }
            }
        }
        result.map(|()| out)
    }

    /// The `$0`, `$1`, ... env names a dynamic replacement may read: always the
    /// first ten (so a stale outer `$3` cannot leak into a replacement whose
    /// match has fewer captures), plus any higher-numbered capture this
    /// substitution actually produced.
    fn subst_capture_env_names(per_match_captures: &[SubstMatchCaps]) -> Vec<String> {
        let max = per_match_captures
            .iter()
            .map(|c| c.positional.len())
            .max()
            .unwrap_or(0)
            .max(10);
        (0..max).map(|n| n.to_string()).collect()
    }
}
