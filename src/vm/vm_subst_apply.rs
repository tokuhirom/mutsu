use super::vm_string_regex_ops::*;
use super::vm_subst_exec::SubstOp;
use super::vm_subst_repl::{ReplPart, SubstMatchCaps, expand_capture_parts};
use super::*;
use crate::ast::Stmt;

/// How [`Interpreter::apply_substitutions_dynamic`] produces one match's
/// replacement text.
pub(super) enum DynamicRepl<'a> {
    /// A parsed `qq` replacement: see `SubstReplPlan::Dynamic`.
    Body {
        body: &'a [Stmt],
        cache_id: u64,
        capture_parts: Option<&'a [ReplPart]>,
    },
    /// An assignment-form RHS's compiled thunk closure.
    Thunk(&'a Value),
}

impl Interpreter {
    /// Create a Match object for a substitution match, including its positional
    /// (`$0`, `$1`, ...) and named (`$<name>`) captures, so the post-`s///` `$/`
    /// exposes them like a plain `m//` match does.
    /// Every Match of one substitution shares `target`, the subject built once
    /// per call: a fresh target per Match copied the subject and its chars for
    /// every match, and `$/` kept all r copies alive (#9143).
    // Cost: O(captures), a refcount bump of the shared `target`.
    pub(super) fn make_subst_match(
        target: &crate::runtime::MatchTarget,
        start: usize,
        end: usize,
        caps: &SubstMatchCaps,
    ) -> Value {
        if let Some(spans) = &caps.spans {
            return Value::make_match_object_full_visible(
                start as i64,
                end as i64,
                &spans.positional,
                &spans.named,
                target.clone(),
            );
        }
        Value::make_match_object_with_captures(
            start as i64,
            end as i64,
            &caps.positional,
            &caps.named,
            target.clone(),
        )
    }

    /// The matches a substitution with `:nth` value `nth` and `:x` value `x`
    /// replaces, out of `all_matches` (in scan order), and whether that
    /// selection is a single Match rather than a List (a single-index `:nth`,
    /// even under `:g`).
    ///
    /// Both values go through the resolvers `.match` and `.subst` use: a
    /// Callable `:nth` (`*-1`, `{2}`) is called once with the match count
    /// (`resolve_callable_nth`), and the indices come from
    /// `resolve_nth_value_indices`, which also rejects an index below 1 or
    /// out of order. `:x` is a count or a Range of counts
    /// (`parse_match_repeat_bounds`); a value that is neither selects
    /// nothing, as rakudo's `s:x("z")///` replaces nothing.
    // Cost: O(r) plus one call of a Callable `:nth`, r = matches.
    pub(super) fn select_substitution_ranges(
        &mut self,
        all_matches: &[(usize, usize)],
        nth: Option<&Value>,
        x: Option<&Value>,
    ) -> Result<(Vec<(usize, usize)>, bool), RuntimeError> {
        let total = all_matches.len();
        let x_bounds = match x {
            Some(value) if !Self::is_valid_match_x_arg(value) => return Ok((Vec::new(), false)),
            Some(value) => match Self::parse_match_repeat_bounds(value) {
                Some(bounds) => Some(bounds),
                // An empty Range of counts (`:x(3..1)`) can never be met.
                None => return Ok((Vec::new(), false)),
            },
            None => None,
        };
        let mut single = false;
        let mut selected: Vec<(usize, usize)> = match nth {
            Some(spec) => {
                let resolved = self.resolve_callable_nth(spec, total)?;
                single = Self::is_single_nth(&resolved)
                    || matches!(resolved.view(), crate::value::ValueView::Whatever);
                // A Callable's answer outside the matches selects nothing,
                // where a literal index below 1 is an error.
                let from_callable = matches!(spec.view(), crate::value::ValueView::Sub(_));
                if from_callable && Self::callable_nth_out_of_range(&resolved, total) {
                    Vec::new()
                } else {
                    self.resolve_nth_value_indices(&resolved, total)?
                        .into_iter()
                        .filter(|&n| n <= total)
                        .map(|n| all_matches[n - 1])
                        .collect()
                }
            }
            None => all_matches.to_vec(),
        };
        match x_bounds {
            Some((lo, hi)) => {
                if selected.len() < lo {
                    return Ok((Vec::new(), single));
                }
                if let Some(hi) = hi {
                    selected.truncate(hi);
                }
            }
            None if nth.is_none() => selected.truncate(1),
            None => {}
        }
        Ok((selected, single))
    }

    // Cost: O(n + r) plus the replacement text, n = chars of `text`, r = ranges
    // (ascending, converted to bytes by one forward cursor).
    pub(super) fn apply_substitutions(
        text: &str,
        ranges: &[(usize, usize)],
        replacement: &str,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
    ) -> String {
        let mut out = String::with_capacity(text.len());
        let mut prev_end_b = 0usize;
        let mut cursor = runtime::CharByteCursor::new(text);
        for (start, end) in ranges {
            let start_b = cursor.byte_of(*start);
            let end_b = cursor.byte_of(*end);
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
    /// `vm_subst_repl`, or an assignment-form RHS's compiled thunk) is
    /// evaluated *per match*, with `$/` bound to that
    /// match -- which is what makes `$0`, `$<name>`, `%h{$/}` and `{ ... }`
    /// blocks see the right capture values.
    #[allow(clippy::too_many_arguments)]
    // Cost: O(n + r) plus one replacement evaluation per range, n = chars of
    // `text`, r = ranges; every per-match `$/` shares `target`.
    pub(super) fn apply_substitutions_dynamic(
        &mut self,
        text: &str,
        target: &crate::runtime::MatchTarget,
        ranges: &[(usize, usize)],
        repl: DynamicRepl<'_>,
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

        let mut out = String::with_capacity(text.len());
        let mut prev_end_b = 0usize;
        let mut cursor = runtime::CharByteCursor::new(text);
        let empty = SubstMatchCaps::default();
        let mut result = Ok(());
        for (i, (start, end)) in ranges.iter().enumerate() {
            let start_b = cursor.byte_of(*start);
            let end_b = cursor.byte_of(*end);
            out.push_str(&text[prev_end_b..start_b]);
            let matched_text = &text[start_b..end_b];

            let caps = per_match_captures.get(i).unwrap_or(&empty);
            let spliced = match &repl {
                DynamicRepl::Body {
                    capture_parts: Some(parts),
                    ..
                } => expand_capture_parts(parts, matched_text, caps),
                _ => None,
            };
            let interpolated = match spliced {
                Some(text) => Ok(text),
                None => {
                    // Bind `$/` to this match. A `$0` / `$<name>` written in the
                    // replacement's *string* half reads through `$/`; one written
                    // inside an embedded `{ ... }` block is an ordinary variable
                    // lookup, so the numbered captures are also published by name.
                    let match_obj = Self::make_subst_match(target, *start, *end, caps);
                    self.env_mut().insert("/".to_string(), match_obj.clone());
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
                    match &repl {
                        DynamicRepl::Body { body, cache_id, .. } => {
                            self.eval_subst_replacement(body, *cache_id)
                        }
                        DynamicRepl::Thunk(thunk) => self.call_subst_thunk(thunk, match_obj),
                    }
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
