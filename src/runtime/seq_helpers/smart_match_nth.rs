//! `m:nth(...)//` and `m:x(...)//` against a topic.
//!
//! The adverb arguments are values: the compiler evaluates them where the
//! match runs and patches them onto the regex (`OpCode::PatchRegexAdverbNth`
//! / `PatchRegexAdverbX`). They are resolved here with the resolvers
//! `.match` and `.subst` use, so `m:nth($n)`, `m:nth({2})`, `m:nth(*-1)`,
//! `m:nth(@list)` and `m:x(1..2)` mean what they mean for the methods.

use super::super::*;
use crate::value::ValueView;

impl Interpreter {
    /// The non-overlapping matches of `pattern` in `text`, leftmost first.
    // Cost: O(n) plus per-match engine work, n = chars of `text`.
    fn smart_match_nth_candidates(&mut self, pattern: &str, text: &str) -> Vec<RegexCaptures> {
        let all = self.regex_match_non_overlapping(pattern, text);
        self.select_non_overlapping_matches(all)
    }

    /// `topic ~~ m:nth(NTH)/pattern/`: select the matches `nth` names, set
    /// `$/` to them (one Match for a single index, else a List), and report
    /// whether any was selected. A Junction `:nth` autothreads: `$/` is a
    /// Junction of each eigenstate's Match (or Nil), and the result follows
    /// the Junction's kind. A bad `:nth` (below 1, out of order) is left as
    /// the pending regex error the match raises.
    // Cost: O(n + r) plus per-match engine work and one call of a Callable
    // `:nth`, n = chars of the topic, r = matches.
    pub(super) fn smart_match_nth(&mut self, pattern: &str, nth: &Value, text: &str) -> bool {
        let candidates = self.smart_match_nth_candidates(pattern, text);
        let total = candidates.len();
        if let ValueView::Junction { kind, values } = nth.view() {
            let mut picked = Vec::with_capacity(values.len());
            let mut hits = 0usize;
            for part in values.iter() {
                let one = match self.nth_selection(part, total) {
                    Ok((indices, true)) if indices.len() == 1 => indices.first().copied(),
                    _ => None,
                };
                match one {
                    Some(n) => {
                        hits += 1;
                        let cap = &candidates[n - 1];
                        picked.push(
                            Value::make_match_object_full(
                                cap.from as i64,
                                cap.to as i64,
                                &cap.positional,
                                &cap.named,
                                cap.target_or_new(text),
                            )
                            .with_match_cursor_pos(cap.narrowed_pos()),
                        );
                    }
                    None => picked.push(Value::NIL),
                }
            }
            let parts = picked.len();
            self.env.assign_match_var(Value::junction(kind, picked));
            return match kind {
                JunctionKind::Any => hits > 0,
                JunctionKind::All => hits == parts,
                JunctionKind::One => hits == 1,
                JunctionKind::None => hits == 0,
            };
        }
        let (indices, single) = match self.nth_selection(nth, total) {
            Ok(selection) => selection,
            Err(err) => {
                crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
                    *e.borrow_mut() = Some(err);
                });
                self.clear_match_state();
                return false;
            }
        };
        let selected: Vec<RegexCaptures> =
            indices.iter().map(|&n| candidates[n - 1].clone()).collect();
        if single {
            match selected.first() {
                Some(cap) => {
                    self.apply_single_regex_captures(cap);
                    true
                }
                None => {
                    self.clear_match_state();
                    false
                }
            }
        } else if selected.is_empty() {
            self.clear_multi_match_state();
            false
        } else {
            self.apply_multi_regex_captures(&selected, text);
            true
        }
    }

    /// `topic ~~ m:x(X)/pattern/` without `:g`/`:ov`/`:ex`: at least the
    /// lower bound of `x` non-overlapping matches, of which up to the upper
    /// bound become `$/` (a List). An `x` that is no count or Range of counts
    /// matches nothing.
    // Cost: O(n + r) plus per-match engine work, n = chars of the topic,
    // r = matches.
    pub(super) fn smart_match_x(&mut self, pattern: &str, x: &Value, text: &str) -> bool {
        let candidates = self.smart_match_nth_candidates(pattern, text);
        let Some((lo, hi)) = Self::match_x_bounds(x) else {
            self.clear_multi_match_state();
            return false;
        };
        let Some(selected) = Self::select_matches_by_repeat_bounds(candidates, lo, hi) else {
            self.clear_multi_match_state();
            return false;
        };
        self.apply_multi_regex_captures(&selected, text);
        true
    }

    /// The `(at least, at most)` match counts an `:x` value asks for, or
    /// `None` for a value that is no count or Range of counts (`:x("z")`) or
    /// an empty Range.
    // Cost: O(1).
    pub(crate) fn match_x_bounds(x: &Value) -> Option<(usize, Option<usize>)> {
        if !Self::is_valid_match_x_arg(x) {
            return None;
        }
        Self::parse_match_repeat_bounds(x)
    }
}
