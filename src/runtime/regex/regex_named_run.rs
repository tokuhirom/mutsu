//! A ratcheted `*` / `+` over a `<subrule>`, matched as one possessive scan
//! (the walk's `walk_ratchet_fast_paths`), as one leaf both engines call
//! (ADR-0135 D4, ADR-0117's rule: a primitive has one implementation).
//!
//! The scan resolves the callee once and loops its first end. A silent call
//! (`<.ws>*`) keeps no captures and asks the position-only matcher; a visible
//! one files one node per iteration under the call's name, marked quantified
//! before the first iteration. Neither applies when the call is wrapped, takes
//! arguments or an alias, or its rule does not resolve to a single candidate:
//! the caller then runs the general chain (`grow_one_iter`).

use super::super::*;
use super::regex_helpers::{is_named_atom_no_args, is_silent_named_atom};
use super::regex_trail::CapStore;
use super::regex_zero_width_iter::zero_width_iter_counts;

impl Interpreter {
    /// The ratcheted `atom{min,}` scan at `pos`: `None` when the fast path
    /// does not apply to this call (take the general chain), `Some(None)` when
    /// it applies and fewer than `min` iterations matched, and
    /// `Some(Some((end, delta)))` for the possessive result, `delta` being the
    /// captures the iterations file (empty for a silent call).
    // Cost: O(k·m), k = the iterations matched, m = one match of the callee.
    pub(super) fn regex_named_ratchet_run(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        min: usize,
        pkg: Symbol,
    ) -> Option<Option<(usize, RegexCaptures)>> {
        let wrapped = matches!(atom, RegexAtom::Named(name)
            if self.token_method_has_wrap_chain(pkg.as_str(), &name.spec().lookup_name));
        if is_silent_named_atom(atom)
            && !wrapped
            && let Some((resolved, resolved_pkg, _)) = self.try_resolve_named_to_pattern(atom, pkg)
        {
            // Ratcheted silent Named token (e.g. <.ws>): resolve the pattern
            // once and match directly; silent atoms produce no captures.
            let mut current = pos;
            let mut count = 0usize;
            while current < chars.len() {
                if let Some(end) =
                    self.regex_match_end_from_in_pkg(&resolved, chars, current, resolved_pkg)
                {
                    if end == current && !zero_width_iter_counts(count, min, None) {
                        break;
                    }
                    current = end;
                    count += 1;
                } else {
                    break;
                }
            }
            return Some((count >= min).then(|| (current, RegexCaptures::default())));
        }
        if is_named_atom_no_args(atom)
            && !wrapped
            && let Some((resolved, resolved_pkg, sym)) =
                self.try_resolve_named_to_pattern(atom, pkg)
        {
            // Ratcheted non-silent Named token (e.g. <huge>*): resolve the
            // pattern once and loop directly, accumulating named captures
            // without re-parsing per iteration.
            let capture_name = if let RegexAtom::Named(name) = atom {
                name.trim().to_string()
            } else {
                String::new()
            };
            let mut filed = CapStore::new(RegexCaptures::default());
            if !capture_name.is_empty() {
                filed.insert_named_quantified(capture_name.clone());
            }
            let mut current = pos;
            let mut count = 0usize;
            while current <= chars.len() {
                // One rule invocation per iteration (#9803).
                self.enter_rule_cursor();
                let matched =
                    self.regex_match_end_from_caps_in_pkg(&resolved, chars, current, resolved_pkg);
                let cursor = self.leave_rule_cursor();
                let Some((end, mut inner_caps)) = matched else {
                    break;
                };
                if let Some(cursor) = cursor {
                    inner_caps.set_cursor(cursor);
                }
                if end == current && !zero_width_iter_counts(count, min, None) {
                    break;
                }
                if !capture_name.is_empty() {
                    let mut subcap = inner_caps;
                    if sym.is_some() {
                        subcap.set_sym(sym);
                    }
                    subcap.from = current;
                    subcap.to = end;
                    let subcap = std::sync::Arc::new(subcap.into_cap_node());
                    // This subrule iteration has REDUCED — log it for the
                    // failed-parse action replay, exactly as the general
                    // `build_named_candidates_from_inner` path does.
                    super::regex_helpers::record_reduced_subrule(&capture_name, &subcap);
                    filed.push_named_node(&capture_name, subcap);
                }
                current = end;
                count += 1;
                if current >= chars.len() {
                    break;
                }
            }
            return Some((count >= min).then(|| (current, filed.snapshot())));
        }
        None
    }
}
