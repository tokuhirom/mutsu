//! Matching [`RegexAtom::QqInterp`]: a `"..."` atom whose compiled qq thunk
//! is run when the atom is reached, not spliced into the pattern text before
//! the parse (see the variant's doc comment).
//!
//! Rakudo evaluates a `"..."` atom each time the cursor reaches it, so a code
//! block earlier in the pattern can change what it matches
//! (`/ x { $q = "b" } "$q" /` matches "xb"), and a quantified atom evaluates
//! once per iteration (#14510). Only a body of compile-time constants is
//! evaluated once, up front: Rakudo inlines it into the declarative prefix.

use super::super::*;

impl Interpreter {
    /// What a scope install binds under the qq key `key` for the thunk
    /// `thunk`: the evaluated string for a body of compile-time constants
    /// (the pre-pass splices it in as a declarative literal), else the thunk
    /// itself, which [`Self::match_qq_interp_result`] runs at match time.
    /// `None` when a constant body's thunk throws (the pre-pass then falls
    /// back to its own reading of the atom).
    // Cost: O(b) for the body scan, b = its length, plus a constant body's
    // thunk run.
    pub(crate) fn regex_qq_install_value(&mut self, key: &str, thunk: &Value) -> Option<Value> {
        let body = key.strip_prefix(crate::meta_ns::MetaNs::RegexQq.prefix())?;
        if !self.qq_body_is_constant(body) {
            return Some(thunk.clone());
        }
        let result = self.call_sub_value(thunk.clone(), Vec::new()).ok()?;
        Some(Value::str(result.to_string_value()))
    }

    /// The end of the qq atom keyed `key` when it matches literally at `pos`:
    /// `Some(Some(end))` on a match, `Some(None)` on a mismatch, and `None`
    /// when nothing usable is installed (no binding, or the thunk threw), so
    /// the caller matches the atom's fallback instead. An installed thunk
    /// runs now; an installed string (a constant body) is compared as is.
    // Cost: O(r) plus the thunk's run, r = the result's length.
    pub(super) fn match_qq_interp_result(
        &mut self,
        key: Symbol,
        chars: &[char],
        pos: usize,
        ignore_case: bool,
    ) -> Option<Option<usize>> {
        let installed = self.env.get_sym(key)?.clone();
        let result = if installed.is_str_value() {
            installed
        } else {
            let r = self.call_sub_value(installed, Vec::new()).ok()?;
            Value::str(r.to_string_value())
        };
        let ValueView::Str(text) = result.view() else {
            return None;
        };
        let mut end = pos;
        for want in text.chars() {
            let Some(&got) = chars.get(end) else {
                return Some(None);
            };
            let same = got == want || (ignore_case && got.to_lowercase().eq(want.to_lowercase()));
            if !same {
                return Some(None);
            }
            end += 1;
        }
        Some(Some(end))
    }
}
