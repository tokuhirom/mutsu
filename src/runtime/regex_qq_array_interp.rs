//! `@name` inside a double-quoted regex literal (`/"x @a[]"/`). The literal
//! follows qq-string rules: a bare `@a` is literal text, `@a[]` / `@a{}` /
//! `@a<>` (a zen slice) interpolate the space-joined elements. Outside the
//! literal a bare `@a` is an alternation over the elements instead, which is
//! what `interpolate_regex_scalars`' `@` arm does.

use super::*;
use crate::runtime::meta_ns::MetaNs;

impl Interpreter {
    /// Handle the `@` at `chars[at]` (inside a double-quoted regex literal),
    /// appending to `out` and returning the position after it, or `None` to
    /// leave it to the caller's ordinary `@name` handling.
    // Cost: O(n + |@name|), n = the name's length.
    pub(super) fn interpolate_qq_array_in_regex(
        &self,
        chars: &[char],
        at: usize,
        out: &mut String,
    ) -> Option<usize> {
        let name_start = at + 1;
        if !chars
            .get(name_start)
            .is_some_and(|c| c.is_alphabetic() || *c == '_')
        {
            return None;
        }
        let mut j = name_start;
        while j < chars.len() && (chars[j].is_alphanumeric() || matches!(chars[j], '_' | '-')) {
            j += 1;
        }
        match (chars.get(j), chars.get(j + 1)) {
            (Some('['), Some(']')) | (Some('{'), Some('}')) | (Some('<'), Some('>')) => {
                let bare_name: String = chars[name_start..j].iter().collect();
                let sigiled_name = format!("@{bare_name}");
                let value = super::regex::regex_helpers::interp_closure_scope_get(&sigiled_name)
                    .or_else(|| self.env.get(&sigiled_name).cloned())
                    .unwrap_or(Value::NIL)
                    .into_descalarized()
                    .into_deref();
                let joined = match value.view() {
                    ValueView::Array(arr, _) => join_str(arr.iter()),
                    ValueView::Seq(items) => join_str(items.iter()),
                    ValueView::Slip(items) => join_str(items.iter()),
                    _ => value.to_string_value(),
                };
                Self::push_value_as_regex_pattern(&Value::str(joined), out);
                Some(j + 2)
            }
            // A subscripted `@a[0]` / `@a{'k'}` or a `"@a.join(',')"` call in a
            // regex literal is lowered to a compiled qq thunk
            // (`splice_regex_qq_thunk_result`) and never reaches here.
            // TODO: `s///`, `token`/`rule` bodies and `<$re>` still take the
            // bare-`@name` alternation path here (#9673).
            (Some('[' | '{' | '<'), _) => None,
            // A bare `@name` is literal text in a qq string (so is `@a.foo`
            // without a trailing call).
            _ => {
                out.push('@');
                Some(at + 1)
            }
        }
    }
}

impl Interpreter {
    /// Handle the double-quoted atom opening at `chars[at]` when the
    /// compiler lowered it to a qq thunk (`crate::regex_qq_atoms`) and the
    /// regex's installed scope holds the thunk's string result: append the
    /// result as a single-quoted literal to `out` and return the position
    /// after the closing quote. `None` leaves the atom to the text scan.
    // Cost: O(n + |r|), n = the pattern's length (quote-state scan), r = the result.
    pub(super) fn splice_regex_qq_thunk_result(
        &self,
        chars: &[char],
        at: usize,
        out: &mut String,
    ) -> Option<usize> {
        let close = crate::regex_qq_atoms::dq_atom_close(chars, at)?;
        let body: String = chars[at + 1..close].iter().collect();
        if !crate::regex_qq_atoms::body_wants_thunk(&body)
            || super::regex_parse::is_inside_regex_quote_literal(chars, at)
        {
            return None;
        }
        let key = MetaNs::RegexQq.key(Symbol::intern(&body));
        let result = self.env.get_sym(key)?;
        let ValueView::Str(text) = result.view() else {
            return None;
        };
        out.push('\'');
        for c in text.chars() {
            if matches!(c, '\\' | '\'') {
                out.push('\\');
            }
            out.push(c);
        }
        out.push('\'');
        Some(close + 1)
    }
}

thread_local! {
    /// The `"..."` qq thunks (`crate::regex_qq_atoms`) whose results are
    /// installed right now, innermost last, each with its result. One match
    /// can install the same regex's scope more than once (the VM's
    /// smartmatch op, then `smart_match` itself); a nested install of a
    /// thunk that is already active reuses its result, so the thunk runs
    /// once per match as in Rakudo.
    static ACTIVE_REGEX_QQ: std::cell::RefCell<Vec<(Value, Value)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

impl Interpreter {
    /// Evaluate a `"..."` atom's compiled qq thunk for a scope install and
    /// mark it active until the matching [`Self::end_regex_qq_thunk`].
    /// Returns the string result, or `None` (nothing marked) when the thunk
    /// throws — the pre-pass then falls back to its own reading of the atom.
    // Cost: O(a) plus the thunk's own run, a = active thunks (nesting depth).
    pub(crate) fn eval_regex_qq_thunk(&mut self, thunk: &Value) -> Option<Value> {
        let active = ACTIVE_REGEX_QQ.with(|a| {
            a.borrow()
                .iter()
                .rev()
                .find(|(t, _)| t.same_binding(thunk))
                .map(|(_, r)| r.clone())
        });
        let result = match active {
            Some(r) => r,
            None => {
                let r = self.call_sub_value(thunk.clone(), Vec::new(), false).ok()?;
                Value::str(r.to_string_value())
            }
        };
        ACTIVE_REGEX_QQ.with(|a| a.borrow_mut().push((thunk.clone(), result.clone())));
        Some(result)
    }

    /// Undo one [`Self::eval_regex_qq_thunk`] (installs nest strictly, so
    /// the innermost entry is the one being uninstalled).
    // Cost: O(1).
    pub(crate) fn end_regex_qq_thunk() {
        ACTIVE_REGEX_QQ.with(|a| a.borrow_mut().pop());
    }
}

fn join_str<'a>(items: impl Iterator<Item = &'a Value>) -> String {
    items
        .map(|v| v.to_string_value())
        .collect::<Vec<_>>()
        .join(" ")
}
