//! `@name` inside a double-quoted regex literal (`/"x @a[]"/`). The literal
//! follows qq-string rules: a bare `@a` is literal text, `@a[]` / `@a{}` /
//! `@a<>` (a zen slice) interpolate the space-joined elements. Outside the
//! literal a bare `@a` is an alternation over the elements instead, which is
//! what `interpolate_regex_scalars`' `@` arm does.

use super::*;

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
            // TODO: a subscripted `@a[0]` / `@a{'k'}` in a double-quoted regex
            // literal should interpolate its (qq-joined) result; it still takes
            // the bare-`@name` alternation path, which is only right for a
            // single-element result.
            (Some('[' | '{' | '<'), _) => None,
            // A bare `@name` is literal text in a qq string (so is `@a.foo`
            // without a trailing call; TODO: `"@a.join(',')"` should call it).
            _ => {
                out.push('@');
                Some(at + 1)
            }
        }
    }
}

fn join_str<'a>(items: impl Iterator<Item = &'a Value>) -> String {
    items
        .map(|v| v.to_string_value())
        .collect::<Vec<_>>()
        .join(" ")
}
