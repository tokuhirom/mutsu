//! `.gist` for an exception carrying the `X::Promise::Broken` role mixin.
//!
//! `Promise.result` on a broken promise rethrows the promise's cause with
//! `X::Promise::Broken` composed into it (see `methods_promise.rs`). In
//! Rakudo that role overrides `gist` — and only `gist`; `.message` and
//! `.Str` still answer the original cause's text — to explain *why* the
//! exception is surfacing here:
//!
//! ```text
//! Tried to get the result of a broken Promise
//!   in block <unit> at f.raku line 1
//!
//! Original exception:
//!     oh no
//!       in block <unit> at f.raku line 1
//! ```
//!
//! The body after `Original exception:` is the base exception's own gist,
//! indented by four — Rakudo writes it as `callsame().indent(4)`, and this
//! module reproduces that by re-dispatching `gist` on the same value with
//! this one role peeled back out of its mixin map.

use super::*;

/// The role whose presence turns on the wrapper rendering.
pub(crate) const PROMISE_BROKEN_ROLE: &str = "X::Promise::Broken";

const PROMISE_BROKEN_HEADER: &str = "Tried to get the result of a broken Promise";

/// `compose_role_on_value` records a composed role as a set of
/// `__mutsu_role*__<name>` entries in the value's mixin map. These are the
/// keys that belong to one role, so peeling it back out is a key removal.
fn role_mixin_keys(role: &str) -> [String; 3] {
    [
        format!("__mutsu_role__{}", role),
        format!("__mutsu_role_seq__{}", role),
        format!("__mutsu_role_typeargs__{}", role),
    ]
}

impl Interpreter {
    /// The same value with `X::Promise::Broken` peeled back off, or `None`
    /// when it does not carry the role. Any *other* mixed-in role is kept —
    /// only this one is removed, so the re-dispatch below is `callsame()`
    /// rather than "drop every mixin".
    fn promise_broken_mixin_base(target: &Value) -> Option<Value> {
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };
        let keys = role_mixin_keys(PROMISE_BROKEN_ROLE);
        if !mixins.contains_key(&keys[0]) {
            return None;
        }
        let mut remaining = (**mixins).clone();
        for k in &keys {
            remaining.remove(k);
        }
        let inner = inner.as_ref().clone();
        Some(if remaining.is_empty() {
            inner
        } else {
            Value::mixin(inner, remaining)
        })
    }

    /// `.gist` for a `...+{X::Promise::Broken}` exception, or `None` when
    /// `target` does not carry the role (the ordinary dispatch path applies).
    pub(crate) fn promise_broken_gist(
        &mut self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let base = Self::promise_broken_mixin_base(target)?;
        // Rakudo's `callsame()`. The peeled value no longer carries the role,
        // so this cannot recurse back into here.
        let inner = match self.call_method_with_values(base, "gist", vec![]) {
            Ok(v) => v.to_string_value(),
            Err(e) => return Some(Err(e)),
        };

        let mut out = String::from(PROMISE_BROKEN_HEADER);
        if let Some(bt) = Self::exception_backtrace_text(target) {
            out.push('\n');
            out.push_str(bt.trim_end_matches('\n'));
        }
        out.push_str("\n\nOriginal exception:\n");
        out.push_str(&indent_by_four(&inner));
        Some(Ok(Value::str(out)))
    }
}

/// Raku's `Str.indent(4)`: prefix every **non-empty** line with four spaces
/// (an empty line stays empty rather than gaining trailing whitespace).
fn indent_by_four(s: &str) -> String {
    s.split('\n')
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else {
                format!("    {}", line)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn role_keys_cover_the_three_composition_entries() {
        let keys = role_mixin_keys("R");
        assert_eq!(keys[0], "__mutsu_role__R");
        assert_eq!(keys[1], "__mutsu_role_seq__R");
        assert_eq!(keys[2], "__mutsu_role_typeargs__R");
    }

    #[test]
    fn indents_non_empty_lines_only() {
        assert_eq!(indent_by_four("a\n\nb"), "    a\n\n    b");
        assert_eq!(indent_by_four("x"), "    x");
    }
}
