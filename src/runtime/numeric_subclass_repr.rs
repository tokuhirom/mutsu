//! `.gist` / `.raku` of an instance of a user subclass of `Int`, `Num` or
//! `Rat` (`class R is Rat {}`), where the base type's own renderer does not
//! simply delegate to the native payload.
//!
//! Rakudo's `Int.gist`, `Num.gist` and `Rational.gist` all return `self.Str`,
//! so a subclass that overrides `Str` gists through it. `Int.raku` is
//! `self.Str` and `Num.raku` is `self.Str` plus an `e0` exponent when it has
//! none. `Rational.raku` prints the decimal form only for a `Rat` itself; a
//! subclass renders as `<numerator/denominator>` (`n.0` when the denominator
//! is 1).

use super::*;

impl Interpreter {
    /// The `.gist` / `.raku` of the numeric-subclass instance `target`, whose
    /// native payload is `payload`, when it differs from the payload's own;
    /// `None` leaves it to the payload.
    // Cost: O(1) plus a user `Str` call when the class defines one.
    pub(super) fn numeric_subclass_repr(
        &mut self,
        target: &Value,
        class_name: &str,
        payload: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let raku = matches!(method, "raku" | "perl");
        if let ValueView::Rat(n, d) = payload.view()
            && raku
            && d != 1
        {
            return Some(Ok(Value::str(format!("<{n}/{d}>"))));
        }
        if !(raku || method == "gist") || !self.has_user_method(class_name, "Str") {
            return None;
        }
        // `Rational.raku` never reads `Str`, so only `Int`/`Num` get here
        // for `.raku`.
        if raku && !matches!(payload.view(), ValueView::Int(_) | ValueView::Num(_)) {
            return None;
        }
        let rendered = match self.call_method_with_values(target.clone(), "Str", vec![]) {
            Ok(v) => v.to_string_value(),
            Err(e) => return Some(Err(e)),
        };
        let rendered = match payload.view() {
            ValueView::Num(f) if raku && f.is_finite() && !rendered.contains(['e', 'E']) => {
                format!("{rendered}e0")
            }
            _ => rendered,
        };
        Some(Ok(Value::str(rendered)))
    }
}
