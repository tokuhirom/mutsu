//! The one-argument rule for the set operators in a reduction.
//!
//! Raku's reduction one-arg rule is `[op]($x)` == `op($x)`, and every set
//! operator declares a genuine one-argument candidate that *coerces*:
//!
//! ```text
//! multi sub infix:<(|)>(QuantHash:D \a) { a }        # (&), (^) likewise
//! multi sub infix:<(|)>(Any        \a) { a.Set }
//! ```
//!
//! so `[(|)] 3` is `Set.new(3)` and not the bare `3`. The candidate set is
//! per-operator (`(+)` promotes to `Bag`, `(-)` demotes a mutable operand to
//! its immutable counterpart, `(.)` keeps mutability), which is why this is a
//! table rather than one rule.

use super::*;

/// Where a value sits in the Set < Bag < Mix tower, and whether it is the
/// mutable (`…Hash`) spelling. `None` means "not a QuantHash at all" — a
/// scalar, a list, a `Pair`, a `Hash`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum QuantLevel {
    Set,
    Bag,
    Mix,
}

fn quant_level(v: &Value) -> Option<(QuantLevel, bool)> {
    match v.view() {
        ValueView::Set(_, mutable) => Some((QuantLevel::Set, mutable)),
        ValueView::Bag(_, mutable) => Some((QuantLevel::Bag, mutable)),
        ValueView::Mix(_, mutable) => Some((QuantLevel::Mix, mutable)),
        _ => None,
    }
}

impl Interpreter {
    /// Whether `op` is one of the six set operators, in either spelling.
    pub(crate) fn is_set_reduction_op(op: &str) -> bool {
        matches!(
            op,
            "(-)"
                | "\u{2216}"
                | "(|)"
                | "\u{222A}"
                | "(&)"
                | "\u{2229}"
                | "(^)"
                | "\u{2296}"
                | "(.)"
                | "\u{228D}"
                | "(+)"
                | "\u{228E}"
        )
    }

    /// `[op] $x` for a set operator and a single operand: the coercion the
    /// operator's one-argument candidate performs. Verified against raku
    /// v2026.07 for every (operator, operand-type) pair below; see
    /// `t/set-reduction-one-arg.t`.
    pub(crate) fn set_reduction_one_arg(
        &mut self,
        op: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let level = quant_level(&value);
        let target = match op {
            // Set-level operators: a QuantHash passes through untouched (a
            // `BagHash` stays a `BagHash`), anything else becomes a `Set`.
            "(|)" | "\u{222A}" | "(&)" | "\u{2229}" | "(^)" | "\u{2296}" => {
                if level.is_some() {
                    return Ok(value);
                }
                "Set"
            }
            // `(-)` keeps the tower level but always yields the IMMUTABLE
            // spelling: `[(-)] <a b>.SetHash` is a `Set`, not a `SetHash`.
            "(-)" | "\u{2216}" => match level {
                Some((QuantLevel::Mix, _)) => "Mix",
                Some((QuantLevel::Bag, _)) => "Bag",
                _ => "Set",
            },
            // `(+)` is baggy: it promotes a `Set`/`SetHash`/plain value to
            // `Bag`, and like `(-)` yields the immutable spelling.
            "(+)" | "\u{228E}" => match level {
                Some((QuantLevel::Mix, _)) => "Mix",
                _ => "Bag",
            },
            // `(.)` is baggy too but PRESERVES mutability, so a `SetHash`
            // becomes a `BagHash` rather than a `Bag`, and a `BagHash`/
            // `MixHash` passes through untouched.
            "(.)" | "\u{228D}" => match level {
                Some((QuantLevel::Mix, _)) | Some((QuantLevel::Bag, _)) => return Ok(value),
                Some((QuantLevel::Set, true)) => "BagHash",
                _ => "Bag",
            },
            _ => return Ok(value),
        };
        self.try_compiled_method_or_interpret(value, target, vec![])
    }
}
