//! Invoking a Supply callback with the values a supply emitted.
//!
//! mutsu tells a call-site named argument from a positional pair *value* by the
//! `Value` variant: a `Pair` in an argument vector binds as a named argument,
//! a `ValuePair` binds positionally (see
//! [`crate::runtime::utils::pair_as_positional`]). A value a supply emits is
//! always ONE positional argument of whatever consumes it — a `tap`/`whenever`
//! body, a `.map`/`.grep`/`.first` transform, a `.reduce`/`.produce` step, a
//! `.unique(:as, :with)` key function, a `zip(:with)` combiner. Passing an
//! emitted `"stdout" => $line` Pair straight through bound it as a *named*
//! argument instead, so `$supply.map(*.value)` (and `-> $_ { }`) saw no
//! positional at all and topicalized whatever `$_` was lying around (#8825).

use super::*;

impl Interpreter {
    /// Call `func` with `args` — values emitted by a supply — bound
    /// positionally: a `Pair` among them is passed as a positional pair value
    /// rather than siphoned off as a named argument.
    // Cost: O(n), n = args.len() (plus the callee's own cost).
    pub(in crate::runtime) fn call_supply_callback(
        &mut self,
        func: Value,
        args: Vec<Value>,
        merge_all: bool,
    ) -> Result<Value, RuntimeError> {
        self.call_sub_value(func, emitted_args_positional(args), merge_all)
    }
}

/// Rewrite every `Pair` in `args` to its positional `ValuePair` form, leaving
/// every other value untouched.
// Cost: O(n), n = args.len().
pub(in crate::runtime) fn emitted_args_positional(mut args: Vec<Value>) -> Vec<Value> {
    for arg in args.iter_mut() {
        if matches!(arg.view(), ValueView::Pair(..)) {
            *arg = crate::runtime::utils::pair_as_positional(arg);
        }
    }
    args
}
