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
    ) -> Result<Value, RuntimeError> {
        // A compiled block runs as bytecode, as a `.map` callback does: its
        // free variables are its capture's, never a same-named lexical of the
        // frame that emitted (#12872). That includes a `whenever` body, whose
        // owned captures ride in its `authoritative_captures`. A `supply`
        // block body keeps the carrier, which hands it its emitter.
        if let ValueView::Sub(data) = func.view()
            && data.compiled_routine.is_none()
            && data
                .compiled_code
                .as_ref()
                .is_some_and(|cc| !cc.is_supply_block_body && cc.supply_emitter_sym.is_none())
        {
            return self.vm_call_on_value(func, emitted_args_positional(args), None);
        }
        self.call_sub_value(func, emitted_args_positional(args))
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
