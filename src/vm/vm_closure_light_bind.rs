//! The **light closure bind** (#8335).
//!
//! A named sub whose signature is plain positionals reaches
//! `Interpreter::call_compiled_function_positional_light_at` and binds its
//! arguments straight into locals slots. A *closure* has no such path:
//! `call_compiled_closure_in_unit` sends every call — a `-> $a { $a }` pointy
//! block included — through the general signature binder
//! (`bind_function_args_values`), which for that signature can only reach one
//! branch: the legacy placeholder path's "plain positional identifier" arm.
//!
//! Reaching that arm costs four `Vec`s (`filtered_args`, `plain_args`, the
//! single-argument-rule re-filter, `positional_args`) and two `@_` arrays —
//! for a bind whose whole content is "itemize the argument and store it under
//! the parameter's key". Measured on the issue's benchmark it was ~4 650 of the
//! ~11 500 instructions a `-> $a { $a }` call cost once #8302 had taken the
//! per-call interning out of the same path.
//!
//! This module is the direct route to that arm. It is *not* a second binder:
//! `Interpreter::closure_light_bind` reproduces the arm's observable effects
//! exactly and declines (returning `false`, so the caller runs the general
//! binder) for every signature or argument list it is not a faithful
//! substitute for. The signature half of that gate is settled once per code
//! object, alongside the parameter symbols it needs, in
//! [`ParamNameSyms::light_bindable`](crate::value::ParamNameSyms::light_bindable)
//! — which also records what each rejected parameter shape would have needed
//! instead.

use super::*;

impl Interpreter {
    /// Bind `args` to `data`'s parameters without the general signature binder,
    /// when both the signature and the arguments make that faithful.
    ///
    /// Returns `true` when the parameters are bound (and the caller must NOT
    /// run the general binder), `false` when nothing was done. It cannot fail:
    /// every condition the general binder would raise on — an arity mismatch
    /// above all — is a *decline* here, so the error still comes from the
    /// binder with its own message and diagnostics.
    ///
    /// The argument half of the gate is about argument *kinds*, not count
    /// alone:
    ///
    /// * a `Pair` argument is a named argument the legacy path filters out of
    ///   the positional list, so the same call binds one fewer positional than
    ///   the arity test here would suggest;
    /// * a `ValuePair` is positional or named depending on
    ///   `legacy_has_plain_positional_param`, and the native `.map`/`.sort`
    ///   loops pass hash elements that way — cheap to refuse, and refusing
    ///   keeps this function's rule "one argument, one parameter, in order".
    pub(super) fn closure_light_bind(
        &mut self,
        data: &crate::value::SubData,
        param_syms: &crate::value::ParamNameSyms,
        args: &[Value],
    ) -> bool {
        if !param_syms.light_bindable {
            return false;
        }
        // Exact arity only. A short or over-supplied call is the general
        // binder's to diagnose (or, for a `^`-placeholder signature, to accept)
        // — this path has no value to give an unbound parameter and no message
        // to report a surplus with.
        if args.len() != param_syms.params.len() {
            return false;
        }
        if args.iter().any(|a| {
            matches!(
                a.unwrap_varref().view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            )
        }) {
            return false;
        }

        // The general binder consumes both pending arg-source channels on
        // every call (`take_pending_call_arg_sources`, and the `mem::take` of
        // `pending_call_arg_source_slots` that feeds `fold_rw_writeback_slots`).
        // Leaving them set would let this call's sources be picked up by an
        // unrelated later writeback in the same frame. No rw binding can arise
        // here — a plain positional `$` parameter is a readonly item binding
        // with no caller container behind it — so the fold itself has nothing
        // to record.
        self.take_pending_call_arg_sources();
        self.pending_call_arg_source_slots.clear();

        // `@_` holds the positionals the signature did NOT consume, which at
        // exact arity is none. The legacy path writes it unconditionally (a
        // fresh, empty array in this frame's overlay), so a body that reaches
        // for `@_` sees its own empty aggregate rather than the caller's
        // through the scope chain.
        self.env_mut().insert_sym(
            crate::symbol::wk::positional_slurpy(),
            Value::array(Vec::new()),
        );

        for (i, sym) in param_syms.params.iter().enumerate() {
            // A plain `$` parameter is an item binding: the legacy path runs
            // the argument through `itemize_scalar_store`, whose name half
            // (`name_is_itemize_exempt`) the `light_bindable` gate already
            // settled — no bare identifier is exempt.
            let val = Self::itemize_scalar_store_value(crate::runtime::types::unwrap_varref_value(
                args[i].clone(),
            ));
            // A `Callable` bound to a `$` parameter is also published under its
            // `&`-sigiled spelling (`bind_param_value_sym` does the same). Only
            // that one branch of that function can fire for a bare identifier:
            // the `@`-re-homing, the `$!attr`/`$.attr` attribute mirrors and the
            // placeholder twigil aliases all key off a sigil or twigil the name
            // cannot have.
            if matches!(
                val.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) {
                let amp = format!("&{}", data.params[i]);
                self.env_mut().insert(amp, val.clone());
            }
            self.env_mut().insert_sym_noting(*sym, val);
        }
        true
    }
}
