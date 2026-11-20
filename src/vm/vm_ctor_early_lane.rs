//! The early form of the default-constructor lane (ADR-0121 D3, #16897).
//!
//! The ctor lane (`vm_ctor_lane`) replays `Class.new(named...)` straight into
//! the native default constructor once the whole `CallMethodMut` probe chain
//! has been observed to land there. Its gate, though, sits *after* the
//! argument stretch at the top of the chain: the arguments are moved off the
//! stack, decoded, gathered, hllized, scanned for a consumed `Seq` and for a
//! deferred `.map`/`.grep`, and the receiver is offered to the lazy-slip and
//! shaped-iteration rewrites. For a plain `Token.new(kind => 'text', :$text)`
//! that stretch is about a quarter of the call.
//!
//! This is the constructor twin of the plain-method lane's early form
//! (`try_plain_method_early_lane`), with the same argument for why it may
//! skip the stretch:
//!
//! - **the memo is the proof**, unchanged: a class enters `ctor_lane` only
//!   from the chain's tail, and the replay requires the very same
//!   `NativeCtorPlan` and registry generation;
//! - **a call whose arguments the stretch would change is refused**, so the
//!   key computed from the arguments as they stand is the late gate's key: a
//!   `|` spread at the call site, a `VarRef`, a value `hllize_for_binding`
//!   rewrites, and a gather or consumed `Seq` (`method_gather_args` reads
//!   those for a built-in `new`);
//! - **the stretch's two non-probe effects are reproduced**: the call's
//!   argument sources are published, and a deferred `.map`/`.grep` argument
//!   is reified;
//! - the receiver is a type object, which neither receiver rewrite touches.
//!
//! A miss leaves the stack as it was and the whole chain runs.

use super::*;

impl Interpreter {
    /// Answer `Class.new(named...)` at the top of `CallMethodMut` when the
    /// ctor lane holds the class (see the module doc). `None`, with the stack
    /// untouched, on a miss.
    // Cost: O(a), a = arguments, to decide; the construction itself is
    // `try_native_default_construct`'s.
    pub(super) fn try_ctor_early_lane(
        &mut self,
        code: &CompiledCode,
        method_sym: crate::symbol::Symbol,
        arity: usize,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if self.caches.ctor_lane.is_empty()
            || method_sym != crate::symbol::wk::new_method()
            || modifier_idx.is_some()
            || quoted
            || self.accessor_ref_pending
            || crate::runtime::find_method_intercept::any_user_find_method()
            || self.registry().method_name_ever_wrapped("new")
        {
            return None;
        }
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        let class_sym = Self::ctor_lane_key(
            &self.stack[target_idx],
            &self.stack[target_idx + 1..],
            None,
            false,
            false,
            method_sym,
        )?;
        let args = &self.stack[target_idx + 1..];
        if args.iter().any(|a| {
            matches!(a.view(), ValueView::VarRef { .. }) || a.needs_hllize_for_binding()
        }) || Self::has_builtin_seq_read(args)
            || !self.ctor_lane_hit(class_sym)
        {
            return None;
        }
        // The stretch's own decode: `true` means a `|` spread at the call site.
        let decoded = self.decode_arg_sources_shared(code, arg_sources_idx);
        if decoded.as_ref().is_some_and(|(_, spread)| *spread) {
            return None;
        }
        let args = self.stack.split_off(target_idx + 1);
        let target = self.stack.pop()?;
        // What the stretch leaves behind for a call with these arguments.
        self.caches.ctor_lane_candidate = None;
        self.caches.plain_method_lane_candidate = None;
        self.set_pending_call_arg_sources_shared(decoded.map(|(names, _)| names));
        if args.iter().any(Self::ctor_arg_may_hold_lazy)
            && let Err(e) = self.reify_map_grep_seq_method_args(&target, method_sym, &args)
        {
            return Some(Err(e));
        }
        match self.try_ctor_lane(class_sym, &args) {
            Some(result) => Some(result.map(|value| self.stack.push(value))),
            None => {
                // The constructor declined these arguments: the whole chain
                // runs on them, as it would have without this lane.
                self.stack.push(target);
                self.stack.extend(args);
                None
            }
        }
    }

    /// Whether a named argument could carry something the deferred-`Seq`
    /// reification touches (`reify_map_grep_seq_args`): a `Seq` or a lazy
    /// list, directly or inside a container. Every other value, the common
    /// case, makes that pass a no-op.
    // Cost: O(1): tag probes, and one container read.
    fn ctor_arg_may_hold_lazy(arg: &Value) -> bool {
        let ValueView::Pair(_, value) = arg.view() else {
            return true;
        };
        let holds_lazy = |v: &Value| v.is_seq_value() || v.is_lazy_list_value();
        holds_lazy(value) || (value.is_container_ref() && holds_lazy(&value.deref_container()))
    }
}
