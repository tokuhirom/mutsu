//! A direct lane for an argument-less `callsame` / `nextsame` call
//! ([#10110](https://github.com/tokuhirom/mutsu/issues/10110)).
//!
//! Both names are core routines that a program almost never redefines, but a
//! `CallFunc` for them used to walk the whole generic call funnel first: the
//! compiled-function and multi-candidate lookups, `resolve_function_with_types`,
//! the native-function tables and the lexical `&name` probes, each of which
//! answered "not a user routine" before the interpreter terminal reached the
//! builtin. In FiniteField's `multi infix:<*>(UInt $a, UInt $b) { callsame()
//! mod $*modulus }` that funnel was about 10k instructions per operator call,
//! against ~2k for the re-dispatch itself.
//!
//! The lane runs exactly what that terminal runs (`vm_call_function` inside a
//! carrier region, then the same writebacks) once it has checked that nothing
//! the funnel consults could shadow the builtin: a lexical or env `&callsame`
//! binding, a user routine, multi or proto of the name, a wrap, or an EXPORT
//! hook override. Any of those sends the call down the generic path.

use super::*;

impl Interpreter {
    /// Run an argument-less `callsame()` / `nextsame()` straight on the
    /// builtin when no user binding shadows it. `arity` counts the call's
    /// stack arguments: none, or the one call-site line marker the parser
    /// stamps on an empty `name()` argument list. `None` when the call is not
    /// one, or something might shadow it; the caller then dispatches the
    /// call normally with the stack untouched.
    // Cost: O(l + d), l = locals declared under the `&` name in `code`
    // (normally 0), d = the env scope-chain depth of the `&` key lookup; the
    // registry probes are generation-cached. The re-dispatch itself is the
    // builtin's own cost.
    pub(super) fn try_dispatcher_builtin_call(
        &mut self,
        code: &CompiledCode,
        name_sym: Symbol,
        arity: u32,
    ) -> Option<Result<(), RuntimeError>> {
        let amp_sym = if name_sym == crate::symbol::wk::callsame() {
            crate::symbol::wk::amp_callsame()
        } else if name_sym == crate::symbol::wk::nextsame() {
            crate::symbol::wk::amp_nextsame()
        } else {
            return None;
        };
        let callsite_line = match arity {
            0 => None,
            1 => {
                let marker = self.stack.last()?;
                if !Self::is_callsite_line_marker(marker) {
                    return None;
                }
                Self::peek_callsite_line(std::slice::from_ref(marker))
            }
            _ => return None,
        };
        let name = name_sym.as_str();
        if self.dispatcher_builtin_shadowed(code, name, name_sym, amp_sym) {
            return None;
        }
        if arity == 1 {
            self.stack.pop();
        }
        // The interpreter terminal of `dispatch_func_call_inner`, unchanged:
        // a carrier region around `call_function`, whose by-name env writes
        // are drained into this frame's slots.
        loan_env!(self, set_pending_callsite_line(callsite_line));
        crate::vm::vm_stats::record_function_carrier(name);
        let reg_gen_before = self.registry_write_generation();
        let carrier_saved = self.begin_carrier();
        let result = self.vm_call_function(name, Vec::new());
        let written = self.end_carrier(carrier_saved);
        self.writeback_carrier_writes(code, &written);
        if self.registry_write_generation() != reg_gen_before {
            self.invalidate_fn_resolution();
        }
        let result = result.and_then(|value| loan_env!(self, maybe_fetch_rw_proxy(value, true)));
        // `exec_call_func_op_inner`'s epilogue, on success and on error alike.
        self.apply_pending_rw_writeback(code);
        Some(result.map(|value| self.stack.push(value)))
    }

    /// Whether anything the generic call funnel consults before the builtin
    /// could answer a call of `name` (`callsame` / `nextsame` / `make`)
    /// instead.
    // Cost: see `try_dispatcher_builtin_call`.
    pub(super) fn dispatcher_builtin_shadowed(
        &mut self,
        code: &CompiledCode,
        name: &str,
        name_sym: Symbol,
        amp_sym: Symbol,
    ) -> bool {
        !code.local_slots_named(amp_sym.as_str()).is_empty()
            || self.env().get_sym(amp_sym).is_some()
            || self.has_declared_function_cached_sym(name, name_sym)
            || self.has_multi_candidates_cached_sym(name_sym)
            || self.has_proto_cached_sym(name, name_sym)
            || self.wrapped_sub_for_call(name).is_some()
            || self.export_hook_callable(name, name_sym, code).is_some()
    }
}
