//! `.wrap` on a method of a built-in class.
//!
//! `$*OUT.^find_method('print').wrap: method (|c) { ... }` is the classic
//! output-capture idiom (Terminal::MultiProgress's test helper). The wrap lands
//! in `Registry::method_wrap_chains` under `("IO::Handle", "print", 0)`, exactly
//! like a wrap of a user method. A built-in method has no `MethodDef`, though,
//! so the user-method entry sites never see the chain. The native dispatch
//! doors call [`Interpreter::try_builtin_method_wrap`] instead. It runs the
//! chain as a dispatcher wrap does (`runtime::dispatcher_wrap`): the innermost
//! `callsame` re-dispatches the method by name with this chain bypassed, which
//! reaches the native implementation.

use super::*;

impl Interpreter {
    /// Run `method` on `invocant`, an instance of the built-in class
    /// `class_name`, through its `.wrap` chain, if it has one. `None` when the
    /// method is not wrapped, or when this is the chain's own terminal
    /// re-dispatch.
    ///
    // Cost: O(1) when nothing in the program is wrapped; otherwise O(1) to find
    // the chain, plus the wrappers' own calls.
    pub(crate) fn try_builtin_method_wrap(
        &mut self,
        class_name: &str,
        invocant: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let chain = self.builtin_method_wrap_chain(class_name, method)?;
        Some(self.run_builtin_method_wrap(class_name, invocant, method, args, &chain))
    }

    /// The `.wrap` chain of `method` on the built-in class `class_name`, if any.
    /// `None` too inside the chain's own terminal re-dispatch.
    ///
    // Cost: O(1).
    pub(crate) fn builtin_method_wrap_chain(
        &self,
        class_name: &str,
        method: &str,
    ) -> Option<Vec<(u64, Value)>> {
        if !self.has_any_wrap_chains() {
            return None;
        }
        if let Some((name, frames, routines)) = &self.dispatcher_wrap_bypass
            && name == method
            && *frames == self.call_frames.len()
            && *routines == self.routine_stack_len()
        {
            return None;
        }
        self.registry()
            .method_wrap_chain(class_name, method, 0)
            .cloned()
    }

    /// Call `chain`'s outermost wrapper with `[invocant, ...args]`, the rest of
    /// the chain and the native method queued behind it.
    ///
    // Cost: O(w), w = wrappers in the chain, plus their own calls.
    pub(crate) fn run_builtin_method_wrap(
        &mut self,
        class_name: &str,
        invocant: &Value,
        method: &str,
        args: &[Value],
        chain: &[(u64, Value)],
    ) -> Result<Value, RuntimeError> {
        let outermost = chain
            .last()
            .map(|(_, wrapper)| wrapper.clone())
            .unwrap_or(Value::NIL);
        self.push_method_samewith_context(class_name, method, args, Some(invocant.clone()));
        self.push_dispatcher_wrap_frame(class_name, method, args, invocant.clone(), chain);
        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(invocant.clone());
        call_args.extend(args.iter().cloned());
        self.shift_arg_sources_for_wrap_invocant();
        let result = self.call_sub_value(outermost, call_args, false);
        self.pop_method_samewith_context();
        self.method_dispatch_stack.pop();
        result
    }
}
