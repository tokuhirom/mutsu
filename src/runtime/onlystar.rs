//! The onlystar term `{*}`, resolved from the dynamic call chain (#10746).
//!
//! Rakudo has no lexical notion of where a `{*}` belongs: it asks the dynamic
//! scope for the nearest caller that has a dispatcher. A proto body has one
//! that dispatches to its candidates; a method call, a multi candidate and a
//! wrapper have one that a `{*}` gets `Nil` from; a plain `sub` and a block
//! have none and are looked through. With no dispatcher in the chain at all,
//! `{*}` dies with `X::NoDispatcher`.
//!
//! mutsu keeps that chain as three facts it already maintains or can count
//! in O(1): the stack of proto bodies (`proto_dispatch_stack`), the multi and
//! wrap deferral frames (stamped with the shared `dispatch_token`), and the
//! number of method calls in progress (`method_call_depth`).

use super::*;

impl Interpreter {
    /// Enter a proto body: push the frame its `{*}` dispatches from, stamped
    /// with the current dispatch token and method-call depth so a `{*}` can
    /// tell whether a dispatcher-bearing routine was entered since (#10746).
    /// `method_ctx` is `Some` for a `proto method` body.
    // Cost: O(1).
    pub(crate) fn push_proto_dispatch_frame(
        &mut self,
        name: String,
        args: Vec<Value>,
        method_ctx: Option<ProtoMethodCtx>,
    ) {
        let dispatch_token = self.next_dispatch_token();
        self.proto_dispatch_stack.push(ProtoDispatchFrame {
            name,
            args,
            method_ctx,
            dispatch_token,
            method_depth: self.method_call_depth,
        });
    }

    /// Leave the proto body entered by `push_proto_dispatch_frame`.
    // Cost: O(1).
    pub(crate) fn pop_proto_dispatch_frame(&mut self) {
        self.proto_dispatch_stack.pop();
    }

    /// Run `f` as a method call: a `{*}` reached from inside it finds this
    /// call's dispatcher before any enclosing proto body's (#10746).
    // Cost: O(1) plus `f`.
    #[inline]
    pub(crate) fn in_method_call<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.enter_method_call();
        let result = f(self);
        self.leave_method_call();
        result
    }

    /// Open a method call for `resolve_onlystar`; pair with
    /// `leave_method_call` on every path, the error path included.
    // Cost: O(1).
    #[inline]
    pub(crate) fn enter_method_call(&mut self) {
        self.method_call_depth += 1;
    }

    /// Close the method call opened by `enter_method_call`.
    // Cost: O(1).
    #[inline]
    pub(crate) fn leave_method_call(&mut self) {
        self.method_call_depth -= 1;
    }

    /// What a `{*}` executing now means, decided from the dynamic call chain
    /// as rakudo does (#10746): it reaches the nearest caller that has a
    /// dispatcher. `Ok(Some(frame))` -- that caller is the innermost proto
    /// body, so `{*}` dispatches with its arguments. `Ok(None)` -- it is a
    /// method call, a multi candidate or a wrapper entered after the proto
    /// body (or with no proto body at all), so `{*}` evaluates to `Nil`. `Err`
    /// -- no caller has a dispatcher: `X::NoDispatcher`.
    ///
    /// A plain `sub` and a block push none of these, so they are transparent:
    /// `sub f2 { {*} }; proto f($) { f2() }` dispatches `f`.
    // Cost: O(1).
    pub(crate) fn resolve_onlystar(&self) -> Result<Option<ProtoDispatchFrame>, RuntimeError> {
        let deferral_token = self
            .multi_dispatch_stack
            .last()
            .map(|entry| entry.4)
            .max(self.wrap_dispatch_stack.last().map(|f| f.dispatch_token));
        if let Some(frame) = self.proto_dispatch_stack.last() {
            let shadowed = frame.method_depth != self.method_call_depth
                || deferral_token.is_some_and(|t| t > frame.dispatch_token);
            return Ok((!shadowed).then(|| frame.clone()));
        }
        if self.method_call_depth > 0 || deferral_token.is_some() {
            return Ok(None);
        }
        let routine = self
            .routine_stack
            .iter()
            .rev()
            .find(|f| !f.is_block)
            .map(|f| f.name.resolve());
        Err(Self::no_dispatcher_error(
            routine.as_deref().unwrap_or("<unit>"),
        ))
    }
}
