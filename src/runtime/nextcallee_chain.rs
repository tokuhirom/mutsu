//! The callable a method's `nextcallee` hands out keeps the rest of the
//! deferral chain (#17501).
//!
//! In rakudo the callee `nextcallee` returns is a dispatcher continuation:
//! calling it runs the next candidate, and a `callsame` inside that candidate
//! continues with the one after it, wherever the callee is called from. Red's
//! `ScopedDriver` calls it from a closure passed to another method
//! (`$!guard.protect: { next(self, |c) }`), by which time other calls have
//! deferral frames of their own on top. A bare routine for the candidate, as
//! `nextcallee` used to return, ran with whatever frame was innermost, so its
//! `callsame` reached nothing (`Nil`) or the wrong chain.
//!
//! The callable is one compiled template, `sub (|c) { ... }`, copied with the
//! chain in its env (under a [`crate::meta_ns::MetaNs::NextcalleeChain`] key
//! no code can name): the receiver class, the candidate, then the
//! continuations after it. The template hands itself (`&?ROUTINE`) to the
//! builtin, which pushes a method deferral frame over the rest of the chain
//! and runs the candidate under it. Each later candidate is a continuation of
//! its own, so a `callsame` in the candidate runs it the same way.

use super::*;

/// The template's source: the callable hands itself to the builtin. It is
/// `is raw`: a dispatcher continuation hands back whatever its candidate
/// returns, so an `is raw`/`is rw` method's container survives the wrapper
/// that called it through `nextcallee` (Method::Protected's
/// `original(self, |c)`).
const TEMPLATE: &str = "sub (|c) is raw { __mutsu_nextcallee_continue(&?ROUTINE, |c) }";

/// The env key of the chain a continuation carries.
// Cost: O(1) after the first call (the key is memoized).
fn chain_key() -> crate::symbol::Symbol {
    crate::meta_ns::MetaNs::NextcalleeChain.key(crate::symbol::Symbol::intern("chain"))
}

impl Interpreter {
    /// The callable for the method candidate `candidate` that continues with
    /// `rest` (each a callable taking the invocant first), dispatched for
    /// `receiver_class`.
    // Cost: O(r), r = candidates left in the chain (copied into the callable).
    pub(crate) fn nextcallee_with_chain(
        &mut self,
        candidate: Value,
        receiver_class: &str,
        rest: Vec<Value>,
    ) -> Value {
        let Some(template) = self.nextcallee_chain_template() else {
            return candidate;
        };
        let ValueView::Sub(data) = template.view() else {
            return candidate;
        };
        let mut chain = Vec::with_capacity(rest.len() + 2);
        chain.push(Value::str(receiver_class.to_string()));
        chain.push(candidate);
        chain.extend(rest);
        let mut bound = crate::value::SubData::clone(&data);
        bound.env.insert_sym(chain_key(), Value::array(chain));
        Value::sub_value(crate::gc::Gc::new(bound))
    }

    // Cost: O(1) after the first call, which parses and compiles the template.
    fn nextcallee_chain_template(&mut self) -> Option<Value> {
        if let Some(template) = &self.caches.nextcallee_chain_template {
            return Some(template.clone());
        }
        let (stmts, _) = crate::runtime::prelude_source::parse_prelude_source(TEMPLATE).ok()?;
        let template = self.eval_block_value(&stmts).ok()?;
        template.as_sub()?;
        self.caches.nextcallee_chain_template = Some(template.clone());
        Some(template)
    }

    /// `__mutsu_nextcallee_continue(&callee, $invocant, *@args)`: run the
    /// chain the callee carries -- its candidate on the invocant and
    /// arguments, under a deferral frame over the rest of the chain.
    // Cost: O(r + a) plus the candidate's call, r = candidates left in the
    // chain, a = arguments.
    pub(super) fn builtin_nextcallee_continue(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut args = args.into_iter();
        let chain = match args.next().as_ref().map(Value::view) {
            Some(ValueView::Sub(data)) => match data.env.get_sym(chain_key()).map(Value::view) {
                Some(ValueView::Array(items, _)) => items.as_slice().to_vec(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        };
        let call_args: Vec<Value> = args.collect();
        let mut chain = chain.into_iter();
        let (Some(receiver_class), Some(candidate)) = (chain.next(), chain.next()) else {
            return Err(RuntimeError::new("__mutsu_nextcallee_continue: bad chain"));
        };
        let invocant = call_args.first().cloned().unwrap_or(Value::NIL);
        let frame = MethodDispatchFrame {
            receiver_class: receiver_class.to_string_value(),
            invocant,
            args: call_args.iter().skip(1).cloned().collect(),
            // Each continuation takes the invocant first, as a wrapper does.
            remaining: chain.map(DeferralEntry::Wrapper).collect(),
            rw_params: Vec::new(),
            dispatch_token: self.next_dispatch_token(),
            arg_sources: None,
            in_wrapper: false,
            lazy_tail: None,
        };
        let depth = self.dispatch.method_dispatch_stack.len();
        self.dispatch.method_dispatch_stack.push(frame);
        let result = self.vm_call_on_value(candidate, call_args, None);
        self.dispatch.method_dispatch_stack.truncate(depth);
        result
    }
}
