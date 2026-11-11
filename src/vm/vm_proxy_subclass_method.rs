//! A method a `Proxy` subclass declares, called on the Proxy's `.VAR` object
//! (`$attr-var.VAR.is-set`), dispatched to its compiled body directly.
//!
//! `.VAR` on a Proxy answers the decontainerized var object on the pseudo-method
//! lane, and the next call on it is a user method of the subclass. That call
//! has no shape the VM's instance lanes recognize (the receiver is a Proxy,
//! not an Instance), so it ran the interpreter's whole method cascade
//! (`call_method_with_values`: the by-name groups, `native_lever_a_user_override`,
//! `builtin_candidate_outranks_user`) only to reach the same
//! `try_dispatch_compiled_method_direct_as(subclass, ...)` the cascade ends in.
//! AttrX::Mooish's `AttrProxy` is asked `.VAR.is-set`, `.VAR.build-acquire`,
//! `.VAR.store-value` ... several times per lazy-attribute access (#14855).
//!
//! The direct call is the cascade's answer exactly when nothing in front of it
//! can claim the call: no built-in method of that name exists for the subclass
//! (so no built-in candidate outranks the user's, and no native fork applies),
//! the name is not one of the subclass's attribute accessors (those are read
//! from the Proxy's own attribute store), no method is wrapped, and no user
//! `^find_method` intercepts dispatch.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// `method(args)` on a Proxy subclass's `.VAR` object, run as the user
    /// method the subclass declares; `None` for any other receiver or when the
    /// cascade could answer differently.
    // Cost: O(1) for the guard plus the memoized `has_user_method` /
    // `is_native_method` probes, then the method's resolve (cached) and body.
    pub(crate) fn try_proxy_subclass_var_method(
        &mut self,
        target: &Value,
        method_sym: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Proxy {
            subclass: Some((class_sym, attrs)),
            var_object: true,
            ..
        } = target.view()
        else {
            return None;
        };
        let class_sym = *class_sym;
        let method = method_sym.as_str();
        if attrs.as_map().contains_key(method)
            || self.has_any_wrap_chains()
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        let class = class_sym.as_str();
        if !self.has_user_method_sym(class, method_sym) || self.is_native_method(class, method) {
            return None;
        }
        self.try_dispatch_compiled_method_direct_as(class_sym, target, method, args)
    }
}
