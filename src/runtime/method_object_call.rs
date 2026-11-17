//! Invoking a `Method`/`Submethod` object (`.^lookup`, `.^find_method`,
//! `.^methods` results): `$obj.$m(...)`, `$m($obj, ...)`.
//!
//! The object answers `CALL-ME`, which used to be reached only at the tail of
//! the interpreter's instance cascade (`dispatch_instance_and_fallback`), and
//! its owner-bound call (`$obj.Owner::name`) then went through the whole
//! `call_method_with_values` cascade a second time before its qualified arm.
//! AttrX::Mooish's `invoke-opt` does `$invocant.$method(|$params)` on every
//! lazy build, and a plain `$o.$m()` cost ~107k instructions against ~18k for
//! `$o.name()`. The value-call entry (`vm_call_on_value_at`) now calls
//! [`Interpreter::call_method_object_instance`] directly, and the owner-bound
//! call goes straight to the qualified arm (`dispatch_qualified_call`).
//!
//! Neither shortcut can change the answer: a Method object is an `Instance`
//! of the core `Method`/`Submethod` class, whose cascade arms in front of
//! `CALL-ME` are all keyed on other method names or other receiver classes;
//! the entry is skipped while any wrap chain or user `^find_method` exists
//! (those are consulted before dispatch), or when `CALL-ME` has a user
//! definition on the object's class.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// `CALL-ME` on a Method/Submethod `Instance`: a non-dispatcher candidate
    /// carries its callable `Sub`/`Routine` directly under
    /// `__mutsu_method_callable` (`make_method_object_with_owner`/
    /// `make_native_method_object`); a multi dispatcher has none and instead
    /// re-dispatches on the first argument as invocant, mirroring the old
    /// Sub-shaped dispatcher's `sub_multi_method_dispatcher_name` re-dispatch
    /// (verified against `raku`: calling a multi's `.^lookup` result
    /// re-resolves by argument type). `None` when `target` is not a method
    /// object or none of its shapes applies.
    // Cost: O(m) to bind the owner (m = invocant MRO length), plus the call.
    pub(crate) fn call_method_object_instance(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
            if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = target.view()
                && self.is_method_object_class(&class_name.resolve())
            {
                let am = attributes.as_map();
                // The invocant is the value its variable holds: a variable
                // spread through a capture (`sub k(|c) { $m(|c) }`) arrives
                // as its container, which the owner-type test below would
                // not recognise as an instance.
                let decont_args: Vec<Value> = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| if i == 0 { a.clone().into_deref() } else { a.clone() })
                    .collect();
                // A Method object obtained from a method table carries the
                // declaring class and method name alongside its callable
                // payload. Re-enter ordinary method dispatch for that
                // shape so the body's compiled method frame (including
                // its final expression and attribute-cell handling) is
                // used. Calling the payload directly falls back to the
                // AST carrier and loses the value of a final attribute
                // assignment (`.^private_method_table<foo>($obj)`).
                let is_multi_candidate = matches!(
                    am.get("multi").map(Value::view),
                    Some(ValueView::Bool(true))
                );
                let is_method_table_entry = matches!(
                    am.get("__mutsu_method_table_entry").map(Value::view),
                    Some(ValueView::Bool(true))
                );
                if (is_method_table_entry
                    || (matches!(class_name.resolve().as_str(), "Method" | "Submethod")
                        && matches!(
                            am.get("__mutsu_lookup_class").map(Value::view),
                            Some(ValueView::Str(owner))
                                if decont_args.first().is_some_and(|inv| (matches!(inv.view(), ValueView::Instance { .. })
                                    // A type object of a user class binds the
                                    // same way; a core owner's type object
                                    // (`Map.^lookup('new')(MyMap)`) re-enters
                                    // the subclass's own `new` by name.
                                    || (matches!(inv.view(), ValueView::Package(_))
                                        && (self.has_class(owner.as_str()) || self.is_role(owner.as_str()))))
                                    && self.type_matches_value(owner.as_str(), inv))
                        )))
                    && !is_multi_candidate
                    && let Some(ValueView::Str(method_name)) =
                        am.get("__mutsu_lookup_method").map(Value::view)
                    && !decont_args.is_empty()
                {
                    let mut args = decont_args;
                    let invocant = args.remove(0);
                    let is_private = matches!(
                        am.get("__mutsu_private_method").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    let method_name = if is_private {
                        format!("!{}", method_name.as_str())
                    } else {
                        method_name.to_string()
                    };
                    if is_private
                        && let Some(ValueView::Str(owner)) =
                            am.get("__mutsu_lookup_class").map(Value::view)
                    {
                        // Calling a private method through its reflection
                        // table is explicitly authorized by the table
                        // itself, even when the caller is outside the
                        // declaring class. Give the normal private
                        // resolver that lexical owner context so it uses
                        // the compiled method path without weakening
                        // ordinary source-level privacy checks.
                        self.push_method_class_authorizing_private(&owner.to_string());
                        let result = self.call_method_with_values(invocant, &method_name, args);
                        self.pop_method_class();
                        return Some(result);
                    }
                    // The entry is its owner's candidate, not whatever
                    // the invocant's class overrides it with (#10344).
                    if !is_private
                        && let Some(ValueView::Str(owner)) =
                            am.get("__mutsu_lookup_class").map(Value::view)
                        && let Some(qualified) = self.owner_bound_method_name(
                            Symbol::intern(&owner),
                            Symbol::intern(&method_name),
                            &invocant,
                        )
                    {
                        // Straight to the cascade's qualified-call arm: nothing in
                        // front of it answers an owner-qualified name on an
                        // instance of a user class (see the module docs).
                        if let Some(result) = self.dispatch_qualified_call(&invocant, qualified.as_str(), &args) {
                            return Some(result);
                        }
                        return Some(self.call_method_with_values(
                            invocant,
                            qualified.as_str(),
                            args,
                        ));
                    }
                    return Some(self.call_method_with_values(invocant, &method_name, args));
                }
                if let Some(result) = self.try_call_bound_method_object(target, args) {
                    return Some(result);
                }
                if let Some(callable) = am.get("__mutsu_method_callable").cloned() {
                    return Some(self.call_sub_value(callable, args.to_vec()));
                }
                if matches!(
                    am.get("is_dispatcher").map(Value::view),
                    Some(ValueView::Bool(true))
                ) && let Some(ValueView::Str(meth)) =
                    am.get("__mutsu_lookup_method").map(Value::view)
                    && !args.is_empty()
                {
                    let mut args = args.to_vec();
                    let invocant = args.remove(0);
                    // Bound to the owner's candidates, like the entry
                    // above: a by-name re-dispatch would ask the
                    // receiver's method lookup again, which a user
                    // `^find_method` answers with this same dispatcher.
                    if let Some(ValueView::Str(owner)) =
                        am.get("__mutsu_lookup_class").map(Value::view)
                        && let Some(qualified) = self.owner_bound_method_name(
                            Symbol::intern(&owner),
                            Symbol::intern(meth.as_str()),
                            &invocant,
                        )
                    {
                        return Some(self.call_method_with_values(
                            invocant,
                            qualified.as_str(),
                            args,
                        ));
                    }
                    return Some(self.call_method_with_values(invocant, &meth.to_string(), args));
                }
            }

        None
    }

    /// `$obj.$m(...)` / `$m($obj, ...)` from the value-call entry: the
    /// method-object call without the instance cascade in front of it, under
    /// the conditions in the module docs. `None` leaves the call to the
    /// cascade's `CALL-ME` path, which reaches the same function.
    // Cost: O(1) guard, then `call_method_object_instance`.
    pub(crate) fn try_call_method_object_value(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        if !matches!(class_name.as_str(), "Method" | "Submethod")
            || self.has_any_wrap_chains()
            || crate::runtime::find_method_intercept::any_user_find_method()
            || self.has_user_method(class_name.as_str(), "CALL-ME")
        {
            return None;
        }
        self.call_method_object_instance(target, args)
    }

    /// A qualified method call (`$obj.Owner::name(...)`, `self.Mu::Str`) as
    /// the interpreter cascade answers it: the universal-base coercions, then
    /// the qualifier's own dispatch frame around `dispatch_qualified_method`.
    /// `None` when neither takes the call.
    // Cost: O(m) qualifier check (m = invocant MRO length), plus the call.
    pub(crate) fn dispatch_qualified_call(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // Qualified coercion to a universal base (self.Mu::Str, self.Any::gist):
        // route to the built-in default, bypassing the receiver's own override.
        // Must precede the instance/non-instance qualified dispatchers, which would
        // otherwise recurse (type object) or reject it as "not inherited" (instance).
        if let Some(result) = self.dispatch_universal_base_coercion(target, method, args) {
            return Some(result);
        }
        // A qualified call defers along the QUALIFIER's own chain, never the
        // receiver's (#11592): `self.Q::m` with a `callsame` reaches Q's parent,
        // and a role's method (`self.R::m`) has nothing to defer to.
        if method.starts_with('!') {
            return None;
        }
        let (qualifier, actual) = super::methods_qualified::split_method_qualifier_last(method)?;
        let qualifier = self.lexical_env_remap_name(qualifier);
        let qualifier_is_role = self.is_role(&qualifier);
        let pushed = if qualifier_is_role {
            self.push_qualified_method_dispatch_frame(&qualifier, args, target.clone())
        } else {
            match self.push_method_dispatch_frame(&qualifier, actual, args, target.clone(), None) {
                Ok(pushed) => pushed,
                Err(e) => return Some(Err(e)),
            }
        };
        let result = self.dispatch_qualified_method(target, method, args);
        if pushed {
            self.pop_method_dispatch();
        }
        // `push_method_dispatch_frame` always pushes the samewith context
        // (the frame itself is only built when a deferral builtin is named
        // anywhere, `pushed`), so it is taken back whether or not a frame
        // went on. A role's qualified frame pushes none. Left behind, a
        // later `samewith` re-dispatched the qualified method (`Eq.new`
        // after `self.Mu::new`) instead of its own routine.
        if !qualifier_is_role {
            self.pop_method_samewith_context();
        }
        result
    }
}
