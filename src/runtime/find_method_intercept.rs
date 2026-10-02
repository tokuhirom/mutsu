//! A user `method ^find_method` intercepts every method call on its type.
//!
//! Rakudo resolves `$obj.foo(...)` by asking the receiver's metaobject:
//! `$obj.HOW.find_method($obj, 'foo')`, and invokes whatever Callable comes
//! back with the invocant and the arguments. A class that declares `method
//! ^find_method(Mu \type, Str:D $name)` adds that method to its own
//! metaclass, so *its* `find_method` answers -- for every name, including the
//! ones the class (or `Mu`) does declare. Object::Trampoline builds a lazy
//! proxy on exactly this: every call is routed to one proto handler.
//!
//! Metamethod calls (`.^name`) go to the metaobject and private calls
//! (`self!priv`) to `find_private_method`, not through
//! `find_method`, and the macro-like `.WHAT`/`.HOW`/`.VAR`/`.WHO`/
//! `.DEFINITE`/`.REPR` never consult method lookup at all; those keep their
//! ordinary path.

use super::*;
use std::sync::atomic::{AtomicBool, Ordering};

/// Whether any registered type declares a `^find_method` metamethod. Set-only
/// and process-global, like `raw_invocant`'s mirror: the gate runs on every
/// method call, and an over-set only makes the (correct) per-class probe run.
static ANY_USER_FIND_METHOD: AtomicBool = AtomicBool::new(false);

/// The metamethod name a user `method ^find_method` is registered under.
pub(crate) const USER_FIND_METHOD: &str = "^find_method";

/// Raise the flag. Called by the registry's method-table writers when a
/// `^find_method` row is installed.
pub(crate) fn note_user_find_method() {
    ANY_USER_FIND_METHOD.store(true, Ordering::Relaxed);
}

/// Whether any type declares `^find_method` (the cheap pre-gate; see
/// [`ANY_USER_FIND_METHOD`]).
#[inline]
pub(crate) fn any_user_find_method() -> bool {
    ANY_USER_FIND_METHOD.load(Ordering::Relaxed)
}

/// The method names Rakudo compiles as macros rather than method calls, so a
/// user `find_method` never sees them.
/// A package-qualified call (`$obj.Foo::bar`, and the `Class::name` form a
/// Method object's `CALL-ME` re-dispatches through) names its candidate
/// explicitly and does not ask the receiver's `find_method` either.
fn bypasses_method_lookup(method: &str) -> bool {
    method.starts_with(['^', '!'])
        || crate::qualified::is_qualified(Symbol::intern(method))
        || matches!(
            method,
            "WHAT" | "HOW" | "VAR" | "WHO" | "DEFINITE" | "REPR" | "WHERE"
        )
}

impl Interpreter {
    /// Route `target.method(|args)` through the receiver's user
    /// `^find_method` when it has one: [`Self::user_find_method_lookup`],
    /// then [`Self::invoke_user_found_method`]. `None` when the receiver's
    /// type declares no `^find_method` (the ordinary dispatch then runs).
    // Cost: as `user_find_method_lookup`, plus the Callable it returns.
    pub(crate) fn try_user_find_method_dispatch(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let found = match self.user_find_method_lookup(target, method)? {
            Ok(found) => found,
            Err(e) => return Some(Err(e)),
        };
        Some(self.invoke_user_found_method(found, target, args))
    }

    /// What the receiver's user `^find_method` answers for `method`: the
    /// metamethod called with the receiver and the name. `None` when the
    /// receiver's type declares no `^find_method`, or `method` never goes
    /// through method lookup.
    // Cost: O(1) when no type declares `^find_method` (one atomic load);
    // otherwise O(1) amortized for the memoized `(class, ^find_method)` probe
    // plus the user metamethod.
    pub(crate) fn user_find_method_lookup(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !any_user_find_method() || bypasses_method_lookup(method) {
            return None;
        }
        let target = target.descalarize();
        let class = match target.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Package(name) => name,
            _ => return None,
        };
        if !self.has_user_method_sym(class.as_str(), Symbol::intern(USER_FIND_METHOD)) {
            return None;
        }
        // The metamethod path expects the type argument prepended (see the
        // `^`-method branch of `call_method_with_values`); like Rakudo, the
        // receiver itself is passed as `type`.
        Some(self.call_method_with_values(
            target.clone(),
            USER_FIND_METHOD,
            vec![target.clone(), Value::str(method.to_string())],
        ))
    }

    /// The string a user `^find_method` answers for the stringifier `method`
    /// (`Str`, `Stringy`, `gist`) on `value`, for the string contexts that
    /// coerce without a method-call opcode (`~`, interpolation, `join`, a
    /// list's `.Str`). `None` when `value`'s type declares no `^find_method`.
    // Cost: as `try_user_find_method_dispatch`.
    pub(crate) fn user_find_method_stringify(
        &mut self,
        value: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        self.try_user_find_method_dispatch(value, method, &[])
            .map(|r| r.map(|v| Value::str(v.to_string_value())))
    }

    /// Invoke the Callable a user `^find_method` answered, as the method:
    /// the receiver first, then the call's arguments.
    // Cost: O(a) plus the callee, a = arguments.
    pub(crate) fn invoke_user_found_method(
        &mut self,
        found: Value,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(target.descalarize().clone());
        call_args.extend_from_slice(args);
        self.vm_call_on_value(found, call_args, None)
    }

    /// The `(owner, name)` of a Method object a `^find_method` answered with
    /// (`.^lookup`'s shape: the proto Object::Trampoline hands back), when
    /// one of its candidates binds the invocant raw -- so the caller's
    /// container must arrive for it (ADR-0067 slice 3b).
    // Cost: O(c), c = candidates of the method.
    pub(crate) fn found_method_raw_invocant_name(&self, found: &Value) -> Option<String> {
        let ValueView::Instance { attributes, .. } = found.view() else {
            return None;
        };
        let attrs = attributes.as_map();
        let name = attrs.get("__mutsu_lookup_method")?.as_str()?.to_string();
        let owner = attrs.get("__mutsu_lookup_class")?.as_str()?.to_string();
        self.registry()
            .user_method_overloads(&owner, &name)?
            .iter()
            .any(crate::runtime::raw_invocant::method_def_has_raw_invocant)
            .then_some(name)
    }
}
