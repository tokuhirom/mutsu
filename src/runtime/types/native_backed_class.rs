//! Coercing INTO a class that inherits a built-in SCALAR type
//! (`class CM is Str {}; CM('x')`): the result is an `Instance` of the class
//! carrying the coerced built-in payload.
//!
//! Sibling of `role_mixin_class`'s wrapper mechanism, but for the CLASS case.
//! A role has no shared attribute node on a non-`Instance` value, so it rides
//! in a wrapper; a class instance is already an `Instance` with a shared
//! `InstanceAttrs` node, so no wrapper is needed -- the coerced built-in value
//! is simply stored as one more attribute, under a name no user `has`
//! declaration can spell (`NATIVE_BACKING_ATTR`).
//!
//! Only the coercion-protocol entry points (`T(value)`, `T.COERCE(value)`,
//! a `T()` type constraint) go through here. General native-method
//! delegation for such an instance (`.uc`, `.substr`, ...) is not part of
//! this: see <https://github.com/tokuhirom/mutsu/issues/8856>.

use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

/// The instance-attribute key a native-scalar-backed class instance carries
/// its coerced built-in payload under. Never user-visible (it is not a
/// declared `has`, so `.^attributes` does not list it) and deliberately not
/// the generic `"value"` name, which a class inheriting `Str` remains free to
/// declare as its own public attribute.
pub(crate) const NATIVE_BACKING_ATTR: &str = "__mutsu_native_backing";

/// Built-in SCALAR types whose payload an `Instance` can box this way.
/// Container built-ins (`is Array`/`is Hash`/`is List`) already have their
/// own backing-storage mechanism (`__mutsu_array_storage`); compound native
/// types mutsu models with hardcoded attributes (`Instant`, `Duration`,
/// `Version`, ...) are out of scope here.
const NATIVE_SCALAR_BACKING_TYPES: &[&str] =
    &["Str", "Int", "Num", "Rat", "FatRat", "Complex", "Bool"];

impl Interpreter {
    /// The built-in scalar type `class_name` inherits, if any -- the type
    /// whose coerced payload a `T(value)` / `T.COERCE(value)` call on this
    /// class would box. `None` for a class with no such built-in ancestor --
    /// including `class_name` itself naming a built-in scalar type (its MRO
    /// trivially "contains" itself, but coercing e.g. `Str` into `Str` is the
    /// ordinary built-in coercion, not this mechanism) or naming no
    /// user-registered class at all. Without the registry guard, a bare
    /// coercion target like the "Str" in `Str:D(Rat)` recursed into this
    /// class-coercion path via `try_coerce_value_with_method`'s own last-resort
    /// call, building a "Str" instance and back into the same coercion —
    /// unbounded (stack overflow, `t/oo/subset-coercive-nested.t`).
    pub(crate) fn native_scalar_backing_parent(
        &mut self,
        class_name: &str,
    ) -> Option<&'static str> {
        if NATIVE_SCALAR_BACKING_TYPES.contains(&class_name)
            || !self.registry().classes.contains_key(class_name)
        {
            return None;
        }
        let mro = self.class_mro(class_name);
        NATIVE_SCALAR_BACKING_TYPES
            .iter()
            .find(|&&builtin| mro.iter().any(|c| c.as_str() == builtin))
            .copied()
    }

    /// The coerced native payload a native-scalar-backed instance carries,
    /// if `target` is one.
    pub(crate) fn native_backing_value(target: &Value) -> Option<Value> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        attributes.as_map().get(NATIVE_BACKING_ATTR).cloned()
    }

    /// Coerce `value` into `class_name`, when that class inherits a built-in
    /// scalar type. Rakudo answers `CM('hello')` (`class CM is Str {}`)
    /// through `Str`'s inherited `COERCE`, building a real `CM` carrying the
    /// string; this builds the equivalent by running the class's own default
    /// construction (so any attributes IT declares still get their
    /// defaults/BUILD/TWEAK, exactly like plain `CM.new`) and then boxing the
    /// coerced payload into the private backing attribute.
    ///
    /// Returns `None` when `class_name` names no such built-in ancestor, so
    /// the caller's own error path is unchanged (mirrors
    /// `coerce_into_builtin_inheriting_role`). Callers reach this only once
    /// the class's own `COERCE`/`new` (if any) has already been tried.
    pub(in crate::runtime) fn coerce_into_builtin_inheriting_class(
        &mut self,
        class_name: &str,
        value: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let builtin = self.native_scalar_backing_parent(class_name)?;
        Some(self.build_native_backed_instance(class_name, builtin, value))
    }

    /// Whether `method` on `target` must be answered by delegating to a
    /// native-scalar-backed instance's boxed payload rather than by the
    /// instance itself -- the scalar twin of
    /// [`Interpreter::delegates_to_array_storage`]. `None` when `target` is
    /// not such an instance, the class overrides `method` itself, or
    /// `method` reports the receiver's own type identity (`.^name`, `.WHAT`,
    /// `.isa`, `.new`, ...), which must never resolve through the payload.
    pub(crate) fn try_native_backing_delegate(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // Qualified calls must resolve against the wrapper class's MRO.  The
        // backing scalar is only the implementation target for ordinary
        // unqualified methods; delegating `self.Rat::new(...)` here would lose
        // the subclass and bypass qualified-constructor dispatch.
        if method.starts_with('!')
            || crate::symbol::Symbol::lookup(method).is_some_and(crate::qualified::is_qualified)
            || Self::is_type_identity_method(method)
        {
            return None;
        }
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        let backing = attributes.as_map().get(NATIVE_BACKING_ATTR).cloned()?;
        if self.has_user_method(&class_name.resolve(), method) {
            return None;
        }
        Some(self.call_method_with_values(backing, method, args.to_vec()))
    }

    fn build_native_backed_instance(
        &mut self,
        class_name: &str,
        builtin: &str,
        value: &Value,
    ) -> Result<Value, RuntimeError> {
        let coerced = self.try_coerce_value_with_method(builtin, value.clone())?;
        if !self.type_matches_value(builtin, &coerced) {
            return Err(coerce_impossible_error(class_name, value));
        }
        let instance = self.call_method_with_values(
            Value::package(Symbol::intern(class_name)),
            "new",
            vec![],
        )?;
        if let ValueView::Instance { attributes, .. } = instance.view() {
            attributes.insert(NATIVE_BACKING_ATTR, coerced);
        }
        Ok(instance)
    }
}
