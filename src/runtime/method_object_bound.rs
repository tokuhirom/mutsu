//! A `Method` object invokes exactly its own candidate (#10344).
//!
//! A native or attribute-accessor Method object
//! (`make_native_method_object_ex_loc`, and the `.^lookup`/`.^find_method`/
//! `.^can`/`.^methods` accessor objects) carries a `Routine { package, name }`
//! callable rather than a compiled `Sub`. Resolving that callable by *name*
//! on the invocant is a virtual dispatch: an override in the invocant's class
//! wins over the object's own candidate, so `D.^lookup('x')($e)` ran `E`'s
//! `x`. Rakudo binds the object to its declaring candidate, which is exactly
//! what a qualified call `$e.D::x` does -- including running a `.wrap`
//! installed on that candidate. So the object is invoked (and assigned
//! through) under its owner-qualified name.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// The owner-qualified method name (`D::x`) a native/accessor `Method`
    /// object binds to when invoked on `invocant`, or `None` when the object
    /// is not such a method, its owner is not a user class, or `invocant` is
    /// not an instance inheriting from that owner -- the callers then keep
    /// their by-name dispatch, which is the only meaning a type object or a
    /// builtin-type invocant has.
    ///
    // Cost: O(m), m = length of the invocant class's MRO (one linear scan);
    // the qualified name is memoized per (owner, name) pair.
    pub(crate) fn bound_method_object_name(
        &self,
        method_obj: &Value,
        invocant: &Value,
    ) -> Option<Symbol> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = method_obj.view()
        else {
            return None;
        };
        if !matches!(class_name.as_str(), "Method" | "Submethod") {
            return None;
        }
        let ValueView::Routine {
            package,
            name,
            is_regex: false,
            ..
        } = attributes
            .as_map()
            .get("__mutsu_method_callable")
            .map(Value::view)?
        else {
            return None;
        };
        self.owner_bound_method_name(package, name, invocant)
    }

    /// `owner::name` when `owner` is a user class `invocant` (an instance, or
    /// the type object itself) inherits from, else `None`.
    ///
    /// A type-object invocant is bound too: re-dispatching it by bare name
    /// would ask the receiver's method lookup again, which a user
    /// `^find_method` answers with this very Method object (#10819).
    ///
    // Cost: O(m), m = length of the invocant class's MRO.
    pub(crate) fn owner_bound_method_name(
        &self,
        owner: Symbol,
        name: Symbol,
        invocant: &Value,
    ) -> Option<Symbol> {
        if !self.has_class(owner.as_str()) {
            return None;
        }
        let inst_class = match invocant.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Package(name) => name,
            _ => return None,
        };
        if !self.class_mro(inst_class.as_str()).contains(&owner) {
            return None;
        }
        Some(crate::qualified::qualified(owner, name))
    }

    /// Invoke a native/accessor `Method` object on `args[0]` bound to its
    /// declaring candidate (see the module docs), or `None` when
    /// [`Self::bound_method_object_name`] does not apply.
    ///
    // Cost: O(m) to bind (see `bound_method_object_name`), plus the call.
    pub(crate) fn try_call_bound_method_object(
        &mut self,
        method_obj: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let invocant = args.first()?;
        let qualified = self.bound_method_object_name(method_obj, invocant)?;
        Some(self.call_method_with_values(invocant.clone(), qualified.as_str(), args[1..].to_vec()))
    }
}
