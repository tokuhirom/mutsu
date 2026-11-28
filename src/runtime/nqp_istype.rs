//! `nqp::istype`: the one implementation behind the `istype` / `istype_nd`
//! table arm (`call_nqp_op`) and the VM's direct form
//! (`Interpreter::exec_nqp_istype_op`), which answers the same question for
//! a plain operand without the generic op's argument vector and name-keyed
//! table walk.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

impl Interpreter {
    /// `nqp::istype(v, ty)` on decontainerized operands.
    // Cost: O(1) expected on a type-check memo hit; O(d) otherwise, d = depth
    // of the value's MRO/role closure. MoarVM: O(1) amortized (type-check
    // cache) -- see #9134.
    pub(crate) fn nqp_istype_answer(&mut self, v: &Value, ty: &Value) -> bool {
        let Some(type_name) = nqp_istype_type_name(ty) else {
            return false;
        };
        // The same type check `~~` makes (smiley, enum and the `is Mu` MRO
        // correction included), not the bare type walk. A BOOT value's
        // `KnowHOW` has no type check at all: it is not even of its own
        // `BOOT*` type (ADR-12666, #12670).
        crate::runtime::boot_collection::boot_type_of(v).is_none()
            && self.type_object_accepts_resolved(v, type_name)
    }
}

/// The type an `nqp::istype` type operand names, or `None` for an operand
/// that names none (the op then answers false).
// Cost: O(1).
fn nqp_istype_type_name(ty: &Value) -> Option<Symbol> {
    match ty.view() {
        ValueView::Package(p) => Some(p),
        ValueView::Instance { class_name, .. } => Some(class_name),
        // `Nil` used as a type argument (`nqp::istype($x, Nil)`) is a bare
        // `ValueView::Nil`, not a `Package("Nil")` type object like other
        // builtin types -- CBOR::Simple's absent-value encoding
        // (`nqp::istype($_, Nil)` on an array element bound to Nil via
        // BIND-POS) hit exactly this gap, always False.
        ValueView::Nil => Some(crate::sym!("Nil")),
        // A role's pun (`R.^pun`) -- and `R.^pun.WHAT`, which the real
        // `Test.rakumod`'s `isa-ok` calls when its expected type isn't a
        // `Str:D` (`nqp::istype($var, $type.WHAT)`) -- is a `Mixin`-wrapped
        // `Package`/`Instance` (see `punned_role_type_object`), not a bare
        // `Package`. Unwrap it so a pun used as a type argument here behaves
        // like the class it puns to, exactly as the `isa`/`~~` fixes for the
        // same shape do.
        ValueView::Mixin(inner, _) => match inner.view() {
            ValueView::Package(p) => Some(p),
            ValueView::Instance { class_name, .. } => Some(class_name),
            _ => None,
        },
        _ => None,
    }
    .filter(|s| !s.as_str().is_empty())
}
