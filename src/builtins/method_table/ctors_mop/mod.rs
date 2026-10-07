//! The constructors' and the metaobject protocol's rows (ADR-11276 §10,
//! slice 3G).
//!
//! `new` per built-in type, `Metamodel::*`, the subscript protocol and the
//! internal `__mutsu_*` names. A slice adds a family module here and lists it
//! in [`FAMILIES`]; no other file names it.

use super::MethodRow;

mod class_how;

/// The HOW classes whose rows answer a metamethod call, in lookup order.
pub(crate) static MOP_OWNERS: &[&str] = &[
    "Metamodel::ClassHOW",
    "Metamodel::SubsetHOW",
    "Metamodel::CoercionHOW",
    "Metamodel::ParametricRoleHOW",
    "Metamodel::ParametricRoleGroupHOW",
    "Metamodel::CurriedRoleHOW",
    "Metamodel::NativeHOW",
];

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[class_how::ROWS];

/// Whether a `Metamodel::*HOW` owner has a row named `method` at any arity a
/// metamethod call carries (the type object plus up to two arguments).
// Cost: O(o), o = `MOP_OWNERS`, three hash lookups each.
pub(crate) fn mop_declares(method: &str) -> bool {
    let method = crate::symbol::Symbol::intern(method);
    MOP_OWNERS.iter().any(|owner| {
        let owner = crate::symbol::Symbol::intern(owner);
        (1..=3).any(|arity| super::owner_row(owner, method, arity).is_some())
    })
}
