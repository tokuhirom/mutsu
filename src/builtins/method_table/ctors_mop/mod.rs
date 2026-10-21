//! The constructors' and the metaobject protocol's rows (ADR-11276 §10,
//! slice 3G).
//!
//! `new` per built-in type, `Metamodel::*`, the subscript protocol and the
//! internal `__mutsu_*` names. A slice adds a family module here and lists it
//! in [`FAMILIES`]; no other file names it.

use super::MethodRow;

pub(crate) mod attribute;
mod class_how;
mod class_how_buildplan;
mod ctor_late;
mod enum_how;
mod grammar;
mod mu_answers;
mod mu_base;
mod mu_emit;
mod mu_identity;
mod mu_isa;
mod mu_numeric;
mod mu_rakuseen;
mod mu_render;
mod mu_str;
mod mu_walk_return;
pub(crate) mod variable;

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

/// The owner of `Metamodel::EnumHOW`'s own metamethods, asked ahead of
/// [`MOP_OWNERS`] (§9.140).
pub(crate) static ENUM_HOW_OWNERS: &[&str] = &["Metamodel::EnumHOW"];

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    attribute::ROWS,
    variable::ROWS,
    class_how::ROWS,
    class_how_buildplan::ROWS,
    ctor_late::ROWS,
    enum_how::ROWS,
    grammar::ROWS,
    mu_base::ROWS,
    mu_answers::ROWS,
    mu_identity::ROWS,
    mu_identity::MAP_ROWS,
    mu_emit::ROWS,
    mu_rakuseen::ROWS,
    mu_walk_return::ROWS,
    mu_isa::ROWS,
    mu_numeric::ROWS,
    mu_render::ROWS,
    mu_str::ROWS,
    mu_str::MAP_ROWS,
];

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
