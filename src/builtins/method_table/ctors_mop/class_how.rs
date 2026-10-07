//! The `Metamodel::*HOW` metamethods' rows (ADR-11276 slice 3G): `.^add_method`,
//! `.^mro`, `.^lookup` and the rest of the metaobject protocol Rakudo declares on
//! its `ClassHOW` and the other HOW classes.
//!
//! A mutsu HOW is one dispatcher over many HOW kinds, so none of these rows has a
//! shape: each is reached through its owner by [`crate::builtins::method_table::invoke_owner_raw`], with
//! the type object first among the arguments, and every handler is one
//! `Interpreter` method (`runtime/methods_classhow_arms_*.rs`). The owner of a
//! row is the HOW class Rakudo declares the metamethod on; the metamethods that
//! only some kinds of HOW have (`candidates`, `nativesize`, ...) decline a type
//! that is not of their kind, as the arms did.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $slurpy:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: if $slurpy {
                RowFlags::OWNER_ONLY.or(RowFlags::SLURPY)
            } else {
                RowFlags::OWNER_ONLY
            },
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!(
        "Metamodel::ClassHOW",
        "mixin",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_mixin(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "set_name",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_set_name(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "set_ver",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_set_meta("set_ver", args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "set_auth",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_set_meta("set_auth", args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "set_api",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_set_meta("set_api", args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "set_rw",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_set_rw(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "rw",
        1,
        false,
        |interp, _target, args, _named| interp
            .mop_rw_applies(args)
            .then(|| interp.mop_rw(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "set_why",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_set_why(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "WHY",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_why_read(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "trusts",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_trusts(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "name",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_name(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "array_type",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_array_type(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "set_array_type",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_set_array_type(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "shortname",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_shortname(args.to_vec()))
    ),
    row!(
        "Metamodel::SubsetHOW",
        "refinement",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_refinement(args.to_vec()))
    ),
    row!(
        "Metamodel::SubsetHOW",
        "refinee",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_refinee(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "mixin_base",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_mixin_base(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "ver",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_ver(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "auth",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_auth(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "api",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_api(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "isa",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_isa(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "mro",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_mro(args.to_vec()))
    ),
    row!(
        "Metamodel::ParametricRoleHOW",
        "pretending_to_be",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_pretending_to_be(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "archetypes",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_archetypes(args.to_vec()))
    ),
    row!(
        "Metamodel::CoercionHOW",
        "nominalize",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_nominalize(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "mro_unhidden",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_mro_unhidden(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "can",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_can(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "declares_method",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_declares_method(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "does",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_does(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "lookup",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_lookup(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "find_method",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_find_method(args.to_vec()))
    ),
    row!(
        "Metamodel::ParametricRoleHOW",
        "parameterize",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_parameterize(args.to_vec()))
    ),
    row!(
        "Metamodel::CoercionHOW",
        "coerce",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_coerce(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "add_role",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_add_role(args.to_vec()))
    ),
    row!(
        "Metamodel::ParametricRoleHOW",
        "set_body_block",
        2,
        true,
        |interp, _target, args, _named| interp
            .mop_set_body_block_applies(args)
            .then(|| interp.mop_set_body_block(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "add_method",
        3,
        true,
        |interp, _target, args, _named| Some(interp.mop_add_method(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "add_multi_method",
        3,
        true,
        |interp, _target, args, _named| Some(interp.mop_add_multi_method(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "add_fallback",
        3,
        true,
        |interp, _target, args, _named| Some(interp.mop_add_fallback(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "compose",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_compose(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "add_parent",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_add_parent(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "add_attribute",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_add_attribute(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "methods",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_methods(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "attributes",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_attributes(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "attribute_table",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_attribute_table(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "get_attribute_for_usage",
        2,
        false,
        |interp, _target, args, _named| Some(interp.mop_get_attribute_for_usage(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "parents",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_parents(args.to_vec()))
    ),
    row!(
        "Metamodel::ParametricRoleHOW",
        "pun",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_pun(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "roles",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_roles(args.to_vec()))
    ),
    row!(
        "Metamodel::ParametricRoleGroupHOW",
        "candidates",
        1,
        true,
        |interp, _target, args, _named| interp
            .mop_candidates_applies(args)
            .then(|| interp.mop_candidates(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "concretization",
        2,
        true,
        |interp, _target, args, _named| Some(interp.mop_concretization(args.to_vec()))
    ),
    row!(
        "Metamodel::CurriedRoleHOW",
        "curried_role",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_curried_role(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "language-revision",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_language_revision(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "method_table",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_method_table(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "method_names",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_method_names(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "private_method_table",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_private_method_table(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "private_methods",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_private_methods(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "roles_to_compose",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_roles_to_compose(args.to_vec()))
    ),
    row!(
        "Metamodel::ClassHOW",
        "submethod_table",
        1,
        true,
        |interp, _target, args, _named| Some(interp.mop_submethod_table(args.to_vec()))
    ),
    row!(
        "Metamodel::NativeHOW",
        "nativesize",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_nativesize(args.to_vec()))
    ),
    row!(
        "Metamodel::NativeHOW",
        "unsigned",
        1,
        false,
        |interp, _target, args, _named| Some(interp.mop_unsigned(args.to_vec()))
    ),
];
