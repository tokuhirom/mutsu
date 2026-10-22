//! A package declared with an `EXPORTHOW::DECLARE` declarator, in RakuAST.
//!
//! A `use`d module can register a class-like keyword
//! (`my package EXPORTHOW { package DECLARE { constant monitor = MonitorHOW } }`);
//! `monitor Name { }` then parses like `class` (or like `role`, for a role
//! HOW). Measured on rakudo 2026.09, the gist of such a declaration is the
//! ordinary `RakuAST::Class` / `RakuAST::Role` with a `parsed-declarator`
//! field holding the keyword, written after `scope` and before `name`; the
//! HOW itself is not part of the rendered tree, and `.parsed-declarator`
//! answers the plain keyword (`"class"`, `"role"`, `"grammar"`) otherwise.
//!
//! The parser records the keyword as the `DECLARE_HOW_TRAIT` marker, which is
//! exactly that field: conversion renders it, lowering hands it back, and
//! registration resolves the HOW from the keyword as for a parsed declaration.

use super::convert::leaf_field;
use super::lower::leaf_str;
use super::{RakuAstClass, RakuAstField, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value};

/// The keyword a package node of `class` declares with when it is written
/// with the built-in declarator.
// Cost: O(1).
fn default_declarator(class: RakuAstClass) -> &'static str {
    match class {
        RakuAstClass::Role => "role",
        RakuAstClass::Grammar => "grammar",
        _ => "class",
    }
}

/// `fields` with the declaration's `parsed-declarator`, when the parser's
/// `custom_traits` name a registered declarator, placed after `scope`.
// Cost: O(f + t), f = number of fields, t = number of custom traits.
pub(super) fn add_field(
    fields: &mut Vec<RakuAstField>,
    custom_traits: &[(String, Option<Expr>)],
    default_declarator: &str,
) {
    let Some(keyword) = crate::parser::declare_how_keyword(custom_traits) else {
        return;
    };
    if keyword == default_declarator {
        if crate::parser::supersede_marker(default_declarator).is_some() {
            fields.push(super::routine_source::package_supersede_field());
        }
        return;
    }
    let at = usize::from(fields.first().is_some_and(|f| f.name == Some("scope")));
    fields.insert(
        at,
        leaf_field(Some("parsed-declarator"), Value::str(keyword)),
    );
}

/// The parser's marker for a package node's `parsed-declarator`, or `None`
/// when the node is written with its built-in declarator.
// Cost: O(f), f = number of fields.
pub(super) fn marker(node: &RakuAstNode) -> Result<Option<(String, Option<Expr>)>, RuntimeError> {
    if !node
        .fields
        .iter()
        .any(|f| f.name == Some("parsed-declarator"))
    {
        return Ok(None);
    }
    let keyword = leaf_str(node, "parsed-declarator")?;
    Ok((keyword != default_declarator(node.class))
        .then(|| crate::parser::declare_how_marker(keyword)))
}
