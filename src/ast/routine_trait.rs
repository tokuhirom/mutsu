//! Written routine traits, before the parser folds them into execution fields.

use super::parsed::ParsedStmt;
use super::{Expr, Stmt};

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RoutineTrait {
    Is {
        name: String,
        argument: Option<TraitArgument>,
    },
    Returns(String),
    Of(String),
    /// `handles TERM` on a method, with the term as written (`<a b>`, `*`).
    Handles(Expr),
    /// `is TYPE`, `does ROLE` or `hides TYPE` on a class, role or grammar
    /// header: the type as written, bracketed arguments included
    /// (`R[Int]`), and the argument expressions the parser read from them.
    Parent {
        kind: ParentKind,
        name: String,
        args: Option<Vec<Expr>>,
    },
    Unsupported(String),
}

/// Which keyword a [`RoutineTrait::Parent`] was written with.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ParentKind {
    Is,
    Does,
    Hides,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum TraitArgument {
    Parentheses(Expr),
    Words(String),
    ExportTags(Vec<String>),
    Operator(String),
}

/// Add a trait the parser has just read to the record being built.
// Cost: O(1) plus building the trait.
pub(crate) fn note(written: &mut Vec<RoutineTrait>, make: impl FnOnce() -> RoutineTrait) {
    written.push(make());
}

/// The record is compiler-inert: only the RakuAST conversion reads it.
// Cost: O(n), n = statements in the body (moving the existing statements).
pub(crate) fn attach(body: &mut Vec<Stmt>, traits: Vec<RoutineTrait>) {
    retain(body, traits);
}

/// [`attach`] for the RakuAST lowering, which keeps the record whichever way
/// the lowered tree was parsed.
// Cost: O(n), n = statements in the body.
pub(crate) fn retain(body: &mut Vec<Stmt>, traits: Vec<RoutineTrait>) {
    if !traits.is_empty() {
        body.insert(0, Stmt::Ast(Box::new(ParsedStmt::RoutineTraits(traits))));
    }
}

/// Whether a routine's return type was written as a `returns` / `of` trait
/// rather than a `-->` arrow: an undeclared one is `X::InvalidType`, while an
/// undeclared `-->` type is `X::Undeclared`.
// Cost: O(n), n = statements in the body.
pub(crate) fn return_type_is_trait(body: &[Stmt]) -> bool {
    get(body).is_some_and(|traits| {
        traits
            .iter()
            .any(|item| matches!(item, RoutineTrait::Returns(_) | RoutineTrait::Of(_)))
    })
}

// Cost: O(n), n = statements in the body.
pub(crate) fn get(body: &[Stmt]) -> Option<&[RoutineTrait]> {
    body.iter().find_map(|stmt| match stmt {
        Stmt::Ast(node) => match node.as_ref() {
            ParsedStmt::RoutineTraits(traits) => Some(traits.as_slice()),
            _ => None,
        },
        _ => None,
    })
}
