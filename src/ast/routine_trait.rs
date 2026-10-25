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

/// Add a trait the parser has just read to the record being built, when a parse
/// that keeps spellings asked for one; the trait is not even built otherwise.
// Cost: O(1) plus building the trait.
pub(crate) fn note(written: &mut Vec<RoutineTrait>, make: impl FnOnce() -> RoutineTrait) {
    if super::spelled::keeping() {
        written.push(make());
    }
}

/// The record is compiler-inert. Ordinary parses build none; the `.AST`
/// parser keeps it.
// Cost: O(n), n = statements in the body (moving the existing statements).
pub(crate) fn attach(body: &mut Vec<Stmt>, traits: Vec<RoutineTrait>) {
    if super::spelled::keeping() {
        retain(body, traits);
    }
}

/// [`attach`] for the RakuAST lowering, which keeps the record whichever way
/// the lowered tree was parsed.
// Cost: O(n), n = statements in the body.
pub(crate) fn retain(body: &mut Vec<Stmt>, traits: Vec<RoutineTrait>) {
    if !traits.is_empty() {
        body.insert(0, Stmt::Ast(Box::new(ParsedStmt::RoutineTraits(traits))));
    }
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
