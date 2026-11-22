//! Colonpairs written one after another as a term (`my $c = :a:!b:42c`),
//! and the dispatch of the other written forms the parser marks for `.DEPARSE`.
//!
//! The parser runs the run as the list of its pairs and marks it
//! `Spelling::ColonPairs`; rakudo keeps `RakuAST::ColonPairs` with one
//! positional child per pair (measured on 2026.11), and lowering takes it back
//! to the list.

use super::view::{node_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// The node of a term the parser runs as another shape but spelled one of
/// the written forms `.DEPARSE` keeps (#14845): a sigiled named capture, a
/// `Z` chain, a hyper slip, a hyper call and a run of colonpairs.
// Cost: O(n), n = size of the term.
pub(super) fn view_written(
    names: &NameScope,
    spelled: &crate::ast::spelled::Spelled,
) -> Result<RakuAstNode, RuntimeError> {
    use crate::ast::spelled::Spelling;
    let expr = &spelled.expr;
    match spelled.spelling {
        Spelling::CaptureSigil(sigil) => super::match_vars::view_sigiled(expr, sigil),
        Spelling::ZipChain => super::zip_chain::view_node(names, expr),
        Spelling::HyperSlip => super::hyper_prefix::slip_node(names, expr),
        Spelling::HyperCallTerm => super::hyper_prefix::call_term_node(names, expr),
        Spelling::ColonPairs => view_node(names, expr),
        Spelling::IndirectTail(ref tail) => {
            super::symbolic_deref::view_indirect_tail(names, expr, tail)
        }
        _ => view_expr(names, expr),
    }
}

/// `expr` as the whole expression of a statement: a run of colonpairs there
/// is a comma list (`:a:!b;` is `ApplyListInfix(",")` in rakudo 2026.11), not
/// `ColonPairs`.
// Cost: O(1).
pub(super) fn statement_form(expr: &Expr) -> &Expr {
    match expr {
        Expr::Spelled(s) if matches!(s.spelling, crate::ast::spelled::Spelling::ColonPairs) => {
            &s.expr
        }
        _ => expr,
    }
}

/// The `ColonPairs` of a marked run of colonpairs.
// Cost: O(n), n = size of the pairs.
pub(super) fn view_node(names: &NameScope, run: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::ArrayLiteral(pairs) = run else {
        return view_expr(names, run);
    };
    let mut fields = Vec::with_capacity(pairs.len());
    for pair in pairs {
        fields.push(node_field(None, view_expr(names, pair)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairs,
        fields,
    })
}

/// `:shape(2;2)`: a colonpair whose parenthesized value has `;` sections,
/// its `ColonPair::Value` with one statement per section in the value's
/// `SemiList` (the `(1; 2)` term's view). `None` for any other value.
// Cost: O(n), n = size of the value.
pub(super) fn sections_pair(
    names: &NameScope,
    key: &str,
    value: &Expr,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let Expr::Spelled(spelled) = value else {
        return Ok(None);
    };
    if !matches!(spelled.spelling, crate::ast::spelled::Spelling::ParenSections(_)) {
        return Ok(None);
    }
    let parentheses = view_expr(names, &Expr::Grouped(Box::new(value.clone())))?;
    Ok(Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairValue,
        fields: vec![
            super::view::leaf_field(
                Some("key"),
                crate::value::Value::str(key.to_string()),
            ),
            node_field(Some("value"), parentheses),
        ],
    }))
}
