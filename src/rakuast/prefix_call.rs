//! `start`, `quietly` and `sink` as statement prefixes.
//!
//! The parser keeps them as calls named after the keyword (`start { ... }`
//! is `start(AnonSub)`, `quietly EXPR` is `quietly(EXPR)`); raku has a node
//! class for each (`StatementPrefix::Start`, `::Quietly`, `::Sink`) that holds
//! its block or statement positionally, like `once` and `try`:
//!
//! ```text
//! start { 1 }    StatementPrefix::Start(Block(body => Blockoid(...)))
//! quietly 1      StatementPrefix::Quietly(Statement::Expression(IntLiteral 1))
//! sink { 1 }     StatementPrefix::Sink(Block(...))
//! ```
//!
//! `start STATEMENT` reaches the converter as the same one-statement
//! `AnonSub` a `start { STATEMENT }` does, so it renders in the block form
//! (raku keeps the bare statement); the parser does not record which was
//! written.

use super::convert::{block_node, convert_expr, node_field, statement_expression};
use super::lower::{lower_block, lower_expr, named_child, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::value::RuntimeError;

fn prefix_class(name: &str) -> Option<RakuAstClass> {
    match name {
        "start" => Some(RakuAstClass::StatementPrefixStart),
        "quietly" => Some(RakuAstClass::StatementPrefixQuietly),
        "sink" => Some(RakuAstClass::StatementPrefixSink),
        _ => None,
    }
}

/// The statement prefix a call named `name` with `args` spells, or `None` for
/// any other call.
// Cost: O(n), n = size of the argument.
pub(super) fn convert(name: &str, args: &[Expr]) -> Option<Result<RakuAstNode, RuntimeError>> {
    let class = prefix_class(name)?;
    let [arg] = args else {
        return None;
    };
    Some(blorst(arg).map(|blorst| RakuAstNode {
        class,
        fields: vec![node_field(None, blorst)],
    }))
}

/// The block (`{ ... }`) or the statement a prefix holds.
fn blorst(arg: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match arg {
        Expr::AnonSub {
            body,
            is_block: true,
            is_rw: false,
            is_raw: false,
            ..
        } => block_node(body),
        other => Ok(statement_expression(convert_expr(other)?)),
    }
}

/// `StatementPrefix::Start` / `::Quietly` / `::Sink` -> the call the parser
/// spells it as.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let name = match node.class {
        RakuAstClass::StatementPrefixStart => "start",
        RakuAstClass::StatementPrefixQuietly => "quietly",
        RakuAstClass::StatementPrefixSink => "sink",
        _ => return Err(unsupported(node)),
    };
    let blorst = named_child_or_positional(node)?;
    let arg = match blorst.class {
        RakuAstClass::Block => make_anon_sub(lower_block(blorst)?),
        RakuAstClass::StatementExpression => {
            let expr = lower_expr(named_child(blorst, "expression")?)?;
            // `start EXPR` runs the expression in a block of its own; the
            // other two take the expression itself.
            if node.class == RakuAstClass::StatementPrefixStart {
                make_anon_sub(vec![Stmt::Expr(expr)])
            } else {
                expr
            }
        }
        _ => return Err(unsupported(node)),
    };
    Ok(Expr::Call {
        name: crate::symbol::Symbol::intern(name),
        args: vec![arg],
        listop: true,
    })
}
