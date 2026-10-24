//! The one lowering of the compound-assignment family (ADR-10723 Stage 2,
//! #14948). A [`CompoundAssign`] the parser built, or one `rakuast::lower`
//! read off a hand-built `ApplyInfix(MetaInfix::Assign)`, becomes the
//! source-level `Expr::CompoundAssign` marker over the execution expansion
//! [`build_compound_assign_expr`] builds for its target. How the infix was
//! spelled (`[+]=`, a scalar `Z+=`) does not change what runs.

use super::*;
use crate::ast::parsed::{CompoundAssign, CompoundMeta, ParsedExpr};

/// The operator a [`CompoundAssign`] names.
// Cost: O(1).
fn operator(node: &CompoundAssign) -> Result<CompoundAssignOp, PError> {
    compound_assign_op_from_name(&node.op)
        .ok_or_else(|| PError::expected("compound assignment operator"))
}

/// Lower a compound assignment in expression position. A short-circuit
/// operator (`//=`, `||=`, `&&=`) on a scalar does not store when it keeps the
/// current value.
// Cost: O(n), n = size of the target and source.
pub(crate) fn lower_compound_expression(node: CompoundAssign) -> Result<Expr, PError> {
    match node.meta {
        CompoundMeta::Reverse => return Ok(lower_reverse(node)),
        CompoundMeta::Set => return lower_set(node),
        _ => {}
    }
    let op = operator(&node)?;
    let CompoundAssign { target, source, .. } = node;
    if let Expr::Var(name) = &target
        && let Some(short) =
            short_circuit_compound_assign_expr(name, target.clone(), op, source.clone())
    {
        return Ok(compound_assign_marker(target, op, source, short));
    }
    preserve_compound_assign(target, op, source)
}

/// Lower a compound assignment written as a statement. A variable target is
/// a `Stmt::Assign` of its expansion; any other target, a public attribute
/// (`$.n`), and a short-circuit operator on a scalar, is the expression form
/// as a statement.
// Cost: O(n), n = size of the target and source.
pub(crate) fn lower_compound_statement(node: CompoundAssign) -> Result<Stmt, PError> {
    if matches!(node.meta, CompoundMeta::Reverse | CompoundMeta::Set) {
        return Ok(Stmt::Expr(lower_compound_expression(node)?));
    }
    let name = match &node.target {
        // `$.n += 1` writes through the accessor: the expression form's
        // `AssignExpr` is what the compiler reads the `$.` twigil from.
        Expr::Var(name) if name.starts_with('.') => {
            return Ok(Stmt::Expr(lower_compound_expression(node)?));
        }
        Expr::Var(name) => name.clone(),
        Expr::ArrayVar(name) => format!("@{name}"),
        Expr::HashVar(name) => format!("%{name}"),
        _ => return Ok(Stmt::Expr(lower_compound_expression(node)?)),
    };
    let op = operator(&node)?;
    if matches!(node.target, Expr::Var(_)) && short_circuit_keep(op).is_some() {
        return Ok(Stmt::Expr(lower_compound_expression(node)?));
    }
    Ok(Stmt::Assign {
        name,
        expr: preserve_compound_assign(node.target, op, node.source)?,
        op: AssignOp::Assign,
        target_is_sigilless: false,
    })
}

/// `TARGET R OP= SOURCE` assigns `SOURCE OP= TARGET`: the marker over the
/// assignment to the right operand.
// Cost: O(n), n = size of the target and source.
fn lower_reverse(node: CompoundAssign) -> Expr {
    let CompoundAssign {
        target, op, source, ..
    } = node;
    let expanded = crate::parser::expr::precedence::build_compound_assign_target_expr(
        source.clone(),
        &op,
        target.clone(),
    );
    reverse_assign_marker(target, &op, source, expanded)
}

/// `TARGET (|)= SOURCE`: the marker over the set operator's expansion.
// Cost: O(n), n = size of the target and source.
fn lower_set(node: CompoundAssign) -> Result<Expr, PError> {
    let set_tok = set_op_from_spelling(&node.op).ok_or_else(|| PError::expected("set operator"))?;
    Ok(preserve_set_compound_assign(
        node.target,
        &node.op,
        set_tok,
        node.source,
    ))
}

/// `TARGET (|)= SOURCE`, kept in its RakuAST shape by a `.AST` parse and
/// lowered at once otherwise. `spelling` is the set operator as written.
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_set_assignment(
    target: Expr,
    spelling: &str,
    set_tok: TokenKind,
    source: Expr,
) -> Expr {
    if crate::ast::spelled::keeping() {
        return Expr::Ast(Box::new(ParsedExpr::CompoundAssign(CompoundAssign {
            target,
            op: spelling.to_string(),
            meta: CompoundMeta::Set,
            source,
        })));
    }
    preserve_set_compound_assign(target, spelling, set_tok, source)
}

/// `TARGET R OP= SOURCE`, kept in its RakuAST shape by a `.AST` parse and
/// lowered at once otherwise. `op` is the reversed infix (`-`, or `=`).
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_reverse_assignment(target: Expr, op: &str, source: Expr) -> Expr {
    let node = CompoundAssign {
        target,
        op: op.to_string(),
        meta: CompoundMeta::Reverse,
        source,
    };
    if crate::ast::spelled::keeping() {
        return Expr::Ast(Box::new(ParsedExpr::CompoundAssign(node)));
    }
    lower_reverse(node)
}

/// `TARGET OP= SOURCE` in expression position: kept in its RakuAST shape by a
/// `.AST` parse, lowered at once otherwise. A `.AST` parse still checks that
/// the target takes the operator, so both parses accept the same programs.
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_compound_assignment(
    target: Expr,
    op: CompoundAssignOp,
    meta: CompoundMeta,
    source: Expr,
) -> Result<Expr, PError> {
    let node = compound_node(target, op, meta, source);
    if crate::ast::spelled::keeping() {
        build_compound_assign_expr(node.target.clone(), op, node.source.clone())?;
        return Ok(Expr::Ast(Box::new(ParsedExpr::CompoundAssign(node))));
    }
    lower_compound_expression(node)
}

/// `TARGET OP= SOURCE;` written as a statement, the same way.
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_compound_assignment_stmt(
    target: Expr,
    op: CompoundAssignOp,
    meta: CompoundMeta,
    source: Expr,
) -> Result<Stmt, PError> {
    let node = compound_node(target, op, meta, source);
    if crate::ast::spelled::keeping() {
        build_compound_assign_expr(node.target.clone(), op, node.source.clone())?;
        return Ok(Stmt::Expr(Expr::Ast(Box::new(ParsedExpr::CompoundAssign(
            node,
        )))));
    }
    lower_compound_statement(node)
}

// Cost: O(1).
fn compound_node(
    target: Expr,
    op: CompoundAssignOp,
    meta: CompoundMeta,
    source: Expr,
) -> CompoundAssign {
    CompoundAssign {
        target,
        op: op.symbol().trim_end_matches('=').to_string(),
        meta,
        source,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_operator_names_itself() {
        use CompoundAssignOp::*;
        for op in [
            Comma,
            DefinedOr,
            LogicalOr,
            LogicalAnd,
            Add,
            Sub,
            Concat,
            Mul,
            Div,
            Mod,
            Power,
            Repeat,
            ListRepeat,
            BitOr,
            BitAnd,
            BitXor,
            BitShiftLeft,
            BitShiftRight,
            Min,
            Max,
            KeywordOr,
            KeywordAnd,
            KeywordXor,
            Orelse,
            Andthen,
            Notandthen,
            IntDiv,
            Lcm,
            Gcd,
            StrBitAnd,
            StrBitOr,
            StrBitXor,
            StrShiftLeft,
            StrShiftRight,
            BoolBitAnd,
            BoolBitOr,
            BoolBitXor,
            XorXor,
            JuncAny,
            JuncAll,
            JuncOne,
        ] {
            let node = compound_node(
                Expr::Var("x".into()),
                op,
                CompoundMeta::Plain,
                Expr::Var("y".into()),
            );
            assert_eq!(operator(&node).ok(), Some(op), "{}", op.symbol());
        }
    }
}
