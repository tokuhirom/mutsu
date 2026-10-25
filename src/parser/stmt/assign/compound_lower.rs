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
        CompoundMeta::TopicDotty => return Ok(topic_dotty_expression(node.source)),
        CompoundMeta::Dotty => {
            return Ok(lower_dotty(
                node,
                false,
                crate::parser::stmt::simple::is_user_declared_value_term,
            ));
        }
        CompoundMeta::BracketedMeta(..) => {
            let (name, value) = meta_assignment(node)?;
            return Ok(Expr::AssignExpr {
                name,
                expr: Box::new(value),
                is_bind: false,
            });
        }
        CompoundMeta::Plain if is_routine_infix(&node.op) => {
            return build_custom_compound_assign_expr(node.target, node.op, node.source);
        }
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
    if matches!(node.meta, CompoundMeta::Dotty | CompoundMeta::TopicDotty) {
        return Ok(Stmt::Expr(lower_dotty(
            node,
            true,
            crate::parser::stmt::simple::is_user_declared_value_term,
        )));
    }
    if let CompoundMeta::BracketedMeta(..) = node.meta {
        let (name, value) = meta_assignment(node)?;
        return Ok(
            match rewrite_scalar_assignment_stmt_as_sink(name.clone(), value.clone()) {
                Some(stmt) => stmt,
                None => Stmt::Assign {
                    name,
                    expr: value,
                    op: AssignOp::Assign,
                    target_is_sigilless: false,
                },
            },
        );
    }
    if matches!(node.meta, CompoundMeta::Reverse | CompoundMeta::Set) {
        return Ok(Stmt::Expr(lower_compound_expression(node)?));
    }
    if node.meta == CompoundMeta::Plain && is_routine_infix(&node.op) {
        return lower_routine_statement(node);
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

/// Whether `op` is an infix with no built-in compound operator: a word
/// operator (`mod`) or a user-declared infix (`+%+`, or `×` while a user
/// `infix:<×>` is in scope), which `OP=` calls by name.
// Cost: O(k), k = number of compound operators.
fn is_routine_infix(op: &str) -> bool {
    compound_assign_op_from_name(op).is_none()
}

/// `VAR mod= SOURCE;` assigns `VAR mod SOURCE` to the variable; any other
/// target is the expression form as a statement.
// Cost: O(n), n = size of the target and source.
fn lower_routine_statement(node: CompoundAssign) -> Result<Stmt, PError> {
    let name = match &node.target {
        Expr::Var(name) if !name.starts_with('.') => name.clone(),
        Expr::ArrayVar(name) => format!("@{name}"),
        Expr::HashVar(name) => format!("%{name}"),
        _ => return Ok(Stmt::Expr(lower_compound_expression(node)?)),
    };
    Ok(Stmt::Assign {
        name,
        expr: Expr::InfixFunc {
            name: node.op,
            left: Box::new(node.target),
            right: vec![node.source],
            modifier: None,
        },
        op: AssignOp::Assign,
        target_is_sigilless: false,
    })
}

/// `TARGET mod= SOURCE` over an infix [`is_routine_infix`] names: kept in its
/// RakuAST shape by a `.AST` parse, lowered at once otherwise. A `.AST` parse
/// still checks that the target takes the operator.
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_routine_compound_assignment(
    target: Expr,
    op: String,
    source: Expr,
) -> Result<Expr, PError> {
    if crate::ast::spelled::keeping() {
        build_custom_compound_assign_expr(target.clone(), op.clone(), source.clone())?;
        return Ok(Expr::Ast(Box::new(ParsedExpr::CompoundAssign(
            CompoundAssign {
                target,
                op,
                meta: CompoundMeta::Plain,
                source,
            },
        ))));
    }
    build_custom_compound_assign_expr(target, op, source)
}

/// [`written_routine_compound_assignment`] written as a statement.
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_routine_compound_assignment_stmt(
    target: Expr,
    op: String,
    source: Expr,
) -> Result<Stmt, PError> {
    let node = CompoundAssign {
        target,
        op,
        meta: CompoundMeta::Plain,
        source,
    };
    if crate::ast::spelled::keeping() {
        build_custom_compound_assign_expr(
            node.target.clone(),
            node.op.clone(),
            node.source.clone(),
        )?;
        return Ok(Stmt::Expr(Expr::Ast(Box::new(ParsedExpr::CompoundAssign(
            node,
        )))));
    }
    lower_routine_statement(node)
}

/// `VAR [X+]= SOURCE` assigns `VAR X+ SOURCE` to the variable: its name and
/// that value. Only a variable takes a bracketed metaoperator assignment.
// Cost: O(n), n = size of the source.
fn meta_assignment(node: CompoundAssign) -> Result<(String, Expr), PError> {
    let CompoundMeta::BracketedMeta(meta, _) = node.meta else {
        return Err(PError::expected("bracketed metaoperator assignment"));
    };
    let name = match &node.target {
        Expr::Var(name) => name.clone(),
        Expr::ArrayVar(name) => format!("@{name}"),
        Expr::HashVar(name) => format!("%{name}"),
        _ => {
            return Err(PError::expected(
                "variable before bracketed metaoperator assignment",
            ));
        }
    };
    Ok((
        name,
        Expr::MetaOp {
            meta: meta.spelling().to_string(),
            op: node.op,
            left: Box::new(node.target),
            right: Box::new(node.source),
        },
    ))
}

/// `VAR [X+]= SOURCE`, kept in its RakuAST shape by a `.AST` parse and
/// lowered at once otherwise.
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_meta_assignment(
    target: Expr,
    meta: crate::ast::parsed::BracketedMeta,
    depth: u8,
    op: String,
    source: Expr,
) -> Result<Expr, PError> {
    let node = CompoundAssign {
        target,
        op,
        meta: CompoundMeta::BracketedMeta(meta, depth),
        source,
    };
    if crate::ast::spelled::keeping() {
        return Ok(Expr::Ast(Box::new(ParsedExpr::CompoundAssign(node))));
    }
    lower_compound_expression(node)
}

/// [`written_meta_assignment`] written as a statement.
// Cost: O(n), n = size of the target and source.
pub(crate) fn written_meta_assignment_stmt(
    target: Expr,
    meta: crate::ast::parsed::BracketedMeta,
    depth: u8,
    op: String,
    source: Expr,
) -> Result<Stmt, PError> {
    let node = CompoundAssign {
        target,
        op,
        meta: CompoundMeta::BracketedMeta(meta, depth),
        source,
    };
    if crate::ast::spelled::keeping() {
        return Ok(Stmt::Expr(Expr::Ast(Box::new(ParsedExpr::CompoundAssign(
            node,
        )))));
    }
    lower_compound_statement(node)
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

/// `TARGET .= meth(args)`: the marker over the `.=` expansion, which writes
/// the method's result back through the target. As a statement, `$_ .= meth`
/// is the topic form, which can write through a read-only whole-container
/// topic (`given @a { .= uc }`). `is_term` says which barewords name a
/// sigilless term: the parser asks its scopes, `rakuast::lower` the
/// declarations its unit carries.
// Cost: O(n), n = size of the target and the call.
pub(crate) fn lower_dotty(
    node: CompoundAssign,
    as_statement: bool,
    is_term: fn(&str) -> bool,
) -> Expr {
    let CompoundAssign {
        target,
        source: call,
        ..
    } = node;
    if as_statement && matches!(&target, Expr::Var(topic) if topic == "_") {
        return crate::parser::stmt::simple_expr_stmt::topic_dot_assign(call);
    }
    crate::parser::expr::wrap_dot_assign_with_terms(
        target,
        |invocant| crate::parser::expr::retarget_dotty_call(&call, invocant),
        is_term,
    )
}

/// `TARGET .= meth(args)`, kept in its RakuAST shape by a `.AST` parse and
/// lowered at once otherwise. `call` is the method call over the target.
// Cost: O(n), n = size of the target and the call.
pub(crate) fn written_dotty_assignment(target: Expr, call: Expr, as_statement: bool) -> Expr {
    let node = CompoundAssign {
        target,
        op: ".".to_string(),
        meta: CompoundMeta::Dotty,
        source: call,
    };
    if crate::ast::spelled::keeping() {
        return Expr::Ast(Box::new(ParsedExpr::CompoundAssign(node)));
    }
    lower_dotty(
        node,
        as_statement,
        crate::parser::stmt::simple::is_user_declared_value_term,
    )
}

/// `.= meth(args)` written with no target: the topic's `.=`, kept as the
/// `Term::TopicCall` a `.AST` parse shows and lowered at once otherwise, the
/// same way `$_ .= meth(args)` is. `call` is the method call over `$_`.
// Cost: O(n), n = size of the call.
pub(crate) fn written_topic_dotty_assignment(call: Expr, as_statement: bool) -> Expr {
    let node = CompoundAssign {
        target: Expr::Var("_".to_string()),
        op: ".".to_string(),
        meta: CompoundMeta::TopicDotty,
        source: call,
    };
    if crate::ast::spelled::keeping() {
        return Expr::Ast(Box::new(ParsedExpr::CompoundAssign(node)));
    }
    if !as_statement {
        return topic_dotty_expression(node.source);
    }
    lower_dotty(
        node,
        true,
        crate::parser::stmt::simple::is_user_declared_value_term,
    )
}

/// The bare `.= meth` in expression position (the right side of an
/// `andthen`/`orelse` chain): the plain `$_ = $_.meth` assignment, which the
/// chain retargets to its root variable. The statement form writes through a
/// whole-container topic instead (`given @a { .=uc }`).
// Cost: O(1).
fn topic_dotty_expression(call: Expr) -> Expr {
    Expr::AssignExpr {
        name: "_".to_string(),
        expr: Box::new(call),
        is_bind: false,
    }
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
