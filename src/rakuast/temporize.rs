//! `temp` / `let` in RakuAST, measured on rakudo 2026.09.
//!
//! Both are a prefix operator over an lvalue: `temp $x` is
//! `ApplyPrefix(Prefix "temp", Var::Lexical)`, an assignment around it is the
//! ordinary `ApplyInfix(ApplyPrefix, Assignment, value)`, a compound one a
//! `MetaInfix::Assign`, and `temp my $x = 1` puts the declaration under the
//! prefix. The parser saves the variable in a `Stmt::Let` and assigns inside
//! it (see `ast::temporize`); this module renders that as the prefix node and
//! rebuilds the same statement from it.

use super::convert::{convert_expr, leaf_field, node_field};
use super::lower::{
    infix_is_assignment, infix_is_compound_assignment, lower_compound_assign_expr,
    lower_dotty_assign, lower_expr, lower_var_decl, named_child, positional_leaf, unsupported,
};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::temporize::{self, Assigned, Target, Temporized};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

/// `ApplyPrefix(Prefix "temp" | "let", operand)`.
fn prefix_node(is_temp: bool, operand: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::ApplyPrefix,
        fields: vec![
            node_field(
                Some("prefix"),
                RakuAstNode {
                    class: RakuAstClass::Prefix,
                    fields: vec![leaf_field(
                        None,
                        Value::str(if is_temp { "temp" } else { "let" }.to_string()),
                    )],
                },
            ),
            node_field(Some("operand"), operand),
        ],
    }
}

/// The lvalue under the prefix.
fn operand_node(target: &Target) -> Result<RakuAstNode, RuntimeError> {
    match target {
        Target::Variable(key) => convert_expr(&temporize::variable_expr(key)),
        Target::Element(element) => convert_expr(element),
        Target::Declaration(decl) => {
            // The declaration is a term here, as it is inside any expression.
            convert_expr(&Expr::DoStmt(decl.clone()))
        }
    }
}

/// Whether the target is a scalar: rakudo marks only a scalar variable's
/// assignment `:item`.
fn is_scalar_variable(target: &Target) -> bool {
    matches!(target, Target::Variable(key) if !key.starts_with(['@', '%']))
}

/// The expression node a `temp` / `let` statement is, or `None` when `stmt`
/// is not one the parser builds.
// Cost: O(n), n = size of the statement.
pub(super) fn convert(stmt: &Stmt) -> Option<Result<RakuAstNode, RuntimeError>> {
    let temporized = temporize::recognize(stmt)?;
    Some(convert_temporized(&temporized))
}

fn convert_temporized(t: &Temporized) -> Result<RakuAstNode, RuntimeError> {
    match &t.assigned {
        Assigned::No => Ok(prefix_node(t.is_temp, operand_node(&t.target)?)),
        Assigned::Plain(value) => super::convert::assignment_around(
            prefix_node(t.is_temp, operand_node(&t.target)?),
            is_scalar_variable(&t.target),
            value,
        ),
        Assigned::Compound(Expr::CompoundAssign {
            target, op, rhs, ..
        }) => {
            let left = prefix_node(t.is_temp, convert_expr(target)?);
            if op == crate::parser::DOTTY_ASSIGN_OP {
                super::convert::dotty_assignment_with_left(left, rhs)
            } else {
                super::convert::compound_assignment_with_left(left, op, rhs)
            }
        }
        Assigned::Compound(_) => Err(super::convert::unsupported(
            "temp with an unrecognised compound assignment",
        )),
    }
}

/// The prefix's name when `node` is `ApplyPrefix(Prefix "temp" | "let")`.
fn temporizer(node: &RakuAstNode) -> Option<bool> {
    if node.class != RakuAstClass::ApplyPrefix {
        return None;
    }
    let prefix = named_child(node, "prefix").ok()?;
    match positional_leaf(prefix).ok()?.view() {
        ValueView::Str(s) if s.as_str() == "temp" => Some(true),
        ValueView::Str(s) if s.as_str() == "let" => Some(false),
        _ => None,
    }
}

/// Whether `node` is a `temp` / `let` the lowering rebuilds: the prefix
/// itself, or the assignment or dotty assignment whose left side is one.
// Cost: O(1).
pub(super) fn is_temporized(node: &RakuAstNode) -> bool {
    match node.class {
        RakuAstClass::ApplyPrefix => temporizer(node).is_some(),
        RakuAstClass::ApplyInfix | RakuAstClass::ApplyDottyInfix => {
            named_child(node, "left").is_ok_and(|left| temporizer(left).is_some())
        }
        _ => false,
    }
}

/// `node` with its `left` operand replaced.
fn with_left(node: &RakuAstNode, left: &RakuAstNode) -> RakuAstNode {
    let mut fields: Vec<_> = node
        .fields
        .iter()
        .filter(|f| f.name != Some("left"))
        .cloned()
        .collect();
    fields.insert(0, node_field(Some("left"), left.clone()));
    RakuAstNode {
        class: node.class,
        fields,
    }
}

/// The `temp` / `let` statement the parser builds for `node`.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let (prefix, wrapper) = match node.class {
        RakuAstClass::ApplyPrefix => (node, None),
        _ => (named_child(node, "left")?, Some(node)),
    };
    let is_temp = temporizer(prefix).ok_or_else(|| unsupported(node))?;
    let operand = named_child(prefix, "operand")?;
    // What follows the prefix: nothing, `= value`, or `OP= value`.
    enum After<'a> {
        Nothing,
        Value(Expr),
        Compound(&'a RakuAstNode),
        Dotty(&'a RakuAstNode),
    }
    let after = match wrapper {
        None => After::Nothing,
        Some(w) if w.class == RakuAstClass::ApplyDottyInfix => After::Dotty(w),
        Some(w) if infix_is_assignment(w) => After::Value(lower_expr(named_child(w, "right")?)?),
        Some(w) if infix_is_compound_assignment(w) => After::Compound(w),
        Some(_) => return Err(unsupported(node)),
    };
    // A declaration under the prefix: the declaration, then the bare save.
    if operand.class == RakuAstClass::VarDeclarationSimple {
        if !matches!(after, After::Nothing) {
            return Err(unsupported(node));
        }
        let decl = lower_var_decl(operand)?;
        let Stmt::VarDecl { name, .. } = &decl else {
            return Err(unsupported(node));
        };
        let saved = temporize::save(name, None, None, true);
        return Ok(Stmt::SyntheticBlock(vec![decl, saved]));
    }
    let lvalue = lower_expr(operand)?;
    let (name, index) = match &lvalue {
        Expr::Index { target, index, .. } => (
            temporize::base_name(target).ok_or_else(|| unsupported(node))?,
            Some(index.as_ref()),
        ),
        other => (
            other.container_var_key().ok_or_else(|| unsupported(node))?,
            None,
        ),
    };
    // A compound or dotty assignment re-parses from the variable on: lower the
    // assignment node over the bare lvalue and run it after the save.
    let compound = match &after {
        After::Compound(w) => Some(lower_compound_assign_expr(&with_left(w, operand))?),
        After::Dotty(w) => Some(lower_dotty_assign(&with_left(w, operand), true)?),
        _ => None,
    };
    if let Some(assignment) = compound {
        let single = single_level(&lvalue);
        if index.is_some() && !single {
            return Err(unsupported(node));
        }
        return Ok(Stmt::SyntheticBlock(vec![
            temporize::save(&name, index, None, is_temp),
            Stmt::Expr(assignment),
        ]));
    }
    let value = match after {
        After::Value(value) => Some(value),
        _ => None,
    };
    match (&lvalue, index) {
        // A single-level element of a plain variable.
        (Expr::Index { .. }, Some(index)) if single_level(&lvalue) => {
            Ok(temporize::save(&name, Some(index), value, is_temp))
        }
        // A deeper subscript or a parenthesised container: the element
        // assignment (or the bare element) rides in the save.
        (
            Expr::Index {
                target,
                index,
                is_positional,
            },
            Some(_),
        ) => {
            let carried = match value {
                Some(value) => Expr::IndexAssign {
                    target: target.clone(),
                    index: index.clone(),
                    value: Box::new(value),
                    is_positional: *is_positional,
                },
                None => lvalue.clone(),
            };
            Ok(temporize::save_nested(&name, carried, is_temp))
        }
        _ => Ok(temporize::save(&name, None, value, is_temp)),
    }
}

/// Whether the element is a subscript of a plain variable (`@a[1]`, `%h<k>`).
fn single_level(lvalue: &Expr) -> bool {
    matches!(
        lvalue,
        Expr::Index { target, .. }
            if matches!(target.as_ref(), Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_))
    )
}
