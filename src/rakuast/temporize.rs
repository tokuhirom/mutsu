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
    lower_dotty_assign, lower_expr, lower_var_decl, named_child, named_child_or_positional,
    positional_leaf, unsupported,
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

/// `temp $obj.method = value` / `temp $.attribute OP= value` as the written
/// assignment around a `temp` prefix. The parser's statement stores the
/// already-expanded writeback value for the compiler; its identity marker
/// preserves the compound operator and original left operand for this model.
// Cost: O(e), e = size of the assignment value and method arguments.
pub(super) fn convert_method_assign(
    var_name: &str,
    method_name: &str,
    method_args: &[Expr],
    value: &Expr,
    is_attribute: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let target = if is_attribute {
        RakuAstNode {
            class: RakuAstClass::VarAttributePublic,
            fields: vec![leaf_field(
                Some("name"),
                Value::str(format!("$.{method_name}")),
            )],
        }
    } else {
        let invocant = if var_name == "self" {
            Expr::BareWord("self".to_string())
        } else {
            Expr::Var(var_name.to_string())
        };
        convert_expr(&Expr::MethodCall {
            target: Box::new(invocant),
            name: crate::symbol::Symbol::intern(method_name),
            args: method_args.to_vec(),
            modifier: None,
            quoted: false,
            sugar: false,
        })?
    };
    let left = prefix_node(true, target);
    if let Expr::Binary {
        left: marked,
        op,
        right,
        ..
    } = value
        && matches!(
            marked.as_ref(),
            Expr::Unary {
                op: crate::token_kind::TokenKind::MetaAssignIdentity(_),
                ..
            }
        )
    {
        let operator = crate::compiler::helpers_ops::token_kind_to_op_name(op);
        return super::convert::compound_assignment_with_left(left, &format!("{operator}="), right);
    }
    super::convert::assignment_around(left, is_attribute, value)
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
    if let Some(statement) = lower_method_assign(node)? {
        return Ok(statement);
    }
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
                ..
            },
            Some(_),
        ) => {
            let carried = match value {
                Some(value) => Expr::IndexAssign {
                    target: target.clone(),
                    index: index.clone(),
                    value: Box::new(value),
                    is_positional: *is_positional,
                    spelling: Default::default(),
                },
                None => lvalue.clone(),
            };
            Ok(temporize::save_nested(&name, carried, is_temp))
        }
        _ => Ok(temporize::save(&name, None, value, is_temp)),
    }
}

/// A temporary method/attribute lvalue is already represented as the source
/// prefix plus assignment. Recover its shared save/restore statement directly.
// Cost: O(e), e = size of the lvalue and assignment expression.
fn lower_method_assign(node: &RakuAstNode) -> Result<Option<Stmt>, RuntimeError> {
    let (prefix, wrapper) = match node.class {
        RakuAstClass::ApplyPrefix => (node, None),
        RakuAstClass::ApplyInfix => (named_child(node, "left")?, Some(node)),
        _ => return Ok(None),
    };
    if !temporizer(prefix).unwrap_or(false) {
        return Ok(None);
    }
    let operand = named_child(prefix, "operand")?;
    let (var_name, method_name, method_args, is_attribute) = if operand.class
        == RakuAstClass::VarAttributePublic
        && !operand
            .fields
            .iter()
            .any(|field| field.name == Some("args"))
    {
        let spelling = super::lower::leaf_str(operand, "name")?;
        let Some(method) = spelling.strip_prefix("$.") else {
            return Err(unsupported(node));
        };
        ("self".to_string(), method.to_string(), Vec::new(), true)
    } else {
        let Expr::MethodCall {
            target, name, args, ..
        } = lower_expr(operand)?
        else {
            return Ok(None);
        };
        let var_name = match target.as_ref() {
            Expr::Var(name) if name == "__ANON_STATE#" || name == "self" => "self".to_string(),
            Expr::Var(name) => name.clone(),
            Expr::BareWord(name) if name == "self" => "self".to_string(),
            _ => return Err(unsupported(node)),
        };
        (var_name, name.resolve(), args, false)
    };
    let value = match wrapper {
        None => return Ok(None),
        Some(wrapper) if infix_is_assignment(wrapper) => {
            lower_expr(named_child(wrapper, "right")?)?
        }
        Some(wrapper) if infix_is_compound_assignment(wrapper) => {
            let left = lower_expr(operand)?;
            let meta = named_child(wrapper, "infix")?;
            let base = match positional_leaf(named_child_or_positional(meta)?)?.view() {
                ValueView::Str(operator) => operator.to_string(),
                _ => return Err(unsupported(node)),
            };
            let compound = crate::parser::compound_assign_op_from_name(&base)
                .ok_or_else(|| unsupported(node))?;
            let right = lower_expr(named_child(wrapper, "right")?)?;
            crate::parser::compound_assigned_value_expr(left, compound, right)
        }
        Some(_) => return Ok(None),
    };
    Ok(Some(Stmt::TempMethodAssign {
        var_name,
        method_name,
        method_args,
        value,
        is_attribute,
    }))
}

/// Whether the element is a subscript of a plain variable (`@a[1]`, `%h<k>`).
fn single_level(lvalue: &Expr) -> bool {
    matches!(
        lvalue,
        Expr::Index { target, .. }
            if matches!(target.as_ref(), Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_))
    )
}
