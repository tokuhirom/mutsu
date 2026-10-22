//! A declaration or variable assignment with a loose tail across the RakuAST boundary.
//!
//! `my $x = 1 and 2` and `my $x = 1, 2, 3` are one expression
//! in RakuAST, with the declaration as the leftmost operand of the whole thing:
//!
//! ```text
//! my $x = 1 and 2   ApplyInfix(left => VarDeclaration::Simple, infix => Infix("and"), right)
//! my $x = 1, 2, 3   ApplyListInfix(Infix(","), operands => (VarDeclaration::Simple, 2, 3))
//! ```
//!
//! The parser splits it into a head and a tail that re-reads the variable
//! ([`crate::ast::decl_tail`]), so the converter puts the head back in place of
//! that re-read, and lowering takes it out again.

use super::convert::{
    anonymous_declaration, convert_expr, convert_stmt, expression_of, node_field, plain_infix,
    split_sigil, var_lexical,
};
use super::lower::{lower_expr, lower_stmt, named_child, rakuast_node_of};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

/// The operand an expression node starts with: the left of an infix, the first
/// operand of a list infix, and (with `postfix`) the operand of a postfix.
///
/// Lowering leaves a postfix out on purpose: rakudo drops the parentheses of a
/// postfix's operand, so `(my @a).push(1)` and `my @a.push(1)` are the same
/// node and cannot be told apart again. They compute the same thing, so the
/// converter may build that node for either, and lowering reads it back the
/// parenthesized way.
// Cost: O(1).
fn leading_operand(node: &RakuAstNode, postfix: bool) -> Option<&RakuAstNode> {
    match node.class {
        RakuAstClass::ApplyInfix => named_child(node, "left").ok(),
        RakuAstClass::ApplyPostfix if postfix => named_child(node, "operand").ok(),
        RakuAstClass::ApplyListInfix => {
            let field = node.fields.iter().find(|f| f.name == Some("operands"))?;
            match &field.value {
                RakuAstFieldValue::List(items) => items.first().and_then(rakuast_node_of),
                _ => None,
            }
        }
        _ => None,
    }
}

/// The node at the end of the chain of leading operands, and how long the
/// chain is.
// Cost: O(d), d = depth of the chain.
fn leftmost(node: &RakuAstNode, postfix: bool) -> (&RakuAstNode, usize) {
    let mut current = node;
    let mut depth = 0;
    while let Some(next) = leading_operand(current, postfix) {
        current = next;
        depth += 1;
    }
    (current, depth)
}

/// `node` with the end of its chain of leading operands replaced.
// Cost: O(d), d = depth of the chain.
fn replace_leftmost(node: &RakuAstNode, replacement: &RakuAstNode) -> RakuAstNode {
    if leading_operand(node, true).is_none() {
        return replacement.clone();
    }
    let leading_field = match node.class {
        RakuAstClass::ApplyInfix => "left",
        RakuAstClass::ApplyPostfix => "operand",
        _ => "operands",
    };
    let fields = node
        .fields
        .iter()
        .map(|field| {
            if field.name != Some(leading_field) {
                return field.clone();
            }
            let value = match &field.value {
                RakuAstFieldValue::Node(child) => match rakuast_node_of(child) {
                    Some(child) => RakuAstFieldValue::Node(Value::rakuast(Box::new(
                        replace_leftmost(child, replacement),
                    ))),
                    None => field.value.clone(),
                },
                RakuAstFieldValue::List(items) => RakuAstFieldValue::List(
                    items
                        .iter()
                        .enumerate()
                        .map(|(at, item)| match (at, rakuast_node_of(item)) {
                            (0, Some(first)) => {
                                Value::rakuast(Box::new(replace_leftmost(first, replacement)))
                            }
                            _ => item.clone(),
                        })
                        .collect(),
                ),
                other => other.clone(),
            };
            RakuAstField {
                name: field.name,
                value,
            }
        })
        .collect();
    RakuAstNode {
        class: node.class,
        fields,
    }
}

/// The variable a declaration or assignment statement names, as the parser
/// spells it (`x`, `@a`, `%h`).
// Cost: O(1).
fn declared_variable(head: &Stmt) -> Option<(&str, &str)> {
    crate::ast::decl_tail::head_name(head).map(split_sigil)
}

/// A recognized `SyntheticBlock([head, Expr(tail)])` as the one expression it
/// was written as; `None` for any other statement.
// Cost: O(n), n = size of the statement.
pub(super) fn convert(stmt: &Stmt) -> Option<Result<RakuAstNode, RuntimeError>> {
    let (head, tail) = crate::ast::decl_tail::recognize(stmt)?;
    Some((|| {
        let converted = convert_expr(tail)?;
        if let Some((sigil, name)) = declared_variable(head) {
            let (bottom, depth) = leftmost(&converted, true);
            // The tail must start with a re-read of the variable the head declares.
            let reread = if name == "__ANON_STATE#" {
                anonymous_declaration(None, "$", None)
            } else {
                var_lexical(sigil, name)
            };
            if depth == 0 || *bottom != reread {
                return Err(unsupported_statement());
            }
            let operand = if let Some(expr) = compound_assignment_expr(head) {
                convert_expr(expr)?
            } else {
                let statement = convert_stmt(head)?.ok_or_else(unsupported_statement)?;
                super::convert::expression_of(&statement).ok_or_else(unsupported_statement)?
            };
            return Ok(replace_leftmost(&converted, &operand));
        }

        let decl =
            crate::ast::decl_tail::signature_declaration(head).ok_or_else(unsupported_statement)?;
        let declaration = super::signature_decl::convert(decl)?;
        replace_group_seed(&converted, &declaration).ok_or_else(unsupported_statement)
    })())
}

/// `my Int $c .= new(), 5, 6`: the parser keeps the `.=` declaration and each
/// further list item as sunk statements of one scopeless block, with no
/// re-read of the variable, so [`convert`] cannot take it apart. RakuAST has
/// one comma application whose first operand is the declaration.
// Cost: O(n), n = size of the statement.
pub(super) fn convert_call_assign_list(stmt: &Stmt) -> Option<Result<RakuAstNode, RuntimeError>> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    let [head, tail @ ..] = stmts.as_slice() else {
        return None;
    };
    let Stmt::VarDecl {
        expr: Expr::CompoundAssign { op, .. },
        ..
    } = head
    else {
        return None;
    };
    if op != crate::ast::method_assign_decl::DECL_INITIALIZER_OP || tail.is_empty() {
        return None;
    }
    let items = tail
        .iter()
        .map(|s| match s {
            Stmt::Expr(expr) => Some(expr),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    let (sigil, name) = declared_variable(head)?;
    Some((|| {
        let first = convert_expr(items[0])?;
        if leftmost(&first, true).0 == &var_lexical(sigil, name) {
            // A postfix chain over the declared variable, not a list item.
            return Err(unsupported_statement());
        }
        let statement = convert_stmt(head)?.ok_or_else(unsupported_statement)?;
        let declaration = expression_of(&statement).ok_or_else(unsupported_statement)?;
        let mut operands = vec![Value::rakuast(Box::new(declaration))];
        operands.push(Value::rakuast(Box::new(first)));
        for item in &items[1..] {
            operands.push(Value::rakuast(Box::new(convert_expr(item)?)));
        }
        Ok(RakuAstNode {
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(Some("infix"), plain_infix(",")),
                RakuAstField {
                    name: Some("operands"),
                    value: RakuAstFieldValue::List(operands),
                },
            ],
        })
    })())
}

// Cost: O(d), d = depth of the left operand chain.
fn replace_group_seed(node: &RakuAstNode, declaration: &RakuAstNode) -> Option<RakuAstNode> {
    let leading = leading_operand(node, false)?;
    if is_comma_list(leading) {
        return replace_leading(node, declaration);
    }
    let replaced = replace_group_seed(leading, declaration)?;
    replace_leading(node, &replaced)
}

// Cost: O(d), d = depth of the left operand chain.
fn replace_signature_operand(node: &RakuAstNode, seed: &RakuAstNode) -> Option<RakuAstNode> {
    let leading = leading_operand(node, false)?;
    if leading.class == RakuAstClass::VarDeclarationSignature {
        return replace_leading(node, seed);
    }
    let replaced = replace_signature_operand(leading, seed)?;
    replace_leading(node, &replaced)
}

// Cost: O(n), n = number of operands in the list node.
fn replace_leading(node: &RakuAstNode, replacement: &RakuAstNode) -> Option<RakuAstNode> {
    let field_name = match node.class {
        RakuAstClass::ApplyInfix => "left",
        RakuAstClass::ApplyListInfix => "operands",
        _ => return None,
    };
    let fields = node
        .fields
        .iter()
        .map(|field| {
            if field.name != Some(field_name) {
                return Some(field.clone());
            }
            let value = match (&field.value, node.class) {
                (RakuAstFieldValue::Node(_), RakuAstClass::ApplyInfix) => {
                    RakuAstFieldValue::Node(Value::rakuast(Box::new(replacement.clone())))
                }
                (RakuAstFieldValue::List(items), RakuAstClass::ApplyListInfix) => {
                    let mut items = items.clone();
                    let first = items.first_mut()?;
                    *first = Value::rakuast(Box::new(replacement.clone()));
                    RakuAstFieldValue::List(items)
                }
                _ => return None,
            };
            Some(RakuAstField {
                name: field.name,
                value,
            })
        })
        .collect::<Option<Vec<_>>>()?;
    Some(RakuAstNode {
        class: node.class,
        fields,
    })
}

// Cost: O(1).
fn is_comma_list(node: &RakuAstNode) -> bool {
    if node.class != RakuAstClass::ApplyListInfix {
        return false;
    }
    let Some(infix) = node.fields.iter().find(|field| field.name == Some("infix")) else {
        return false;
    };
    let RakuAstFieldValue::Node(value) = &infix.value else {
        return false;
    };
    let ValueView::RakuAst(infix) = value.view() else {
        return false;
    };
    matches!(infix.fields.first(), Some(RakuAstField {
        value: RakuAstFieldValue::Node(value), ..
    }) if matches!(value.view(), ValueView::Str(op) if op.as_str() == ","))
}

// Cost: O(1).
fn compound_assignment_expr(head: &Stmt) -> Option<&crate::ast::Expr> {
    match head {
        Stmt::Expr(expr @ crate::ast::Expr::CompoundAssign { .. }) => Some(expr),
        _ => None,
    }
}

fn unsupported_statement() -> RuntimeError {
    super::convert::unsupported("a declaration with a loose tail")
}

/// An expression node whose leading operand is a declaration or an assignment
/// to a variable, as the parser's `SyntheticBlock([declaration, Expr(tail)])`.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Stmt, RuntimeError>> {
    let (bottom, depth) = leftmost(node, false);
    let is_declaration = bottom.class == RakuAstClass::VarDeclarationSimple;
    let is_compound_assignment = bottom.class == RakuAstClass::ApplyInfix
        && super::lower::infix_is_compound_assignment(bottom);
    let is_signature_declaration = bottom.class == RakuAstClass::VarDeclarationSignature;
    if depth == 0 || (!is_declaration && !is_compound_assignment && !is_signature_declaration) {
        return None;
    }
    let head = if is_declaration || is_signature_declaration {
        match lower_stmt(bottom) {
            Ok(head) => head,
            Err(error) => return Some(Err(error)),
        }
    } else {
        match lower_expr(bottom) {
            Ok(expr) => Stmt::Expr(expr),
            Err(error) => return Some(Err(error)),
        }
    };
    if is_signature_declaration {
        let seed = signature_seed(&head)?;
        let seed_node = match convert_expr(&seed) {
            Ok(node) => node,
            Err(error) => return Some(Err(error)),
        };
        let Some(tail_node) = replace_signature_operand(node, &seed_node) else {
            return Some(Err(unsupported_statement()));
        };
        let tail = match lower_expr(&tail_node) {
            Ok(tail) => tail,
            Err(error) => return Some(Err(error)),
        };
        return Some(Ok(crate::ast::decl_tail::expand(head, tail)));
    }
    let (sigil, name) = declared_variable(&head)?;
    let seed = var_lexical(sigil, name);
    let tail = match lower_expr(&replace_leftmost(node, &seed)) {
        Ok(tail) => tail,
        Err(error) => return Some(Err(error)),
    };
    Some(Ok(crate::ast::decl_tail::expand(head, tail)))
}

// Cost: O(v), v = number of variables in the grouped declaration.
fn signature_seed(head: &Stmt) -> Option<Expr> {
    let decl = crate::ast::decl_tail::signature_declaration(head)?;
    if decl.is_state || decl.is_our || decl.type_constraint.is_some() || decl.vars.is_empty() {
        return None;
    }
    let vars = decl
        .vars
        .iter()
        .map(|var| {
            if var.is_slurpy
                || var.is_optional
                || var.is_named
                || var.default.is_some()
                || var.per_var_type_constraint.is_some()
                || var.where_constraint.is_some()
                || var.sigilless
                || var.literal_value.is_some()
                || var.param_trait.is_some()
            {
                return None;
            }
            Some(if let Some(name) = var.name.strip_prefix('@') {
                Expr::ArrayVar(name.to_string())
            } else if let Some(name) = var.name.strip_prefix('%') {
                Expr::HashVar(name.to_string())
            } else if var.name.starts_with('&') {
                return None;
            } else {
                Expr::Var(var.name.clone())
            })
        })
        .collect::<Option<Vec<_>>>()?;
    Some(Expr::ArrayLiteral(vars))
}
