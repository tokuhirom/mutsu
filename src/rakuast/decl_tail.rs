//! A declaration or assignment with a loose tail across the RakuAST boundary.
//!
//! `my $x = 1 and 2`, `my $x = 1, 2, 3` and `my $s = 1 .foo` are one expression
//! in RakuAST, with the declaration as the leftmost operand of the whole thing:
//!
//! ```text
//! my $x = 1 and 2   ApplyInfix(left => VarDeclaration::Simple, infix => Infix("and"), right)
//! my $x = 1, 2, 3   ApplyListInfix(Infix(","), operands => (VarDeclaration::Simple, 2, 3))
//! ```
//!
//! The parser splits it into the declaration and a tail that re-reads the
//! variable ([`crate::ast::decl_tail`]), so the converter puts the declaration
//! back in place of that re-read, and lowering takes it out again.

use super::convert::{convert_expr, convert_stmt, split_sigil, var_lexical};
use super::lower::{lower_expr, lower_stmt, named_child, rakuast_node_of};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Stmt;
use crate::value::{RuntimeError, Value};

/// The operand an expression node starts with: the left of an infix, the
/// operand of a postfix, the first operand of a list infix.
// Cost: O(1).
fn leading_operand(node: &RakuAstNode) -> Option<&RakuAstNode> {
    match node.class {
        RakuAstClass::ApplyInfix => named_child(node, "left").ok(),
        RakuAstClass::ApplyPostfix => named_child(node, "operand").ok(),
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
fn leftmost(node: &RakuAstNode) -> (&RakuAstNode, usize) {
    let mut current = node;
    let mut depth = 0;
    while let Some(next) = leading_operand(current) {
        current = next;
        depth += 1;
    }
    (current, depth)
}

/// `node` with the end of its chain of leading operands replaced.
// Cost: O(d), d = depth of the chain.
fn replace_leftmost(node: &RakuAstNode, replacement: &RakuAstNode) -> RakuAstNode {
    if leading_operand(node).is_none() {
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
    let (sigil, name) = declared_variable(head)?;
    Some((|| {
        let converted = convert_expr(tail)?;
        let (bottom, depth) = leftmost(&converted);
        // The tail must start with a re-read of the variable the head declares.
        if depth == 0 || *bottom != var_lexical(sigil, name) {
            return Err(unsupported_statement());
        }
        let statement = convert_stmt(head)?.ok_or_else(unsupported_statement)?;
        let declaration = super::convert::expression_of(&statement)
            .ok_or_else(unsupported_statement)?;
        Ok(replace_leftmost(&converted, &declaration))
    })())
}

fn unsupported_statement() -> RuntimeError {
    super::convert::unsupported("a declaration with a loose tail")
}

/// An expression node whose leading operand is a declaration or an assignment
/// to a variable, as the parser's `SyntheticBlock([declaration, Expr(tail)])`.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Stmt, RuntimeError>> {
    let (bottom, depth) = leftmost(node);
    if depth == 0 || bottom.class != RakuAstClass::VarDeclarationSimple {
        return None;
    }
    let head = match lower_stmt(bottom) {
        Ok(head) => head,
        Err(error) => return Some(Err(error)),
    };
    let (sigil, name) = declared_variable(&head)?;
    let seed = var_lexical(sigil, name);
    let tail = match lower_expr(&replace_leftmost(node, &seed)) {
        Ok(tail) => tail,
        Err(error) => return Some(Err(error)),
    };
    Some(Ok(crate::ast::decl_tail::expand(head, tail)))
}
