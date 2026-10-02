//! A variable declaration's `is default(EXPR)` trait across the RakuAST
//! boundary.
//!
//! Measured against rakudo 2026.09, `my $x is default(3) = 5` is
//!
//! ```text
//! VarDeclaration::Simple(sigil => "$", desigilname => …,
//!   traits => (Trait::Is(name => Name.from-identifier("default"),
//!              argument => Circumfix::Parentheses(SemiList(Statement::Expression(3)))),),
//!   initializer => Initializer::Assign(5))
//! ```
//!
//! The parser keeps a declaration's traits in source order as
//! `VarDecl.custom_traits` (beside its `__has_initializer` marker), so the list
//! renders in that order; any trait other than `is default(…)` stays refused.

use super::convert::{convert_expr, name_from_identifier, node_field, statement_expression};
use super::lower::{named_child, named_child_or_positional, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The parser's `custom_traits` entry for an `is default(…)` trait.
const DEFAULT: &str = "default";

/// Whether the converter renders `custom_traits` entry `(name, arg)`.
pub(super) fn is_rendered(name: &str, arg: &Option<Expr>) -> bool {
    name == DEFAULT && arg.is_some()
}

/// The `traits` items for a declaration's rendered custom traits, in source
/// order.
// Cost: O(t), t = custom traits of the declaration.
pub(super) fn convert(
    custom_traits: &[(String, Option<Expr>)],
) -> Result<Vec<Value>, RuntimeError> {
    let mut items = Vec::new();
    for (name, arg) in custom_traits {
        if !is_rendered(name, arg) {
            continue;
        }
        let Some(arg) = arg else { continue };
        let semilist = RakuAstNode {
            class: RakuAstClass::SemiList,
            fields: vec![node_field(None, statement_expression(convert_expr(arg)?))],
        };
        let argument = RakuAstNode {
            class: RakuAstClass::CircumfixParentheses,
            fields: vec![node_field(None, semilist)],
        };
        items.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields: vec![
                node_field(Some("name"), name_from_identifier(name)),
                node_field(Some("argument"), argument),
            ],
        })));
    }
    Ok(items)
}

/// Put `items` into `decl` as its `traits` field, ahead of the initializer.
// Cost: O(f), f = fields of `decl`.
pub(super) fn insert(decl: &mut RakuAstNode, items: Vec<Value>) {
    if items.is_empty() {
        return;
    }
    let at = decl
        .fields
        .iter()
        .position(|f| f.name == Some("initializer"))
        .unwrap_or(decl.fields.len());
    decl.fields.insert(
        at,
        RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(items),
        },
    );
}

/// A declaration's `traits` back as `custom_traits` entries, in order.
// Cost: O(t), t = traits of the declaration.
pub(super) fn lower(node: &RakuAstNode) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
    let refuse = || super::lower::unsupported(node);
    let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok(Vec::new());
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Err(refuse());
    };
    let mut traits = Vec::with_capacity(items.len());
    for item in items {
        let ValueView::RakuAst(t) = item.view() else {
            return Err(refuse());
        };
        if t.class != RakuAstClass::TraitIs || t.fields.len() != 2 {
            return Err(refuse());
        }
        let name = positional_leaf(named_child(t, "name")?)?;
        if !matches!(name.view(), ValueView::Str(s) if s.as_str() == DEFAULT) {
            return Err(refuse());
        }
        let argument = named_child(t, "argument")?;
        if argument.class != RakuAstClass::CircumfixParentheses {
            return Err(refuse());
        }
        let statement = named_child_or_positional(named_child_or_positional(argument)?)?;
        if statement.class != RakuAstClass::StatementExpression {
            return Err(refuse());
        }
        let value = super::lower::lower_expr(named_child(statement, "expression")?)?;
        traits.push((DEFAULT.to_string(), Some(value)));
    }
    Ok(traits)
}
