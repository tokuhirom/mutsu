//! A variable declaration's `is default(EXPR)` and `is TYPE` traits across
//! the RakuAST boundary.
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
//! renders in that order.
//!
//! `my %h is SetHash` is `Trait::Is(type => Type::Simple(SetHash))`: rakudo
//! renders `is NAME` as a container type when `NAME` resolves to a type at
//! parse time, and the parser records it as a bare `(NAME, None)` entry. Any
//! other trait stays refused.

use super::convert::{name_from_identifier, node_field};
use super::lower::{named_child, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The parser's `custom_traits` entry for an `is default(…)` trait.
const DEFAULT: &str = "default";

/// A declaration's `custom_traits` entries.
type CustomTraits = Vec<(String, Option<Expr>)>;

/// The trait name of `is dynamic`.
const DYNAMIC: &str = "dynamic";

/// Whether the converter renders `custom_traits` entry `(name, arg)`.
pub(super) fn is_rendered(name: &str, arg: &Option<Expr>) -> bool {
    match arg {
        Some(_) => name == DEFAULT || is_custom_name(name),
        None => is_container_type(name) || is_custom_name(name),
    }
}

/// Whether `name` is a plain trait name a program can give a variable with
/// its own `trait_mod:<is>` (`is marked`, `is checked(5)`), as opposed to an
/// internal marker or a type spelling.
fn is_custom_name(name: &str) -> bool {
    !name.starts_with("__")
        && !name.is_empty()
        && name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
}

/// Whether a bare `is NAME` entry is a container type (`is SetHash`).
fn is_container_type(name: &str) -> bool {
    !name.starts_with("__")
        && super::name_parts::identifier_segments(name)
            .nth(1)
            .is_none()
        && super::bareword::names_type(name)
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
        let Some(arg) = arg else {
            // `is NAME`: a type when it names one at parse time, else a trait
            // name.
            let field = if is_container_type(name) {
                node_field(Some("type"), super::bareword::simple_type_node(name))
            } else {
                node_field(Some("name"), name_from_identifier(name))
            };
            items.push(Value::rakuast(Box::new(RakuAstNode {
                class: RakuAstClass::TraitIs,
                fields: vec![field],
            })));
            continue;
        };
        // A list `(a, b)` is the parser's `Grouped(ArrayLiteral)`, written
        // inside the argument's own parentheses.
        let argument = match arg {
            Expr::Grouped(inner) if matches!(**inner, Expr::ArrayLiteral(_)) => {
                super::attribute::paren_argument(inner)?
            }
            other => super::attribute::paren_argument(other)?,
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

/// `is dynamic`, which the parser keeps as the declaration's `is_dynamic`
/// flag rather than as a `custom_traits` entry.
pub(super) fn dynamic_trait() -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::TraitIs,
        fields: vec![node_field(Some("name"), name_from_identifier(DYNAMIC))],
    }))
}

/// A declaration's `traits` back as `custom_traits` entries, in order, and
/// whether one of them was `is dynamic`.
// Cost: O(t + a), t = traits of the declaration, a = size of their arguments.
pub(super) fn lower(node: &RakuAstNode) -> Result<(CustomTraits, bool), RuntimeError> {
    let refuse = || super::lower::unsupported(node);
    let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok((Vec::new(), false));
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Err(refuse());
    };
    let mut traits = Vec::with_capacity(items.len());
    let mut is_dynamic = false;
    for item in items {
        let ValueView::RakuAst(t) = item.view() else {
            return Err(refuse());
        };
        if t.class != RakuAstClass::TraitIs {
            return Err(refuse());
        }
        // `is TYPE`: a container type.
        if let [field] = t.fields.as_slice()
            && field.name == Some("type")
        {
            let type_node = named_child(t, "type")?;
            if type_node.class != RakuAstClass::TypeSimple {
                return Err(refuse());
            }
            let name = super::type_lower::type_constraint(t, type_node)?;
            traits.push((name, None));
            continue;
        }
        let name = match positional_leaf(named_child(t, "name")?)?.view() {
            ValueView::Str(s) => s.to_string(),
            _ => return Err(refuse()),
        };
        if !is_custom_name(&name) && name != DEFAULT {
            return Err(refuse());
        }
        match t.fields.as_slice() {
            // `is dynamic` is the declaration's flag, not a custom trait.
            [_] if name == DYNAMIC => is_dynamic = true,
            // `is marked`.
            [_] => traits.push((name, None)),
            // `is default(EXPR)` / `is marked(ARGS)`.
            [_, _] => {
                let argument = named_child(t, "argument")?;
                let value = super::attribute::lower_paren_argument(t, argument)?;
                // A list `(a, b)` is the parser's grouped array literal.
                let value = match value {
                    Expr::ArrayLiteral(_) if name != DEFAULT => Expr::Grouped(Box::new(value)),
                    other => other,
                };
                traits.push((name, Some(value)));
            }
            _ => return Err(refuse()),
        }
    }
    Ok((traits, is_dynamic))
}
