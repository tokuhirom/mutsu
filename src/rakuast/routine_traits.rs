//! A routine's `multi`, `!private`, `is rw`, `is raw` and `is export` across
//! the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, `multi method !p() is rw { … }` is
//!
//! ```text
//! Method(multiness => "multi", private => True, name => …,
//!        traits => (Trait::Is(name => Name.from-identifier("rw")),), body => …)
//! ```
//!
//! and `sub f() is export(:a, :b) { … }` is a `Sub` whose trait carries
//! `argument => Circumfix::Parentheses(SemiList(Statement::Expression(
//! ApplyListInfix(",", ColonPair::True("a"), ColonPair::True("b")))))`; a single
//! tag is the bare `ColonPair::True`, and a bare `is export` has no argument.
//!
//! `sub infix:<+++>($a, $b) is assoc<left>` carries its associativity as
//! `Trait::Is(name => assoc, argument => QuotedString(<words val>, "left"))` and
//! `is tighter(&infix:<+>)` (also `looser`, `equiv`) a parenthesised
//! `Var::Lexical("&infix:<+>")`.
//!
//! `multiness` and `private` precede `name`; a trait sits in `traits` before
//! `body`. The traits of a parsed routine are rendered from the list the
//! source wrote, in written order (`routine_source`); the execution fields the
//! parser folds them into (`is_rw`, `associativity`, `custom_traits`) are not
//! read back. The helpers here serve the flags and the lowering direction.
//!
//! Any other `is NAME` / `is NAME(ARGS)` on a sub (`is native("libc")`,
//! `is test-assertion`, a user `trait_mod:<is>`) is a `Trait::Is` too, with
//! the argument as a `Circumfix::Parentheses`. A list argument `(a, b)` is the
//! parser's `Grouped(ArrayLiteral)`, and the parentheses are the circumfix.
//! `is DEPRECATED` / `is DEPRECATED("message")` is a `Trait::Is` too: the
//! parser keeps it as the custom trait `DEPRECATED` / `DEPRECATED:message`.

use super::convert::{leaf_field, name_from_identifier, node_field, statement_expression};
use super::lower::{named_child, named_child_or_positional, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

/// The tag a bare `is export` exports under.
const DEFAULT_TAG: &str = "DEFAULT";

pub(super) use super::routine_is_traits::IsTraits;

pub(super) fn trait_is(name: &str, argument: Option<RakuAstNode>) -> RakuAstNode {
    let mut fields = vec![node_field(Some("name"), name_from_identifier(name))];
    if let Some(argument) = argument {
        fields.push(node_field(Some("argument"), argument));
    }
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TraitIs,
        fields,
    }
}

/// Build an `is TYPE` trait, whose AST stores a type node rather than a name.
// Cost: O(n + a), n = type name length, a = argument node size.
pub(super) fn trait_is_type(
    name: &str,
    argument: Option<RakuAstNode>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(
        Some("type"),
        super::type_build::build_type_node(name)?,
    )];
    if let Some(argument) = argument {
        fields.push(node_field(Some("argument"), argument));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TraitIs,
        fields,
    })
}

/// Raku treats a qualified name or a capitalized identifier in an `is` trait
/// position as a type; lowercase names are custom trait names.
// Cost: O(n), n = trait name length.
pub(super) fn is_type_trait_name(name: &str) -> bool {
    name != "DEPRECATED"
        && (crate::qualified::is_qualified_str(name)
            || name.chars().next().is_some_and(char::is_uppercase))
}

fn colon_pair_true(tag: &str) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairTrue,
        fields: vec![leaf_field(None, Value::str(tag.to_string()))],
    }))
}

/// `(:a)` / `(:a, :b)` for `is export`'s tags; `None` for the bare form.
pub(super) fn export_argument(tags: &[String]) -> Option<RakuAstNode> {
    if tags == [DEFAULT_TAG] {
        return None;
    }
    explicit_export_argument(tags)
}

pub(super) fn explicit_export_argument(tags: &[String]) -> Option<RakuAstNode> {
    let expression = match tags {
        [only] => match colon_pair_true(only).view() {
            ValueView::RakuAst(node) => (*node).clone(),
            _ => unreachable!("colon_pair_true builds a node"),
        },
        _ => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(
                    Some("infix"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::Infix,
                        fields: vec![leaf_field(None, Value::str_from(","))],
                    },
                ),
                RakuAstField {
                    name: Some("operands"),
                    value: RakuAstFieldValue::List(
                        tags.iter().map(|t| colon_pair_true(t)).collect(),
                    ),
                },
            ],
        },
    };
    let semilist = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(expression))],
    };
    Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    })
}

/// The tags of an `is export(…)` argument, or `None` for a shape other than
/// `:TAG` colonpairs.
pub(super) fn export_tags(argument: &RakuAstNode) -> Result<Option<Vec<String>>, RuntimeError> {
    if argument.class != RakuAstClass::CircumfixParentheses {
        return Ok(None);
    }
    let statement = named_child_or_positional(named_child_or_positional(argument)?)?;
    if statement.class != RakuAstClass::StatementExpression {
        return Ok(None);
    }
    let expression = named_child(statement, "expression")?;
    let pairs: Vec<&RakuAstNode> = match expression.class {
        RakuAstClass::ColonPairTrue => vec![expression],
        RakuAstClass::ApplyListInfix => {
            let Some(RakuAstFieldValue::List(items)) = expression
                .fields
                .iter()
                .find(|f| f.name == Some("operands"))
                .map(|f| &f.value)
            else {
                return Ok(None);
            };
            let mut pairs = Vec::with_capacity(items.len());
            for item in items {
                let ValueView::RakuAst(node) = item.view() else {
                    return Ok(None);
                };
                pairs.push(node);
            }
            pairs
        }
        _ => return Ok(None),
    };
    let mut tags = Vec::with_capacity(pairs.len());
    for pair in pairs {
        if pair.class != RakuAstClass::ColonPairTrue {
            return Ok(None);
        }
        match positional_leaf(pair)?.view() {
            ValueView::Str(tag) => tags.push(tag.to_string()),
            _ => return Ok(None),
        }
    }
    Ok(Some(tags))
}

/// Add a routine's `multiness` and `private` to the node `routine_node` built;
/// its traits are the ones the source wrote (`routine_source::apply`).
// Cost: O(f), f = fields of `node`.
pub(super) fn add_flags(node: &mut RakuAstNode, multi: bool, private: bool) {
    if private {
        node.fields
            .insert(0, leaf_field(Some("private"), Value::truth(true)));
    }
    if multi {
        node.fields
            .insert(0, leaf_field(Some("multiness"), Value::str_from("multi")));
    }
}

/// Trait names the parser folds into a field of its own, or keeps as an
/// internal marker, rather than leaving as a plain custom trait.
fn is_generic_trait_name(name: &str) -> bool {
    !(name.starts_with("__")
        || name.starts_with("DEPRECATED")
        || crate::qualified::is_qualified_str(name)
        || matches!(
            name,
            "rw" | "raw"
                | "export"
                | "assoc"
                | "equiv"
                | "tighter"
                | "looser"
                | "readonly"
                | "hidden-from-backtrace"
                | "nodal"
                | "pure"
        ))
}

/// The message of a `DEPRECATED` / `DEPRECATED:message` custom trait: empty for
/// the bare trait. `None` for any other trait.
// Cost: O(|name|).
pub(super) fn deprecated_message(name: &str) -> Option<&str> {
    if name == "DEPRECATED" {
        return Some("");
    }
    name.strip_prefix("DEPRECATED:")
}

/// Lower a `Trait::Is` that names a plain custom trait into the parser's
/// `custom_traits` entry; `None` for a name the parser would not leave there.
// Cost: O(n + a), n = type name length, a = size of the argument.
pub(super) fn lower_custom(
    owner: &RakuAstNode,
    t: &RakuAstNode,
) -> Result<Option<(String, Option<Expr>)>, RuntimeError> {
    let type_name = named_child(t, "type")
        .ok()
        .map(|type_node| super::type_lower::type_constraint(owner, type_node))
        .transpose()?;
    let is_type = type_name.is_some();
    let name = if let Some(type_name) = type_name {
        type_name
    } else {
        let name = positional_leaf(named_child(t, "name")?)?;
        let ValueView::Str(name) = name.view() else {
            return Ok(None);
        };
        name.to_string()
    };
    if name == "DEPRECATED" {
        // `DEPRECATED` / `DEPRECATED:message`, as the parser keeps it.
        return match named_child(t, "argument") {
            Err(_) if t.fields.len() == 1 => Ok(Some(("DEPRECATED".to_string(), None))),
            Ok(argument) => {
                let expr = lower_argument(owner, argument)?;
                match fold_constant_message(&expr) {
                    Some(message) if !message.is_empty() => {
                        Ok(Some((format!("DEPRECATED:{message}"), None)))
                    }
                    Some(_) => Ok(None),
                    // Any other message is evaluated when the routine is
                    // declared: the expression travels with the trait.
                    None => Ok(Some(("DEPRECATED".to_string(), Some(expr)))),
                }
            }
            Err(_) => Ok(None),
        };
    }
    if !is_type && !is_generic_trait_name(&name) {
        return Ok(None);
    }
    let argument = match named_child(t, "argument") {
        Ok(argument) => {
            let expr = lower_argument(owner, argument)?;
            Some(match expr {
                list @ Expr::ArrayLiteral(_) => Expr::Grouped(Box::new(list)),
                other => other,
            })
        }
        Err(_) if t.fields.len() == 1 => None,
        Err(_) => return Ok(None),
    };
    Ok(Some((name, argument)))
}

/// The string a `DEPRECATED` message argument statically evaluates to: a string
/// literal, or an immediately invoked parameterless anonymous sub / block whose
/// body is one such expression (`(sub { "a" })()`, rakudo's RT #120908 shape).
/// Anything else is evaluated when the routine is declared, and travels as
/// `("DEPRECATED", Some(expr))`.
// Cost: O(a), a = size of the argument.
fn fold_constant_message(expr: &Expr) -> Option<String> {
    let mut current = expr;
    loop {
        match current {
            Expr::Literal(v) => return v.as_str().map(str::to_string),
            Expr::Grouped(inner) => current = inner,
            Expr::CallOn { target, args } if args.is_empty() => {
                let Expr::Grouped(callee) = target.as_ref() else {
                    return None;
                };
                let Expr::AnonSub { body, .. } = callee.as_ref() else {
                    return None;
                };
                let mut stmts = body.iter().filter(|s| !matches!(s, Stmt::SetLine(..)));
                match (stmts.next(), stmts.next()) {
                    (Some(Stmt::Expr(inner)), None) => current = inner,
                    _ => return None,
                }
            }
            _ => return None,
        }
    }
}

// Cost: O(a), a = size of the argument.
fn lower_argument(owner: &RakuAstNode, argument: &RakuAstNode) -> Result<Expr, RuntimeError> {
    if argument.class == RakuAstClass::QuotedString {
        super::lower::lower_expr(argument)
    } else {
        super::attribute::lower_paren_argument(owner, argument)
    }
}
