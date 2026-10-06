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
//! `Var::Lexical("&infix:<+>")`. The parser keeps the first in `associativity`
//! and the second in `precedence_trait` (also copying its kind into
//! `associativity`), so a routine with both is refused: their order is not kept.
//!
//! `multiness` and `private` precede `name`; a trait sits in `traits` before
//! `body`. The parser keeps these traits as flags beside the return-type trait,
//! not in source order, so a routine carrying more than one of them is refused
//! rather than rendered in an invented order. The parser records a bare
//! `is export` as the `DEFAULT` tag, which renders bare: `is export(:DEFAULT)`
//! means the same and comes back in that spelling.
//!
//! Any other `is NAME` / `is NAME(ARGS)` on a sub (`is native("libc")`,
//! `is test-assertion`, a user `trait_mod:<is>`) is a `Trait::Is` too, with
//! the argument as a `Circumfix::Parentheses`. The parser keeps these in
//! `custom_traits` in source order, beside the `returns`/`of` marker, so they
//! render in that order around the return-type trait. A list argument
//! `(a, b)` is the parser's `Grouped(ArrayLiteral)`, and the parentheses are
//! the circumfix. An angle argument `<x>` reads the same as `('x')` and
//! comes back in that spelling; a multi-word `<a b>`, an internal `__` marker,
//! a qualified name and the traits the parser folds into other fields stay
//! refused. `is DEPRECATED` / `is DEPRECATED("message")` is a `Trait::Is` too:
//! the parser keeps it as the custom trait `DEPRECATED` / `DEPRECATED:message`,
//! and a method's `is default` and `is DEPRECATED` as fields of their own, which
//! [`method_custom_traits`] puts back among the custom traits.

use super::convert::{
    leaf_field, name_from_identifier, node_field, statement_expression, unsupported,
};
use super::lower::{named_child, named_child_or_positional, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The tag a bare `is export` exports under.
const DEFAULT_TAG: &str = "DEFAULT";

/// The flag-valued `is` traits a routine can carry.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(super) struct IsTraits {
    pub(super) is_rw: bool,
    pub(super) is_raw: bool,
    /// `is export`'s tags; empty when not exported.
    pub(super) export_tags: Vec<String>,
    /// `is assoc<left>`'s value.
    pub(super) assoc: Option<String>,
    /// `is tighter(&infix:<+>)`'s kind (`tighter` / `looser` / `equiv`) and
    /// reference operator.
    pub(super) precedence: Option<(String, String)>,
}

impl IsTraits {
    /// Read one `Trait::Is` into the flags; `false` for a trait this set does
    /// not model.
    // Cost: O(a), a = size of the trait's argument.
    pub(super) fn read(&mut self, t: &RakuAstNode) -> Result<bool, RuntimeError> {
        let name = positional_leaf(named_child(t, "name")?)?;
        let ValueView::Str(name) = name.view() else {
            return Ok(false);
        };
        let argument = named_child(t, "argument").ok();
        match (name.as_str(), argument) {
            ("assoc", Some(argument)) if t.fields.len() == 2 => {
                let Some(value) = assoc_value(argument)? else {
                    return Ok(false);
                };
                self.assoc = Some(value);
            }
            (kind @ ("tighter" | "looser" | "equiv"), Some(argument)) if t.fields.len() == 2 => {
                let Some(reference) = precedence_reference(argument)? else {
                    return Ok(false);
                };
                self.precedence = Some((kind.to_string(), reference));
            }
            ("rw", None) if t.fields.len() == 1 => self.is_rw = true,
            ("raw", None) if t.fields.len() == 1 => self.is_raw = true,
            ("export", None) if t.fields.len() == 1 => {
                self.export_tags = vec![DEFAULT_TAG.to_string()];
            }
            ("export", Some(argument)) if t.fields.len() == 2 => {
                let Some(tags) = export_tags(argument)? else {
                    return Ok(false);
                };
                self.export_tags = tags;
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    /// The written flag traits, as `Trait::Is` nodes.
    pub(super) fn nodes(&self) -> Vec<RakuAstNode> {
        let mut nodes = Vec::new();
        for (on, name) in [(self.is_rw, "rw"), (self.is_raw, "raw")] {
            if on {
                nodes.push(trait_is(name, None));
            }
        }
        if !self.export_tags.is_empty() {
            nodes.push(trait_is("export", export_argument(&self.export_tags)));
        }
        if let Some(value) = &self.assoc {
            nodes.push(trait_is(
                "assoc",
                Some(super::package_header::words_value(value)),
            ));
        }
        if let Some((kind, reference)) = &self.precedence {
            let variable = RakuAstNode {
                class: RakuAstClass::VarLexical,
                fields: vec![leaf_field(None, Value::str(reference.clone()))],
            };
            nodes.push(trait_is(kind, Some(paren_around(variable))));
        }
        nodes
    }

    /// The traits a `SubDecl` records in `associativity` and `precedence_trait`.
    /// A precedence trait also copies its kind into `associativity`, so the two
    /// fields name two traits only when they disagree, whose order is not kept.
    // Cost: O(|reference|).
    pub(super) fn with_precedence(
        mut self,
        associativity: Option<&String>,
        precedence_trait: Option<&(String, String)>,
    ) -> Result<Self, RuntimeError> {
        match (associativity, precedence_trait) {
            (None, None) => {}
            (Some(assoc), None) if !is_precedence_kind(assoc) => self.assoc = Some(assoc.clone()),
            (Some(kind), Some(precedence)) if *kind == precedence.0 => {
                self.precedence = Some(precedence.clone());
            }
            (None, Some(precedence)) => self.precedence = Some(precedence.clone()),
            (Some(kind), None) if is_precedence_kind(kind) => {}
            _ => {
                return Err(unsupported(
                    "routine with `is assoc` and a precedence trait (their source order is not kept)",
                ));
            }
        }
        if let Some((_, reference)) = &self.precedence
            && !is_operator_reference(reference)
        {
            return Err(unsupported(
                "precedence trait with a reference that is not `&category:<op>`",
            ));
        }
        Ok(self)
    }
}

fn is_precedence_kind(name: &str) -> bool {
    matches!(name, "looser" | "tighter" | "equiv")
}

/// Whether `reference` is the `&infix:<+>` spelling of an operator sub.
fn is_operator_reference(reference: &str) -> bool {
    reference.strip_prefix('&').is_some_and(|rest| {
        rest.split_once(":<").is_some_and(|(category, op)| {
            !category.is_empty()
                && category.chars().all(|c| c.is_ascii_alphabetic())
                && op.ends_with('>')
                && !op[..op.len() - 1].is_empty()
                && !op[..op.len() - 1].contains(['<', '>', ' '])
        })
    })
}

/// `(NODE)` as a trait argument, for a node already converted.
fn paren_around(node: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(
            None,
            RakuAstNode {
                class: RakuAstClass::SemiList,
                fields: vec![node_field(None, statement_expression(node))],
            },
        )],
    }
}

/// The word of an `is assoc<word>` argument (`QuotedString(<words val>)`).
fn assoc_value(argument: &RakuAstNode) -> Result<Option<String>, RuntimeError> {
    if argument.class != RakuAstClass::QuotedString {
        return Ok(None);
    }
    let [segment] = super::lower::list_field(argument, "segments")? else {
        return Ok(None);
    };
    let ValueView::RakuAst(segment) = segment.view() else {
        return Ok(None);
    };
    match positional_leaf(segment)?.view() {
        ValueView::Str(word) if !word.is_empty() && !word.contains(char::is_whitespace) => {
            Ok(Some(word.to_string()))
        }
        _ => Ok(None),
    }
}

/// The `&infix:<+>` of an `is tighter(&infix:<+>)` argument.
fn precedence_reference(argument: &RakuAstNode) -> Result<Option<String>, RuntimeError> {
    if argument.class != RakuAstClass::CircumfixParentheses {
        return Ok(None);
    }
    let statement = named_child_or_positional(named_child_or_positional(argument)?)?;
    if statement.class != RakuAstClass::StatementExpression {
        return Ok(None);
    }
    let variable = named_child(statement, "expression")?;
    if variable.class != RakuAstClass::VarLexical {
        return Ok(None);
    }
    match positional_leaf(variable)?.view() {
        ValueView::Str(reference) if is_operator_reference(&reference) => {
            Ok(Some(reference.to_string()))
        }
        _ => Ok(None),
    }
}

pub(super) fn trait_is(name: &str, argument: Option<RakuAstNode>) -> RakuAstNode {
    let mut fields = vec![node_field(Some("name"), name_from_identifier(name))];
    if let Some(argument) = argument {
        fields.push(node_field(Some("argument"), argument));
    }
    RakuAstNode {
        class: RakuAstClass::TraitIs,
        fields,
    }
}

fn colon_pair_true(tag: &str) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::ColonPairTrue,
        fields: vec![leaf_field(None, Value::str(tag.to_string()))],
    }))
}

/// `(:a)` / `(:a, :b)` for `is export`'s tags; `None` for the bare form.
pub(super) fn export_argument(tags: &[String]) -> Option<RakuAstNode> {
    let expression = match tags {
        [only] if only == DEFAULT_TAG => return None,
        [only] => match colon_pair_true(only).view() {
            ValueView::RakuAst(node) => (*node).clone(),
            _ => unreachable!("colon_pair_true builds a node"),
        },
        _ => RakuAstNode {
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(
                    Some("infix"),
                    RakuAstNode {
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
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(expression))],
    };
    Some(RakuAstNode {
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    })
}

/// The tags of an `is export(…)` argument, or `None` for a shape other than
/// `:TAG` colonpairs.
fn export_tags(argument: &RakuAstNode) -> Result<Option<Vec<String>>, RuntimeError> {
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

/// Add a routine's `multiness`, `private` and flag traits to the node
/// `routine_node` built.
// Cost: O(f + t), f = fields of `node`, t = export tags.
pub(super) fn add_flags(
    node: &mut RakuAstNode,
    multi: bool,
    private: bool,
    traits: &IsTraits,
) -> Result<(), RuntimeError> {
    let written = traits.nodes();
    if !written.is_empty() {
        let has_traits = node.fields.iter().any(|f| f.name == Some("traits"));
        if written.len() > 1 || has_traits {
            return Err(unsupported(
                "routine with several traits (their source order is not kept)",
            ));
        }
        let at = node
            .fields
            .iter()
            .position(|f| f.name == Some("body"))
            .unwrap_or(node.fields.len());
        node.fields.insert(
            at,
            RakuAstField {
                name: Some("traits"),
                value: RakuAstFieldValue::List(
                    written
                        .into_iter()
                        .map(|t| Value::rakuast(Box::new(t)))
                        .collect(),
                ),
            },
        );
    }
    if private {
        node.fields
            .insert(0, leaf_field(Some("private"), Value::truth(true)));
    }
    if multi {
        node.fields
            .insert(0, leaf_field(Some("multiness"), Value::str_from("multi")));
    }
    Ok(())
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

/// Whether the converter renders the custom trait `name` as a `Trait::Is`.
fn is_renderable_trait_name(name: &str) -> bool {
    is_generic_trait_name(name) || deprecated_message(name).is_some()
}

/// Whether `custom_traits` holds a trait that renders as a plain `Trait::Is`.
// Cost: O(t), t = custom traits.
pub(super) fn has_generic_traits(custom_traits: &[(String, Option<Expr>)]) -> bool {
    custom_traits
        .iter()
        .any(|(t, _)| is_renderable_trait_name(t))
}

/// A method's custom traits with its `is default` and `is DEPRECATED`, which
/// the parser keeps in fields of their own, put back: those traits carry no
/// order among the custom ones, so they are accepted only alone.
// Cost: O(t), t = custom traits.
pub(super) fn method_custom_traits(
    custom_traits: &[(String, Option<Expr>)],
    is_default: bool,
    deprecated: Option<&str>,
) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
    let mut all = custom_traits.to_vec();
    let extras = usize::from(is_default) + usize::from(deprecated.is_some());
    if extras > 0 && (extras > 1 || has_generic_traits(custom_traits)) {
        return Err(unsupported(
            "method with several traits (their source order is not kept)",
        ));
    }
    if is_default {
        all.push(("default".to_string(), None));
    }
    if let Some(message) = deprecated {
        let name = if message.is_empty() {
            "DEPRECATED".to_string()
        } else {
            format!("DEPRECATED:{message}")
        };
        all.push((name, None));
    }
    Ok(all)
}

/// The parser's argument of a custom trait, as the `(…)` circumfix rakudo
/// renders: `Grouped(ArrayLiteral)` is `(a, b)` and any other expression is
/// `(EXPR)`. `None` for a spelling the lowering would not rebuild.
// Cost: O(e), e = size of the argument.
fn custom_argument(argument: &Expr) -> Result<Option<RakuAstNode>, RuntimeError> {
    match argument {
        Expr::Grouped(inner) if matches!(**inner, Expr::ArrayLiteral(_)) => {
            Ok(Some(super::attribute::paren_argument(inner)?))
        }
        Expr::Grouped(_) | Expr::ArrayLiteral(_) => Ok(None),
        other => Ok(Some(super::attribute::paren_argument(other)?)),
    }
}

/// Add a sub's plain custom traits (`is native("libc")`) to `node`'s
/// `traits`, around the return-type trait as `custom_traits` orders them.
/// `flags` says whether `add_flags` wrote an `is rw` / `raw` / `export`, whose
/// place among these the parser does not keep.
// Cost: O(t + a), t = custom traits, a = size of their arguments.
pub(super) fn add_custom(
    node: &mut RakuAstNode,
    custom_traits: &[(String, Option<Expr>)],
    flags: bool,
) -> Result<(), RuntimeError> {
    if !has_generic_traits(custom_traits) {
        return Ok(());
    }
    if flags {
        return Err(unsupported(
            "routine with several traits (their source order is not kept)",
        ));
    }
    let mut before = Vec::new();
    let mut after = Vec::new();
    let mut seen_return = false;
    for (name, argument) in custom_traits {
        if matches!(name.as_str(), "__return_via_trait" | "__return_via_of") {
            seen_return = true;
            continue;
        }
        if !is_renderable_trait_name(name) {
            continue;
        }
        let item = if let Some(message) = deprecated_message(name) {
            let argument = if message.is_empty() {
                None
            } else {
                Some(super::attribute::paren_argument(&Expr::Literal(
                    Value::str(message.to_string()),
                ))?)
            };
            Value::rakuast(Box::new(trait_is("DEPRECATED", argument)))
        } else {
            let argument = match argument {
                None => None,
                Some(argument) => Some(
                    custom_argument(argument)?
                        .ok_or_else(|| unsupported("trait with an angle-word list argument"))?,
                ),
            };
            Value::rakuast(Box::new(trait_is(name, argument)))
        };
        if seen_return {
            after.push(item)
        } else {
            before.push(item)
        }
    }
    if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("traits")) {
        let RakuAstFieldValue::List(existing) = &mut field.value else {
            return Err(unsupported("routine traits field"));
        };
        before.append(existing);
        before.append(&mut after);
        *existing = before;
        return Ok(());
    }
    before.append(&mut after);
    let at = node
        .fields
        .iter()
        .position(|f| f.name == Some("body"))
        .unwrap_or(node.fields.len());
    node.fields.insert(
        at,
        RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(before),
        },
    );
    Ok(())
}

/// Lower a `Trait::Is` that names a plain custom trait into the parser's
/// `custom_traits` entry; `None` for a name the parser would not leave there.
// Cost: O(a), a = size of the argument.
pub(super) fn lower_custom(
    owner: &RakuAstNode,
    t: &RakuAstNode,
) -> Result<Option<(String, Option<Expr>)>, RuntimeError> {
    let name = positional_leaf(named_child(t, "name")?)?;
    let ValueView::Str(name) = name.view() else {
        return Ok(None);
    };
    if name.as_str() == "DEPRECATED" {
        // `DEPRECATED` / `DEPRECATED:message`, as the parser keeps it.
        return match named_child(t, "argument") {
            Err(_) if t.fields.len() == 1 => Ok(Some(("DEPRECATED".to_string(), None))),
            Ok(argument) => match super::attribute::lower_paren_argument(owner, argument)? {
                Expr::Literal(v) => match v.as_str() {
                    Some(message) if !message.is_empty() => {
                        Ok(Some((format!("DEPRECATED:{message}"), None)))
                    }
                    _ => Ok(None),
                },
                _ => Ok(None),
            },
            Err(_) => Ok(None),
        };
    }
    if !is_generic_trait_name(&name) {
        return Ok(None);
    }
    let argument = match named_child(t, "argument") {
        Ok(argument) => {
            let expr = super::attribute::lower_paren_argument(owner, argument)?;
            Some(match expr {
                list @ Expr::ArrayLiteral(_) => Expr::Grouped(Box::new(list)),
                other => other,
            })
        }
        Err(_) if t.fields.len() == 1 => None,
        Err(_) => return Ok(None),
    };
    Ok(Some((name.to_string(), argument)))
}
