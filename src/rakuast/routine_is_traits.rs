//! Execution flags read from written routine traits.
use super::convert::{leaf_field, node_field, statement_expression, unsupported};
use super::lower::{named_child, named_child_or_positional, positional_leaf};
use super::routine_traits::{export_argument, export_tags, trait_is};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};
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
                if !self.export_tags.iter().any(|tag| tag == DEFAULT_TAG) {
                    self.export_tags.push(DEFAULT_TAG.to_string());
                }
            }
            ("export", Some(argument)) if t.fields.len() == 2 => {
                let Some(tags) = export_tags(argument)? else {
                    return Ok(false);
                };
                for tag in tags {
                    if !self.export_tags.contains(&tag) {
                        self.export_tags.push(tag);
                    }
                }
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
    if argument.class == RakuAstClass::CircumfixParentheses {
        return Ok(
            match super::attribute::lower_paren_argument(argument, argument)? {
                Expr::Literal(value) => value.as_str().map(str::to_string),
                _ => None,
            },
        );
    }
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
    if argument.class == RakuAstClass::QuotedString {
        return assoc_value(argument);
    }
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
