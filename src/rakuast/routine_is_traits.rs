//! Execution flags read from written routine traits.
use crate::scope::NameScope;
use super::lower::{named_child, named_child_or_positional, positional_leaf};
use super::routine_traits::export_tags;
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, ValueView};
const DEFAULT_TAG: &str = "DEFAULT";

/// The flag-valued `is` traits a routine can carry.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(super) struct IsTraits {
    pub(super) is_rw: bool,
    pub(super) is_raw: bool,
    /// `is hidden-from-backtrace`.
    pub(super) is_hidden_from_backtrace: bool,
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
    pub(super) fn read(&mut self, names: &NameScope, t: &RakuAstNode) -> Result<bool, RuntimeError> {
        let Ok(name_node) = named_child(t, "name") else {
            // `Trait::Is(type => TYPE)` is a user type trait, lowered through
            // the routine's custom-trait path.
            return Ok(false);
        };
        let name = positional_leaf(name_node)?;
        let ValueView::Str(name) = name.view() else {
            return Ok(false);
        };
        let argument = named_child(t, "argument").ok();
        match (name.as_str(), argument) {
            ("assoc", Some(argument)) if t.fields.len() == 2 => {
                let Some(value) = assoc_value(names, argument)? else {
                    return Ok(false);
                };
                self.assoc = Some(value);
            }
            (kind @ ("tighter" | "looser" | "equiv"), Some(argument)) if t.fields.len() == 2 => {
                let Some(reference) = precedence_reference(names, argument)? else {
                    return Ok(false);
                };
                self.precedence = Some((kind.to_string(), reference));
            }
            ("rw", None) if t.fields.len() == 1 => self.is_rw = true,
            ("raw", None) if t.fields.len() == 1 => self.is_raw = true,
            ("hidden-from-backtrace", None) if t.fields.len() == 1 => {
                self.is_hidden_from_backtrace = true
            }
            // These parser-recognized annotations have no execution field.
            // Their spelling remains in the routine source record.
            ("nodal" | "pure" | "readonly", None) if t.fields.len() == 1 => {}
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
}

/// Whether `reference` is the `&infix:<+>` spelling of an operator sub.
fn is_operator_reference(reference: &str) -> bool {
    reference.strip_prefix('&').is_some_and(|rest| {
        rest.split_once(":<").is_some_and(|(category, op)| {
            !category.is_empty()
                && category.chars().all(|c| c.is_ascii_alphabetic())
                && op.ends_with('>')
                && !op[..op.len() - 1].is_empty()
                // `>` is a valid operator character (`&infix:<|>>`); a `<`
                // would start a second subscript.
                && !op[..op.len() - 1].contains('<')
                // Only a bracketing operator's name holds a space
                // (`&postcircumfix:<[ ]>`); `<a b>` is not an operator.
                && (!op.contains(' ') || category.ends_with("circumfix"))
        })
    })
}

/// The word of an `is assoc<word>` argument (`QuotedString(<words val>)`).
fn assoc_value(names: &NameScope, argument: &RakuAstNode) -> Result<Option<String>, RuntimeError> {
    if argument.class == RakuAstClass::CircumfixParentheses {
        return Ok(
            match super::attribute::lower_paren_argument(names, argument, argument)? {
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
fn precedence_reference(names: &NameScope, argument: &RakuAstNode) -> Result<Option<String>, RuntimeError> {
    if argument.class == RakuAstClass::QuotedString {
        return assoc_value(names, argument);
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
        ValueView::Str(reference) => {
            let reference = canonical_operator_reference(&reference);
            Ok(is_operator_reference(&reference).then_some(reference))
        }
        _ => Ok(None),
    }
}

/// `&infix:«|>»` (the interpolating spelling of an operator's name, needed when
/// the symbol holds a `>`) as the `&infix:<|>>` form the precedence lookup and
/// the parser's own trait reference use. Any other spelling is returned as is.
// Cost: O(|reference|).
fn canonical_operator_reference(reference: &str) -> String {
    reference
        .split_once(":\u{ab}")
        .and_then(|(head, rest)| {
            rest.strip_suffix('\u{bb}')
                .map(|symbol| format!("{head}:<{symbol}>"))
        })
        .unwrap_or_else(|| reference.to_string())
}
