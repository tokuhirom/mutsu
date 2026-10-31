//! Term names preserve an imported source binding through the model boundary.

use crate::scope::NameScope;
use super::convert::{name_from_identifier, node_field};
use super::lower::{lower_expr, named_child_or_positional, term_identifier_expr, unsupported};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::RuntimeError;

/// The parser resolved this name as an imported value. Keep that decision
/// without changing the public constructor shape or reloading a module.
pub(super) fn imported(names: &NameScope, name: &str) -> RakuAstNode {
    let mut node = super::bareword::convert(names, name).unwrap_or_else(|| RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermName,
        fields: vec![node_field(None, name_from_identifier(name))],
    });
    if node.class == RakuAstClass::TermName {
        node.fields.push(super::declared_routines::bareword_field());
    }
    node
}

pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let name = named_child_or_positional(node)?;
    if super::declared_routines::is_source_bareword(node)
        && let Some(NameShape::Identifier(name)) = name_parts::name_shape(name)
    {
        return Ok(Expr::BareWord(name));
    }
    lower_name(names, name)
}

fn lower_name(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match name_parts::name_shape(node).ok_or_else(|| unsupported(node))? {
        NameShape::Identifier(name) => Ok(match super::label_context::lookup(&name) {
            Some(value) if !name_parts::has_leading_empty(node) => Expr::Literal(value),
            _ => term_identifier_expr(&name),
        }),
        NameShape::Stash(stash) => Ok(Expr::PseudoStash(stash)),
        NameShape::Indirect {
            expr,
            tail,
            trailing,
        } => {
            let head = Box::new(lower_expr(names, expr)?);
            if tail.is_empty() && !trailing {
                return Ok(Expr::IndirectTypeLookup(head));
            }
            let tail = tail
                .iter()
                .flat_map(|seg| ["::", seg.as_str()])
                .collect::<String>();
            Ok(Expr::IndirectTypeLookupTail {
                head,
                tail: tail.into_boxed_str(),
                trailing,
            })
        }
    }
}
