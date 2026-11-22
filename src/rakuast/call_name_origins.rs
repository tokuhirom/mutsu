//! Origins of a call by name (ADR-10723 Stage 2): `name(args)` /
//! `name args`, from `infix_origins::derive`.

use super::infix_origins::{byte_range, child, child_at, leaf_text, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// `name(args)` / `name args`: the `Name` is the identifier the call starts
/// with, the `ArgList` its arguments, in parentheses
/// ([`super::call_origins::parenthesized`]) or from its first argument to its
/// last.
// Cost: O(n + a), n = length of the call's source, a = its arguments.
pub(super) fn call_name(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    if super::indirect_name_origins::call(node, SrcSpan::new(from, to))
        || super::indirect_name_origins::bare_call(node, SrcSpan::new(from, to))
    {
        return;
    }
    let Some(name) = child(node, "name").and_then(|n| {
        super::indirect_name_origins::written_operator(n)
            .or_else(|| leaf_text(n))
            // `Foo::run()`: a qualified name.
            .or_else(|| match super::name_parts::name_shape(n)? {
                super::name_parts::NameShape::Identifier(name) => Some(name),
                _ => None,
            })
    }) else {
        return;
    };
    let parens = node.class == RakuAstClass::CallName;
    let fits = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        let ok = if parens {
            rest.starts_with('(') && rest.ends_with(')')
        } else {
            rest.is_empty() || rest.starts_with(char::is_whitespace)
        };
        ok.then_some(())
    });
    if fits.is_none() {
        return;
    }
    let name_end = from + name.len();
    if !parens {
        super::label_origins::name_argument(node, name_end, to);
        super::call_origins::leading_block_argument(node, name_end, to);
        super::fragment_origins::arguments(names, node, SrcSpan::new(name_end, to));
    }
    let args = if parens {
        super::fragment_origins::arguments(names, node, SrcSpan::new(name_end + 1, to - 1));
        super::call_origins::parenthesized(child(node, "args"), SrcSpan::new(name_end + 1, to - 1))
    } else if child(node, "args").is_none() {
        // A bare `foo`: Rakudo's empty `ArgList` sits right after the name.
        (name_end == to).then_some((to, to))
    } else {
        child(node, "args").and_then(|list| {
            let first = child_at(list, 0).and_then(byte_range)?;
            let last = child_at(list, list.fields.len().checked_sub(1)?).and_then(byte_range)?;
            // `say :{ 1 => 2 }`: an object hash's range starts at its `{`.
            let brace =
                super::unit_source::with_text_from(first.0, |t| t.starts_with('{').then_some(()));
            let gap = super::unit_source::with_text(SrcSpan::new(name_end, first.0), |gap| {
                let gap = brace.and(gap.strip_suffix(':')).unwrap_or(gap);
                gap.chars().all(char::is_whitespace).then_some(())
            });
            (gap.is_some() && last.1 == to).then_some((first.0, last.1))
        })
    };
    with_child(node, "name", |n| {
        super::origin::set_span(n, SrcSpan::new(from, name_end));
        super::indirect_name_origins::operator_quote(n, SrcSpan::new(from, name_end));
    });
    if let Some((args_from, args_to)) = args {
        super::empty_fields::set_span(node, "args", SrcSpan::new(args_from, args_to));
        with_child(node, "args", |list| {
            super::origin::set_span(list, SrcSpan::new(args_from, args_to))
        });
    }
}
