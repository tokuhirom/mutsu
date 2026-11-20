//! The written order of an `enum` / `subset` declaration's traits.
//!
//! The parser keeps an enum's roles and a subset's own `is` traits in lists of
//! their own and `of` / `is export` as fields, so their interleaving is gone
//! from the declaration; `TypeDeclSpans::order` records where `of` and
//! `is export` stood among the clauses. Rakudo keeps the `traits` in source
//! order (`enum E is export does R`, `subset S is foo of Int`; measured on
//! 2026.11).

use super::view::node_field;
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::ast::parsed::TypeDeclSpans;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// A trait node over a type named `name` (`Trait::Does(R)`, `Trait::Of(Int)`).
// Cost: O(|name|).
fn type_trait(names: &NameScope, class: RakuAstClass, name: &str) -> Result<Value, RuntimeError> {
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields: vec![node_field(
            None,
            super::type_build::build_type_node(names, name)?,
        )],
    })))
}

/// The `traits` of an `enum`: its `does` roles and `is export`, as written.
// Cost: O(t log t), t = traits.
pub(super) fn enum_traits(
    names: &NameScope,
    roles: &[String],
    export: Option<Value>,
    spans: Option<&TypeDeclSpans>,
) -> Result<Vec<Value>, RuntimeError> {
    let does = roles
        .iter()
        .map(|role| type_trait(names, RakuAstClass::TraitDoes, role))
        .collect::<Result<Vec<_>, _>>()?;
    // Without the spans, `is export` follows the roles.
    let at = spans.map_or(u8::MAX, |s| s.order.export);
    Ok(interleave(
        does,
        export.into_iter().map(|e| (at, e)).collect(),
    ))
}

/// The `traits` of a `subset`: `is export`, its own `is` traits and the `of`
/// base, as written. A clause nobody wrote (no spans, or `my Int subset S`,
/// whose base is no `of` clause) keeps the order `is export`, the `is`
/// traits, `of`.
// Cost: O(t log t), t = traits.
pub(super) fn subset_traits(
    names: &NameScope,
    export: Option<Value>,
    custom_traits: &[(String, Option<Expr>)],
    of: Option<&str>,
    spans: Option<&TypeDeclSpans>,
) -> Result<Vec<Value>, RuntimeError> {
    let others = super::decl_traits::view_node(names, custom_traits)?;
    let written = |at: u8, span: crate::ast::SrcSpan, default: u8| {
        if span.is_some() { at } else { default }
    };
    let (export_at, of_at) = spans.map_or((0, u8::MAX), |s| {
        (
            written(s.order.export, s.export.whole, 0),
            written(s.order.of, s.of.whole, u8::MAX),
        )
    });
    let mut placed: Vec<(u8, Value)> = export.into_iter().map(|e| (export_at, e)).collect();
    if let Some(base) = of {
        placed.push((of_at, type_trait(names, RakuAstClass::TraitOf, base)?));
    }
    Ok(interleave(others, placed))
}

/// `others` (in their own order) with each `(place, trait)` of `placed` put at
/// its written place. A place past the end appends.
// Cost: O(t log t), t = traits.
pub(super) fn interleave(others: Vec<Value>, mut placed: Vec<(u8, Value)>) -> Vec<Value> {
    placed.sort_by_key(|(at, _)| *at);
    let mut placed = placed.into_iter().peekable();
    let mut others = others.into_iter();
    let mut out = Vec::new();
    loop {
        let at = out.len();
        if let Some((_, value)) = placed.next_if(|(place, _)| usize::from(*place) <= at) {
            out.push(value);
        } else if let Some(value) = others.next() {
            out.push(value);
        } else if let Some((_, value)) = placed.next() {
            out.push(value);
        } else {
            return out;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::interleave;
    use crate::value::Value;

    fn names(values: &[Value]) -> Vec<String> {
        values.iter().map(|v| v.to_string_value()).collect()
    }

    #[test]
    fn places_the_fixed_traits_among_the_others() {
        let others = vec![Value::str_from("a"), Value::str_from("b")];
        let placed = vec![(2, Value::str_from("of")), (0, Value::str_from("export"))];
        assert_eq!(
            names(&interleave(others, placed)),
            ["export", "a", "of", "b"]
        );
    }

    #[test]
    fn a_place_past_the_end_appends() {
        let placed = vec![(5, Value::str_from("of"))];
        assert_eq!(
            names(&interleave(vec![Value::str_from("a")], placed)),
            ["a", "of"]
        );
    }
}
