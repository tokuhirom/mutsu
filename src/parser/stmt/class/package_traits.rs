//! Package header traits, including their source spelling.

use crate::ast::Expr;
use crate::ast::routine_trait::{RoutineTrait, TraitArgument};
use crate::parser::expr::expression;
use crate::parser::helpers::{skip_balanced_parens, ws, ws1};
use crate::parser::parse_result::{PError, PResult};
use crate::parser::stmt::{keyword, qualified_ident};
use crate::value::Value;

#[derive(Default)]
pub(crate) struct PackageTraits {
    pub(crate) export_tags: Option<Vec<String>>,
    pub(crate) custom: Vec<(String, Option<Expr>)>,
    pub(crate) source: Vec<RoutineTrait>,
}

pub(crate) fn parse_package_is_traits(input: &str) -> PResult<'_, PackageTraits> {
    let mut rest = input;
    let mut parsed = PackageTraits::default();
    while let Some(after_is) = keyword("is", rest) {
        let (after_is, _) = ws1(after_is)?;
        let (after_name, name) = qualified_ident(after_is)?;
        let (after_name, _) = ws(after_name)?;
        let (after, argument, source_argument) = if name == "export" {
            let mut tags = Vec::new();
            super::class_decl::push_export_tags(after_name, &mut tags);
            parsed.export_tags = Some(tags.clone());
            let source = (!tags.is_empty()).then_some(TraitArgument::ExportTags(tags));
            (skip_balanced_parens(after_name), None, source)
        } else if after_name.starts_with('<') {
            let (after, words) = crate::parser::helpers::parse_trait_angle_arg(after_name)?;
            (
                after,
                Some(Expr::Literal(Value::str(words.clone()))),
                Some(TraitArgument::Words(words)),
            )
        } else if let Some(inner) = after_name.strip_prefix('(') {
            let (inner, _) = ws(inner)?;
            let (after, argument) = if let Some(after) = inner.strip_prefix(')') {
                (after, Expr::ArrayLiteral(Vec::new()))
            } else {
                let (after, argument) = expression(inner)?;
                let (after, _) = ws(after)?;
                let after = after
                    .strip_prefix(')')
                    .ok_or_else(|| PError::expected("closing trait parenthesis"))?;
                (after, argument)
            };
            (
                after,
                Some(argument.clone()),
                Some(TraitArgument::Parentheses(argument)),
            )
        } else {
            (after_name, None, None)
        };
        if crate::ast::spelled::keeping() {
            parsed.source.push(RoutineTrait::Is {
                name: name.clone(),
                argument: source_argument,
            });
        }
        // Export has its existing registration; deprecation follows the
        // class declarator's existing core-trait handling.
        if !matches!(name.as_str(), "export" | "DEPRECATED") {
            parsed.custom.push((name, argument));
        }
        let (after, _) = ws(after)?;
        rest = after;
    }
    Ok((rest, parsed))
}
