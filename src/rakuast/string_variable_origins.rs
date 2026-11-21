//! Origins of a variable interpolated in a string (`"a $x b"`, `"$.n"`,
//! `"$<word>"`; ADR-10723 Stage 2, #15780).

use super::infix_origins::leaf_text;
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Whether `class` is a variable a string interpolates by its spelling.
// Cost: O(1).
pub(super) fn is_variable(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::VarLexical
            | RakuAstClass::VarDynamic
            | RakuAstClass::VarAttribute
            | RakuAstClass::VarAttributePublic
    )
}

/// The range of a variable written at `cursor`, when its spelling is there
/// and nothing that would extend the interpolation (`[`, `{`, `<`, `(`, `.`)
/// follows it, unless that is the text of `literal`, the literal segment
/// after it (`"$n.new answers"`: no call without parentheses).
// Cost: O(v + l), v = length of the variable's name, l = of `literal`.
pub(super) fn variable_at(
    segment: &RakuAstNode,
    cursor: usize,
    end: usize,
    literal: Option<&str>,
) -> Option<(usize, usize)> {
    let name = leaf_text(segment)?;
    super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        if literal.is_some_and(|l| l.starts_with('.') && rest.starts_with(l)) {
            return Some((cursor, cursor + name.len()));
        }
        // A `-` or `'` continues the name only before a letter (`$x-y`,
        // `$don't`); `'$src' is` ends it.
        let joins = rest
            .strip_prefix(['-', '\''])
            .is_some_and(|after| after.starts_with(|c: char| c.is_alphabetic() || c == '_'));
        let extends = joins
            || rest.starts_with(['[', '{', '<', '(', '.'])
            || rest.starts_with(|c: char| c.is_alphanumeric() || c == '_');
        (!extends).then_some((cursor, cursor + name.len()))
    })
}

/// The range of a match variable (`$<word>`, `$0`) written at `cursor`;
/// a `$<word>`'s word quote is placed between its brackets.
// Cost: O(n), n = length of the variable.
pub(super) fn match_variable_at(
    segment: &mut RakuAstNode,
    cursor: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let written = super::match_vars::interpolated_text(segment)?;
    super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let rest = text.strip_prefix(written.as_str())?;
        let extends = segment.class == RakuAstClass::VarPositionalCapture
            && rest.starts_with(|c: char| c.is_ascii_digit());
        (!extends && !rest.starts_with(['[', '{', '<', '.'])).then_some(())
    })?;
    let range = (cursor, cursor + written.len());
    super::origin::set_span(segment, SrcSpan::new(range.0, range.1));
    super::match_vars::named_capture_origins(segment);
    Some(range)
}
