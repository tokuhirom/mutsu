use super::*;
use crate::ast::parsed::{ModifierKind, StatementModifier};

/// Parse a `for` statement modifier at `rest`: its list as written. `stmt` is
/// the statement it modifies. Returns None if `rest` starts a `for` loop's
/// header instead.
pub(super) fn parse_for_modifier<'a>(
    rest: &'a str,
    stmt: &mut Stmt,
) -> Result<Option<(&'a str, StatementModifier)>, PError> {
    if let Some(r) = keyword("for", rest) {
        if matches!(stmt.peel_parsed(), Stmt::For { .. }) {
            return Err(PError::raw(
                "double statement-modifying for is not allowed".to_string(),
                Some(rest.len()),
            ));
        }
        let (r, _) = ws1(r)?;
        // Sequence operators absorb a comma-separated seed list on their left:
        // `for 1, { $_ + 1 } ... 3` iterates one sequence, not an array whose
        // second item is another sequence.  This is the same special case used
        // by listop and assignment parsing.
        let (r, iterable) =
            if let Some(result) = crate::parser::primary::try_parse_sequence_arg_list(r) {
                result?
            } else {
                let (r, first) = expression(r).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected iterable expression after 'for'",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r.len())),
                    exception: None,
                })?;
                // Parse comma-separated list for `for` modifier: `expr for 1, 2, 3`
                let (r, iterable) = {
                    let mut items = vec![first];
                    let mut r = r;
                    let mut trailing_comma = false;
                    loop {
                        let (r2, _) = ws(r)?;
                        if !r2.starts_with(',') {
                            break;
                        }
                        let r2 = &r2[1..];
                        let (r2, _) = ws(r2)?;
                        if r2.is_empty() || r2.starts_with('}') || r2.starts_with(';') {
                            r = r2;
                            trailing_comma = true;
                            break;
                        }
                        let (r2, next) = expression(r2)?;
                        items.push(next);
                        r = r2;
                    }
                    // A trailing comma builds a 1-element list even with a single item:
                    // `for @a,` iterates once over `(@a,)` (the array itemized), matching
                    // the parenthesized `for (@a,)` form, rather than flattening `@a`.
                    if items.len() == 1 && !trailing_comma {
                        (r, items.into_iter().next().unwrap())
                    } else {
                        (r, Expr::ArrayLiteral(items))
                    }
                };
                (r, iterable)
            };
        let span = crate::parser::primary::term_span(rest, r);
        let (r, _) = ws(r)?;
        // Detect "Two terms in a row" on the same line after the for iterable.
        // e.g., `.say for (1, 2, 3)«~» "!"` — the `"!"` is a term without
        // an infix operator separating it from the iterable.
        if !r.is_empty()
            && !r.starts_with(';')
            && !r.starts_with('}')
            && !r.starts_with(')')
            && !r.starts_with("->")
            && !r.starts_with('{')
            && !is_stmt_modifier_keyword(r)
            && starts_with_term_char(r)
        {
            return Err(PError::fatal_at(
                "Confused. Two terms in a row".to_string(),
                r,
            ));
        }
        // Do not consume full loop headers as statement modifiers.
        // This preserves parsing of:
        //   { ... }
        //   for @values -> $v { ... }
        // as two statements, rather than block + postfix modifier + lambda.
        if r.starts_with("->") || r.starts_with('{') {
            return Ok(None);
        }
        return Ok(Some((
            r,
            StatementModifier {
                kind: ModifierKind::For,
                expr: iterable,
                span,
            },
        )));
    }
    Ok(None)
}
