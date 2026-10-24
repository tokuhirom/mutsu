use super::*;
use crate::ast::parsed::{ModifierKind, StatementModifier};

/// Parse one statement modifier at `rest`: its keyword and its operand as
/// written. `stmt` is the statement it modifies; a `with` / `without`
/// operand followed by a comma list extends the arguments of a modified call.
/// Returns None if no modifier matched. Lowering the statement with its
/// modifiers is `rakuast::modifier_lower`'s.
pub(super) fn parse_single_modifier<'a>(
    rest: &'a str,
    stmt: &mut Stmt,
) -> Result<Option<(&'a str, StatementModifier)>, PError> {
    let Some(word) = leading_modifier_keyword(rest) else {
        return Ok(None);
    };
    let kind = ModifierKind::from_keyword(word).expect("a modifier keyword");
    if kind == ModifierKind::For {
        return parse_for_modifier(rest, stmt);
    }
    let r = keyword(word, rest).expect("the keyword just matched");
    let (r, _) = ws1(r)?;
    let operand_input = r;
    let expected = match kind {
        ModifierKind::Given => "expected topic expression after 'given'",
        ModifierKind::When => "expected match expression after 'when'",
        ModifierKind::If => "expected condition expression after 'if'",
        ModifierKind::Unless => "expected condition expression after 'unless'",
        ModifierKind::While => "expected condition expression after 'while'",
        ModifierKind::Until => "expected condition expression after 'until'",
        ModifierKind::With | ModifierKind::Without | ModifierKind::For => "",
    };
    let parsed = match kind {
        ModifierKind::If | ModifierKind::Unless | ModifierKind::Given | ModifierKind::When => {
            parse_comma_or_expr(r)
        }
        _ => expression(r),
    };
    let (r, expr) = parsed.map_err(|err| {
        if expected.is_empty() {
            return err;
        }
        PError {
            messages: merge_expected_messages(expected, &err.messages),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        }
    })?;
    if matches!(kind, ModifierKind::If | ModifierKind::Unless) {
        check_two_terms_across_lines(operand_input, r)?;
    }
    let span = crate::parser::primary::term_span(rest, r);
    let mut r = r;
    if matches!(kind, ModifierKind::With | ModifierKind::Without) {
        // Do not consume a full `with EXPR -> $param { ... }` block header
        // as a statement modifier. This preserves parsing of:
        //   subtest 'sub' => { ... }
        //   with make-temp-dir() -> $dir { ... }
        // as two statements, rather than stmt + postfix modifier + lambda.
        let (r_ws, _) = ws(r)?;
        if r_ws.starts_with("->") || r_ws.starts_with('{') {
            return Ok(None);
        }
        if let Stmt::Call { args, .. } = stmt {
            loop {
                let (r_ws, _) = ws(r)?;
                if !r_ws.starts_with(',') {
                    r = r_ws;
                    break;
                }
                let (after_comma, _) = ws(&r_ws[1..])?;
                let (after_comma, arg_expr) = expression(after_comma)?;
                args.push(CallArg::Positional(arg_expr));
                r = after_comma;
            }
        }
    }
    Ok(Some((r, StatementModifier { kind, expr, span })))
}
