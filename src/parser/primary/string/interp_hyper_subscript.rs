//! Hyper subscripts after an interpolated term (`"@rows>>.[1]"`).

use crate::ast::Expr;
use crate::symbol::Symbol;

use super::helpers::literal_str;

/// `>>.[...]` / `».[...]` (AT-POS), `>>.{...}` (AT-KEY) or `>>.<...>` (AT-KEY
/// of the words), the dot optional, right after an interpolated term: the hyper subscript, and
/// what follows it. `None` when `input` does not open with one or its bracket
/// is unbalanced (the text then stays literal).
// Cost: O(n), n = chars of the bracketed text (scanned, then parsed).
pub(super) fn try_parse_interp_hyper_subscript<'a>(
    input: &'a str,
    target: &Expr,
) -> Option<(Expr, &'a str)> {
    let after_hyper = input
        .strip_prefix(">>")
        .or_else(|| input.strip_prefix('\u{bb}'))?;
    // The dot between the hyper marker and the bracket is optional.
    let after_hyper = after_hyper.strip_prefix('.').unwrap_or(after_hyper);
    let open = after_hyper.chars().next()?;
    let close = match open {
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => return None,
    };
    let mut depth = 0usize;
    let mut end = None;
    for (idx, ch) in after_hyper.char_indices() {
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                end = Some(idx);
                break;
            }
        }
    }
    let end = end?;
    let inner = &after_hyper[open.len_utf8()..end];
    let after = &after_hyper[end + close.len_utf8()..];
    let index = if open == '<' {
        let words: Vec<Expr> = inner
            .split_whitespace()
            .map(|word| Expr::Literal(literal_str(word)))
            .collect();
        match words.len() {
            1 => words.into_iter().next()?,
            _ => Expr::ArrayLiteral(words),
        }
    } else {
        let (rest, index) = crate::parser::expr::expression(inner.trim()).ok()?;
        if !rest.trim().is_empty() {
            return None;
        }
        index
    };
    let name = if open == '[' { "AT-POS" } else { "AT-KEY" };
    Some((
        Expr::HyperMethodCall {
            target: Box::new(target.clone()),
            name: Symbol::intern(name),
            args: vec![index],
            modifier: None,
            quoted: false,
        },
        after,
    ))
}
