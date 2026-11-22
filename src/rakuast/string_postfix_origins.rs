//! Origins of an interpolated postfix application in a string (ADR-10723
//! Stage 2, #15780): `"@x[0]"`, `"%h<a>"`, `"$x.ord.fmt('%d')"`. Each
//! application is placed from the end of its operand, itself an interpolated
//! variable or an application placed the same way, so a chain of calls and
//! subscripts is placed link by link.

use super::RakuAstClass;
use super::RakuAstNode;
use super::infix_origins::{child, leaf_text, with_child};
use crate::ast::SrcSpan;

/// The variable an interpolated postfix application (`@x[0]`, `%h<a>`,
/// `$x.ord.chr()`) is applied to, below any applications it is chained on.
// Cost: O(c * f), c = links of the chain, f = fields of a node.
pub(super) fn interpolated_variable(node: &RakuAstNode) -> Option<&RakuAstNode> {
    let operand = child(node, "operand")?;
    if operand.class == RakuAstClass::ApplyPostfix {
        return interpolated_variable(operand);
    }
    super::string_variable_origins::is_variable(operand.class).then_some(operand)
}

/// The range of an interpolated `$x[...]` / `%h{...}` / `%h<...>` /
/// `$x.name(...)` written at `cursor`, a chain of them included, placing its
/// variable and, from those, each subscript or method call. A subscript or
/// argument list holding a backslash, a comment or a quote that could hide
/// its closing bracket is left alone: exact or absent.
// Cost: O(n), n = length of the interpolation.
pub(super) fn postfix_at(
    names: &crate::scope::NameScope,
    segment: &mut RakuAstNode,
    cursor: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let chained = child(segment, "operand")?.class == RakuAstClass::ApplyPostfix;
    let operand_end = if chained {
        let mut placed = None;
        with_child(segment, "operand", |inner| {
            placed = postfix_at(names, inner, cursor, end);
        });
        placed?.1
    } else {
        let name = leaf_text(interpolated_variable(segment)?)?;
        let matches = super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
            text.starts_with(name.as_str()).then_some(())
        });
        matches?;
        cursor + name.len()
    };
    let postfix = child(segment, "postfix")?.class;
    // `"@x.[]"`: the dot a subscript may be written after.
    let mut dot = 0;
    let to = super::unit_source::with_text(SrcSpan::new(operand_end, end), |text| {
        let mut rest = text;
        if postfix != RakuAstClass::CallMethod
            && let Some(after) = rest.strip_prefix('.')
            && after.starts_with(['[', '{', '<'])
        {
            rest = after;
            dot = 1;
        }
        let close = match postfix {
            RakuAstClass::PostcircumfixArrayIndex => balanced(rest, '[', ']')?,
            RakuAstClass::PostcircumfixHashIndex => balanced(rest, '{', '}')?,
            RakuAstClass::PostcircumfixLiteralHashIndex => {
                let inner = rest.strip_prefix('<')?;
                if inner.starts_with('<') || inner.contains(['\\', '"', '\'']) {
                    return None;
                }
                inner.find('>')? + 2
            }
            RakuAstClass::CallMethod => {
                let method = rest.strip_prefix('.')?;
                let name_len = method
                    .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-')))
                    .filter(|&n| n > 0)?;
                // `.ord` of `"$x.ord.chr()"`: a link of a chain needs no
                // parentheses (the whole interpolation ends in some).
                let args = &method[name_len..];
                if args.starts_with('(') {
                    1 + name_len + balanced(args, '(', ')')?
                } else {
                    1 + name_len
                }
            }
            _ => return None,
        };
        Some(operand_end + dot + close)
    })?;
    super::origin::set_span(segment, SrcSpan::new(cursor, to));
    if !chained {
        with_child(segment, "operand", |var| {
            super::origin::set_span(var, SrcSpan::new(cursor, operand_end))
        });
    }
    if postfix == RakuAstClass::CallMethod {
        super::call_origins::postfix_call(segment, SrcSpan::new(cursor, to));
    } else {
        // The subscript's expressions were read without positions.
        if matches!(
            postfix,
            RakuAstClass::PostcircumfixArrayIndex | RakuAstClass::PostcircumfixHashIndex
        ) {
            let open = operand_end + dot + 1;
            let inner =
                super::unit_source::with_text(SrcSpan::new(open, to - 1), |t| Some(t.to_string()));
            if let Some(inner) = inner {
                with_child(segment, "postfix", |sub| {
                    with_child(sub, "index", |list| {
                        super::fragment_origins::statements(names, list, &inner, open)
                    })
                });
            }
        }
        super::list_origins::postcircumfix(segment);
    }
    Some((cursor, to))
}

/// The length of the bracketed text `text` starts with, `open` to its
/// matching `close`. A quote inside (`('%d')`) is skipped to its closing
/// quote when it holds nothing that could hide one; a backslash, a comment
/// or any other quote gives `None`.
// Cost: O(n), n = length of the bracketed text.
fn balanced(text: &str, open: char, close: char) -> Option<usize> {
    if !text.starts_with(open) {
        return None;
    }
    let mut depth = 0usize;
    let mut chars = text.char_indices();
    while let Some((at, c)) = chars.next() {
        match c {
            '"' | '\'' => {
                let after = &text[at + 1..];
                let len = after.find(c)?;
                let quoted = &after[..len];
                if quoted.contains(['\\', '{', '}', '$', '@', '"', '\'']) {
                    return None;
                }
                // Step past the quoted text and its closing quote.
                for _ in quoted.chars() {
                    chars.next();
                }
                chars.next();
            }
            '\\' | '#' | '「' | '」' => return None,
            c if c == open => depth += 1,
            c if c == close => {
                depth -= 1;
                if depth == 0 {
                    return Some(at + c.len_utf8());
                }
            }
            _ => {}
        }
    }
    None
}
