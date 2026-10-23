//! Shared postcircumfix parsing for interpolated variable references.
use super::*;

fn parse_one_postcircumfix_index(input: &str, target: Expr) -> (Expr, &str) {
    // A `.`-prefixed postcircumfix calls the same operator via method
    // syntax (`$m.<x>` same as `$m<x>`, `@a.[1]` same as `@a[1]`,
    // `%h.<a>` same as `%h<a>`) — strip the dot before matching so the
    // branches below see the bracket form directly. Only strip it when
    // an opener actually follows, so an ordinary `.method` call (handled
    // by `try_parse_interp_method_call`) is left untouched.
    let input = match input.strip_prefix('.') {
        Some(after_dot)
            if after_dot.starts_with(['<', '[', '{']) || after_dot.starts_with('\u{00AB}') =>
        {
            after_dot
        }
        _ => input,
    };
    // Double angle bracket indexing: $var<<key>> (must be checked before single <)
    if let Some(after_dlt) = input.strip_prefix("<<")
        && let Some(end) = after_dlt.find(">>")
    {
        let content = &after_dlt[..end];
        let index = parse_shell_words_index(content);
        return (
            Expr::Index {
                target: Box::new(target),
                index: Box::new(index),
                is_positional: false,
                spelling: Default::default(),
            },
            &after_dlt[end + 2..],
        );
    }
    // French guillemet indexing: $var«key»
    if let Some(after_guillemet) = input.strip_prefix('\u{00AB}')
        && let Some(end) = after_guillemet.find('\u{00BB}')
    {
        let content = &after_guillemet[..end];
        let index = parse_shell_words_index(content);
        return (
            Expr::Index {
                target: Box::new(target),
                index: Box::new(index),
                is_positional: false,
                spelling: Default::default(),
            },
            &after_guillemet[end + '\u{00BB}'.len_utf8()..],
        );
    }
    // Angle bracket indexing: $var<key>
    if let Some(after_lt) = input.strip_prefix('<')
        && let Some(end) = after_lt.find('>')
    {
        let content = &after_lt[..end];
        let words: Vec<&str> = content.split_whitespace().collect();
        let index = if words.len() <= 1 {
            Expr::Literal(literal_str(
                words.first().copied().unwrap_or("").to_string(),
            ))
        } else {
            Expr::ArrayLiteral(
                words
                    .into_iter()
                    .map(|w| Expr::Literal(literal_str(w.to_string())))
                    .collect(),
            )
        };
        return (
            Expr::Index {
                target: Box::new(target),
                index: Box::new(index),
                is_positional: false,
                spelling: crate::ast::IndexSpelling::Angle,
            },
            &after_lt[end + 1..],
        );
    }
    // Square bracket indexing: $var[expr]
    if let Some(after_bracket) = input.strip_prefix('[')
        && let Some(end) = after_bracket.find(']')
    {
        let content = after_bracket[..end].trim();
        // Parse the index expression. A top-level comma makes it a slice
        // (`@a[1,2]`), which the non-interpolated parser represents as an
        // `ArrayLiteral` of the elements; split on top-level commas and build
        // the same so `"@a[1,2]"` interpolates both elements (and a trailing
        // `.method` chains correctly).
        let parse_one = |s: &str| -> Expr {
            let s = s.trim();
            if let Ok(n) = s.parse::<i64>() {
                Expr::Literal(Value::int(n))
            } else if let Ok((_, expr)) = crate::parser::expr::expression(s) {
                expr
            } else {
                Expr::Literal(literal_str(s.to_string()))
            }
        };
        let parts = split_top_level_commas(content);
        let index = if parts.len() > 1 {
            Expr::ArrayLiteral(parts.iter().map(|p| parse_one(p)).collect())
        } else {
            parse_one(content)
        };
        // The subscript is a `semilist`: its statements are numbered for
        // `use trace` like those of one written outside a string.
        if let Some(semilist) = crate::parser::stmt::trace::semilist_open(after_bracket) {
            semilist.close_at(&after_bracket[end..]);
        }
        return (
            Expr::Index {
                target: Box::new(target),
                index: Box::new(index),
                is_positional: true,
                spelling: Default::default(),
            },
            &after_bracket[end + 1..],
        );
    }
    // Curly brace indexing: %hash{expr}
    if let Some(after_brace) = input.strip_prefix('{') {
        // Find matching closing brace respecting nesting
        let mut depth = 1usize;
        let mut brace_end = None;
        for (idx, ch) in after_brace.char_indices() {
            if ch == '{' {
                depth += 1;
            } else if ch == '}' {
                depth -= 1;
                if depth == 0 {
                    brace_end = Some(idx);
                    break;
                }
            }
        }
        if let Some(end) = brace_end {
            let content = after_brace[..end].trim();
            if !content.is_empty() {
                // A top-level comma makes it a hash slice (`%h{'a','c'}`):
                // build an ArrayLiteral of the keys, matching the
                // non-interpolated parser, so all selected values interpolate.
                let parts = split_top_level_commas(content);
                let index = if parts.len() > 1 {
                    let elems: Option<Vec<Expr>> = parts
                        .iter()
                        .map(|p| {
                            crate::parser::expr::expression(p.trim())
                                .ok()
                                .map(|(_, e)| e)
                        })
                        .collect();
                    elems.map(Expr::ArrayLiteral)
                } else {
                    crate::parser::expr::expression(content)
                        .ok()
                        .map(|(_, e)| e)
                };
                if let Some(index) = index {
                    // The subscript is a `semilist`: its statements are
                    // numbered for `use trace` like those of one written
                    // outside a string.
                    if let Some(semilist) = crate::parser::stmt::trace::semilist_open(after_brace) {
                        semilist.close_at(&after_brace[end..]);
                    }
                    return (
                        Expr::Index {
                            target: Box::new(target),
                            index: Box::new(index),
                            is_positional: false,
                            spelling: Default::default(),
                        },
                        &after_brace[end + 1..],
                    );
                }
            }
        }
    }
    (target, input)
}
// Interpolated variables use the same postcircumfix chaining rules as
// ordinary expressions: `@units[*-1]<plural>` is an index followed by an
// associative lookup, not an index plus literal `<plural>` text.
pub(super) fn parse_postcircumfix_index(mut input: &str, mut target: Expr) -> (Expr, &str) {
    loop {
        let (next_target, remainder) = parse_one_postcircumfix_index(input, target);
        if remainder.len() == input.len() {
            return (next_target, remainder);
        }
        target = next_target;
        input = remainder;
    }
}
