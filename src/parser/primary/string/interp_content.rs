use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::parse_result::PError;
use crate::value::ValueView;

use super::helpers::literal_str;
/// Assemble interpolation parts into a final expression.
pub(crate) fn finalize_interpolation(parts: Vec<Expr>, current: String) -> Expr {
    if parts.is_empty() {
        Expr::Literal(literal_str(current))
    } else {
        let mut parts = parts;
        if !current.is_empty() {
            parts.push(Expr::Literal(literal_str(current)));
        }
        if parts.len() == 1
            && matches!(&parts[0], Expr::Literal(v) if matches!(v.view(), ValueView::Str(_)))
        {
            return parts.into_iter().next().unwrap();
        }
        Expr::StringInterpolation(parts)
    }
}

/// Interpolate variables in string content (used by qq// etc.)
pub(crate) fn interpolate_string_content(content: &str) -> Expr {
    interpolate_string_content_with_modes(content, true, false)
}

pub(crate) fn interpolate_string_content_with_modes(
    content: &str,
    interpolate_vars: bool,
    interpolate_closures: bool,
) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
            // `\q[...]` / `\qq[...]` / `\qw[...]` re-quote their body into a
            // whole expression, so they run before the char-level handler.
            if let Some(r) = crate::parser::primary::quote_adverbs::process_q_escape(
                rest,
                &mut parts,
                &mut current,
            ) {
                rest = r;
                continue;
            }
            match process_escape_sequence(rest, &mut current, &[]) {
                Ok(Some((r, needs_continue))) => {
                    rest = r;
                    if needs_continue {
                        continue;
                    }
                }
                Ok(None) | Err(_) => {
                    let c = rest.as_bytes()[1] as char;
                    current.push('\\');
                    current.push(c);
                    rest = &rest[2..];
                }
            }
            continue;
        }
        if interpolate_closures
            && rest.starts_with('{')
            && let Some((after, inner)) = parse_braced_interpolation(rest)
            && let Some(expr) = parse_closure_part(inner.trim())
        {
            if !current.is_empty() {
                parts.push(Expr::Literal(literal_str(std::mem::take(&mut current))));
            }
            parts.push(expr);
            rest = after;
            continue;
        }
        if interpolate_vars && let Some(r) = try_interpolate_var(rest, &mut parts, &mut current) {
            rest = r;
            continue;
        }
        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    finalize_interpolation(parts, current)
}

/// A `{ … }` closure part of a `qq`-interpolated body (a heredoc, `qq{…}`, a
/// `qq:to`, an `s///` replacement): the same scope-isolated `DoStmt(Block(…))`
/// the `"…"` parser builds, so it is its own Raku call frame (`callframe(0)`
/// in it is the `Block`) and its placeholders belong to it. `None` when the
/// body does not parse, in which case the caller keeps the `{` literal.
pub(in crate::parser::primary) fn parse_closure_part(inner: &str) -> Option<Expr> {
    parse_interpolation_block(inner).ok().flatten()
}

/// Parse the body of an assignment-form substitution RHS (`s[pat] = EXPR`)
/// as a thunk expression. It may hold a full statement list, not just a single
/// expression — mirror the `$( … )` interpolation path: try a statement list
/// first, then fall back to a single expression. Unlike [`parse_closure_part`]
/// a single expression comes back bare, not wrapped in a Block: the thunk is
/// no frame of its own and a placeholder in it belongs to the enclosing block.
pub(in crate::parser) fn parse_thunk_expr_body(inner: &str) -> Option<Expr> {
    // Parsed in a fresh lexical scope so the implicit
    // `state $__ANON_STATE_<id>__;` declaration a bare `$` mints lands in the
    // returned statements rather than in the scope that is being parsed when
    // the replacement is (re)parsed at run time.
    crate::parser::stmt::simple::push_scope();
    let parsed = parse_thunk_expr_body_scoped(inner);
    crate::parser::stmt::simple::pop_scope();
    parsed
}

/// Parse the body of a `"…{ … }…"` interpolation block into the scope-isolated
/// `DoStmt(Block(…))` the double-quote parser wraps it in.
///
/// Shares [`parse_thunk_expr_body`]'s lexical-scope discipline: the block is
/// its own block, so a bare `$` in it is a `state` of that block and its
/// implicit declaration belongs inside the returned statement list, not hoisted
/// into the enclosing routine.
pub(in crate::parser::primary) fn parse_interpolation_block(
    block_src: &str,
) -> Result<Option<Expr>, PError> {
    crate::parser::stmt::simple::push_scope();
    let stmts = parse_interpolation_block_stmts(block_src);
    crate::parser::stmt::simple::pop_scope();
    stmts.map(|stmts| stmts.map(|stmts| Expr::DoStmt(Box::new(crate::ast::Stmt::Block(stmts)))))
}

/// [`parse_interpolation_block`]'s body, with the block's scope already pushed.
fn parse_interpolation_block_stmts(
    block_src: &str,
) -> Result<Option<Vec<crate::ast::Stmt>>, PError> {
    let mut stmts = match crate::parser::stmt::stmt_list_pub(block_src) {
        Ok((sr, stmts)) if sr.trim().is_empty() => stmts,
        Err(error) if error.is_fatal() => return Err(error),
        _ => match expression(block_src) {
            Ok((expr_rest, expr)) if expr_rest.trim().is_empty() => {
                vec![crate::ast::Stmt::Expr(expr)]
            }
            Err(error) if error.is_fatal() => return Err(error),
            _ => return Ok(None),
        },
    };
    crate::parser::stmt::simple::prepend_anon_state_decls(&mut stmts);
    Ok(Some(stmts))
}

/// [`parse_thunk_expr_body`]'s body, run with the block's own lexical scope
/// already pushed so the anonymous-`state` declarations it mints land inside it.
fn parse_thunk_expr_body_scoped(inner: &str) -> Option<Expr> {
    if let Ok((leftover, mut stmts)) = crate::parser::stmt::stmt_list_pub(inner)
        && leftover.trim().is_empty()
        && !stmts.is_empty()
    {
        crate::parser::stmt::simple::prepend_anon_state_decls(&mut stmts);
        return Some(if stmts.len() == 1 {
            Expr::DoStmt(Box::new(stmts.into_iter().next().unwrap()))
        } else {
            Expr::desugar_block(stmts)
        });
    }
    if let Ok((leftover, expr)) = expression(inner)
        && leftover.trim().is_empty()
    {
        let mut stmts = vec![crate::ast::Stmt::Expr(expr)];
        crate::parser::stmt::simple::prepend_anon_state_decls(&mut stmts);
        if stmts.len() == 1 {
            let Some(crate::ast::Stmt::Expr(expr)) = stmts.into_iter().next() else {
                unreachable!("single-element vec built from Stmt::Expr");
            };
            return Some(expr);
        }
        return Some(Expr::desugar_block(stmts));
    }
    None
}

pub(crate) fn parse_braced_interpolation(input: &str) -> Option<(&str, &str)> {
    if !input.starts_with('{') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, ch) in input.char_indices() {
        if ch == '{' {
            depth += 1;
        } else if ch == '}' {
            depth -= 1;
            if depth == 0 {
                let inner = &input[1..idx];
                let after = &input[idx + 1..];
                return Some((after, inner));
            }
        }
    }
    None
}

/// Try to consume an embedded `\qqw[...]` or `\qw[...]` quote-words escape at
/// the start of `rest`. `\qqw` interpolates the body first, `\qw` keeps it
/// literal; both then split on whitespace into a word list, which joins with
/// single spaces in string context (matching raku). Returns the remainder and
/// the word-list expression, or `None` when the marker does not match.
pub(crate) fn try_embedded_qw(rest: &str) -> Option<(&str, Expr)> {
    for &(marker, interpolate) in &[("\\qqw", true), ("\\qw", false)] {
        let Some(after_marker) = rest.strip_prefix(marker) else {
            continue;
        };
        let Some(open) = after_marker.chars().next() else {
            continue;
        };
        if open.is_alphanumeric() || open.is_whitespace() {
            continue;
        }
        let parsed = if let Some(close) = unicode_bracket_close(open) {
            read_bracketed(after_marker, open, close, true).ok()
        } else {
            let body = &after_marker[open.len_utf8()..];
            body.find(open)
                .map(|end| (&body[end + open.len_utf8()..], &body[..end]))
        };
        let (after, inner) = parsed?;
        let base = if interpolate {
            interpolate_string_content(inner)
        } else {
            Expr::Literal(literal_str(inner.to_string()))
        };
        let words = Expr::MethodCall {
            target: Box::new(base),
            name: crate::symbol::Symbol::intern("words"),
            args: vec![],
            modifier: None,
            quoted: false,
        };
        return Some((after, words));
    }
    None
}

pub(crate) fn parse_single_quote_qq(content: &str) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        // The whole `\q`/`\qq`/`\qw`/`\qqw` family goes through the one shared
        // implementation (see `quote_adverbs::process_q_escape`); this walk used
        // to carry its own partial copy that knew `\qq` and `\qw` but not `\q`.
        if let Some(r) =
            crate::parser::primary::quote_adverbs::process_q_escape(rest, &mut parts, &mut current)
        {
            rest = r;
            continue;
        }

        if let Some(after_backslash) = rest.strip_prefix('\\')
            && let Some(next) = after_backslash.chars().next()
        {
            if next == '\'' || next == '\\' {
                current.push(next);
            } else {
                current.push('\\');
                current.push(next);
            }
            rest = &after_backslash[next.len_utf8()..];
            continue;
        }

        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    finalize_interpolation(parts, current)
}
