use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
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
            && let Some(expr) = parse_braced_closure_body(inner.trim())
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

/// Parse the body of a `{ … }` string-interpolation block. A block may hold a
/// full statement list (`{$c++; "new"}`), not just a single expression — mirror
/// the `$( … )` interpolation path: try a statement list first (so multi-statement
/// blocks and statement-modifiers work), then fall back to a single expression.
pub(in crate::parser::primary) fn parse_braced_closure_body(inner: &str) -> Option<Expr> {
    // The `{ … }` is its OWN block, so a bare `$` inside it is a `state`
    // variable of *that* block, not of the enclosing routine. Parsing it in a
    // fresh lexical scope is what puts the implicit
    // `state $__ANON_STATE_<id>__;` declaration inside the interpolation block,
    // where the block's per-execution clone restarts it — raku's documented
    // trap `sub count-it { say "Count is {$++}" }` prints `0` on every call
    // (Language/traps.rakudoc, "Using a block to interpolate anon state vars").
    crate::parser::stmt::simple::push_scope();
    let parsed = parse_braced_closure_body_scoped(inner);
    crate::parser::stmt::simple::pop_scope();
    parsed
}

/// Parse the body of a `"…{ … }…"` interpolation block into the scope-isolated
/// `DoStmt(Block(…))` the double-quote parser wraps it in.
///
/// Shares [`parse_braced_closure_body`]'s lexical-scope discipline: the block is
/// its own block, so a bare `$` in it is a `state` of that block and its
/// implicit declaration belongs inside the returned statement list, not hoisted
/// into the enclosing routine.
pub(in crate::parser::primary) fn parse_interpolation_block(block_src: &str) -> Option<Expr> {
    crate::parser::stmt::simple::push_scope();
    let stmts = parse_interpolation_block_stmts(block_src);
    crate::parser::stmt::simple::pop_scope();
    stmts.map(|stmts| Expr::DoStmt(Box::new(crate::ast::Stmt::Block(stmts))))
}

/// [`parse_interpolation_block`]'s body, with the block's scope already pushed.
fn parse_interpolation_block_stmts(block_src: &str) -> Option<Vec<crate::ast::Stmt>> {
    let mut stmts = if let Ok((sr, stmts)) = crate::parser::stmt::stmt_list_pub(block_src)
        && sr.trim().is_empty()
    {
        stmts
    } else if let Ok((expr_rest, expr)) = expression(block_src)
        && expr_rest.trim().is_empty()
    {
        vec![crate::ast::Stmt::Expr(expr)]
    } else {
        return None;
    };
    crate::parser::stmt::simple::prepend_anon_state_decls(&mut stmts);
    Some(stmts)
}

/// [`parse_braced_closure_body`]'s body, run with the block's own lexical scope
/// already pushed so the anonymous-`state` declarations it mints land inside it.
fn parse_braced_closure_body_scoped(inner: &str) -> Option<Expr> {
    if let Ok((leftover, mut stmts)) = crate::parser::stmt::stmt_list_pub(inner)
        && leftover.trim().is_empty()
        && !stmts.is_empty()
    {
        crate::parser::stmt::simple::prepend_anon_state_decls(&mut stmts);
        return Some(if stmts.len() == 1 {
            Expr::DoStmt(Box::new(stmts.into_iter().next().unwrap()))
        } else {
            Expr::DoBlock {
                body: stmts,
                label: None,
            }
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
        return Some(Expr::DoBlock {
            body: stmts,
            label: None,
        });
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
