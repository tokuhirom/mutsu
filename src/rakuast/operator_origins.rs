//! Origins of prefix, postfix and ternary operators, contextualizers and
//! pairs (ADR-10723 Stage 2, expression families).
//!
//! Like `infix_origins`, each range is derived from origins the node or its
//! children already carry and checked against the source text it claims:
//! the operator, the `??` / `!!`, the sigil and parentheses.

use super::infix_origins::{byte_range, child, child_at, leaf_text, with_child, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::ValueView;

/// Place the operator / parts of `node` (an `ApplyPrefix`, `ApplyPostfix`,
/// `Ternary`, `Contextualizer::*` or `ColonPair::Value`).
// Cost: O(f + n), f = fields of `node`, n = length of the source text read.
pub(super) fn derive(node: &mut RakuAstNode) {
    match node.class {
        RakuAstClass::ApplyPrefix => prefix(node),
        RakuAstClass::ApplyPostfix => postfix(node),
        RakuAstClass::Ternary => ternary(node),
        RakuAstClass::ContextualizerItem
        | RakuAstClass::ContextualizerList
        | RakuAstClass::ContextualizerHash => contextualizer(node),
        RakuAstClass::ColonPairValue => colonpair_value(node),
        RakuAstClass::ColonPairVariable | RakuAstClass::ColonPairNumber => colonpair_term(node),
        RakuAstClass::FatArrow => fat_arrow(node),
        _ => {}
    }
}

/// `-$x`: the `Prefix` is the operator the application starts with.
// Cost: O(o), o = length of the operator.
fn prefix(node: &mut RakuAstNode) {
    word_prefix(node);
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    // `-<< @x` / `|« @x`: a `MetaPrefix::Hyper` over the operator and its
    // `<<`, holding the `Prefix`.
    if let Some(hyper) = child(node, "prefix").filter(|p| p.class == RakuAstClass::MetaPrefixHyper)
    {
        let Some(op) = child_at(hyper, 0)
            .filter(|p| p.class == RakuAstClass::Prefix)
            .and_then(leaf_text)
        else {
            return;
        };
        let marker =
            super::unit_source::with_text(SrcSpan::new(from, from + op.len() + 3), |text| {
                let rest = text.strip_prefix(op.as_str())?;
                ["<<", "\u{ab}"]
                    .into_iter()
                    .find(|m| rest.starts_with(m))
                    .map(str::len)
            });
        if let Some(marker) = marker {
            with_child(node, "prefix", |hyper| {
                super::origin::set_span(hyper, SrcSpan::new(from, from + op.len() + marker));
                with_child_at(hyper, 0, |p| {
                    super::origin::set_span(p, SrcSpan::new(from, from + op.len()))
                });
            });
        }
        return;
    }
    let Some(op) = child(node, "prefix")
        .filter(|p| p.class == RakuAstClass::Prefix)
        .and_then(leaf_text)
    else {
        return;
    };
    let written = super::unit_source::with_text(SrcSpan::new(from, from + op.len()), |text| {
        (text == op).then_some(())
    });
    if written.is_some() {
        with_child(node, "prefix", |p| {
            super::origin::set_span(p, SrcSpan::new(from, from + op.len()))
        });
        if let Some(index) = node.fields.iter().position(|f| f.name == Some("operand")) {
            glued_variable_operand(node, index, from + op.len());
        }
    }
}

/// `⚛$n` / `$@a`: a variable operand the parser kept no range for, the
/// child at `index` of `node`, written right after the operator or sigil at
/// `at` up to `node`'s end.
// Cost: O(k), k = length of the variable's name.
fn glued_variable_operand(node: &mut RakuAstNode, index: usize, at: usize) {
    let Some((_, to)) = byte_range(node) else {
        return;
    };
    let Some(name) = child_at(node, index)
        .filter(|v| v.class == RakuAstClass::VarLexical && v.origin.0.is_none())
        .and_then(leaf_text)
    else {
        return;
    };
    let exact =
        super::unit_source::with_text(SrcSpan::new(at, to), |text| (text == name).then_some(()));
    if exact.is_some() {
        with_child_at(node, index, |v| {
            super::origin::set_span(v, SrcSpan::new(at, to))
        });
    }
}

/// `so 1` / `not $x`: a word prefix the parser left unplaced runs from the
/// word, written before its operand after blanks, to the operand's end.
// Cost: O(w), w = the blanks between the word and the operand.
fn word_prefix(node: &mut RakuAstNode) {
    if node.origin.0.is_some() {
        return;
    }
    let Some(op) = child(node, "prefix")
        .filter(|p| p.class == RakuAstClass::Prefix)
        .and_then(leaf_text)
        .filter(|op| op.chars().all(char::is_alphabetic))
    else {
        return;
    };
    let Some((operand_from, operand_to)) = child(node, "operand").and_then(byte_range) else {
        return;
    };
    // The longest run of blanks tried between the word and the operand.
    const BLANKS: usize = 16;
    let start = (1..=BLANKS).find_map(|blanks| {
        let start = operand_from.checked_sub(op.len() + blanks)?;
        let before = start.checked_sub(1);
        super::unit_source::with_text(SrcSpan::new(start, operand_from), |text| {
            let rest = text.strip_prefix(op.as_str())?;
            rest.chars().all(char::is_whitespace).then_some(())
        })?;
        // Not the tail of a longer word.
        let boundary = before.is_none_or(|b| {
            super::unit_source::with_text(SrcSpan::new(b, start), |c| {
                (!c.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-'))
                    .then_some(())
            })
            .is_some()
        });
        boundary.then_some(start)
    });
    if let Some(start) = start {
        super::origin::set_span(node, SrcSpan::new(start, operand_to));
    }
}

/// `$x++`: the `Postfix` is the operator written after the operand.
// Cost: O(n), n = length of the application's source.
fn postfix(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let operand = child(node, "operand");
    let operand_end = match operand.and_then(byte_range) {
        Some((_, end)) => Some(end),
        // The curried `*` of `*++` has no origin of its own (rakudo gives
        // none); the operator follows it.
        None if operand.is_some_and(|o| {
            matches!(
                o.class,
                RakuAstClass::TermWhatever | RakuAstClass::WhateverCodeArgument
            )
        }) =>
        {
            super::unit_source::with_text(SrcSpan::new(from, to), |text| {
                text.starts_with('*').then_some(from + 1)
            })
        }
        // `0.0i` / `3e2i`: a numeric literal written right before the
        // operator, which the parser read as one token.
        None if operand.is_some_and(|o| {
            matches!(
                o.class,
                RakuAstClass::IntLiteral | RakuAstClass::RatLiteral | RakuAstClass::NumLiteral
            )
        }) =>
        {
            let op = child(node, "postfix")
                .filter(|p| p.class == RakuAstClass::Postfix)
                .and_then(leaf_text);
            let end = op.and_then(|op| {
                super::unit_source::with_text(SrcSpan::new(from, to), |text| {
                    let number = text.strip_suffix(op.as_str())?;
                    // `.5i`: a rational may start at its point.
                    let digits = number.strip_prefix('.').unwrap_or(number);
                    digits
                        .starts_with(|c: char| c.is_ascii_digit())
                        .then_some(from + number.len())
                })
            });
            if let Some(end) = end {
                with_child(node, "operand", |o| {
                    super::origin::set_span(o, SrcSpan::new(from, end))
                });
            }
            end
        }
        None => None,
    };
    let Some(operand_end) = operand_end else {
        return;
    };
    let Some(op) = child(node, "postfix")
        .filter(|p| p.class == RakuAstClass::Postfix)
        .and_then(leaf_text)
    else {
        return;
    };
    if operand_end > to {
        return;
    }
    let at = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
        // Parentheses around the operand (`($i)++`) are not a node of
        // their own, so the operand ends before their `)`.
        let after = text.trim_start_matches(|c: char| c == ')' || c.is_whitespace());
        (after == op).then_some(operand_end + (text.len() - after.len()))
    });
    if let Some(at) = at {
        with_child(node, "postfix", |p| {
            super::origin::set_span(p, SrcSpan::new(at, at + op.len()))
        });
    }
}

/// `c ?? a !! b`: the ternary spans its three operands when `??` and `!!`
/// are what separates them.
// Cost: O(n), n = length of the ternary's source.
fn ternary(node: &mut RakuAstNode) {
    if node.origin.0.is_some() {
        return;
    }
    let parts = ["condition", "then", "else"].map(|name| child(node, name).and_then(byte_range));
    let [Some(condition), Some(then), Some(otherwise)] = parts else {
        return;
    };
    let separated = |(from, to): (usize, usize), op: &str| {
        from <= to
            && super::unit_source::with_text(SrcSpan::new(from, to), |gap| {
                (gap.trim() == op).then_some(())
            })
            .is_some()
    };
    if separated((condition.1, then.0), "??") && separated((then.1, otherwise.0), "!!") {
        super::origin::set_span(node, SrcSpan::new(condition.0, otherwise.1));
    }
}

/// `$( ... )` / `@( ... )` / `%( ... )`: the `StatementSequence` is the text
/// inside the parentheses after the sigil, and a nested contextualizer
/// (`$@( ... )`) starts after the outer sigil.
// Cost: O(n + d), n = length of the source, d = nesting of contextualizers.
fn contextualizer(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let sigil = match node.class {
        RakuAstClass::ContextualizerItem => '$',
        RakuAstClass::ContextualizerList => '@',
        _ => '%',
    };
    let starts = super::unit_source::with_text(SrcSpan::new(from, from + 1), |text| {
        text.starts_with(sigil).then_some(())
    });
    if starts.is_none() {
        return;
    }
    let Some(inner) = child_at(node, 0) else {
        return;
    };
    if matches!(
        inner.class,
        RakuAstClass::ContextualizerItem
            | RakuAstClass::ContextualizerList
            | RakuAstClass::ContextualizerHash
    ) {
        with_child_at(node, 0, |inner| {
            super::origin::set_span(inner, SrcSpan::new(from + 1, to));
            contextualizer(inner);
        });
        return;
    }
    // `$@a` / `%$hash`: a variable written right after the sigil.
    if inner.class == RakuAstClass::VarLexical && inner.origin.0.is_none() {
        glued_variable_operand(node, 0, from + 1);
        return;
    }
    if let Some(inside) = super::list_origins::bracketed(from + 1, to, ('(', ')')) {
        with_child_at(node, 0, |list| {
            super::list_origins::statement_list(list, inside)
        });
    }
}

/// `:key(...)`: the parenthesized value runs from the `(` after the key to
/// the end of the pair.
// Cost: O(n + s), n = length of the pair's source, s = statements in it.
fn colonpair_value(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(key) = node.fields.iter().find_map(|f| match &f.value {
        super::RakuAstFieldValue::Node(v) if f.name == Some("key") => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    }) else {
        return;
    };
    let Some(value) = child(node, "value") else {
        return;
    };
    let open = from + 1 + key.len();
    // `:key<a b>`: the value is the word quote after the key.
    if value.class == RakuAstClass::QuotedString && value.origin.0.is_none() {
        let written = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
            text.strip_prefix(':')?
                .strip_prefix(key.as_str())?
                .starts_with(['<', '«'])
                .then_some(())
        });
        if written.is_some() && open < to {
            with_child(node, "value", |value| {
                super::origin::set_span(value, SrcSpan::new(open, to));
                super::origin::quoted_text(value, SrcSpan::new(open, to));
            });
        }
        return;
    }
    // `:key{...}`: the block or hash composer is the value, from its `{`.
    if matches!(
        value.class,
        RakuAstClass::Block | RakuAstClass::CircumfixHashComposer
    ) && value.origin.0.is_none()
    {
        let written = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
            let rest = text.strip_prefix(':')?.strip_prefix(key.as_str())?;
            (rest.starts_with('{') && rest.ends_with('}')).then_some(())
        });
        if written.is_some() && open < to {
            with_child(node, "value", |value| {
                super::origin::set_span(value, SrcSpan::new(open, to));
                super::expr_block_origins::derive(value);
            });
        }
        return;
    }
    // `:key(...)` / `:key[...]`: the parentheses or brackets after the key.
    let (open_bracket, close_bracket) = match value.class {
        RakuAstClass::CircumfixParentheses => ('(', ')'),
        RakuAstClass::CircumfixArrayComposer => ('[', ']'),
        _ => return,
    };
    if value.origin.0.is_some() {
        return;
    }
    let written = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text.strip_prefix(':')?.strip_prefix(key.as_str())?;
        (rest.starts_with(open_bracket) && rest.ends_with(close_bracket)).then_some(())
    });
    if written.is_some() && open < to {
        with_child(node, "value", |value| {
            super::origin::set_span(value, SrcSpan::new(open, to));
            super::list_origins::circumfix(value);
        });
    }
}

/// `:$x` / `:5h`: the value is the text after the colon (the variable with
/// its sigil, the number without its trailing key).
// Cost: O(n), n = length of the pair's source.
fn colonpair_term(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(value) = child(node, "value") else {
        return;
    };
    if value.origin.0.is_some() {
        return;
    }
    let end = if node.class == RakuAstClass::ColonPairNumber {
        let Some(len) = super::unit_source::with_text(SrcSpan::new(from + 1, to), |text| {
            let digits = text
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(text.len());
            (digits > 0).then_some(digits)
        }) else {
            return;
        };
        from + 1 + len
    } else {
        to
    };
    if from + 1 < end {
        with_child(node, "value", |value| {
            super::origin::set_span(value, SrcSpan::new(from + 1, end))
        });
    }
}

/// `key => value`: the pair runs from its key to the end of its value, when
/// the key and `=>` are written right before the value.
// Cost: O(n), n = length of the source before the value.
fn fat_arrow(node: &mut RakuAstNode) {
    if node.origin.0.is_some() {
        return;
    }
    let Some(key) = node.fields.iter().find_map(|f| match &f.value {
        super::RakuAstFieldValue::Node(v) if f.name == Some("key") => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    }) else {
        return;
    };
    let Some((value_from, value_to)) = child(node, "value").and_then(byte_range) else {
        return;
    };
    // The key, `=>` and the whitespace around it, at most a line's worth.
    let window = value_from.saturating_sub(key.len() + 256);
    let from = super::unit_source::with_text(SrcSpan::new(window, value_from), |before| {
        let head = before.trim_end().strip_suffix("=>")?.trim_end();
        let rest = head.strip_suffix(key.as_str())?;
        let boundary = !rest.ends_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-'));
        boundary.then_some(window + rest.len())
    });
    if let Some(from) = from {
        super::origin::set_span(node, SrcSpan::new(from, value_to));
    }
}
