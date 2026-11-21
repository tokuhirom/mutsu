//! Origins of the blocks of expression terms (ADR-10723 Stage 2, expression
//! families): a bare `{ ... }`, the block of `do` / `start` / `gather` /
//! `try` and the other statement prefixes, an anonymous `sub { ... }` and a
//! pointy `-> $x { ... }`.
//!
//! The parser notes the term, not the block inside it, so the block is placed
//! from the term's range: a block runs from its `{` to the end of the term
//! that holds it, and the `{` is read back from the source text. Every range
//! is checked against that text, so a block it cannot confirm stays without
//! one: exact or absent.

use super::infix_origins::{byte_range, with_child, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the block parts of `node`, a node just converted.
// Cost: O(n + s), n = length of the block's source, s = its statements.
pub(super) fn derive(node: &mut RakuAstNode) {
    match node.class {
        RakuAstClass::Block => bare_block(node),
        RakuAstClass::Sub | RakuAstClass::Method | RakuAstClass::Submethod => anonymous_sub(node),
        RakuAstClass::PointyBlock => pointy_block(node),
        class if is_prefix(class) => prefix_block(node),
        _ => {}
    }
}

/// `whenever TRIGGER BLOCK`: the block runs from the first thing after the
/// trigger to the end of the statement.
// Cost: O(n + s), n = length of the statement's source, s = its statements.
pub(super) fn whenever(statement: &mut RakuAstNode, span: SrcSpan) {
    let Some((_, trigger_end)) =
        super::infix_origins::child(statement, "trigger").and_then(byte_range)
    else {
        return;
    };
    let to = span.to as usize;
    let at = super::unit_source::with_text(SrcSpan::new(trigger_end, to), |text| {
        let body = text.trim_start();
        ((body.starts_with("->") || body.starts_with('{')) && body.ends_with('}'))
            .then(|| to - body.len())
    });
    let Some(at) = at else {
        return;
    };
    with_child(statement, "body", |block| {
        super::origin::set_span(block, SrcSpan::new(at, to));
        derive(block);
    });
}

// Cost: O(1).
fn is_prefix(class: RakuAstClass) -> bool {
    use RakuAstClass as C;
    matches!(
        class,
        C::StatementPrefixDo
            | C::StatementPrefixTry
            | C::StatementPrefixGather
            | C::StatementPrefixStart
            | C::StatementPrefixQuietly
            | C::StatementPrefixSink
            | C::StatementPrefixLazy
            | C::StatementPrefixHyper
            | C::StatementPrefixRace
            | C::StatementPrefixEager
            | C::StatementPrefixOnce
            | C::StatementPrefixReact
            | C::StatementPrefixSupply
    )
}

/// `{ ... }`: the `Blockoid` is the block's range.
// Cost: O(n + s), n = length of the block's source, s = its statements.
fn bare_block(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    blockoid(node, from, to);
}

/// `try EXPR` / `do EXPR`: the prefix holds the expression as a
/// `Statement::Expression`, which covers exactly that expression.
// Cost: O(f), f = fields of the nodes on the path.
fn statement_operand(node: &mut RakuAstNode) {
    with_child_at(node, 0, |statement| {
        if statement.class != RakuAstClass::StatementExpression || statement.origin.0.is_some() {
            return;
        }
        let operand = statement.fields.first().and_then(|f| match &f.value {
            super::RakuAstFieldValue::Node(v) => match v.view() {
                crate::value::ValueView::RakuAst(inner) => inner.origin.0.clone(),
                _ => None,
            },
            _ => None,
        });
        statement.origin.0 = operand;
    });
}

/// `do { ... }` and the other prefixes: the block is the braces after the
/// keyword, to the end of the term.
// Cost: O(n + s), n = length of the term's source, s = its statements.
fn prefix_block(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(at) = brace_after_word(from, to) else {
        statement_operand(node);
        return;
    };
    let whole = SrcSpan::new(at, to);
    with_child_at(node, 0, |block| {
        if block.class == RakuAstClass::Block {
            super::origin::set_span(block, whole);
            blockoid(block, at, to);
        }
    });
}

/// `sub { ... }`: the `Blockoid` is the braces after the keyword.
// Cost: O(n + s), n = length of the term's source, s = its statements.
fn anonymous_sub(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    if super::infix_origins::child(node, "name").is_some() {
        return;
    }
    let Some(at) = brace_after_word(from, to).or_else(|| brace_after_signature(from, to)) else {
        return;
    };
    blockoid(node, at, to);
}

/// The offset of the `{` after the parenthesized signature that follows the
/// first word of `from..to` (`sub ($a) { ... }`), when the term ends with
/// the `}` closing it.
// Cost: O(n), n = length of the signature.
fn brace_after_signature(from: usize, to: usize) -> Option<usize> {
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let word = text.find(|c: char| !(c.is_alphanumeric() || c == '-' || c == '_'))?;
        let signature = text[word..].trim_start().strip_prefix('(')?;
        let close = super::subst_origins::closing(signature, '(', ')')?;
        let rest = signature[close + 1..].trim_start();
        (rest.starts_with('{') && rest.ends_with('}')).then_some(to - rest.len())
    })
}

/// `-> $x, Int $y { ... }`: the `Signature` is the parameters between the
/// arrow and the braces, the `Blockoid` the braces to the end of the term.
// Cost: O(n + s), n = length of the term's source, s = its statements.
fn pointy_block(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let parameters = match super::infix_origins::child(node, "signature") {
        Some(signature) if signature.fields.is_empty() => Some(Vec::new()),
        Some(signature) => super::signature_decl_origins::parameters(signature),
        None => Some(Vec::new()),
    };
    let Some(parameters) = parameters else {
        return;
    };
    if parameters.is_empty() {
        // `-> --> Int { ... }`: the `Signature` is the return part.
        let returned = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
            let rest = text.strip_prefix("->")?;
            let lead = rest.len() - rest.trim_start().len();
            returns_before_brace(&rest[lead..], from + 2 + lead)
        });
        if let Some((arrow, ret, brace)) = returned {
            with_child(node, "signature", |signature| {
                super::origin::set_span(signature, SrcSpan::new(arrow, ret.to as usize));
                with_child(signature, "returns", |ty| {
                    super::routine_origins::type_origin(ty, ret)
                });
            });
            blockoid(node, brace, to);
            return;
        }
        // `-> { ... }`: the empty `Signature` sits right after the arrow.
        let brace = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
            let rest = text.strip_prefix("->")?;
            let body = rest.trim_start();
            body.starts_with('{')
                .then(|| from + 2 + (rest.len() - body.len()))
        });
        if let Some(brace) = brace {
            super::empty_fields::set_span(node, "signature", SrcSpan::new(from + 2, from + 2));
            with_child(node, "signature", |signature| {
                super::origin::set_span(signature, SrcSpan::new(from + 2, from + 2))
            });
            blockoid(node, brace, to);
        }
        return;
    }
    let placed = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text.strip_prefix("->")?;
        let base = from + 2;
        let (spans, at) = super::signature_decl_origins::scan(rest, &parameters, base)?;
        if rest[at..].starts_with('{') {
            return Some((spans, None, base + at));
        }
        // `-> $x --> Int { ... }`: a return part between the parameters and
        // the braces.
        let lead = rest[at..].len() - rest[at..].trim_start().len();
        let (_, ret, brace) = returns_before_brace(&rest[at + lead..], base + at + lead)?;
        Some((spans, Some(ret), brace))
    });
    let Some((spans, returns, brace)) = placed else {
        return;
    };
    let whole = SrcSpan::new(
        spans.first().map_or(0, |s| s.param.from as usize),
        returns.map_or_else(
            || spans.last().map_or(0, |s| s.param.to as usize),
            |ret| ret.to as usize,
        ),
    );
    with_child(node, "signature", |signature| {
        super::origin::set_span(signature, whole);
        super::signature_decl_origins::place(signature, &spans);
        if let Some(ret) = returns {
            with_child(signature, "returns", |ty| {
                super::routine_origins::type_origin(ty, ret)
            });
        }
    });
    blockoid(node, brace, to);
}

/// A pointy block's `--> RETURN {`: `text` starts at the `-->`, written at
/// byte `base`. The arrow's offset, the return part's range (blanks around it
/// excluded) and the offset of the `{` after it; `None` unless the return part
/// is a plain word, literal or type application.
// Cost: O(n), n = length of the return part.
fn returns_before_brace(text: &str, base: usize) -> Option<(usize, SrcSpan, usize)> {
    let after = text.strip_prefix("-->")?;
    let lead = after.len() - after.trim_start().len();
    let mut depth = 0usize;
    let brace = after
        .char_indices()
        .skip(lead)
        .find_map(|(at, c)| match c {
            '[' => {
                depth += 1;
                None
            }
            ']' => {
                depth = depth.saturating_sub(1);
                None
            }
            '{' if depth == 0 => Some(Some(at)),
            '(' | ')' | '"' | '\'' | '<' | '#' | '}' => Some(None),
            _ => None,
        })??;
    let written = after[lead..brace].trim_end();
    if written.is_empty() {
        return None;
    }
    let start = base + 3 + lead;
    Some((
        base,
        SrcSpan::new(start, start + written.len()),
        base + 3 + brace,
    ))
}

/// The offset of the `{` that follows the first word of `from..to`, when the
/// term ends with the `}` closing it.
// Cost: O(1) plus the word's length.
fn brace_after_word(from: usize, to: usize) -> Option<usize> {
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let word = text.find(|c: char| !(c.is_alphanumeric() || c == '-' || c == '_'))?;
        if word == 0 {
            return None;
        }
        let rest = text[word..].trim_start();
        (rest.starts_with('{') && rest.ends_with('}')).then_some(to - rest.len())
    })
}

/// Give `node`'s `body` (`Blockoid`) the range `at..to` and its
/// `StatementList` the range of the statements inside the braces
/// ([`super::list_origins::statement_list`]).
// Cost: O(n + s), n = length of the block's source, s = its statements.
fn blockoid(node: &mut RakuAstNode, at: usize, to: usize) {
    let braced = super::unit_source::with_text(SrcSpan::new(at, to), |text| {
        (text.starts_with('{') && text.ends_with('}') && text.len() >= 2).then_some(())
    });
    if braced.is_none() {
        return;
    }
    with_child(node, "body", |blockoid| {
        super::origin::set_span(blockoid, SrcSpan::new(at, to));
        with_child_at(blockoid, 0, |list| {
            super::list_origins::statement_list(list, SrcSpan::new(at + 1, to - 1))
        });
    });
}

/// `LEAVE say $x;` / `END pass "x";`: a phaser written over a statement, not
/// a block. Its `Statement::Expression` and the expression in it cover the
/// text after the keyword, to the end of the phaser.
// Cost: O(n), n = length of the phaser's source.
pub(super) fn phaser_statement(names: &crate::scope::NameScope, node: &mut RakuAstNode) {
    use RakuAstClass as C;
    if !matches!(
        node.class,
        C::StatementPrefixPhaserBegin
            | C::StatementPrefixPhaserCheck
            | C::StatementPrefixPhaserInit
            | C::StatementPrefixPhaserEnd
            | C::StatementPrefixPhaserEnter
            | C::StatementPrefixPhaserLeave
            | C::StatementPrefixPhaserKeep
            | C::StatementPrefixPhaserUndo
            | C::StatementPrefixPhaserFirst
            | C::StatementPrefixPhaserNext
            | C::StatementPrefixPhaserLast
            | C::StatementPrefixPhaserPre
            | C::StatementPrefixPhaserPost
            | C::StatementPrefixPhaserQuit
            | C::StatementPrefixPhaserClose
    ) {
        return;
    }
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(span) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let keyword = text.find(char::is_whitespace)?;
        if !text[..keyword].chars().all(|c| c.is_ascii_uppercase()) {
            return None;
        }
        let body = text[keyword..].trim_start();
        (!body.is_empty() && !body.starts_with('{')).then(|| SrcSpan::new(to - body.len(), to))
    }) else {
        return;
    };
    with_child_at(node, 0, |statement| {
        if statement.class != RakuAstClass::StatementExpression || statement.origin.0.is_some() {
            return;
        }
        super::origin::set_span(statement, span);
        with_child_at(statement, 0, |expression| {
            if expression.origin.0.is_none() {
                super::origin::set_span(expression, span);
                super::origin::named_term(expression, span);
                super::origin::quoted_text(expression, span);
                super::call_origins::postfix_call(expression, span);
                super::infix_origins::derive(names, expression);
            }
        });
    });
}
