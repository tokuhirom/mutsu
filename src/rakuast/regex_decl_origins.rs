//! Origins of a `token` / `rule` / `regex` declaration (ADR-10723 Stage 2;
//! #15780): its name, its signature (`token word($value)`, an empty `rule r()`
//! at its `)`) and the body between its braces, which `regex_origins` lays
//! over the text.

use super::RakuAstNode;
use super::infix_origins::{byte_range, leaf_text};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// Place the body of a `token` / `rule` / `regex` declaration written at its
/// range: the text between the braces after its name.
// Cost: O(n), n = length of the declaration's source.
pub(super) fn declaration(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    // An anonymous `token { ... }` has no name.
    let name = match super::infix_origins::child(node, "name") {
        Some(name) => written_name(name, from, to).or_else(|| leaf_text(name)),
        None => Some(String::new()),
    };
    let Some(name) = name else {
        return;
    };
    let body = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let word = ["token", "rule", "regex"]
            .into_iter()
            .find_map(|k| text.find(k).map(|at| at + k.len()))?;
        let rest = &text[word..];
        let name_at = from + word + (rest.len() - rest.trim_start().len());
        let after_name = rest.trim_start().strip_prefix(name.as_str())?;
        if !name.is_empty()
            && after_name.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
        {
            return None;
        }
        // `token word($value) { ... }`: the parameters between parentheses.
        let mut after_signature = after_name;
        let mut signature = None;
        // `token word ($e)`: blanks may come before the parameters.
        if let Some(inner) = after_name.trim_start().strip_prefix('(') {
            let close = closing_paren(inner)?;
            let at = to - text.len() + (text.len() - inner.len());
            signature = Some((SrcSpan::new(at, at + close), inner[..close].to_string()));
            after_signature = &inner[close + 1..];
        }
        let brace = after_signature.trim_start();
        if !brace.starts_with('{') || !text.ends_with('}') {
            return None;
        }
        Some((
            SrcSpan::new(name_at, name_at + name.len()),
            signature,
            SrcSpan::new(to - brace.len() + 1, to - 1),
        ))
    });
    if let Some((name_at, signature, body)) = body {
        super::infix_origins::with_child(node, "name", |n| {
            super::origin::set_span(n, name_at);
            super::name_quote_origins::place(n, name_at);
        });
        if let Some((inner, written)) = signature {
            if written.trim().is_empty() {
                // `rule r() { }`: the empty signature sits at its `)`.
                let at = SrcSpan::new(inner.to as usize, inner.to as usize);
                super::empty_fields::set_span(node, "signature", at);
                super::infix_origins::with_child(node, "signature", |sig| {
                    super::origin::set_span(sig, at)
                });
            }
            super::infix_origins::with_child(node, "signature", |sig| {
                place_signature(sig, inner, &written)
            });
        }
        super::infix_origins::with_child(node, "body", |regex| {
            // `proto token a {*}`: Rakudo's `OnlyStar` is empty, at the `}`.
            if regex.class == super::RakuAstClass::OnlyStar {
                let star = super::unit_source::with_text(body, |t| (t.trim() == "*").then_some(()));
                if star.is_some() && regex.origin.0.is_none() {
                    let at = body.to as usize;
                    super::origin::set_span(regex, SrcSpan::new(at, at));
                }
                return;
            }
            super::regex_origins::walk_body(names, regex, body)
        });
    }
}

/// `token a:sym<foo>`: the name as written, when its `Name` carries a
/// colonpair, read from the source after the declarator up to the blank,
/// `(` or `{` that ends it.
// Cost: O(n), n = length of the declaration's head.
fn written_name(name: &RakuAstNode, from: usize, to: usize) -> Option<String> {
    name.fields.iter().find(|f| f.name == Some("colonpairs"))?;
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let word = ["token", "rule", "regex"]
            .into_iter()
            .find_map(|k| text.find(k).map(|at| at + k.len()))?;
        let rest = text[word..].trim_start();
        let end = rest.find(|c: char| c.is_whitespace() || c == '(' || c == '{')?;
        rest[..end].contains(':').then(|| rest[..end].to_string())
    })
}

/// Place a regex declaration's `signature`, whose parameters are written
/// `written` at `inner` (the text between its parentheses).
// Cost: O(n), n = length of the signature.
fn place_signature(signature: &mut RakuAstNode, inner: SrcSpan, written: &str) {
    let Some(parameters) = super::signature_decl_origins::parameters(signature) else {
        return;
    };
    if parameters.is_empty() {
        return;
    }
    let lead = written.len() - written.trim_start().len();
    let Some((spans, at)) = super::signature_decl_origins::scan(
        &written[lead..],
        &parameters,
        inner.from as usize + lead,
    ) else {
        return;
    };
    if !written[lead + at..].trim().is_empty() {
        return;
    }
    let (Some(first), Some(last)) = (spans.first(), spans.last()) else {
        return;
    };
    super::origin::set_span(
        signature,
        SrcSpan::new(first.param.from as usize, last.param.to as usize),
    );
    super::signature_decl_origins::place(signature, &spans);
}

/// The offset of the `)` that closes a signature whose text after its `(` is
/// `inner`; `None` when a quote or comment comes first.
// Cost: O(n), n = length of the signature.
fn closing_paren(inner: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (at, c) in inner.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ']' | '}' => depth = depth.checked_sub(1)?,
            ')' if depth == 0 => return Some(at),
            ')' => depth -= 1,
            '"' | '\'' | '#' | '\\' => return None,
            _ => {}
        }
    }
    None
}
