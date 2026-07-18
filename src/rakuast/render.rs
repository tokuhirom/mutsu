//! RakuAST node → `.gist`/`.raku`/`.Str` rendering (ADR-0011).
//!
//! Reproduces raku's constructor-form gist exactly: 2-space indent per level,
//! named-arg keys left-padded to the class's alignment width, and
//! `List`-valued fields printed as a parenthesised trailing-comma list. A node
//! renders inline on one line iff every field is a positional leaf literal (no
//! named field, no child node, no list); any named field, child node, or list
//! forces the multi-line form (e.g. `Postfix.new(operator => "++")` is
//! multi-line despite its single leaf, because the field is named).

use super::{Constructor, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

pub(super) fn render_node(node: &RakuAstNode, indent: usize) -> String {
    let name = node.class.printed_name();
    let ctor = match node.class.constructor() {
        Constructor::New => "new",
        Constructor::FromIdentifier => "from-identifier",
    };

    // A class-specific gist quirk: raku's `Assignment` list form omits even the
    // empty parens (`RakuAST::Assignment.new`), unlike the generic `.new()`.
    if node.fields.is_empty() && node.class.empty_parens_omitted() {
        return format!("{name}.{ctor}");
    }

    // Inline when every field is a positional leaf or a colonpair adverb
    // (`Assignment.new(:item)`); any named field, child node, or list forces
    // the multi-line form.
    if node
        .fields
        .iter()
        .all(|f| f.name.is_none() && is_inline_field(f))
    {
        let inner = node
            .fields
            .iter()
            .map(render_inline_field)
            .collect::<Vec<_>>()
            .join(", ");
        return format!("{name}.{ctor}({inner})");
    }

    let width = node.class.named_align_width();
    let mut s = format!("{name}.{ctor}(\n");
    let child_indent = indent + 2;
    let pad = " ".repeat(child_indent);
    for (i, f) in node.fields.iter().enumerate() {
        s.push_str(&pad);
        s.push_str(&render_field_line(f, child_indent, width));
        if i + 1 != node.fields.len() {
            s.push(',');
        }
        s.push('\n');
    }
    s.push_str(&" ".repeat(indent));
    s.push(')');
    s
}

fn is_inline_field(f: &RakuAstField) -> bool {
    match &f.value {
        RakuAstFieldValue::List(_) => false,
        RakuAstFieldValue::Node(v) => !matches!(v.view(), ValueView::RakuAst(_)),
        RakuAstFieldValue::Adverb(_) => true,
    }
}

fn render_inline_field(f: &RakuAstField) -> String {
    let val = match &f.value {
        RakuAstFieldValue::Node(v) => render_leaf(v),
        RakuAstFieldValue::Adverb(name) => return format!(":{name}"),
        // Unreachable while all fields are inline leaves, but keep it total.
        RakuAstFieldValue::List(_) => "()".to_string(),
    };
    match f.name {
        Some(key) => format!("{key} => {val}"),
        None => val,
    }
}

fn render_field_line(f: &RakuAstField, indent: usize, width: usize) -> String {
    let val = render_field_value(&f.value, indent);
    match f.name {
        Some(key) => format!("{key:width$} => {val}"),
        None => val,
    }
}

fn render_field_value(fv: &RakuAstFieldValue, indent: usize) -> String {
    match fv {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(node) => render_node(node, indent),
            _ => render_leaf(v),
        },
        RakuAstFieldValue::List(items) => render_paren_list(items, indent),
        RakuAstFieldValue::Adverb(name) => format!(":{name}"),
    }
}

/// A parenthesised, trailing-comma list — every element gets a trailing comma
/// (so a single element reads `( x, )`).
fn render_paren_list(items: &[Value], indent: usize) -> String {
    let child_indent = indent + 2;
    let pad = " ".repeat(child_indent);
    let mut s = String::from("(\n");
    for item in items {
        s.push_str(&pad);
        match item.view() {
            ValueView::RakuAst(node) => s.push_str(&render_node(node, child_indent)),
            _ => s.push_str(&render_leaf(item)),
        }
        s.push_str(",\n");
    }
    s.push_str(&" ".repeat(indent));
    s.push(')');
    s
}

fn render_leaf(v: &Value) -> String {
    match v.view() {
        ValueView::Str(s) => render_str_literal(&s),
        _ => v.to_string_value(),
    }
}

/// Render a Raku double-quoted string literal, escaping the characters that
/// would otherwise be special inside `"..."`.
fn render_str_literal(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '$' => out.push_str("\\$"),
            '@' => out.push_str("\\@"),
            '%' => out.push_str("\\%"),
            '&' => out.push_str("\\&"),
            _ => out.push(c),
        }
    }
    out.push('"');
    out
}
