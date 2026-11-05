//! A hand-built `RakuAST::Doc::Block` written back as Pod source (#14880).
//!
//! `$=pod` is built from Pod source text, so `EVAL` of a tree holding
//! `Doc::Block` statements establishes it by writing those blocks as the text
//! they read from and handing that to the same builder as a source string.
//! Only the plain shapes are written (no configuration, no margin, no rows);
//! any other block leaves `$=pod` as it was, as before.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::ValueView;

/// The Pod source of the `Doc::Block` statements of the `StatementList` or
/// block `node` (top level only); `None` when there are none or one is not
/// a shape written here.
// Cost: O(n), n = size of the blocks.
pub(crate) fn pod_source(node: &RakuAstNode) -> Option<String> {
    if node.class == RakuAstClass::DocBlock {
        return block_source(node);
    }
    if node.class != RakuAstClass::StatementList {
        return None;
    }
    let mut out = String::new();
    for field in &node.fields {
        let RakuAstFieldValue::Node(value) = &field.value else {
            continue;
        };
        let ValueView::RakuAst(child) = value.view() else {
            continue;
        };
        if child.class == RakuAstClass::DocBlock {
            out.push_str(&block_source(child)?);
        }
    }
    (!out.is_empty()).then_some(out)
}

fn str_field(node: &RakuAstNode, name: &str) -> Option<String> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                ValueView::Int(i) => Some(i.to_string()),
                _ => None,
            },
            _ => None,
        })
}

fn field_items<'a>(node: &'a RakuAstNode, name: &str) -> &'a [crate::value::Value] {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => Some(items.as_slice()),
            _ => None,
        })
        .unwrap_or(&[])
}

fn flag(node: &RakuAstNode, name: &str) -> bool {
    node.fields.iter().any(|f| f.name == Some(name))
}

// Cost: O(n), n = size of the block.
fn block_source(node: &RakuAstNode) -> Option<String> {
    let kind = str_field(node, "type")?;
    if kind.is_empty()
        || !kind
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_'))
    {
        return None;
    }
    let name = match str_field(node, "level") {
        Some(level) => format!("{kind}{level}"),
        None => kind.clone(),
    };
    let items: &[crate::value::Value] = node
        .fields
        .iter()
        .find(|f| f.name == Some("paragraphs"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => Some(items.as_slice()),
            _ => None,
        })
        .unwrap_or(&[]);
    let pairs = config_source(node)?;
    let table = kind == "table";
    let mut body = String::new();
    for item in items {
        match item.view() {
            // A table's separators are lines of their own.
            ValueView::Str(s) if table => {
                body.push_str(&s);
                if !s.ends_with('\n') {
                    body.push('\n');
                }
            }
            ValueView::Str(s) => body.push_str(&paragraph(&s)),
            ValueView::RakuAst(inner) => match inner.class {
                RakuAstClass::DocBlock => body.push_str(&block_source(inner)?),
                RakuAstClass::DocLegacyRow if table => body.push_str(&super::doc_str::text(inner)),
                RakuAstClass::DocParagraph => {
                    body.push_str(&paragraph(&super::doc_str::text(inner)));
                }
                _ => return None,
            },
            _ => return None,
        }
    }
    let source = if flag(node, "directive") {
        // `=config head1 :numbered`: the configured type is its only paragraph.
        let [only] = items else { return None };
        let ValueView::Str(target) = only.view() else {
            return None;
        };
        format!("=config {}{pairs}\n", target.as_str())
    } else if flag(node, "abbreviated") {
        // `=head1 text`: the first line follows the name.
        let first = body.trim_end_matches('\n');
        if first.contains("\n\n") || items.len() > 1 || !pairs.is_empty() {
            return None;
        }
        format!("={name} {first}\n\n")
    } else if flag(node, "for") {
        format!("=for {name}{pairs}\n{body}\n")
    } else {
        format!("=begin {name}{pairs}\n{body}=end {kind}\n")
    };
    // Every line carries the block's margin.
    match str_field(node, "margin").filter(|m| !m.is_empty()) {
        Some(margin) => Some(
            source
                .split_inclusive('\n')
                .map(|line| {
                    if line == "\n" {
                        line.to_string()
                    } else {
                        format!("{margin}{line}")
                    }
                })
                .collect(),
        ),
        None => Some(source),
    }
}

/// The ` :name(value)` pairs of the block's configuration as written after
/// its type; `None` for a value not written back (only flags, integers and
/// plain strings are).
// Cost: O(c), c = configuration pairs.
fn config_source(node: &RakuAstNode) -> Option<String> {
    let mut out = String::new();
    let Some(field) = node.fields.iter().find(|f| f.name == Some("config")) else {
        return Some(out);
    };
    // A hand-built block may hold one pair directly, or a list of them.
    let pairs: Vec<crate::value::Value> = match &field.value {
        RakuAstFieldValue::List(items) => items.clone(),
        RakuAstFieldValue::Node(v) => match v.as_list_items() {
            Some(items) => items.to_vec(),
            None => vec![v.clone()],
        },
        RakuAstFieldValue::Adverb(_) => return None,
    };
    for pair in &pairs {
        let (key, value) = match pair.view() {
            ValueView::Pair(key, value) => (key.to_string(), value.clone()),
            ValueView::ValuePair(key, value) => (key.to_string_value(), value.clone()),
            _ => return None,
        };
        let ValueView::RakuAst(mut value) = value.view() else {
            return None;
        };
        // `:n(3)` is a parenthesised statement holding the literal.
        while matches!(
            value.class,
            RakuAstClass::CircumfixParentheses
                | RakuAstClass::SemiList
                | RakuAstClass::StatementExpression
        ) {
            let inner = value.fields.iter().find_map(|f| match &f.value {
                RakuAstFieldValue::Node(v) if !super::origin::is_origin(f) => match v.view() {
                    ValueView::RakuAst(n) => Some(n),
                    _ => None,
                },
                _ => None,
            })?;
            value = inner;
        }
        // A quoted string with only a literal segment is that literal.
        let quoted;
        if value.class == RakuAstClass::QuotedString {
            let [segment] = field_items(value, "segments") else {
                return None;
            };
            let ValueView::RakuAst(literal) = segment.view() else {
                return None;
            };
            if literal.class != RakuAstClass::StrLiteral || flag(value, "processors") {
                return None;
            }
            quoted = literal;
            value = quoted;
        }
        let leaf = value.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if !super::origin::is_origin(f) => Some(v),
            _ => None,
        })?;
        if key.is_empty()
            || !key
                .chars()
                .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_'))
        {
            return None;
        }
        match (value.class, leaf.view()) {
            (RakuAstClass::TermEnum, ValueView::Str(word)) if &**word == "True" => {
                out.push_str(&format!(" :{key}"));
            }
            (RakuAstClass::TermEnum, ValueView::Str(word)) if &**word == "False" => {
                out.push_str(&format!(" :!{key}"));
            }
            (RakuAstClass::IntLiteral, ValueView::Int(n)) => out.push_str(&format!(" :{key}({n})")),
            (RakuAstClass::StrLiteral, ValueView::Str(text))
                if !text.contains(['\'', '\\', '\n']) =>
            {
                out.push_str(&format!(" :{key}('{}')", **text));
            }
            _ => return None,
        }
    }
    Some(out)
}

/// A paragraph ends in a line break and, between paragraphs, a blank line.
// Cost: O(n).
fn paragraph(text: &str) -> String {
    let mut out = text.to_string();
    if !out.ends_with('\n') {
        out.push('\n');
    }
    if !out.ends_with("\n\n") {
        out.push('\n');
    }
    out
}
