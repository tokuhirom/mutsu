//! `RakuAST::Doc::Block.from-paragraphs(:@paragraphs, ...)`: rakudo 2026.11's
//! constructor in `src/core.c/RakuAST/Fixups.rakumod`. The block is built by
//! `new` from the other arguments; each string paragraph loses the block's
//! margin (`!marginalize`) and is then interpreted by the block's type:
//! verbatim (`comment`, `data`, `ignore`), formatting codes only (`code`,
//! `input`, `output` and any other type), a table (`table`, `numtable`), or
//! text with implicit code blocks (`defn` after its lemma, `item`, `pod`,
//! ...). Nodes among the paragraphs are kept as they are (#18981).

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// The types whose paragraphs hold implicit code blocks.
const IMPLICIT: &[&str] = &[
    "cell", "defn", "item", "nested", "pod", "rakudoc", "section",
];

/// `Doc::Block.from-paragraphs(:@paragraphs, *%_)`.
// Cost: O(a + n), a = arguments, n = size of the paragraphs.
pub(crate) fn from_paragraphs(args: &[Value]) -> Result<Value, RuntimeError> {
    let mut raw = Vec::new();
    let mut rest = Vec::with_capacity(args.len());
    for arg in args {
        match arg.view() {
            ValueView::Pair(key, value) if key == "paragraphs" => {
                raw = items(value);
            }
            ValueView::ValuePair(key, value) if key.to_string_value() == "paragraphs" => {
                raw = items(value);
            }
            _ => rest.push(arg.clone()),
        }
    }
    let block =
        super::named_ctor::construct("RakuAST::Doc::Block", &rest).unwrap_or_else(|| {
            Err(RuntimeError::new(
                "RakuAST::Doc::Block.new is not available",
            ))
        })?;
    let ValueView::RakuAst(node) = block.view() else {
        return Ok(block);
    };
    let mut node = (*node).clone();
    let paragraphs = marginalize(&str_field(&node, "margin"), raw)?;
    let kind = str_field(&node, "type");
    let mut out = Vec::new();
    match kind.as_str() {
        "comment" | "data" | "ignore" => out = paragraphs,
        "table" | "numtable" if paragraphs.first().is_some_and(is_str) => {
            table(&mut node, &paragraphs, &mut out);
        }
        "table" | "numtable" => out = paragraphs,
        "defn" | "numdefn" => {
            let mut parts = paragraphs.into_iter();
            if let Some(first) = parts.next() {
                let mut rest_parts = Vec::new();
                match first.as_str() {
                    Some(text) => {
                        let (lemma, body) = match text.split_once('\n') {
                            Some((lemma, body)) => (lemma.to_string(), Some(body.to_string())),
                            None => (text.to_string(), None),
                        };
                        out.push(Value::str(lemma));
                        if let Some(body) = body {
                            rest_parts.push(Value::str(body));
                        }
                    }
                    None => out.push(first),
                }
                rest_parts.extend(parts);
                implicit_code_blocks(&rest_parts, &mut out)?;
            }
        }
        k if IMPLICIT.contains(&k) => implicit_code_blocks(&paragraphs, &mut out)?,
        _ => {
            out = paragraphs
                .into_iter()
                .map(|p| from_string_value(&p))
                .collect::<Result<_, _>>()?
        }
    }
    for paragraph in out {
        super::doc_block_edit::add_paragraph(&mut node, &paragraph)?;
    }
    Ok(Value::rakuast(Box::new(node)))
}

/// The items of a list argument, or the argument itself.
// Cost: O(n), n = items.
fn items(value: &Value) -> Vec<Value> {
    value.as_list_items().map_or_else(
        || vec![value.clone()],
        |items| {
            items
                .iter()
                .map(|i| i.with_deref(|v| v.descalarize().clone()))
                .collect()
        },
    )
}

// Cost: O(1).
fn is_str(value: &Value) -> bool {
    value.as_str().is_some()
}

/// The text of the leaf field `name`, empty when absent.
// Cost: O(f), f = fields of the node.
fn str_field(node: &RakuAstNode, name: &str) -> String {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => v.as_str().map(str::to_string),
            _ => None,
        })
        .unwrap_or_default()
}

/// Rakudo's `Str.leading-whitespace`.
// Cost: O(w), w = the leading whitespace.
fn leading_ws(line: &str) -> &str {
    let end = line
        .char_indices()
        .find(|(_, c)| !c.is_whitespace())
        .map_or(line.len(), |(i, _)| i);
    &line[..end]
}

/// `.substr($n)`: `line` without its first `n` characters.
// Cost: O(n).
fn after_chars(line: &str, n: usize) -> &str {
    line.char_indices().nth(n).map_or("", |(i, _)| &line[i..])
}

/// `!marginalize(@raw)`: each line of a string paragraph loses `margin`'s
/// width of leading whitespace; a blank line becomes `"\n"`; a line with too
/// little whitespace dies, as rakudo does.
// Cost: O(n), n = size of the paragraphs.
fn marginalize(margin: &str, raw: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let width = margin.chars().count();
    if width == 0 {
        return Ok(raw);
    }
    raw.into_iter()
        .map(|paragraph| {
            if matches!(paragraph.view(), ValueView::RakuAst(_)) {
                return Ok(paragraph);
            }
            let text = paragraph.to_string_value();
            let mut out = String::with_capacity(text.len());
            for line in text.split_inclusive('\n') {
                if leading_ws(line).chars().count() >= width {
                    out.push_str(after_chars(line, width));
                } else if line.chars().all(char::is_whitespace) {
                    out.push('\n');
                } else {
                    return Err(RuntimeError::new(format!(
                        "'{}'\n    does not have enough whitespace to allow for a margin of {width} positions",
                        line.strip_suffix('\n').unwrap_or(line)
                    )));
                }
            }
            Ok(Value::str(out))
        })
        .collect()
}

/// `Paragraph.from-string($text)` of a string paragraph; a node stays.
// Cost: O(n), n = length of the text.
fn from_string_value(paragraph: &Value) -> Result<Value, RuntimeError> {
    match paragraph.as_str() {
        Some(text) => super::doc_edit::paragraph_from_text(text),
        None => Ok(paragraph.clone()),
    }
}

/// The leading whitespace of a node paragraph (`""` when it has none).
// Cost: O(w + d), w = the whitespace, d = depth of the first paragraph.
fn node_leading_ws(paragraph: &Value) -> String {
    let ws = match paragraph.view() {
        ValueView::RakuAst(node) => match node.class {
            RakuAstClass::DocBlock => super::doc_block_edit::leading_whitespace(node),
            RakuAstClass::DocParagraph => super::doc_edit::leading_whitespace(node),
            _ => return String::new(),
        },
        ValueView::Str(_) => return leading_ws(&paragraph.to_string_value()).to_string(),
        _ => return String::new(),
    };
    ws.as_str().map(str::to_string).unwrap_or_default()
}

/// `!interpret-implicit-code-blocks(@paragraphs)`: lines indented deeper than
/// the text around them become `implicit-code` blocks, the rest paragraphs.
// Cost: O(n), n = size of the paragraphs.
fn implicit_code_blocks(paragraphs: &[Value], out: &mut Vec<Value>) -> Result<(), RuntimeError> {
    let mut ws = String::new();
    let mut offset = 0usize;
    let mut lines: Vec<String> = Vec::new();
    let mut codes: Vec<String> = Vec::new();
    let add_lines = |lines: &mut Vec<String>, out: &mut Vec<Value>| {
        if !lines.is_empty() {
            out.push(super::doc_edit::paragraph_from_text(&lines.concat())?);
            lines.clear();
        }
        Ok::<(), RuntimeError>(())
    };
    let add_codes = |codes: &mut Vec<String>, ws: &str, out: &mut Vec<Value>| {
        if !codes.is_empty() {
            let paragraph = super::doc_edit::paragraph_from_text(&codes.concat())?;
            out.push(implicit_code(ws, paragraph));
            codes.clear();
        }
        Ok::<(), RuntimeError>(())
    };
    for paragraph in paragraphs {
        let Some(text) = paragraph.as_str() else {
            add_codes(&mut codes, &ws, out)?;
            add_lines(&mut lines, out)?;
            ws = node_leading_ws(paragraph);
            offset = ws.chars().count();
            out.push(paragraph.clone());
            continue;
        };
        for line in text.split_inclusive('\n') {
            if line.chars().all(char::is_whitespace) {
                if codes.is_empty() {
                    lines.push("\n".to_string());
                } else {
                    codes.push("\n".to_string());
                }
            } else if leading_ws(line) == ws
                || (!lines.is_empty() && lines.last().is_some_and(|l| l != "\n"))
            {
                if codes.is_empty() {
                    lines.push(line.trim_start().to_string());
                } else {
                    codes.push(after_chars(line, offset).to_string());
                }
            } else {
                let line_ws = leading_ws(line).to_string();
                let leading = line_ws.chars().count();
                if leading > offset {
                    add_lines(&mut lines, out)?;
                    if codes.is_empty() {
                        ws = line_ws;
                        offset = leading;
                    }
                    codes.push(after_chars(line, offset).to_string());
                } else if leading > 0 {
                    add_codes(&mut codes, &ws, out)?;
                    ws = line_ws;
                    offset = leading;
                    codes.push(after_chars(line, offset).to_string());
                } else {
                    add_codes(&mut codes, &ws, out)?;
                    ws = line_ws;
                    offset = leading;
                    lines.push(line.trim_start().to_string());
                }
            }
        }
        add_lines(&mut lines, out)?;
    }
    add_codes(&mut codes, &ws, out)?;
    add_lines(&mut lines, out)?;
    Ok(())
}

/// `Doc::Block.new(:margin($ws), :type<implicit-code>, :paragraphs(...))`.
// Cost: O(1).
fn implicit_code(ws: &str, paragraph: Value) -> Value {
    let leaf = |name: &'static str, value: Value| RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    };
    let mut fields = Vec::with_capacity(3);
    if !ws.is_empty() {
        fields.push(leaf("margin", Value::str(ws.to_string())));
    }
    fields.push(leaf("type", Value::str("implicit-code".to_string())));
    fields.push(RakuAstField {
        name: Some("paragraphs"),
        value: RakuAstFieldValue::List(vec![paragraph]),
    });
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::DocBlock,
        fields,
    }))
}

/// `!interpret-as-table(@paragraphs)`: the rows and dividers of the table the
/// paragraphs spell, and `:header-row(0)` when the legacy heuristic finds a
/// header and none was configured.
// Cost: O(n), n = size of the paragraphs.
fn table(node: &mut RakuAstNode, paragraphs: &[Value], out: &mut Vec<Value>) {
    let text: String = paragraphs.iter().map(Value::to_string_value).collect();
    let lines: Vec<&str> = text.lines().collect();
    let Some(table) = super::pod_table::read(&lines) else {
        return;
    };
    for part in table.parts {
        out.push(match part {
            super::pod_table::TablePart::Row(row) => {
                Value::rakuast(Box::new(super::pod_table::row_node(&row)))
            }
            super::pod_table::TablePart::Separator(text) => Value::str(text),
        });
    }
    let configured = super::doc_block_edit::config_keys(node)
        .as_list_items()
        .is_some_and(|keys| keys.iter().any(|k| k.as_str() == Some("header-row")));
    if table.header_row && !configured {
        let zero = Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::IntLiteral,
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(Value::int(0)),
            }],
        }));
        let key = Value::str("header-row".to_string());
        // `add-config` refuses only a non-block or a non-Str key.
        let _ = super::doc_block_edit::add_config(node, &key, &zero);
    }
}
