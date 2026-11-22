//! Origins of the `also` statements of a package body (ADR-10723 Stage 2,
//! #18598): `also is Int;`, `also does R;`, `also is rw;`. The parser lifts
//! them out of the body into the package's parents, roles and flags, and the
//! view puts each back as a leading `Statement::Also`, so none keeps a range
//! of its own. Each is found again in the body's text, in source order, and
//! placed with its trait and the trait's name or type when the text spells
//! them: exact or absent.

use super::infix_origins::{byte_range, child, child_at, leaf_text, with_child, with_child_at};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Place the leading `Statement::Also` statements of `decl`'s body.
// Cost: O(n + s), n = length of the body's statements, s = its statements.
pub(super) fn place(decl: &mut RakuAstNode) {
    with_child(decl, "body", |block| {
        with_child(block, "body", |blockoid| {
            with_child_at(blockoid, 0, statements);
        });
    });
}

/// Place the `Statement::Also` children of `list`, a placed `StatementList`.
// Cost: O(n + s), n = length of the list's text, s = its statements.
fn statements(list: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(list) else {
        return;
    };
    let Some(text) = super::unit_source::with_text(SrcSpan::new(from, to), |t| Some(t.to_string()))
    else {
        return;
    };
    let mut cursor = 0;
    for field in &mut list.fields {
        let RakuAstFieldValue::Node(value) = &mut field.value else {
            continue;
        };
        let ValueView::RakuAst(statement) = value.view() else {
            continue;
        };
        if statement.class != RakuAstClass::StatementAlso {
            continue;
        }
        let Some(written) = next_also(&text, cursor) else {
            return;
        };
        cursor = written.end;
        let mut statement = RakuAstNode::clone(statement);
        if !place_statement(
            &mut statement,
            &text[written.start..written.end],
            from + written.start,
        ) {
            return;
        }
        *value = Value::rakuast(Box::new(statement));
    }
}

/// Where an `also` statement is written in a body's text.
struct Written {
    start: usize,
    end: usize,
}

/// The next `also ...` statement in `text` from byte `cursor`: from the
/// keyword to the end of its clause (before the `;`, blanks trimmed).
// Cost: O(n), n = length of `text` after `cursor`.
fn next_also(text: &str, mut cursor: usize) -> Option<Written> {
    while let Some(found) = text[cursor..].find("also") {
        let start = cursor + found;
        cursor = start + 4;
        let before_ok = text[..start]
            .chars()
            .next_back()
            .is_none_or(|c| c.is_whitespace() || matches!(c, ';' | '{'));
        if !before_ok || !text[cursor..].starts_with(char::is_whitespace) {
            continue;
        }
        let rest = &text[start..];
        let clause = &rest[..rest.find(';').unwrap_or(rest.len())];
        return Some(Written {
            start,
            end: start + clause.trim_end().len(),
        });
    }
    None
}

/// Place `statement`, a `Statement::Also` with one trait written as `text`
/// (`also is Int`) at byte `at`. Whether the text spells it.
// Cost: O(n), n = length of `text`.
fn place_statement(statement: &mut RakuAstNode, text: &str, at: usize) -> bool {
    let after = text["also".len()..].trim_start();
    let trait_at = at + text.len() - after.len();
    let (keyword, class) = if after.starts_with("is") {
        ("is", RakuAstClass::TraitIs)
    } else if after.starts_with("does") {
        ("does", RakuAstClass::TraitDoes)
    } else {
        return false;
    };
    let name = after[keyword.len()..].trim_start();
    let name_at = trait_at + after.len() - name.len();
    let name_span = SrcSpan::new(name_at, name_at + name.len());
    let mut placed = false;
    let Some(field) = statement
        .fields
        .iter_mut()
        .find(|f| f.name == Some("traits"))
    else {
        return false;
    };
    let RakuAstFieldValue::List(traits) = &mut field.value else {
        return false;
    };
    let [only] = traits.as_mut_slice() else {
        return false;
    };
    let ValueView::RakuAst(trait_node) = only.view() else {
        return false;
    };
    if trait_node.class != class {
        return false;
    }
    let mut trait_node = RakuAstNode::clone(trait_node);
    // `is rw`: a name; `is Int` / `does R`: a type over a name.
    if let Some(written) = child(&trait_node, "name").and_then(leaf_text) {
        if written == name {
            with_child(&mut trait_node, "name", |n| {
                super::origin::set_span(n, name_span)
            });
            placed = true;
        }
    } else if let Some(ty) = child(&trait_node, "type").or_else(|| child_at(&trait_node, 0))
        && super::resolve::type_node_name(ty).is_some_and(|written| written == name)
    {
        let field = if child(&trait_node, "type").is_some() {
            Some("type")
        } else {
            None
        };
        match field {
            Some(field) => with_child(&mut trait_node, field, |ty| {
                super::routine_origins::type_origin(ty, name_span)
            }),
            None => with_child_at(&mut trait_node, 0, |ty| {
                super::routine_origins::type_origin(ty, name_span)
            }),
        }
        placed = true;
    }
    if placed {
        super::origin::set_span(&mut trait_node, SrcSpan::new(trait_at, at + text.len()));
        *only = Value::rakuast(Box::new(trait_node));
        super::origin::set_span(statement, SrcSpan::new(at, at + text.len()));
    }
    placed
}
