//! The executable shape of a semicolon-sectioned list: `[a, b; c]` and `(a, b; c)`.
//! The parse keeps the sections as written (`Spelling::{Array,Paren}Sections`);
//! these are the lowering `rakuast::lower` and the parser share.

use crate::ast::Expr;
use crate::value::Value;

use super::list_infix::finalize_paren_list;

/// `finalize_array_sections` (parser) without the written sections.
// Cost: O(n), n = size of the sections.
pub(crate) fn lower_array_sections(
    mut sections: Vec<Vec<Expr>>,
    current: Vec<Expr>,
    saw_semicolon: bool,
    trailing_comma: bool,
) -> Expr {
    if !saw_semicolon {
        return Expr::BracketArray(normalize_array_items(current), trailing_comma);
    }
    if !current.is_empty() {
        sections.push(current);
    }
    build_array_sections(sections)
}

fn build_array_sections(sections: Vec<Vec<Expr>>) -> Expr {
    let sections: Vec<_> = sections
        .into_iter()
        .map(|section| {
            if section.is_empty() {
                vec![Expr::Literal(Value::package(crate::symbol::wk::any()))]
            } else {
                section
            }
        })
        .collect();
    if sections.len() <= 1 {
        let flat = sections.into_iter().next().unwrap_or_default();
        return Expr::BracketArray(normalize_array_items(flat), false);
    }
    let elems = sections.into_iter().map(build_array_section).collect();
    Expr::BracketArray(elems, false)
}

/// `array_literal` splits the composer's contents on commas itself, so the operators that
/// are looser than the comma have to be given their real precedence afterwards — the same
/// repair `parse_comma_or_expr` applies to a bare comma list.
fn normalize_array_items(items: Vec<Expr>) -> Vec<Expr> {
    crate::parser::normalize_comma_list_items(items)
}

/// Render one array section: a single item stays bare; multiple items form a
/// sub-list (`ArrayLiteral`, so they render as `(a, b)`).
fn build_array_section(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        items.into_iter().next().unwrap()
    } else {
        Expr::ArrayLiteral(normalize_array_items(items))
    }
}

/// `finalize_paren_sections` (parser) without the written sections.
// Cost: O(n), n = size of the sections.
pub(crate) fn lower_paren_sections(
    mut sections: Vec<Vec<Expr>>,
    current: Vec<Expr>,
    saw_semicolon: bool,
) -> Expr {
    if !saw_semicolon {
        return finalize_paren_list(current);
    }
    if !current.is_empty() {
        sections.push(current);
    }
    match sections.len() {
        0 => Expr::ArrayLiteral(Vec::new()),
        1 => build_paren_section(sections.into_iter().next().unwrap()),
        _ => Expr::ArrayLiteral(sections.into_iter().map(build_paren_section).collect()),
    }
}

/// Render one semicolon-section: a single item stays bare; multiple items form
/// a sub-list (via `finalize_paren_list`, so meta-ops etc. still lift).
fn build_paren_section(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        items.into_iter().next().unwrap()
    } else {
        finalize_paren_list(items)
    }
}
