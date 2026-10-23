//! Origins of the module `Name` in `use`, `need` and `import` statements
//! (ADR-10723 Stage 2, statement families).
//!
//! The statement's range is known; the name is the word after the keyword.
//! The range is attached only when that word is exactly the module the node
//! names: exact or absent.

use super::name_parts::{NameShape, name_shape};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Place the module `Name` of `statement`, a `Statement::Use`, `Need` or
/// `Import` occupying `span`.
// Cost: O(n), n = length of the statement's source.
pub(super) fn module_name(statement: &mut RakuAstNode, span: SrcSpan) {
    let keyword = match statement.class {
        RakuAstClass::StatementUse => "use",
        RakuAstClass::StatementNeed => "need",
        RakuAstClass::StatementImport => "import",
        _ => return,
    };
    let Some(field) = statement
        .fields
        .iter_mut()
        .find(|f| matches!(f.name, Some("module-name" | "module-names")))
    else {
        return;
    };
    let held = match &mut field.value {
        RakuAstFieldValue::Node(v) => v,
        RakuAstFieldValue::List(items) => match items.as_mut_slice() {
            [only] => only,
            _ => return,
        },
        _ => return,
    };
    let ValueView::RakuAst(name) = held.view() else {
        return;
    };
    let Some(NameShape::Identifier(module)) = name_shape(name) else {
        return;
    };
    let at = super::unit_source::with_text(span, |text| {
        let rest = text.strip_prefix(keyword)?;
        let word = rest.trim_start();
        if word.len() == rest.len() {
            return None;
        }
        (written_name(word) == module).then(|| {
            let from = span.from as usize + (text.len() - word.len());
            SrcSpan::new(from, from + module.len())
        })
    });
    if let Some(at) = at.filter(|at| at.is_some()) {
        let mut name = RakuAstNode::clone(name);
        super::origin::set_span(&mut name, at);
        *held = Value::rakuast(Box::new(name));
    }
}

/// The module name a statement's text starts with: identifier characters
/// joined by `::`.
// Cost: O(n), n = length of the name.
fn written_name(text: &str) -> &str {
    let mut end = 0;
    let mut chars = text.char_indices().peekable();
    while let Some((i, c)) = chars.next() {
        if c.is_alphanumeric() || matches!(c, '_' | '-') {
            end = i + c.len_utf8();
        } else if c == ':' && chars.peek().is_some_and(|(_, n)| *n == ':') {
            chars.next();
            end = i + 2;
        } else {
            break;
        }
    }
    &text[..end]
}
