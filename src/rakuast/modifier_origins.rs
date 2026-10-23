//! Origins of a statement modifier and of the expression it modifies
//! (ADR-10723 Stage 2, statement families).
//!
//! `say 1 if $x` is one statement. The parser desugars the modifier into a
//! `Stmt::If` around the body, so neither the modifier nor the body is a
//! statement of its own. Both are placed from what is known: the statement's
//! range, and the origin of the modifier's condition (a written term). The
//! modifier starts at its keyword, which is read back from the source text
//! before the condition, and the modified expression is the text before the
//! keyword. A range that is not confirmed by the text is not attached.

use super::infix_origins::{byte_range, child_at};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// The keyword a modifier node was written with.
// Cost: O(1).
fn keyword(class: RakuAstClass) -> Option<&'static str> {
    Some(match class {
        RakuAstClass::StatementModifierIf => "if",
        RakuAstClass::StatementModifierUnless => "unless",
        RakuAstClass::StatementModifierWith => "with",
        RakuAstClass::StatementModifierWithout => "without",
        RakuAstClass::StatementModifierWhen => "when",
        RakuAstClass::StatementModifierFor => "for",
        RakuAstClass::StatementModifierWhile => "while",
        RakuAstClass::StatementModifierUntil => "until",
        RakuAstClass::StatementModifierGiven => "given",
        _ => return None,
    })
}

/// Place the modifier of `statement`, a `Statement::Expression` occupying
/// `span`, and the expression it modifies.
// Cost: O(n), n = length of the statement's source.
pub(super) fn statement(statement: &mut RakuAstNode, span: SrcSpan) {
    let [expression, modifier] = statement.fields.as_mut_slice() else {
        return;
    };
    let (RakuAstFieldValue::Node(expression), RakuAstFieldValue::Node(modifier)) =
        (&mut expression.value, &mut modifier.value)
    else {
        return;
    };
    let (ValueView::RakuAst(expr_node), ValueView::RakuAst(modifier_node)) =
        (expression.view(), modifier.view())
    else {
        return;
    };
    let Some(keyword) = keyword(modifier_node.class) else {
        return;
    };
    let Some((cond_from, cond_to)) = child_at(modifier_node, 0).and_then(byte_range) else {
        return;
    };
    let (from, to) = (span.from as usize, span.to as usize);
    if cond_from <= from || cond_to > to {
        return;
    }
    // The keyword is the last word before the condition.
    let placed = super::unit_source::with_text(SrcSpan::new(from, cond_from), |before| {
        let head = before.trim_end().strip_suffix(keyword)?;
        if head.ends_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-') {
            return None;
        }
        let body = head.trim_end();
        // A comment between the parts would sit inside the expression's text.
        let body_end = (!body.is_empty() && !body.contains('#')).then_some(from + body.len());
        Some((body_end, from + head.len()))
    });
    let Some((body_end, keyword_at)) = placed else {
        return;
    };
    let mut modifier_copy = RakuAstNode::clone(modifier_node);
    super::origin::set_span(&mut modifier_copy, SrcSpan::new(keyword_at, cond_to));
    *modifier = Value::rakuast(Box::new(modifier_copy));
    if let Some(body_end) = body_end {
        let mut expr_copy = RakuAstNode::clone(expr_node);
        super::origin::set_span(&mut expr_copy, SrcSpan::new(from, body_end));
        super::infix_origins::derive(&mut expr_copy);
        *expression = Value::rakuast(Box::new(expr_copy));
    }
}
