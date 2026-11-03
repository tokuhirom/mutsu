//! Origins of the arguments of a regex call assertion, `<name(ARGS)>`
//! (ADR-10723 Stage 2).
//!
//! The regex tree (ADR-0088) keeps the arguments as expressions parsed from
//! the call's text, with no positions. The text is parsed again here as a
//! fragment that keeps its spelled terms, those ranges are moved to where the
//! text sits in the unit, and the argument list is converted with them in
//! reach: the conversion finds each term's range by its shape within the
//! arguments' range (`expr_origins::of`). Exact or absent.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::spelled::Spelling;
use crate::ast::{Expr, SrcSpan};
use crate::ast_visit::VisitMut;
use crate::scope::NameScope;
use crate::value::Value;

/// Moves every term range of a fragment by the fragment's offset in the unit.
pub(super) struct Shift(pub(super) usize);

impl VisitMut for Shift {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if let Expr::Spelled(spelled) = expr
            && let Spelling::Origin(at) = &mut spelled.spelling
        {
            *at = SrcSpan::new(at.from as usize + self.0, at.to as usize + self.0);
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }
}

/// The byte length of the call's argument text starting right after its `(`
/// (the `)` excluded), when brackets and quotes in it balance plainly.
// Cost: O(n), n = length of the text.
pub(super) fn args_len(text: &str) -> Option<usize> {
    let mut stack: Vec<char> = Vec::new();
    let mut chars = text.char_indices();
    while let Some((at, c)) = chars.next() {
        match c {
            '(' | '[' | '{' => stack.push(c),
            ')' | ']' | '}' => {
                let Some(open) = stack.pop() else {
                    return (c == ')').then_some(at);
                };
                if !matches!((open, c), ('(', ')') | ('[', ']') | ('{', '}')) {
                    return None;
                }
            }
            '\'' | '"' => loop {
                match chars.next()?.1 {
                    '\\' => {
                        chars.next()?;
                    }
                    q if q == c => break,
                    _ => {}
                }
            },
            // Comments, heredocs and the like are not read here.
            '#' | '\\' | '「' | '」' => return None,
            _ => {}
        }
    }
    None
}

/// Give the `args` of `node`, the call assertion whose argument text `inner`
/// starts at byte `at` of the unit, their ranges.
// Cost: O(n), n = length of the argument text, plus the parse of it.
pub(super) fn place(
    names: &NameScope,
    node: &mut RakuAstNode,
    inner: &str,
    at: usize,
) -> Option<()> {
    let lead = inner.len() - inner.trim_start().len();
    let text = inner.trim();
    if text.is_empty() {
        return Some(());
    }
    let base = at + lead;
    let (_, args) = crate::regex_tree::parse_subrule_target(&format!("x({text})"))?;
    let args = args?;
    let (mut stmts, _) = crate::parser::parse_fragment_spelled(text).ok()?;
    crate::ast_visit::walk_stmts_mut(&mut Shift(base), &mut stmts);
    let _origins = super::expr_origins::strip(&mut stmts);
    let range = SrcSpan::new(base, base + text.len());
    let _statement = super::expr_origins::InStatement::enter(Some(range));
    let mut list = super::view::regex_arg_list(names, &args).ok()?;
    super::origin::set_span(&mut list, range);
    let field = node.fields.iter_mut().find(|f| f.name == Some("args"))?;
    debug_assert_eq!(list.class, RakuAstClass::ArgList);
    field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(list)));
    Some(())
}
