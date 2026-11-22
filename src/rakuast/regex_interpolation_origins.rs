//! Origins of a contextualizer interpolated into a regex (ADR-10723 Stage 2,
//! #15780): `/$<cat>=@(%what.keys) 4/`. A method of the regex body walk
//! (`regex_origins::Walk`), kept apart from it.

use super::RakuAstNode;
use super::RakuAstClass;
use super::regex_origins::Walk;
use crate::ast::SrcSpan;
use crate::scope::NameScope;

impl Walk<'_> {
    /// `@(%what.keys)`: an interpolated contextualizer runs from its sigil
    /// to its `)`, and its statements are the text inside, parsed again.
    // Cost: O(n), n = length of the contextualizer's text, plus its parse.
    pub(super) fn contextualized(&mut self, names: &NameScope, node: &mut RakuAstNode) -> Option<()> {
        let var = super::infix_origins::child(node, "var")?;
        let sigil = match var.class {
            RakuAstClass::ContextualizerItem => "$(",
            RakuAstClass::ContextualizerList => "@(",
            RakuAstClass::ContextualizerHash => "%(",
            _ => return None,
        };
        let rest = self.text[self.at..].strip_prefix(sigil)?;
        let close = super::subst_origins::closing(rest, '(', ')')?;
        let at = self.at;
        let inner = rest[..close].to_string();
        let inner_at = self.base + at + sigil.len();
        self.at += sigil.len() + close + 1;
        let span = SrcSpan::new(self.base + at, self.base + self.at);
        super::infix_origins::with_child(node, "var", |v| {
            super::origin::set_span(v, span);
            super::infix_origins::with_child_at(v, 0, |sequence| {
                super::fragment_origins::statements(names, sequence, &inner, inner_at);
                if !inner.trim().is_empty() {
                    let lead = inner.len() - inner.trim_start().len();
                    super::origin::set_span(
                        sequence,
                        SrcSpan::new(inner_at + lead, inner_at + inner.trim_end().len()),
                    );
                }
            });
        });
        self.alias_tail();
        Some(())
    }

}
