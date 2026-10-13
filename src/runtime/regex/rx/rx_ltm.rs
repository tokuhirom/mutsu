//! The branch order of a compiled `|` (ADR-0135 D4): the walk's own LTM rank
//! key per branch, best first. ADR-0022 / ADR-0046 / ADR-0127 decide what the
//! ranking is; the compiled engine only consumes it.

use super::{LtmAltTable, RxProgram};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::RegexAtom;
use crate::symbol::Symbol;

impl Interpreter {
    /// Fill `order` with `table`'s branch indexes, best-ranked first; ties
    /// keep declaration order, as `drive_alternation_candidates` sorts them.
    /// A markless program ([`RxProgram::markless`]) ranks over the
    /// mark-stripped subject, as `:m` compares: only the order is used, so no
    /// position needs mapping back.
    // Cost: O(n * t + b log b): one run of the branches' NFA (see
    // `ltm_rank_alternation`), b = the branches. Markless over a subject that
    // is not the published match target, plus O(n) to strip `chars[pos..]`.
    pub(super) fn rx_ltm_order(
        &mut self,
        program: &RxProgram,
        table: &LtmAltTable,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        order: &mut Vec<(usize, (usize, usize))>,
    ) {
        let RegexAtom::Alternation(alts) = &program.toks[table.tok as usize].atom else {
            unreachable!("an LtmAlt table names an alternation token");
        };
        if !program.markless {
            self.ltm_rank_alternation(&table.nfa, alts, chars, pos, pkg, order);
            return;
        }
        let view = MarklessView::of(chars);
        self.ltm_rank_alternation(&table.nfa, alts, view.chars(), view.at(pos), pkg, order);
    }
}

/// The mark-stripped view of a subject that a markless program
/// ([`RxProgram::markless`]) decides a zero-width question or an LTM ranking
/// on: the published match target's cached stripped view when the subject is
/// that target, else a fresh strip.
pub(super) enum MarklessView {
    Target(crate::value::regex_caps::MatchTarget),
    Owned(Vec<char>, Vec<usize>),
}

impl MarklessView {
    // Cost: O(1) for the published target once its stripped view exists;
    // O(n) otherwise, n = chars.
    pub(super) fn of(chars: &[char]) -> Self {
        match crate::runtime::regex::regex_helpers::current_match_target()
            .filter(|target| std::ptr::eq(target.chars(), chars))
        {
            Some(target) => Self::Target(target),
            None => {
                let (stripped, map) = crate::value::regex_caps::strip_marks_text(chars);
                Self::Owned(stripped, map)
            }
        }
    }

    pub(super) fn chars(&self) -> &[char] {
        match self {
            Self::Target(target) => target.stripped().chars(),
            Self::Owned(chars, _) => chars,
        }
    }

    /// Where grapheme-boundary position `pos` of the subject falls.
    // Cost: O(1) for the target; O(n) for a fresh strip, n = chars.
    pub(super) fn at(&self, pos: usize) -> usize {
        match self {
            Self::Target(target) => target.stripped().original_to_stripped(pos),
            Self::Owned(chars, map) => map.iter().position(|&o| o >= pos).unwrap_or(chars.len()),
        }
    }
}
