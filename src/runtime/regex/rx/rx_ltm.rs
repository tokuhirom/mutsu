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
    // Cost: O(b·m + b log b), b = the branches, m = one LTM measurement.
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
        order.clear();
        for (i, alt) in alts.iter().enumerate() {
            let rank = self.ltm_branch_rank_key(alt, chars, pos, pkg);
            order.push((i, rank));
        }
        order.sort_by_key(|(_, rank)| std::cmp::Reverse(*rank));
    }
}
