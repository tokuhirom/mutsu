//! The compound constructs of the regex compiler (`rx_compile`): ordered
//! alternation (`||`) and separated quantifiers (`%` / `%%`). Split out to
//! keep `rx_compile.rs` within the file-size budget; the layout rules are the
//! ones `rx_compile`'s module doc states.

use super::super::regex_helpers::{
    alternation_list_flags, atom_contains_backref, atom_contains_code,
};
use super::RxOp;
use super::rx_compile::{
    Compiler, Decline, atom_captures, has_numbered_alias, min_len, pattern_captures,
    pattern_contains_backref, pattern_contains_code, pattern_reads_enclosing_state,
};
use crate::runtime::regex_types::{RegexPattern, RegexQuant, RegexToken};

impl Compiler {
    /// `a || b || c`, as `walk_seq_alternation` drives it: every way branch
    /// *k* can match is tried against the rest of the pattern before branch
    /// *k+1* is entered. Each branch ends with an `AltTail` that pads the
    /// alternation's positional slot space and marks its list-valued names
    /// (`alternation_branch_delta`). Under ratchet the alternation commits to
    /// the first branch that matches and to that branch's first end; the walk
    /// moves past a branch whose every end is zero-width, so a ratcheted
    /// alternation with such a branch (other than the last) is declined.
    pub(super) fn seq_alternation(
        &mut self,
        token: &RegexToken,
        alts: &[RegexPattern],
    ) -> Result<(), Decline> {
        if alts.iter().any(has_numbered_alias) {
            return Err("seqalt-numbered-alias");
        }
        if token.ratchet
            && alts
                .iter()
                .take(alts.len().saturating_sub(1))
                .any(|a| min_len(a) == 0)
        {
            return Err("seqalt-nullable-ratchet");
        }
        let alt = self.alts.len() as u32;
        self.alts.push(alternation_list_flags(alts));
        let pos_base = self.reg();
        self.ops.push(RxOp::PosBase(pos_base));
        let height = token.ratchet.then(|| self.reg());
        if let Some(h) = height {
            self.ops.push(RxOp::Height(h));
        }
        let suppress_padding = self.quant_alt_depth > 0;
        let mut joins = Vec::with_capacity(alts.len());
        for (k, branch) in alts.iter().enumerate() {
            let split = (k + 1 < alts.len()).then(|| {
                self.ops.push(RxOp::Split { prefer: 0, alt: 0 }); // patched below
                self.pc() - 1
            });
            self.pattern(branch)?;
            self.ops.push(RxOp::AltTail {
                alt,
                pos_base,
                suppress_padding,
            });
            if let Some(h) = height {
                self.ops.push(RxOp::Cut(h));
            }
            if let Some(split) = split {
                joins.push(self.pc());
                self.ops.push(RxOp::Jmp(0)); // patched below
                let next = self.pc();
                self.ops[split as usize] = RxOp::Split {
                    prefer: split + 1,
                    alt: next,
                };
            }
        }
        let end = self.pc();
        for j in joins {
            self.ops[j as usize] = RxOp::Jmp(end);
        }
        Ok(())
    }

    /// `a | b | c`, as `drive_alternation_candidates` drives it: one
    /// `LtmAlt` ranks the branches at run time and enters them best first,
    /// each one only after every higher-ranked branch has failed against the
    /// rest of the pattern. Branch captures merge as for `||` (`AltTail`).
    /// Under ratchet the alternation commits to the first branch that
    /// matches, and to that branch's first end.
    pub(super) fn ltm_alternation(
        &mut self,
        token: &RegexToken,
        alts: &[RegexPattern],
    ) -> Result<(), Decline> {
        if alts.iter().any(has_numbered_alias) {
            return Err("alt-numbered-alias");
        }
        let alt = self.alts.len() as u32;
        self.alts.push(alternation_list_flags(alts));
        let pos_base = self.reg();
        self.ops.push(RxOp::PosBase(pos_base));
        let height = token.ratchet.then(|| self.reg());
        if let Some(h) = height {
            self.ops.push(RxOp::Height(h));
        }
        // Reserved before the branches, which may hold `|`s of their own.
        let table = self.ltm_alts.len();
        let tok = self.toks.len() as u32;
        self.toks.push(token.clone());
        self.ltm_alts.push(super::LtmAltTable {
            tok,
            pcs: Box::default(),
        });
        self.ops.push(RxOp::LtmAlt(table as u32));
        let suppress_padding = self.quant_alt_depth > 0;
        let mut pcs = Vec::with_capacity(alts.len());
        let mut joins = Vec::with_capacity(alts.len());
        for branch in alts {
            pcs.push(self.pc());
            self.pattern(branch)?;
            self.ops.push(RxOp::AltTail {
                alt,
                pos_base,
                suppress_padding,
            });
            if let Some(h) = height {
                self.ops.push(RxOp::Cut(h));
            }
            joins.push(self.pc());
            self.ops.push(RxOp::Jmp(0)); // patched below
        }
        let end = self.pc();
        for j in joins {
            self.ops[j as usize] = RxOp::Jmp(end);
        }
        self.ltm_alts[table].pcs = pcs.into_boxed_slice();
        Ok(())
    }

    /// `inner ~ goal`, as the walk's `GoalMatch` arm matches it: every end of
    /// the inner pattern (a regex of its own, so a level of its own) is a start
    /// for the goal (another), whose every end is a candidate; both levels'
    /// captures merge with the goal's first. A goal that matches nowhere after
    /// an end of the inner pattern records the failure for the "expected goal"
    /// report. Under ratchet the atom commits to its first end.
    pub(super) fn goal_match(
        &mut self,
        token: &RegexToken,
        goal: &RegexPattern,
        inner: &RegexPattern,
    ) -> Result<(), Decline> {
        // Each side is a regex of its own: code and backreferences there read
        // that side's captures, not the enclosing level's.
        if [goal, inner]
            .iter()
            .any(|p| pattern_contains_backref(p) || pattern_reads_enclosing_state(p))
        {
            return Err("goal-match-code");
        }
        let height = token.ratchet.then(|| self.reg());
        if let Some(h) = height {
            self.ops.push(RxOp::Height(h));
        }
        let base = self.reg();
        self.ops.push(RxOp::SepBase(base));
        self.ops.push(RxOp::OpenIsolated);
        self.pattern(inner)?;
        self.ops.push(RxOp::Collect { sep: false });
        // The failure handler sits below the goal's own choice points, so it is
        // reached only when the goal found nothing after this inner end.
        let handler_height = self.reg();
        self.ops.push(RxOp::Height(handler_height));
        let split = self.pc();
        self.ops.push(RxOp::Split { prefer: 0, alt: 0 }); // patched below
        self.ops.push(RxOp::OpenIsolated);
        self.pattern(goal)?;
        self.ops.push(RxOp::GoalEnd { base });
        self.ops.push(RxOp::GoalOk {
            height: handler_height,
        });
        let join = self.pc();
        self.ops.push(RxOp::Jmp(0)); // patched below
        let handler = self.pc();
        let tok = self.toks.len() as u32;
        self.toks.push(token.clone());
        self.ops.push(RxOp::GoalFail { tok });
        let end = self.pc();
        self.ops[join as usize] = RxOp::Jmp(end);
        self.ops[split as usize] = RxOp::Split {
            prefer: split + 1,
            alt: handler,
        };
        if let Some(h) = height {
            self.ops.push(RxOp::Cut(h));
        }
        Ok(())
    }

    /// `a & b & c`, as `drive_conjunction_candidates` drives it: every end
    /// of the first branch, in priority order, is a candidate once each
    /// other branch matches exactly the same span. The first branch runs
    /// inline in a capture level of its own; the others are checked by
    /// `ConjTail` with a nested run of their own programs, so each must
    /// compile (`conjunction-branch`). Under ratchet the conjunction commits
    /// to the first end that every branch agrees on.
    pub(super) fn conjunction(
        &mut self,
        token: &RegexToken,
        branches: &[RegexPattern],
    ) -> Result<(), Decline> {
        let Some((first, rest)) = branches.split_first() else {
            // An empty conjunction matches zero-width.
            return Ok(());
        };
        if branches.iter().any(pattern_contains_backref) {
            // A branch shares the enclosing capture scope, which the first
            // branch's own level and the other branches' nested runs hide.
            return Err("conjunction-backref");
        }
        if rest
            .iter()
            .any(|b| super::rx_entry::program_for(b).is_none())
        {
            return Err("conjunction-branch");
        }
        if branches.iter().any(pattern_reads_enclosing_state) {
            // Every branch shares the enclosing regex's scope, which a level of
            // its own (the first branch) and a nested run (the others) hide
            // from code and from a `$x` lexical: they would see their own
            // branch's state only.
            return Err("conjunction-code");
        }
        let start = self.reg();
        self.ops.push(RxOp::Mark(start));
        let height = token.ratchet.then(|| self.reg());
        if let Some(h) = height {
            self.ops.push(RxOp::Height(h));
        }
        self.ops.push(RxOp::OpenCapture);
        self.pattern(first)?;
        let tok = self.toks.len() as u32;
        self.toks.push(token.clone());
        self.ops.push(RxOp::ConjTail { tok, start });
        if let Some(h) = height {
            self.ops.push(RxOp::Cut(h));
        }
        Ok(())
    }

    /// `atom ** min..max % sep` (and `%%`, which may end on a separator), as
    /// the walk's separated quantifier matches it. The first atom is not
    /// required to advance; every later separator-and-atom step is. Without
    /// ratchet (`for_each_separated_candidate`) every longer chain is tried
    /// before a shorter one unless frugal, a chain's `%%` trailing separator
    /// before its plain end, and zero iterations first when frugal. Under ratchet
    /// (`match_separated_quantifier_ratchet`) each atom and separator takes
    /// its first match, the chain grows while it can, and nothing is given
    /// back. Captures fold side by side (`SepEmit`).
    pub(super) fn separated(
        &mut self,
        token: &RegexToken,
        sep: &RegexPattern,
        trailing: bool,
    ) -> Result<(), Decline> {
        if token.named_capture.is_some() {
            return Err("separator-alias");
        }
        if atom_contains_backref(&token.atom) || pattern_contains_backref(sep) {
            // The walk matches each iteration against the captures folded so
            // far (`InlineCaptureScope`); a level of its own would hide them.
            return Err("separator-backref");
        }
        if atom_contains_code(&token.atom) || pattern_contains_code(sep) {
            // Code is a reader of the captures too: inside a separated
            // quantifier `$/[*-1][*-1]` addresses the iterations folded so far
            // (Net::Whois's octet check, `InlineCaptureScope` in the walk), and
            // a level of its own would show only the current iteration.
            return Err("separator-code");
        }
        // Each atom and separator then matches in a capture level of its own,
        // collected for `SepEmit` to fold side by side.
        let collect = atom_captures(&token.atom) || pattern_captures(sep);
        let (min, max) = match token.quant {
            RegexQuant::ZeroOrMore => (0, None),
            RegexQuant::OneOrMore => (1, None),
            RegexQuant::Repeat(min, max) => (min, max),
            RegexQuant::RepeatCode(_) => return Err("repeat-code"),
            RegexQuant::One | RegexQuant::ZeroOrOne => return Err("separator-quant"),
        };
        if max.is_some_and(|max| max == 0 || min > max) {
            return Err("empty-range");
        }
        let (Ok(min), Ok(max)) = (u32::try_from(min), max.map_or(Ok(u32::MAX), u32::try_from))
        else {
            return Err("too-large");
        };
        let ratchet = token.ratchet;
        let base = collect.then(|| self.reg());
        if let Some(b) = base {
            self.ops.push(RxOp::SepBase(b));
        }
        let ctr = self.reg();
        self.ops.push(RxOp::CtrZero(ctr));
        let whole = ratchet.then(|| self.reg());
        if let Some(h) = whole {
            self.ops.push(RxOp::Height(h));
        }
        // Zero iterations: preferred last, and only reachable when the first
        // atom fails outright under ratchet (the cut at `emit` drops it).
        let zero_split = (min == 0 || ratchet).then(|| {
            self.ops.push(RxOp::Split { prefer: 0, alt: 0 }); // patched below
            self.pc() - 1
        });
        self.collected(collect, false, |c| c.committed(ratchet, |c| c.atom(token)))?;
        self.ops.push(RxOp::CtrInc(ctr));
        let head = self.pc();
        self.ops.push(RxOp::Jmp(0)); // patched below
        let ext = self.pc();
        let step = self.reg();
        self.ops.push(RxOp::Mark(step));
        self.collected(collect, true, |c| c.committed(ratchet, |c| c.pattern(sep)))?;
        self.collected(collect, false, |c| c.committed(ratchet, |c| c.atom(token)))?;
        self.ops.push(RxOp::Advanced { start: step });
        self.ops.push(RxOp::CtrInc(ctr));
        self.ops.push(RxOp::Jmp(head));
        let emit = self.pc();
        self.ops[head as usize] = RxOp::Repeat {
            ctr,
            min: 0,
            max,
            body: ext,
            exit: emit,
            greedy: !token.frugal,
        };
        if let Some(h) = whole {
            self.ops.push(RxOp::Cut(h));
        }
        self.ops.push(RxOp::AtLeast { ctr, min });
        let mut to_end = Vec::new();
        if trailing {
            let h = ratchet.then(|| self.reg());
            if let Some(h) = h {
                self.ops.push(RxOp::Height(h));
            }
            let split = self.pc();
            self.ops.push(RxOp::Split { prefer: 0, alt: 0 }); // patched below
            self.collected(collect, true, |c| c.pattern(sep))?;
            if let Some(h) = h {
                self.ops.push(RxOp::Cut(h));
            }
            to_end.push((split, true));
        }
        if let Some(split) = zero_split {
            to_end.push((self.pc(), false));
            self.ops.push(RxOp::Jmp(0)); // patched below
            let zero = self.pc();
            self.ops[split as usize] = if token.frugal {
                RxOp::Split {
                    prefer: zero,
                    alt: split + 1,
                }
            } else {
                RxOp::Split {
                    prefer: split + 1,
                    alt: zero,
                }
            };
            if let Some(h) = whole {
                self.ops.push(RxOp::Cut(h));
            }
            self.ops.push(RxOp::AtLeast { ctr, min });
        }
        let end = self.pc();
        for (at, is_split) in to_end {
            self.ops[at as usize] = if is_split {
                RxOp::Split {
                    prefer: at + 1,
                    alt: end,
                }
            } else {
                RxOp::Jmp(end)
            };
        }
        if let Some(base) = base {
            let tok = self.toks.len() as u32;
            self.toks.push(token.clone());
            self.ops.push(RxOp::SepEmit { tok, base });
        }
        Ok(())
    }

    /// Emit `body`, in a capture level collected as one separated-quantifier
    /// iteration (a separator's when `sep`) when `collect`.
    fn collected(
        &mut self,
        collect: bool,
        sep: bool,
        body: impl FnOnce(&mut Self) -> Result<(), Decline>,
    ) -> Result<(), Decline> {
        if collect {
            self.ops.push(RxOp::OpenCapture);
        }
        body(self)?;
        if collect {
            self.ops.push(RxOp::Collect { sep });
        }
        Ok(())
    }

    /// Emit `body`, committed to its first match when `ratchet`.
    fn committed(
        &mut self,
        ratchet: bool,
        body: impl FnOnce(&mut Self) -> Result<(), Decline>,
    ) -> Result<(), Decline> {
        let h = ratchet.then(|| self.reg());
        if let Some(h) = h {
            self.ops.push(RxOp::Height(h));
        }
        body(self)?;
        if let Some(h) = h {
            self.ops.push(RxOp::Cut(h));
        }
        Ok(())
    }
}
