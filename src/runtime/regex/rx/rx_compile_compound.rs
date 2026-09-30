//! The compound constructs of the regex compiler (`rx_compile`): ordered
//! alternation (`||`) and separated quantifiers (`%` / `%%`). Split out to
//! keep `rx_compile.rs` within the file-size budget; the layout rules are the
//! ones `rx_compile`'s module doc states.

use super::super::regex_helpers::{alternation_list_flags, atom_contains_backref};
use super::RxOp;
use super::rx_compile::{
    Compiler, Decline, atom_captures, has_numbered_alias, min_len, pattern_captures,
    pattern_contains_backref,
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

    /// `atom ** min..max % sep` (and `%%`, which may end on a separator), as
    /// the walk's separated quantifier matches it. The first atom is not
    /// required to advance; every later separator-and-atom step is. Without
    /// ratchet (`for_each_separated_candidate`) every longer chain is tried
    /// before a shorter one, a chain's `%%` trailing separator before its
    /// plain end, and zero iterations last. Under ratchet
    /// (`match_separated_quantifier_ratchet`) each atom and separator takes
    /// its first match, the chain grows while it can, and nothing is given
    /// back. Captures under a separated quantifier fold side by side
    /// (`append_separated_captures`), which Slice A does not model yet, and
    /// the walk ignores frugality here (#10306): both decline.
    pub(super) fn separated(
        &mut self,
        token: &RegexToken,
        sep: &RegexPattern,
        trailing: bool,
    ) -> Result<(), Decline> {
        if token.frugal {
            return Err("separator-frugal");
        }
        if token.named_capture.is_some() {
            return Err("separator-alias");
        }
        if atom_contains_backref(&token.atom) || pattern_contains_backref(sep) {
            // The walk matches each iteration against the captures folded so
            // far (`InlineCaptureScope`); a level of its own would hide them.
            return Err("separator-backref");
        }
        // Each atom and separator then matches in a capture level of its own,
        // collected for `SepEmit` to fold side by side.
        let collect = atom_captures(&token.atom) || pattern_captures(sep);
        let (min, max) = match token.quant {
            RegexQuant::ZeroOrMore => (0, None),
            RegexQuant::OneOrMore => (1, None),
            RegexQuant::Repeat(min, max) => (min, max),
            RegexQuant::RepeatCode(_) => return Err("code"),
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
            greedy: true,
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
            self.ops[split as usize] = RxOp::Split {
                prefer: split + 1,
                alt: zero,
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
