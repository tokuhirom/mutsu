//! `RegexPattern` → [`RxProgram`] (ADR-0135 D1), for Slice A's regular core.
//!
//! Every construct outside the slice declines the whole pattern with a
//! reason; the caller keeps the tree walk for it (D5). The layout mirrors the
//! walk's priority order exactly, because the first complete match found is
//! the answer: a greedy quantifier tries the body before the exit, a frugal
//! one the exit first, and a ratchet cuts the choice points the walk would
//! never have revisited.

use super::super::regex_helpers::{
    AlternationListFlags, atom_contains_alternation, atom_contains_backref,
};
use super::{RxOp, RxProgram};
use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexQuant, RegexToken};

/// Why a pattern was not compiled. Reported per pattern under
/// `MUTSU_VM_STATS` (`regex-vm: … declined=(reason=count …)`).
pub(in crate::runtime::regex) type Decline = &'static str;

pub(super) struct Compiler {
    pub(super) ops: Vec<RxOp>,
    pub(super) atoms: Vec<crate::runtime::regex_types::RegexAtom>,
    /// Per atom: its pattern level's `:i`.
    pub(super) atom_ic: Vec<bool>,
    /// The `:i` of the pattern level being compiled.
    ignore_case: bool,
    pub(super) toks: Vec<RegexToken>,
    pub(super) alts: Vec<AlternationListFlags>,
    pub(super) ltm_alts: Vec<super::LtmAltTable>,
    pub(super) nregs: usize,
    /// How many enclosing quantified bodies contain an alternation: the walk
    /// matches those bodies with `IN_QUANTIFIED_ALTERNATION_MATCH` set, which
    /// turns off a `||` branch's positional padding.
    pub(super) quant_alt_depth: usize,
}

/// Compile `pattern`, or say why not.
// Cost: O(t), t = the number of tokens in the pattern tree.
pub(in crate::runtime::regex) fn compile(pattern: &RegexPattern) -> Result<RxProgram, Decline> {
    let mut c = Compiler {
        ops: Vec::new(),
        atoms: Vec::new(),
        atom_ic: Vec::new(),
        ignore_case: false,
        toks: Vec::new(),
        alts: Vec::new(),
        ltm_alts: Vec::new(),
        nregs: 0,
        quant_alt_depth: 0,
    };
    c.pattern(pattern)?;
    c.ops.push(RxOp::Match);
    if c.nregs > u16::MAX as usize || c.ops.len() > u32::MAX as usize {
        return Err("too-large");
    }
    Ok(RxProgram {
        ops: c.ops,
        atoms: c.atoms,
        atom_ic: c.atom_ic,
        toks: c.toks,
        alts: c.alts,
        ltm_alts: c.ltm_alts,
        nregs: c.nregs,
        ascii: std::sync::OnceLock::new(),
    })
}

/// The one-grapheme atoms `match_consuming_atom` decides.
pub(super) fn is_consuming(atom: &RegexAtom) -> bool {
    matches!(
        atom,
        RegexAtom::Literal(_)
            | RegexAtom::LiteralGrapheme(_)
            | RegexAtom::Any
            | RegexAtom::CharClass(_)
            | RegexAtom::UnicodeProp { .. }
            | RegexAtom::CompositeClass { .. }
            | RegexAtom::Newline
            | RegexAtom::NotNewline
    )
}

/// The zero-width assertions `regex_match_atom_in_pkg` decides without a
/// capture or a subrule frame.
fn is_assertion(atom: &RegexAtom) -> bool {
    matches!(
        atom,
        RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. }
            | RegexAtom::LeftWordBoundary
            | RegexAtom::RightWordBoundary
            | RegexAtom::WordBoundary { .. }
            | RegexAtom::WithinWord { .. }
            | RegexAtom::StartOfLine
            | RegexAtom::EndOfLine
            | RegexAtom::EndOfString
            | RegexAtom::SameAssertion { .. }
            | RegexAtom::AtPosition(_)
    )
}

/// Does anything in `pattern` record a capture?
pub(super) fn pattern_captures(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().any(|t| {
        t.named_capture.is_some()
            || t.hash_capture.is_some()
            || t.secondary_named_capture.is_some()
            || atom_captures(&t.atom)
    })
}

/// Does matching `atom` itself record a capture (its token's alias aside)?
pub(super) fn atom_captures(atom: &RegexAtom) -> bool {
    match atom {
        RegexAtom::CaptureGroup(_) => true,
        RegexAtom::Group(p) => pattern_captures(p),
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
            alts.iter().any(pattern_captures)
        }
        _ => false,
    }
}

/// Does `pattern` (a separator) hold a backreference anywhere?
pub(super) fn pattern_contains_backref(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().any(|t| {
        atom_contains_backref(&t.atom)
            || t.separator
                .as_ref()
                .is_some_and(|sep| pattern_contains_backref(&sep.pattern))
    })
}

/// Is any token under `pattern` a numbered alias (`$0=…`)? The walk matches a
/// `||` branch in a capture scope of its own, so such an alias there numbers
/// from the branch's start, not from the enclosing level's.
pub(super) fn has_numbered_alias(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().any(|t| {
        t.named_capture
            .as_ref()
            .is_some_and(|n| n.parse::<usize>().is_ok())
            || match &t.atom {
                RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => has_numbered_alias(p),
                RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
                    alts.iter().any(has_numbered_alias)
                }
                _ => false,
            }
    })
}

/// The fewest characters any match of `pattern` consumes (for the patterns
/// this compiler accepts).
pub(super) fn min_len(pattern: &RegexPattern) -> usize {
    pattern
        .tokens
        .iter()
        .map(|t| {
            let atom = atom_min_len(&t.atom);
            let reps = match t.quant {
                RegexQuant::One | RegexQuant::OneOrMore => 1,
                RegexQuant::ZeroOrOne | RegexQuant::ZeroOrMore => 0,
                RegexQuant::Repeat(min, _) => min,
                RegexQuant::RepeatCode(_) => 0,
            };
            atom.saturating_mul(reps)
        })
        .fold(0usize, usize::saturating_add)
}

/// Does the walk explore every candidate of each iteration of a
/// non-ratcheted quantifier over `atom` (`quantifier_atom_needs_candidate_backtracking`
/// or an alternation inside), rather than growing a chain of first candidates?
fn loop_body_backtracks(atom: &RegexAtom) -> bool {
    matches!(atom, RegexAtom::Group(_) | RegexAtom::CaptureGroup(_))
        || atom_contains_alternation(atom)
}

/// The fewest characters one match of `atom` consumes.
fn atom_min_len(atom: &RegexAtom) -> usize {
    match atom {
        a if is_consuming(a) => 1,
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => min_len(p),
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
            alts.iter().map(min_len).min().unwrap_or(0)
        }
        _ => 0,
    }
}

impl Compiler {
    pub(super) fn reg(&mut self) -> u16 {
        self.nregs += 1;
        (self.nregs - 1) as u16
    }

    /// Add `atom` to the atom table, tested under the current level's `:i`.
    fn push_atom(&mut self, atom: &RegexAtom) -> u32 {
        self.atoms.push(atom.clone());
        self.atom_ic.push(self.ignore_case);
        (self.atoms.len() - 1) as u32
    }

    pub(super) fn pc(&self) -> u32 {
        self.ops.len() as u32
    }

    pub(super) fn pattern(&mut self, pattern: &RegexPattern) -> Result<(), Decline> {
        if pattern.ignore_mark {
            return Err("ignoremark");
        }
        // The walk tests a level's atoms under that level's own `:i`
        // (`ctx.pattern.ignore_case`), so a scoped `[:i …]` covers its body only.
        let outer_ic = std::mem::replace(&mut self.ignore_case, pattern.ignore_case);
        let result = self.pattern_tokens(pattern);
        self.ignore_case = outer_ic;
        result
    }

    fn pattern_tokens(&mut self, pattern: &RegexPattern) -> Result<(), Decline> {
        // The walk checks a pattern level's `^` when that level is entered
        // and its `$` when the level's tokens run out; so do these.
        if pattern.anchor_start {
            self.ops.push(RxOp::AssertStart);
        }
        for token in &pattern.tokens {
            self.token(token)?;
        }
        if pattern.anchor_end {
            self.ops.push(RxOp::AssertEnd);
        }
        Ok(())
    }

    fn token(&mut self, token: &RegexToken) -> Result<(), Decline> {
        if token.hash_capture.is_some() {
            return Err("hash-capture");
        }
        if token.frugal && token.ratchet {
            return Err("frugal-ratchet");
        }
        if let Some(sep) = &token.separator {
            return self.separated(token, &sep.pattern, sep.allow_trailing);
        }
        if matches!(token.quant, RegexQuant::ZeroOrOne) {
            // `?` applies its alias on the matched arm only; see `zero_or_one`.
            return self.zero_or_one(token);
        }
        // A quantified token applies its alias once per iteration; see `repeat`.
        let alias = if token.named_capture.is_some() && matches!(token.quant, RegexQuant::One) {
            let (pos_base, start) = (self.reg(), self.reg());
            self.ops.push(RxOp::PosBase(pos_base));
            self.ops.push(RxOp::Mark(start));
            Some((pos_base, start))
        } else {
            None
        };
        match token.quant {
            RegexQuant::One => self.atom(token)?,
            RegexQuant::ZeroOrOne => unreachable!("handled above"),
            RegexQuant::ZeroOrMore => self.repeat(token, 0, None)?,
            RegexQuant::OneOrMore => self.repeat(token, 1, None)?,
            RegexQuant::Repeat(min, max) => {
                if max.is_some_and(|max| min > max) {
                    // The walk raises "Quantifier range is empty".
                    return Err("empty-range");
                }
                self.repeat(token, min, max)?
            }
            RegexQuant::RepeatCode(_) => return Err("code"),
        }
        if let Some((pos_base, start)) = alias {
            let tok = self.toks.len() as u32;
            self.toks.push(token.clone());
            self.ops.push(RxOp::Named {
                tok,
                start,
                pos_base,
            });
        }
        Ok(())
    }

    /// One match of `token`'s atom. Under ratchet the atom commits to its
    /// first candidate, as the walk's `for_each_atom_candidate(.., ratchet)`
    /// does — which for a non-capturing `[ … ]` is no commitment at all
    /// (the walk's ratchet only stops a capture group from trying another
    /// inner end; see `regex_match_lazy.rs`).
    pub(super) fn atom(&mut self, token: &RegexToken) -> Result<(), Decline> {
        match &token.atom {
            a if is_consuming(a) => {
                let i = self.push_atom(a);
                self.ops.push(RxOp::Atom(i));
            }
            a if is_assertion(a) => {
                let i = self.push_atom(a);
                self.ops.push(RxOp::Assert(i));
            }
            RegexAtom::Group(p) => self.pattern(p)?,
            RegexAtom::CaptureGroup(p) => {
                // A body that captures gets a level of its own, so its
                // captures number from zero and become the group's sub-Match.
                // So does one with a backreference: a capture group is its own
                // capture scope, and `$0` / `$<x>` inside it do not see the
                // enclosing level's captures (`/ $<x>=(\w) ( $<x> ) /` fails).
                let nested = pattern_captures(p) || pattern_contains_backref(p);
                let start = self.reg();
                self.ops.push(RxOp::Mark(start));
                if nested {
                    self.ops.push(RxOp::OpenCapture);
                }
                let height = token.ratchet.then(|| self.reg());
                if let Some(h) = height {
                    self.ops.push(RxOp::Height(h));
                }
                self.pattern(p)?;
                if let Some(h) = height {
                    self.ops.push(RxOp::Cut(h));
                }
                self.ops.push(RxOp::CloseCapture { start, nested });
            }
            RegexAtom::Named(_) => return Err("subrule"),
            RegexAtom::Alternation(alts) => self.ltm_alternation(token, alts)?,
            RegexAtom::SequentialAlternation(alts) => self.seq_alternation(token, alts)?,
            RegexAtom::Lookaround { pattern, .. } => {
                // The walk's own lookaround test (`<?before …>`, `<!after …>`)
                // runs the body through `regex_match_end_from_caps_in_pkg`,
                // which answers from the body's own compiled program. Compile
                // the lookaround only when that program exists, so the body
                // never drops back to the walk in mid-program (D5).
                if super::rx_vm::program_for(pattern).is_none() {
                    return Err("lookaround-body");
                }
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::CapAtom(i));
            }
            RegexAtom::Backref(_)
            | RegexAtom::NamedBackref(_)
            | RegexAtom::CaptureStartMarker
            | RegexAtom::CaptureEndMarker => {
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::CapAtom(i));
            }
            RegexAtom::CodeAssertion { .. }
            | RegexAtom::ClosureInterpolation { .. }
            | RegexAtom::VarDecl { .. } => return Err("code"),
            RegexAtom::WsRule => return Err("ws-rule"),
            RegexAtom::CaptureIsolatedGroup(_) | RegexAtom::CaptureIsolatedGroupScoped(..) => {
                return Err("isolated-group");
            }
            RegexAtom::Conjunction(_) => return Err("conjunction"),
            RegexAtom::VarInterp(..)
            | RegexAtom::CodeInterp { .. }
            | RegexAtom::QqInterp { .. } => {
                return Err("interpolation");
            }
            RegexAtom::TildeMarker | RegexAtom::GoalMatch { .. } => return Err("goal-match"),
            RegexAtom::RecurseSelf(_) => return Err("recurse-self"),
            _ => return Err("other-atom"),
        }
        Ok(())
    }

    /// `x?`: the body first (greedy), the empty arm first (frugal), or the
    /// body committed to its first candidate with the empty arm only when it
    /// failed outright (ratchet). The matched arm applies the token's alias
    /// over what it matched; the empty arm reserves the atom's capture slots
    /// and applies the alias only where the walk does
    /// (`walk_zero_or_one_zero_arm`).
    fn zero_or_one(&mut self, token: &RegexToken) -> Result<(), Decline> {
        let (pos_base, start) = (self.reg(), self.reg());
        self.ops.push(RxOp::PosBase(pos_base));
        self.ops.push(RxOp::Mark(start));
        // Recorded before the split, so the ratchet's cut also drops the
        // empty arm once the body has matched.
        let height = token.ratchet.then(|| self.reg());
        if let Some(h) = height {
            self.ops.push(RxOp::Height(h));
        }
        let split = self.pc();
        self.ops.push(RxOp::Split { prefer: 0, alt: 0 }); // patched below
        let body = self.pc();
        self.atom(token)?;
        let tok = self.toks.len() as u32;
        self.toks.push(token.clone());
        if token.named_capture.is_some() {
            self.ops.push(RxOp::Named {
                tok,
                start,
                pos_base,
            });
        }
        if let Some(h) = height {
            self.ops.push(RxOp::Cut(h));
        }
        let join = self.pc();
        self.ops.push(RxOp::Jmp(0)); // patched below
        let zero = self.pc();
        self.ops.push(RxOp::ZeroArm { tok, pos_base });
        let end = self.pc();
        self.ops[join as usize] = RxOp::Jmp(end);
        self.ops[split as usize] = if token.frugal {
            RxOp::Split {
                prefer: zero,
                alt: body,
            }
        } else {
            RxOp::Split {
                prefer: body,
                alt: zero,
            }
        };
        Ok(())
    }

    /// `x*`, `x+`, `x ** min..max`. Ratchet is possessive and each iteration
    /// commits to the body's first candidate, as the walk's ratcheted chain
    /// does. A body that can match empty ends each iteration with a
    /// `ZeroIter` guard: an iteration that consumed nothing is accepted only
    /// while `zero_width_iter_counts` says it counts. Rejecting it retries
    /// the body's other candidates, which is the walk's group DFS
    /// (`walk_quant_group_candidates`); for the walk's chain, whose iterations
    /// take the first candidate only, the body is either ratcheted or has a
    /// single candidate, so the rejection stops the loop there instead.
    fn repeat(
        &mut self,
        token: &RegexToken,
        min: usize,
        max: Option<usize>,
    ) -> Result<(), Decline> {
        let nullable = atom_min_len(&token.atom) == 0;
        if nullable && !token.ratchet && !loop_body_backtracks(&token.atom) {
            // The walk's chain takes an iteration's first candidate only;
            // mirroring that needs a body with a single candidate.
            if !is_assertion(&token.atom) {
                return Err("nullable-loop");
            }
        }
        let (Ok(min), Ok(max)) = (u32::try_from(min), max.map_or(Ok(u32::MAX), u32::try_from))
        else {
            return Err("too-large");
        };
        let named = token.named_capture.is_some();
        if !nullable && is_consuming(&token.atom) && !token.frugal && !named {
            // A single one-grapheme atom needs no loop: the iterations are
            // scanned up front and given back from a position list.
            let atom = self.push_atom(&token.atom);
            self.ops.push(RxOp::AtomRun {
                atom,
                min,
                max,
                possessive: token.ratchet,
            });
            return Ok(());
        }
        // A body that captures folds its per-iteration slots into lists at
        // the loop's exit, after the names under it (and the token's own
        // alias) were marked quantified up front — `walk_quant_chain` /
        // `descend_folded`'s order. The alias itself is applied per
        // iteration, over that iteration's span, as `grow_one_iter` does.
        let fold = if named || atom_captures(&token.atom) {
            let pos_base = self.reg();
            self.ops.push(RxOp::PosBase(pos_base));
            let tok = self.toks.len() as u32;
            self.toks.push(token.clone());
            self.ops.push(RxOp::QuantNames { tok });
            Some((pos_base, tok))
        } else {
            None
        };
        let ctr = self.reg();
        self.ops.push(RxOp::CtrZero(ctr));
        let whole = token.ratchet.then(|| self.reg());
        if let Some(h) = whole {
            self.ops.push(RxOp::Height(h));
        }
        let head = self.pc();
        self.ops.push(RxOp::Jmp(0)); // patched below
        let body = self.pc();
        let iter_start = (nullable || named).then(|| self.reg());
        if let Some(r) = iter_start {
            self.ops.push(RxOp::Mark(r));
        }
        let iter = token.ratchet.then(|| self.reg());
        if let Some(h) = iter {
            self.ops.push(RxOp::Height(h));
        }
        let alt_body = atom_contains_alternation(&token.atom);
        self.quant_alt_depth += usize::from(alt_body);
        let body_result = self.atom(token);
        self.quant_alt_depth -= usize::from(alt_body);
        body_result?;
        if let Some(h) = iter {
            self.ops.push(RxOp::Cut(h));
        }
        if let (true, Some((pos_base, tok)), Some(start)) = (named, fold, iter_start) {
            self.ops.push(RxOp::Named {
                tok,
                start,
                pos_base,
            });
        }
        if let Some(start) = iter_start.filter(|_| nullable) {
            self.ops.push(RxOp::ZeroIter {
                ctr,
                start,
                min,
                max,
            });
        }
        self.ops.push(RxOp::CtrInc(ctr));
        self.ops.push(RxOp::Jmp(head));
        let exit = self.pc();
        self.ops[head as usize] = RxOp::Repeat {
            ctr,
            min,
            max,
            body,
            exit,
            greedy: !token.frugal,
        };
        if let Some(h) = whole {
            self.ops.push(RxOp::Cut(h));
        }
        if let Some((pos_base, tok)) = fold {
            self.ops.push(RxOp::Fold { tok, pos_base });
        }
        Ok(())
    }
}
