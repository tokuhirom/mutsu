//! `RegexPattern` → [`RxProgram`] (ADR-0135 D1), for Slice A's regular core.
//!
//! Every construct outside the slice declines the whole pattern with a
//! reason; the caller keeps the tree walk for it (D5). The layout mirrors the
//! walk's priority order exactly, because the first complete match found is
//! the answer: a greedy quantifier tries the body before the exit, a frugal
//! one the exit first, and a ratchet cuts the choice points the walk would
//! never have revisited.

use super::super::regex_helpers::{
    AlternationListFlags, alternation_list_flags, atom_contains_alternation,
};
use super::{RxOp, RxProgram};
use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexQuant, RegexToken};

/// Why a pattern was not compiled. Reported per pattern under
/// `MUTSU_VM_STATS` (`regex-vm: … declined=(reason=count …)`).
pub(in crate::runtime::regex) type Decline = &'static str;

struct Compiler {
    ops: Vec<RxOp>,
    atoms: Vec<crate::runtime::regex_types::RegexAtom>,
    toks: Vec<RegexToken>,
    alts: Vec<AlternationListFlags>,
    nregs: usize,
    /// How many enclosing quantified bodies contain an alternation: the walk
    /// matches those bodies with `IN_QUANTIFIED_ALTERNATION_MATCH` set, which
    /// turns off a `||` branch's positional padding.
    quant_alt_depth: usize,
}

/// Compile `pattern`, or say why not.
// Cost: O(t), t = the number of tokens in the pattern tree.
pub(in crate::runtime::regex) fn compile(pattern: &RegexPattern) -> Result<RxProgram, Decline> {
    let mut c = Compiler {
        ops: Vec::new(),
        atoms: Vec::new(),
        toks: Vec::new(),
        alts: Vec::new(),
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
        toks: c.toks,
        alts: c.alts,
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
    )
}

/// Does anything in `pattern` record a capture?
fn pattern_captures(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().any(|t| {
        t.named_capture.is_some()
            || t.hash_capture.is_some()
            || t.secondary_named_capture.is_some()
            || atom_captures(&t.atom)
    })
}

/// Does matching `atom` itself record a capture (its token's alias aside)?
fn atom_captures(atom: &RegexAtom) -> bool {
    match atom {
        RegexAtom::CaptureGroup(_) => true,
        RegexAtom::Group(p) => pattern_captures(p),
        RegexAtom::SequentialAlternation(alts) => alts.iter().any(pattern_captures),
        _ => false,
    }
}

/// Is any token under `pattern` a numbered alias (`$0=…`)? The walk matches a
/// `||` branch in a capture scope of its own, so such an alias there numbers
/// from the branch's start, not from the enclosing level's.
fn has_numbered_alias(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().any(|t| {
        t.named_capture
            .as_ref()
            .is_some_and(|n| n.parse::<usize>().is_ok())
            || match &t.atom {
                RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => has_numbered_alias(p),
                RegexAtom::SequentialAlternation(alts) => alts.iter().any(has_numbered_alias),
                _ => false,
            }
    })
}

/// The fewest characters any match of `pattern` consumes (for the patterns
/// this compiler accepts).
fn min_len(pattern: &RegexPattern) -> usize {
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
        RegexAtom::SequentialAlternation(alts) => alts.iter().map(min_len).min().unwrap_or(0),
        _ => 0,
    }
}

/// Can a quantified `atom`'s captures be folded one level deep, the way the
/// walk's `fold_quantified` does? True for `( … )` whose body captures
/// nothing, and for `[ … ]` whose tokens are capture-free or exactly such a
/// non-quantified, unaliased `( … )`.
fn flat_captures(atom: &RegexAtom) -> bool {
    match atom {
        RegexAtom::CaptureGroup(p) => !pattern_captures(p),
        RegexAtom::Group(p) => p.tokens.iter().all(|t| {
            let plain = t.named_capture.is_none()
                && t.hash_capture.is_none()
                && t.secondary_named_capture.is_none();
            match &t.atom {
                RegexAtom::CaptureGroup(inner) => {
                    plain && matches!(t.quant, RegexQuant::One) && !pattern_captures(inner)
                }
                a => plain && !atom_captures(a),
            }
        }),
        _ => false,
    }
}

impl Compiler {
    fn reg(&mut self) -> u16 {
        self.nregs += 1;
        (self.nregs - 1) as u16
    }

    fn pc(&self) -> u32 {
        self.ops.len() as u32
    }

    fn pattern(&mut self, pattern: &RegexPattern) -> Result<(), Decline> {
        if pattern.ignore_case {
            return Err("ignorecase");
        }
        if pattern.ignore_mark {
            return Err("ignoremark");
        }
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
        if token.separator.is_some() {
            return Err("separator");
        }
        if token.hash_capture.is_some() {
            return Err("hash-capture");
        }
        if token.secondary_named_capture.is_some() || token.force_list_capture {
            return Err("alias-form");
        }
        if token.frugal && token.ratchet {
            return Err("frugal-ratchet");
        }
        let named = token.named_capture.is_some();
        let optional = matches!(token.quant, RegexQuant::ZeroOrOne);
        if named && !matches!(token.quant, RegexQuant::One) && !optional {
            return Err("quantified-alias");
        }
        if atom_captures(&token.atom)
            && !matches!(token.quant, RegexQuant::One)
            && !flat_captures(&token.atom)
        {
            // A quantified body whose captures nest (`( (a) )+`, `[ $<x>=[..] ]*`)
            // needs the walk's nested fold; Slice A folds one level only.
            return Err("quantified-nested-capture");
        }
        if optional {
            // `?` applies its alias on the matched arm only; see `zero_or_one`.
            return self.zero_or_one(token);
        }
        let alias = if named {
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
    fn atom(&mut self, token: &RegexToken) -> Result<(), Decline> {
        match &token.atom {
            a if is_consuming(a) => {
                let i = self.atoms.len() as u32;
                self.atoms.push(a.clone());
                self.ops.push(RxOp::Atom(i));
            }
            a if is_assertion(a) => {
                let i = self.atoms.len() as u32;
                self.atoms.push(a.clone());
                self.ops.push(RxOp::Assert(i));
            }
            RegexAtom::Group(p) => self.pattern(p)?,
            RegexAtom::CaptureGroup(p) => {
                if pattern_captures(p) {
                    return Err("nested-capture");
                }
                let start = self.reg();
                self.ops.push(RxOp::Mark(start));
                let height = token.ratchet.then(|| self.reg());
                if let Some(h) = height {
                    self.ops.push(RxOp::Height(h));
                }
                self.pattern(p)?;
                if let Some(h) = height {
                    self.ops.push(RxOp::Cut(h));
                }
                self.ops.push(RxOp::CloseCapture { start });
            }
            RegexAtom::CompositeClass { .. } => return Err("composite-class"),
            RegexAtom::Named(_) => return Err("subrule"),
            RegexAtom::Alternation(_) => return Err("alternation"),
            RegexAtom::SequentialAlternation(alts) => self.seq_alternation(token, alts)?,
            RegexAtom::Lookaround { .. } => return Err("lookaround"),
            RegexAtom::Backref(_) | RegexAtom::NamedBackref(_) => return Err("backref"),
            RegexAtom::CodeAssertion { .. }
            | RegexAtom::ClosureInterpolation { .. }
            | RegexAtom::VarDecl { .. } => return Err("code"),
            RegexAtom::CaptureStartMarker | RegexAtom::CaptureEndMarker => {
                return Err("capture-marker");
            }
            _ => return Err("other-atom"),
        }
        Ok(())
    }

    /// `a || b || c`, as `walk_seq_alternation` drives it: every way branch
    /// *k* can match is tried against the rest of the pattern before branch
    /// *k+1* is entered. Each branch ends with an `AltTail` that pads the
    /// alternation's positional slot space and marks its list-valued names
    /// (`alternation_branch_delta`). Under ratchet the alternation commits to
    /// the first branch that matches and to that branch's first end; the walk
    /// moves past a branch whose every end is zero-width, so a ratcheted
    /// alternation with such a branch (other than the last) is declined.
    fn seq_alternation(
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
        if !nullable && is_consuming(&token.atom) && !token.frugal {
            // A single one-grapheme atom needs no loop: the iterations are
            // scanned up front and given back from a position list.
            let atom = self.atoms.len() as u32;
            self.atoms.push(token.atom.clone());
            self.ops.push(RxOp::AtomRun {
                atom,
                min,
                max,
                possessive: token.ratchet,
            });
            return Ok(());
        }
        // A body that captures folds its per-iteration slots into lists at
        // the loop's exit, after the names under it were marked quantified
        // up front — `walk_quant_chain` / `descend_folded`'s order.
        let fold = if flat_captures(&token.atom) {
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
        let iter_start = nullable.then(|| self.reg());
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
        if let Some(start) = iter_start {
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
