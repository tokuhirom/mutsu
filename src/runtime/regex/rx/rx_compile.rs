//! `RegexPattern` → [`RxProgram`] (ADR-0135 D1), for Slice A's regular core.
//!
//! Every construct outside the slice declines the whole pattern with a
//! reason; the caller keeps the tree walk for it (D5). The layout mirrors the
//! walk's priority order exactly, because the first complete match found is
//! the answer: a greedy quantifier tries the body before the exit, a frugal
//! one the exit first, and a ratchet cuts the choice points the walk would
//! never have revisited.

use super::{RxOp, RxProgram};
use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexQuant, RegexToken};

/// Why a pattern was not compiled. Reported per pattern under
/// `MUTSU_VM_STATS` (`regex-vm: … declined=(reason=count …)`).
pub(in crate::runtime::regex) type Decline = &'static str;

struct Compiler {
    ops: Vec<RxOp>,
    atoms: Vec<crate::runtime::regex_types::RegexAtom>,
    toks: Vec<RegexToken>,
    nregs: usize,
}

/// Compile `pattern`, or say why not.
// Cost: O(t), t = the number of tokens in the pattern tree.
pub(in crate::runtime::regex) fn compile(pattern: &RegexPattern) -> Result<RxProgram, Decline> {
    let mut c = Compiler {
        ops: Vec::new(),
        atoms: Vec::new(),
        toks: Vec::new(),
        nregs: 0,
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
        nregs: c.nregs,
    })
}

/// The one-grapheme atoms `match_consuming_atom` decides.
fn is_consuming(atom: &RegexAtom) -> bool {
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
            || match &t.atom {
                RegexAtom::CaptureGroup(_) => true,
                RegexAtom::Group(p) => pattern_captures(p),
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
            let atom = match &t.atom {
                a if is_consuming(a) => 1,
                RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => min_len(p),
                _ => 0,
            };
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
        if named && !matches!(token.quant, RegexQuant::One) {
            return Err("quantified-alias");
        }
        let body_captures = match &token.atom {
            RegexAtom::CaptureGroup(_) => true,
            RegexAtom::Group(p) => pattern_captures(p),
            _ => false,
        };
        if body_captures && !matches!(token.quant, RegexQuant::One) {
            return Err("quantified-capture");
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
            RegexQuant::ZeroOrOne => self.zero_or_one(token)?,
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
            RegexAtom::SequentialAlternation(_) => return Err("sequential-alternation"),
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

    /// `x?`: the body first (greedy), the empty arm first (frugal), or the
    /// body committed to its first candidate with the empty arm only when it
    /// failed outright (ratchet).
    fn zero_or_one(&mut self, token: &RegexToken) -> Result<(), Decline> {
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
        if let Some(h) = height {
            self.ops.push(RxOp::Cut(h));
        }
        let end = self.pc();
        self.ops[split as usize] = if token.frugal {
            RxOp::Split {
                prefer: end,
                alt: body,
            }
        } else {
            RxOp::Split {
                prefer: body,
                alt: end,
            }
        };
        Ok(())
    }

    /// `x*`, `x+`, `x ** min..max`. A body that can match empty would need
    /// the walk's zero-width iteration rule (`zero_width_iter_counts`), so
    /// Slice A declines it. Ratchet is possessive and each iteration commits
    /// to the body's first candidate, as the walk's ratcheted chain does.
    fn repeat(
        &mut self,
        token: &RegexToken,
        min: usize,
        max: Option<usize>,
    ) -> Result<(), Decline> {
        let body_min = match &token.atom {
            a if is_consuming(a) => 1,
            RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => min_len(p),
            _ => 0,
        };
        if body_min == 0 {
            return Err("nullable-loop");
        }
        let (Ok(min), Ok(max)) = (u32::try_from(min), max.map_or(Ok(u32::MAX), u32::try_from))
        else {
            return Err("too-large");
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
        let iter = token.ratchet.then(|| self.reg());
        if let Some(h) = iter {
            self.ops.push(RxOp::Height(h));
        }
        self.atom(token)?;
        if let Some(h) = iter {
            self.ops.push(RxOp::Cut(h));
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
        Ok(())
    }
}
