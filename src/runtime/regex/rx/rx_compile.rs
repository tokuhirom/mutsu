//! `RegexPattern` → [`RxProgram`] (ADR-0135 D1), for Slice A's regular core.
//!
//! Every construct outside the slice declines the whole pattern with a
//! reason; the caller keeps the tree walk for it (D5). The layout mirrors the
//! walk's priority order exactly, because the first complete match found is
//! the answer: a greedy quantifier tries the body before the exit, a frugal
//! one the exit first, and a ratchet cuts the choice points the walk would
//! never have revisited.

use super::super::regex_helpers::{
    AlternationListFlags, atom_contains_alternation, atom_contains_backref, count_capture_groups,
};
use super::super::regex_match_plain_view::plain_iter_needs_view;
use super::{RxOp, RxProgram};
use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexQuant, RegexToken};

/// Why a pattern was not compiled. Reported per pattern under
/// `MUTSU_VM_STATS` (`regex-vm: … declined=(reason=count …)`).
pub(in crate::runtime::regex) type Decline = &'static str;

/// A quantifier's iteration bounds: known at compile time, or read from the
/// registers a `RepeatCount` filled when the quantifier was reached.
#[derive(Clone, Copy)]
enum Bounds {
    /// `min`, and `max` (`u32::MAX`: no bound).
    Fixed(u32, u32),
    /// The registers holding `min` and `max` (`usize::MAX`: no bound).
    Dyn(u16, u16),
}

pub(super) struct Compiler {
    pub(super) ops: Vec<RxOp>,
    pub(super) atoms: Vec<crate::runtime::regex_types::RegexAtom>,
    /// Per atom: its pattern level's `:i`.
    pub(super) atom_ic: Vec<bool>,
    /// The `:i` of the pattern level being compiled.
    ignore_case: bool,
    pub(super) toks: Vec<RegexToken>,
    pub(super) alts: Vec<AlternationListFlags>,
    pub(super) name_sets: Vec<Box<[crate::symbol::Symbol]>>,
    pub(super) zero_arms: Vec<super::ZeroArmPlan>,
    pub(super) ltm_alts: Vec<super::LtmAltTable>,
    pub(super) nregs: usize,
    /// Set when a `Code` or `VarDecl` op is emitted.
    has_code: bool,
    /// Set when a `Call` op is emitted.
    has_call: bool,
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
        name_sets: Vec::new(),
        zero_arms: Vec::new(),
        ltm_alts: Vec::new(),
        nregs: 0,
        has_code: false,
        has_call: false,
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
        name_sets: c.name_sets,
        zero_arms: c.zero_arms,
        ltm_alts: c.ltm_alts,
        nregs: c.nregs,
        has_code: c.has_code,
        has_call: c.has_call,
        ascii: std::sync::OnceLock::new(),
    })
}

/// A set of capture names interned, in name order, so the order they are
/// marked in does not depend on a hash seed.
fn sorted_symbols(names: std::collections::HashSet<String>) -> Box<[crate::symbol::Symbol]> {
    let mut names: Vec<String> = names.into_iter().collect();
    names.sort_unstable();
    names
        .iter()
        .map(|n| crate::symbol::Symbol::intern(n))
        .collect()
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
        // A subrule call files its own Match under its name (or, silent, under
        // the hidden marker when it has nested captures or an action).
        RegexAtom::CaptureGroup(_) | RegexAtom::Named(_) => true,
        RegexAtom::Group(p) => pattern_captures(p),
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => alts.iter().any(pattern_captures),
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

/// Does `pattern` hold a code atom (`{ … }`, `<?{ … }>`, `:my …;`) at its own
/// capture level — that is, not inside a lookaround, which matches in a nested
/// run of its own?
pub(super) fn pattern_contains_code(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().any(|t| {
        matches!(t.quant, RegexQuant::RepeatCode(_))
            || t.separator
                .as_ref()
                .is_some_and(|sep| pattern_contains_code(&sep.pattern))
            || match &t.atom {
                RegexAtom::CodeAssertion { .. }
                | RegexAtom::VarDecl { .. }
                | RegexAtom::ClosureInterpolation { .. }
                | RegexAtom::CodeInterp { .. } => true,
                RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => pattern_contains_code(p),
                RegexAtom::Alternation(alts)
                | RegexAtom::SequentialAlternation(alts)
                | RegexAtom::Conjunction(alts) => alts.iter().any(pattern_contains_code),
                _ => false,
            }
    })
}

/// Does `pattern` run code or read an in-regex lexical (`$x`) at its own
/// capture level? Either needs the enclosing level's view of the match, which a
/// nested run of its own would not have.
pub(super) fn pattern_reads_enclosing_state(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().any(|t| {
        matches!(t.quant, RegexQuant::RepeatCode(_))
            || t.separator
                .as_ref()
                .is_some_and(|sep| pattern_reads_enclosing_state(&sep.pattern))
            || match &t.atom {
                RegexAtom::CodeAssertion { .. }
                | RegexAtom::VarDecl { .. }
                | RegexAtom::ClosureInterpolation { .. }
                | RegexAtom::CodeInterp { .. }
                | RegexAtom::VarInterp(_) => true,
                RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => {
                    pattern_reads_enclosing_state(p)
                }
                RegexAtom::Alternation(alts)
                | RegexAtom::SequentialAlternation(alts)
                | RegexAtom::Conjunction(alts) => alts.iter().any(pattern_reads_enclosing_state),
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
    matches!(
        atom,
        RegexAtom::Group(_) | RegexAtom::CaptureGroup(_) | RegexAtom::CaptureIsolatedGroup(_)
    ) || atom_contains_alternation(atom)
}

/// The fewest characters one match of `atom` consumes.
fn atom_min_len(atom: &RegexAtom) -> usize {
    match atom {
        a if is_consuming(a) => 1,
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) | RegexAtom::CaptureIsolatedGroup(p) => {
            min_len(p)
        }
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
            alts.iter().map(min_len).min().unwrap_or(0)
        }
        // Every branch matches the same span, so the longest minimum binds.
        RegexAtom::Conjunction(alts) => alts.iter().map(min_len).max().unwrap_or(0),
        _ => 0,
    }
}

impl Compiler {
    pub(super) fn reg(&mut self) -> u16 {
        self.nregs += 1;
        (self.nregs - 1) as u16
    }

    /// Intern a set of capture names into `name_sets` ([`sorted_symbols`]).
    pub(super) fn name_set(&mut self, names: std::collections::HashSet<String>) -> u32 {
        self.name_sets.push(sorted_symbols(names));
        (self.name_sets.len() - 1) as u32
    }

    /// The names under the quantified `token`, interned (`name_set`).
    pub(super) fn quantified_names(&mut self, token: &RegexToken) -> u32 {
        self.name_set(crate::runtime::Interpreter::collect_quantified_names_for_token(token))
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
            RegexQuant::RepeatCode(_) => self.repeat_code(token)?,
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
        // Consumed here, so the atoms of a group nested under this one do not
        // inherit it.
        match &token.atom {
            a if is_consuming(a) => {
                let i = self.push_atom(a);
                self.ops.push(RxOp::Atom(i));
            }
            a if is_assertion(a) => {
                let i = self.push_atom(a);
                self.ops.push(RxOp::Assert(i));
            }
            // `[:m …]`: the walk matches the body against the mark-stripped
            // subject and maps its ends back (`ignoremark_on_target`), every
            // end up front, so the op asks the same entry for them and enters
            // them highest priority first; under ratchet it commits to the
            // first. Code or a backreference in the body would read the
            // enclosing level through the walk's inline seeds, which the
            // nested run does not arm.
            RegexAtom::Group(p) if p.ignore_mark => {
                if pattern_contains_code(p) || pattern_contains_backref(p) {
                    return Err("ignoremark-code");
                }
                let height = token.ratchet.then(|| self.reg());
                if let Some(h) = height {
                    self.ops.push(RxOp::Height(h));
                }
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::GroupEnds(i));
                if let Some(h) = height {
                    self.ops.push(RxOp::Cut(h));
                }
            }
            RegexAtom::Group(p) => self.pattern(p)?,
            RegexAtom::CaptureGroup(p) => {
                // A body that captures gets a level of its own, so its
                // captures number from zero and become the group's sub-Match.
                // So does one with a backreference: a capture group is its own
                // capture scope, and `$0` / `$<x>` inside it do not see the
                // enclosing level's captures (`/ $<x>=(\w) ( $<x> ) /` fails).
                // So does one with code: `$/` inside a capture group's block is
                // the group's own match so far, and `$0` its own first capture.
                let nested =
                    pattern_captures(p) || pattern_contains_backref(p) || pattern_contains_code(p);
                let start = self.reg();
                self.ops.push(RxOp::Mark(start));
                if nested {
                    self.ops.push(RxOp::OpenCapture);
                }
                let height = token.ratchet.then(|| self.reg());
                if let Some(h) = height {
                    self.ops.push(RxOp::Height(h));
                }
                if p.ignore_mark {
                    // `(:m …)`: the body's ends come from the mark-stripped
                    // subject, as for `[:m …]` (`GroupEnds`), into the
                    // capture's own level.
                    if pattern_contains_code(p) || pattern_contains_backref(p) {
                        return Err("ignoremark-code");
                    }
                    let i = self.push_atom(&RegexAtom::Group(p.clone()));
                    self.ops.push(RxOp::GroupEnds(i));
                } else {
                    self.pattern(p)?;
                }
                if let Some(h) = height {
                    self.ops.push(RxOp::Cut(h));
                }
                self.ops.push(RxOp::CloseCapture { start, nested });
            }
            RegexAtom::Named(_) => {
                let i = self.push_atom(&token.atom);
                // A quantified call (`<x>*`) is the same frame call per
                // iteration: committed under ratchet, and otherwise resumable,
                // so a later failure backtracks into an iteration's callee as
                // in rakudo (`regex r { <x>+ a }; regex x { a+ }` on `aaa`).
                // The walk's chain took each iteration's first end only.
                self.ops.push(RxOp::Call {
                    atom: i,
                    commit: token.ratchet,
                });
                self.has_call = true;
                // The callee may run code, read lexicals or capture: the
                // position-only matcher must not run this program.
                self.has_code = true;
            }
            RegexAtom::Alternation(alts) => self.ltm_alternation(token, alts)?,
            RegexAtom::SequentialAlternation(alts) => self.seq_alternation(token, alts)?,
            RegexAtom::Lookaround { pattern, .. } => {
                // The walk's own lookaround test (`<?before …>`, `<!after …>`)
                // runs the body through `regex_match_end_from_caps_in_pkg`,
                // which answers from the body's own compiled program. Compile
                // the lookaround only when that program exists, so the body
                // never drops back to the walk in mid-program (D5).
                let Some(body) = super::rx_entry::program_for(pattern) else {
                    return Err("lookaround-body");
                };
                // The body runs code of its own in a nested run.
                self.has_code |= body.has_code;
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
            RegexAtom::CodeAssertion { .. } => {
                // A call-out: the code runs on the caller's interpreter where
                // the cursor reaches it (ADR-0135 D4, ADR-0009). Nothing is
                // precomputed, so a code atom never runs speculatively.
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::Code(i));
                self.has_code = true;
            }
            RegexAtom::VarDecl { .. } => {
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::VarDecl(i));
                self.has_code = true;
            }
            RegexAtom::ClosureInterpolation { .. } => {
                // `<{ … }>`: the code yields a pattern that is matched here, its
                // first match only (the walk's own single-candidate arm, which
                // `CapAtom` calls).
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::CapAtom(i));
                self.has_code = true;
            }
            // `:sigspace`'s `<.ws>`: one candidate, captures nothing (a
            // wrapped `ws` method is dispatched by the op itself).
            RegexAtom::WsRule => self.ops.push(RxOp::Ws),
            RegexAtom::CaptureIsolatedGroup(p) => {
                // `<$rx>` and friends: the body is a regex of its own, matched in
                // a level whose captures are dropped when it closes
                // (`GroupShape::Isolated`). Under ratchet the group commits to
                // its first end, as a capturing group does.
                self.ops.push(RxOp::OpenIsolated);
                let height = token.ratchet.then(|| self.reg());
                if let Some(h) = height {
                    self.ops.push(RxOp::Height(h));
                }
                self.pattern(p)?;
                if let Some(h) = height {
                    self.ops.push(RxOp::Cut(h));
                }
                self.ops.push(RxOp::DropCapture);
            }
            // A spliced Regex value that closed over its own scope: the body is
            // an isolated group, run with that scope installed. The install is
            // an op pair whose effects backtracking undoes and redoes
            // (`rx_scope`), so the body is matched lazily like any other.
            RegexAtom::CaptureIsolatedGroupScoped(p, _) => {
                let slot = self.reg();
                let atom = self.push_atom(&token.atom);
                self.ops.push(RxOp::ScopeEnter { atom, slot });
                self.ops.push(RxOp::OpenIsolated);
                let height = token.ratchet.then(|| self.reg());
                if let Some(h) = height {
                    self.ops.push(RxOp::Height(h));
                }
                self.pattern(p)?;
                if let Some(h) = height {
                    self.ops.push(RxOp::Cut(h));
                }
                self.ops.push(RxOp::DropCapture);
                self.ops.push(RxOp::ScopeExit { slot });
                self.has_code = true;
            }
            RegexAtom::Conjunction(branches) => self.conjunction(token, branches)?,
            RegexAtom::VarInterp(_) => {
                // `$x` of an in-regex `:my` lexical (or an outer one): the
                // value is read from the level's lexicals when the atom is
                // matched, and matched as a literal.
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::CapAtom(i));
                self.has_code = true;
            }
            RegexAtom::CodeInterp { .. } => {
                // `$( … )` / `@( … )`: the code yields a pattern (or a list of
                // them) matched here. The walk asks for every end up front, so
                // the op does too and enters them highest priority first; under
                // ratchet the atom commits to the first.
                let height = token.ratchet.then(|| self.reg());
                if let Some(h) = height {
                    self.ops.push(RxOp::Height(h));
                }
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::InterpEnds(i));
                if let Some(h) = height {
                    self.ops.push(RxOp::Cut(h));
                }
                self.has_code = true;
            }
            RegexAtom::QqInterp { .. } => {
                // A `"…"` atom whose interpolations a thunk resolved at rule
                // entry: the result is read from the environment, so there is
                // no code to run here; without one, the fallback pattern is
                // matched (the walk's own single-candidate arm).
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::CapAtom(i));
            }
            RegexAtom::GoalMatch { goal, inner, .. } => self.goal_match(token, goal, inner)?,
            RegexAtom::TildeMarker => return Err("goal-match"),
            RegexAtom::RecurseSelf(_) => {
                // `<~~>`: the enclosing regex's first end at the cursor, its
                // captures discarded, guarded against re-entry at the same
                // position (`regex_match_recurse_self`). That leaf matches the
                // regex through `regex_match_end_from_caps_in_pkg`, which
                // answers from its compiled program, so no walk is entered.
                let i = self.push_atom(&token.atom);
                self.ops.push(RxOp::CapAtom(i));
                // The recursion runs the whole regex, code included.
                self.has_code = true;
            }
            _ => return Err("other-atom"),
        }
        Ok(())
    }

    /// `x?`: the body first (greedy), the empty arm first (frugal), or the
    /// body committed to its first candidate with the empty arm only when it
    /// failed outright (ratchet). A frugal `??` under ratchet still tries the
    /// empty arm first, and then the body committed to its first candidate:
    /// the cut's height is taken before the split, which the empty arm has
    /// already consumed by then. The matched arm applies the token's alias
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
        let mut list_names = std::collections::HashSet::new();
        crate::runtime::Interpreter::collect_nested_list_quantified_names(
            &token.atom,
            &mut list_names,
        );
        let plan = self.zero_arms.len() as u32;
        self.zero_arms.push(super::ZeroArmPlan {
            flags: super::super::regex_helpers::capture_group_list_flags(token, false)
                .into_boxed_slice(),
            list_names: sorted_symbols(list_names),
            named_zero_capture: !matches!(
                token.atom,
                RegexAtom::CaptureGroup(_) | RegexAtom::Named(_)
            ) && !token.subrule_call_capture,
        });
        self.ops.push(RxOp::ZeroArm {
            tok,
            pos_base,
            plan,
        });
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
        let (Ok(min), Ok(max)) = (u32::try_from(min), max.map_or(Ok(u32::MAX), u32::try_from))
        else {
            return Err("too-large");
        };
        self.repeat_bounded(token, Bounds::Fixed(min, max))
    }

    /// `x ** { code }`: the walk evaluates the count where the quantifier is
    /// reached, before it marks the names under it, so `RepeatCount` comes first
    /// and the loop reads its bounds from registers. A body that can match empty
    /// declines: its `ZeroIter` guard is built from static bounds.
    fn repeat_code(&mut self, token: &RegexToken) -> Result<(), Decline> {
        if atom_min_len(&token.atom) == 0 {
            return Err("repeat-code-nullable");
        }
        let (min, max) = (self.reg(), self.reg());
        let tok = self.toks.len() as u32;
        self.toks.push(token.clone());
        self.ops.push(RxOp::RepeatCount { tok, min, max });
        self.has_code = true;
        self.repeat_bounded(token, Bounds::Dyn(min, max))
    }

    fn repeat_bounded(&mut self, token: &RegexToken, bounds: Bounds) -> Result<(), Decline> {
        let nullable = atom_min_len(&token.atom) == 0;
        // The walk's chain takes an iteration's first candidate only. A body
        // with a single candidate (an assertion, a `<subrule>`) needs nothing
        // more; any other nullable body the chain grows is committed per
        // iteration below, so a `ZeroIter` rejection stops the loop there
        // instead of retrying the body's other candidates. (Rakudo has no
        // answer to compare with here: it loops forever on `[a?]* b`.)
        let nullable_chain = nullable
            && !token.ratchet
            && !loop_body_backtracks(&token.atom)
            && !is_assertion(&token.atom)
            && !matches!(token.atom, RegexAtom::Named(_));
        let named = token.named_capture.is_some();
        if let (Bounds::Fixed(min, max), true) = (
            bounds,
            !nullable && is_consuming(&token.atom) && !token.frugal && !named,
        ) {
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
            let names = self.quantified_names(token);
            self.ops.push(RxOp::QuantNames { names });
            Some((pos_base, tok))
        } else {
            None
        };
        let ctr = self.reg();
        self.ops.push(RxOp::CtrZero(ctr));
        // Ratchet makes a greedy loop possessive. A frugal one keeps growing
        // on demand under ratchet too (raku grows `\S+?` inside a `token`):
        // only its iterations commit.
        let whole = (token.ratchet && !token.frugal).then(|| self.reg());
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
        // Ratchet commits each iteration to the body's first candidate. So
        // does the walk's chain (`grow_one_iter` takes the single-candidate
        // matcher) for a body the compiled form could otherwise re-enter: a
        // conjunction, whose first branch can end elsewhere.
        let chain_commit = nullable_chain
            || (!loop_body_backtracks(&token.atom)
                && matches!(token.atom, RegexAtom::Conjunction(_)));
        let iter = (token.ratchet || chain_commit).then(|| self.reg());
        if let Some(h) = iter {
            self.ops.push(RxOp::Height(h));
        }
        let alt_body = atom_contains_alternation(&token.atom);
        // Code in the body sees the iterations so far folded.
        let iter_level =
            fold.filter(|_| plain_iter_needs_view(&token.atom, count_capture_groups(token)));
        if let Some((pos_base, tok)) = iter_level {
            self.ops.push(RxOp::OpenPlainIter { tok, pos_base });
        }
        self.quant_alt_depth += usize::from(alt_body);
        let body_result = self.atom(token);
        self.quant_alt_depth -= usize::from(alt_body);
        body_result?;
        if iter_level.is_some() {
            self.ops.push(RxOp::ClosePlainIter);
        }
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
        if let (Some(start), Bounds::Fixed(min, max)) = (iter_start.filter(|_| nullable), bounds) {
            self.ops.push(RxOp::ZeroIter {
                ctr,
                start,
                min,
                max,
            });
        }
        // The walk runs a committed `*` / `+` iteration's subrule action here
        // when a `$*` variable it may write is read (`grow_one_iter`); `**`
        // does not.
        if matches!(token.atom, RegexAtom::Named(_))
            && matches!(token.quant, RegexQuant::ZeroOrMore | RegexQuant::OneOrMore)
        {
            let tok = self.toks.len() as u32;
            self.toks.push(token.clone());
            self.ops.push(RxOp::ReduceAction { tok });
        }
        self.ops.push(RxOp::CtrInc(ctr));
        self.ops.push(RxOp::Jmp(head));
        let exit = self.pc();
        self.ops[head as usize] = match bounds {
            Bounds::Fixed(min, max) => RxOp::Repeat {
                ctr,
                min,
                max,
                body,
                exit,
                greedy: !token.frugal,
            },
            Bounds::Dyn(min, max) => RxOp::RepeatDyn {
                ctr,
                min,
                max,
                body,
                exit,
                greedy: !token.frugal,
            },
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
