//! Choice points and subrule call frames of the compiled engine (ADR-0135 D2,
//! D3).
//!
//! A `<subrule>` call that resolves to a plain rule with a compiled program
//! pushes a [`Frame`] and runs the callee in the same loop, on the same
//! backtrack stack. Frames are persistent (never mutated except for the
//! end-dedup list and the cursor), and every choice point records the frame it
//! was pushed in. So a failure after the callee has returned resumes *inside*
//! the callee with the right chain of callers behind it: Rakudo's bstack model,
//! which is what a non-ratchet callee needs. A ratcheted call (`commit`)
//! simply drops the callee's choice points when it returns, so nothing ever
//! resumes in it.
//!
//! Frames live in one arena (`Scratch::frames`) and link to their caller by
//! index (ADR-10488 D4): a call allocated an `Rc` per frame. A frame is pushed
//! when its call is entered, and the arena is truncated where no choice point
//! can still name what is cut: to the length a choice point recorded when it
//! is resumed, and to a frame's own index when it returns leaving no choice
//! point in its callee (everything it called has returned the same way).
//!
//! The registers of a frame are a window of one arena (`Scratch::regs`) that
//! starts at the frame's `base`. A window is never reused while a choice point
//! may resume in it: the arena is truncated only to the length a choice point
//! recorded when it was pushed, after the register trail has been rewound.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

/// A frame's index in the arena.
pub(super) type FrameId = u32;

use super::super::regex_token_candidates::TokenCandidates;
use super::RxProgram;
use crate::runtime::regex_types::RegexCaptures;
use crate::symbol::Symbol;
use crate::value::Value;

/// One active `<subrule>` call.
pub(super) struct Frame {
    /// The caller's frame; `None` is the pattern the run started from.
    pub(super) parent: Option<FrameId>,
    /// The callee's program, and the package its body matches in (a rule is
    /// matched in the package that defines it).
    pub(super) program: Arc<RxProgram>,
    pub(super) pkg: Symbol,
    /// Where the callee's register window starts in the arena.
    pub(super) base: usize,
    /// The caller's pc to continue at when the callee returns.
    pub(super) ret_pc: u32,
    /// Where the call started: the callee's Match starts here.
    pub(super) entry_pos: usize,
    /// The call's `Named` atom, in the caller's program.
    pub(super) site: u32,
    /// The call's token is ratcheted: the first end is the only one.
    pub(super) commit: bool,
    /// The backtrack-stack height when the call was made.
    pub(super) stack_base: usize,
    /// Where the capture journal, the register trail and the `AtomRun` ends
    /// stood at the call: a return that leaves no choice point in the callee
    /// forgets everything above them, since nothing can resume there.
    pub(super) journal_base: usize,
    pub(super) trail_base: usize,
    pub(super) ends_base: usize,
    /// A proto candidate's call: the candidates and which one this frame runs,
    /// whose `:sym<…>` the callee's Match carries.
    pub(super) proto: Option<(Arc<TokenCandidates>, usize)>,
    /// How many frames deep this call is.
    pub(super) depth: u32,
    /// The grammar instance this invocation owns (Rakudo's cursor), created when
    /// a call in the callee first runs a grammar method (#9803). The return files
    /// it on the callee's Match.
    pub(super) cursor: RefCell<Option<Value>>,
    /// The call's binding window (`rx_scope`'s handle), installed while the
    /// callee runs and uninstalled by its return.
    pub(super) window: Option<usize>,
    /// The frame runs the pattern a `$( … )` / `@( … )` yielded (`site` is
    /// that `CodeInterp` atom): its return drops the callee's captures, as
    /// rakudo keeps none of an interpolated regex's, instead of filing a
    /// subrule Match.
    pub(super) interp: bool,
}

/// Calls nested deeper than this fail: a rule that re-enters itself without
/// consuming input (left recursion the call graph did not name) would
/// otherwise grow the arena without bound.
pub(super) const MAX_FRAME_DEPTH: u32 = 100_000;

/// What every choice point restores besides its own resume point: how far to
/// rewind the capture journal and the register trail.
#[derive(Clone, Copy)]
pub(super) struct Mark {
    /// The capture-level journal length (`Levels::mark`).
    pub(super) cap: usize,
    /// The register-trail length.
    pub(super) reg: usize,
}

/// What a choice point pushed inside a callee also restores. A run that never
/// calls has none of these: `Scratch::fmarks` holds one only for a choice point
/// pushed while a callee frame, or a callee's register window, is live.
pub(super) struct FMark {
    /// The choice point's index in the backtrack stack.
    pub(super) at: usize,
    /// The register arena length: windows above it were opened after this
    /// choice point and are dead once it resumes.
    pub(super) regs_len: usize,
    /// The frame arena length, for the same reason.
    pub(super) frames_len: usize,
    /// The frame the choice point was pushed in.
    pub(super) frame: Option<FrameId>,
}

/// The next candidate of a proto call, in the order the call ranked them:
/// `cands[ranked[next]]` runs as a frame, called at `pos` and returning to `pc`,
/// once the one before it failed. A candidate that returns commits the call,
/// which drops this entry with the rest.
pub(super) struct ProtoChoice {
    pub(super) pc: u32,
    pub(super) pos: usize,
    pub(super) atom: u32,
    pub(super) cands: Arc<TokenCandidates>,
    pub(super) ranked: Rc<[usize]>,
    pub(super) next: usize,
    pub(super) mark: Mark,
    /// The call's binding window, which every candidate runs in.
    pub(super) window: Option<usize>,
    /// The call site is ratcheted: a candidate keeps only its first end.
    pub(super) commit: bool,
}

/// A point to resume from on failure.
pub(super) enum Choice {
    /// A choice point that was given up (`GoalOk`): popping it fails on.
    Dead,
    /// Resume at `pc` with the cursor at `pos`.
    At { pc: u32, pos: usize, mark: Mark },
    /// An `AtomRun`'s give-back: resume at `pc` from `ends[hi - 1]`, one
    /// iteration shorter each time, while `hi > lo`; `ends` is truncated back
    /// to `base` once the run is exhausted.
    Run {
        pc: u32,
        base: usize,
        lo: usize,
        hi: usize,
        mark: Mark,
    },
    /// The next candidate of a proto call (boxed: it is the one wide variant, and
    /// every choice point pays for the widest).
    Proto(Box<ProtoChoice>),
    /// The remaining candidates of an `InterpEnds` or a bridged `Call`:
    /// `cands[left - 1]` (end and capture delta) is next, then the ones below
    /// it. `cands` is lowest priority first.
    Cands {
        pc: u32,
        cands: Rc<Vec<(usize, RegexCaptures)>>,
        left: usize,
        mark: Mark,
    },
}
