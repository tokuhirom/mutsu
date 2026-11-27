//! The `EVAL`s running on top of the routine stack, as backtrace frames.
//!
//! An `EVAL`'d string is a compilation unit of its own (`EVAL_<n>`), and
//! rakudo's backtrace shows its mainline as a `<unit>` frame between the code
//! the snippet called and the frame that called `EVAL`. mutsu runs the snippet
//! on the caller's frame (`EVAL` pushes no `RoutineFrame`: a real frame would
//! change what `callframe`, `CALLER::`, `self` and `return` see inside it), so
//! the snippet's frame is recorded here instead, beside the stack, and spliced
//! into a backtrace when one is rendered ([#19242]).
//!
//! Without it the snippet's line was reported under its caller's frame, and
//! under the caller's *file* when that caller was a module routine -- so code
//! that walks `.backtrace` for the `EVAL_<n>` frame to locate an error inside
//! the snippet (a REPL) found none.
//!
//! [#19242]: https://github.com/tokuhirom/mutsu/issues/19242

use super::RoutineFrame;
use crate::symbol::Symbol;

/// One running `EVAL`: the routine-stack depth it was entered at, and the
/// synthetic `<unit>` frame standing for its compilation unit.
#[derive(Clone, Copy, Debug)]
pub(crate) struct EvalFrame {
    pub(crate) depth: usize,
    pub(crate) frame: RoutineFrame,
}

/// The running `EVAL`s, innermost last.
#[derive(Clone, Debug, Default)]
pub(crate) struct EvalFrames(Vec<EvalFrame>);

impl EvalFrames {
    /// Enter an `EVAL` of `unit`, called from `line` of `file` while the
    /// routine stack is `depth` deep. Returns the count to [`Self::leave`] to.
    // Cost: O(1) amortized.
    pub(crate) fn enter(
        &mut self,
        depth: usize,
        unit: Symbol,
        package: Symbol,
        line: Option<u32>,
        file: Option<Symbol>,
    ) -> usize {
        let saved = self.0.len();
        let frame = RoutineFrame {
            package,
            lexical_package: None,
            name: Symbol::intern("<unit>"),
            line,
            file,
            is_method: false,
            is_submethod: false,
            is_block: false,
            is_inlined_block: false,
            is_hidden_from_backtrace: false,
            def_file: Some(unit),
            invocation_id: 0,
            callable_id: 0,
            code: 0,
            fixtures: 0,
            unit_scoped: false,
            fixtures_floor: 0,
        };
        self.0.push(EvalFrame { depth, frame });
        saved
    }

    /// Leave every `EVAL` entered after [`Self::enter`] returned `saved`.
    // Cost: O(1).
    pub(crate) fn leave(&mut self, saved: usize) {
        self.0.truncate(saved);
    }

    /// The running `EVAL`s whose entry depth is within a `len`-deep stack,
    /// outermost first.
    // Cost: O(e), e = running EVALs (almost always 0 or 1).
    pub(crate) fn within(&self, len: usize) -> Vec<EvalFrame> {
        self.0.iter().filter(|e| e.depth <= len).copied().collect()
    }

    /// Splice `evals` (outermost first) into `frames` (outermost first) at
    /// their entry depths.
    // Cost: O(s + e), s = frames, e = evals.
    pub(crate) fn splice(frames: Vec<RoutineFrame>, evals: &[EvalFrame]) -> Vec<RoutineFrame> {
        if evals.is_empty() {
            return frames;
        }
        let mut out = Vec::with_capacity(frames.len() + evals.len());
        let mut pending = evals.iter().peekable();
        for (i, frame) in frames.into_iter().enumerate() {
            while let Some(e) = pending.next_if(|e| e.depth <= i) {
                out.push(e.frame);
            }
            out.push(frame);
        }
        out.extend(pending.map(|e| e.frame));
        out
    }
}
