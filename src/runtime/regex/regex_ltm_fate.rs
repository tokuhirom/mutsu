//! Fates of an LTM declarative-prefix measurement (issue #9053).
//!
//! Rakudo builds an NFA from a candidate's declarative prefix and runs it:
//! every path through the NFA advances in lockstep, a non-declarative atom
//! (`<.ws>`, a code block, a subtraction class, ...) ends its path in a
//! *fate*, and the prefix length is the furthest position any path reaches a
//! fate or the end of the pattern. A fate stops only its own path.
//!
//! mutsu's NFA (ADR-0125) keeps its own fates. The frame here collects the
//! ones the matcher records while it answers one of the NFA's leaves under
//! `LTM_DECLARATIVE_MODE` — a code block inside a token that a `<+name>`
//! class calls, say: the NFA run opens a frame around its walk and counts the
//! frame's furthest fate with its own.
//!
//! Positions are indices into the character array being walked. A matcher
//! that re-slices or re-maps the subject (`:i` case folding, `:m` mark
//! stripping) opens its own frame and maps the inner fate back when it closes
//! it.

use std::cell::Cell;

thread_local! {
    /// The furthest fate recorded in the current measurement frame.
    static LTM_FATE_MAX: Cell<Option<usize>> = const { Cell::new(None) };
}

/// A non-declarative atom ended a path of the measurement at `pos`. The caller
/// must then fail that path rather than continue past the atom.
pub(crate) fn ltm_record_fate(pos: usize) {
    LTM_FATE_MAX.with(|f| f.set(Some(f.get().map_or(pos, |max| max.max(pos)))));
}

/// Open a fate frame, returning the enclosing frame's fate for
/// [`ltm_fate_frame_close`].
pub(crate) fn ltm_fate_frame_open() -> Option<usize> {
    LTM_FATE_MAX.with(|f| f.replace(None))
}

/// Close a frame opened by [`ltm_fate_frame_open`]: restore the enclosing
/// frame's fate and return this frame's own furthest fate.
pub(crate) fn ltm_fate_frame_close(enclosing: Option<usize>) -> Option<usize> {
    LTM_FATE_MAX.with(|f| f.replace(enclosing))
}

/// Close a frame whose positions are in a different coordinate space, folding
/// its fate into the enclosing frame through `map`.
pub(crate) fn ltm_fate_frame_close_into(
    enclosing: Option<usize>,
    map: impl FnOnce(usize) -> usize,
) {
    if let Some(inner) = ltm_fate_frame_close(enclosing) {
        ltm_record_fate(map(inner));
    }
}

impl crate::runtime::Interpreter {
    /// `<?before X>` in a match nested in an NFA leaf: Rakudo's NFA inlines
    /// `X` and puts a fate at each place it ends, so each end of `X` is a
    /// fate. A path of `X` that fails ends nowhere, and fates inside `X`
    /// record themselves.
    pub(super) fn ltm_record_lookahead_fates(
        &mut self,
        inner: &crate::runtime::RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: crate::symbol::Symbol,
    ) {
        for (end, _) in self.regex_match_ends_from_caps_in_pkg(inner, chars, pos, pkg) {
            ltm_record_fate(end);
        }
    }
}
