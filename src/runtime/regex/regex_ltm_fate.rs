//! Fates of an LTM declarative-prefix measurement (issue #9053).
//!
//! Rakudo builds an NFA from a candidate's declarative prefix and runs it:
//! every path through the NFA advances in lockstep, a non-declarative atom
//! (`<.ws>`, a code block, a subtraction class, ...) ends its path in a
//! *fate*, and the prefix length is the furthest position any path reaches a
//! fate or the end of the pattern. A fate stops only its own path.
//!
//! mutsu measures by walking the ordinary backtracking matcher under
//! `LTM_DECLARATIVE_MODE`. A stopper used to report its position as a
//! zero-width match and unwind the WHOLE walk, so the measurement was the
//! first fate the depth-first walk happened to reach, not the furthest one:
//! `<[a..z]> | <-[Z \n]>` inside a `*` loop measured the loop as ending where
//! the second branch fired, one character in, even though the first branch
//! carries the path on to the end of the word.
//!
//! Now a stopper records its position here as a fate and fails its own path,
//! and the walk carries on through every other path. A measurement entry point
//! (`ltm_prefix_len_at`, `declarative_prefix_match_len`) opens a frame, and its
//! result is the larger of the furthest full match and the furthest fate.
//!
//! Positions are indices into the character array being walked. A matcher
//! that re-slices or re-maps the subject (a subrule matched on
//! `&chars[pos..]`, `:i` case folding, `:m` mark stripping) opens its own
//! frame and maps the inner fate back when it closes it.

use super::regex_helpers::LTM_PREFIX_TERMINATED;
use std::cell::Cell;

thread_local! {
    /// The furthest fate recorded in the current measurement frame.
    static LTM_FATE_MAX: Cell<Option<usize>> = const { Cell::new(None) };
}

/// A non-declarative atom ended a path of the measurement at `pos`. The caller
/// must then fail that path rather than continue past the atom.
pub(crate) fn ltm_record_fate(pos: usize) {
    LTM_PREFIX_TERMINATED.with(|f| f.set(true));
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
    /// `<?before X>` under measurement: Rakudo's NFA inlines `X` and puts a
    /// fate at each place it ends, so each end of `X` is a fate. A path of
    /// `X` that fails ends nowhere, and fates inside `X` record themselves.
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
