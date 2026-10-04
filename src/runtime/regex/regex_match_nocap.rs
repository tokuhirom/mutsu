use super::super::*;

impl Interpreter {
    /// The end of `pattern`'s first match at `start`, with code atoms inert:
    /// a position-only probe that runs none of the user's code.
    // Cost: the match itself (`rx_match_first_no_code`).
    pub(super) fn regex_match_end_from_in_pkg(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<usize> {
        self.rx_match_first_no_code(pattern, chars, start, pkg)
            .map(|(end, _)| end)
    }
}
