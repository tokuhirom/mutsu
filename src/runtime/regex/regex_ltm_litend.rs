//! Which literals of a declarative prefix count toward `litlen`, the
//! longest-literal tie-break of LTM ranking (ADR-0022 §2).
//!
//! Rakudo decides this while building each rule's NFA, not while running it.
//! NQP's `QRegex::NFA` carries a build-time flag, `$!LITEND`, that starts at 0
//! for every rule body (and every `|` branch). A literal built while it is 0
//! gets an `_LL` edge; `regex_nfa` sets it to 1 before building any node that
//! is not a `literal`, `concat` or `alt`, and `method alt` leaves it 0 after
//! the alternation only when every branch left it 0. At run time MoarVM
//! records, per fate, the offset just past the last `_LL` edge any thread
//! crossed. A subrule's `_LL` edges keep that status when the subrule is
//! merged into its caller (`mergesubstates`), wherever the call sits: the
//! literals leading `token e { 'x' ... }` count through `[<e> '-']?`, while
//! the quantified `'-'` itself does not.
//!
//! The builder works backwards, so the flag is computed forwards here: a node
//! is *open* when `$!LITEND` would be 0 on entry to it. A procedure body and
//! the measured pattern itself start open.

use super::super::*;

/// Is `$!LITEND` still 0 after `token`, entered open when `open` is set?
// Cost: O(s), s = size of the token's atom.
pub(super) fn open_after_token(token: &RegexToken, open: bool) -> bool {
    open && token_keeps_open(token) && open_after_atom(&token.atom)
}

/// Can `token`'s atom be entered open? Not under a quantifier or separator
/// (`method quant`), a capture alias (`subcapture`) or an interpolated
/// literal (a fate).
// Cost: O(1).
pub(super) fn token_keeps_open(token: &RegexToken) -> bool {
    !token.from_runtime_interpolation
        && matches!(token.quant, RegexQuant::One)
        && token.separator.is_none()
        && token.named_capture.is_none()
        && token.secondary_named_capture.is_none()
        && token.hash_capture.is_none()
}

/// Is `$!LITEND` still 0 after `atom`, entered open?
// Cost: O(s), s = size of the atom.
fn open_after_atom(atom: &RegexAtom) -> bool {
    match atom {
        RegexAtom::Literal(_) | RegexAtom::LiteralGrapheme(_) => true,
        RegexAtom::Group(pattern) => open_after_pattern(pattern, true),
        RegexAtom::Alternation(alternatives) => {
            alternatives.iter().all(|alt| open_after_pattern(alt, true))
        }
        _ => false,
    }
}

/// Is `$!LITEND` still 0 after `pattern`, entered open when `open` is set?
// Cost: O(s), s = size of the pattern.
pub(super) fn open_after_pattern(pattern: &RegexPattern, open: bool) -> bool {
    let mut open = open_at_pattern_start(pattern, open);
    for token in &pattern.tokens {
        open = open_after_token(token, open);
    }
    open && !pattern.anchor_end
}

/// Is the first token of `pattern` entered open? An anchor (`^`) is not a
/// literal, and an ignoremark literal has no `_LL` form (NFA.nqp's
/// "XXX _M_LL" note), so either closes it.
// Cost: O(1).
pub(super) fn open_at_pattern_start(pattern: &RegexPattern, open: bool) -> bool {
    open && !pattern.anchor_start && !pattern.ignore_mark
}
