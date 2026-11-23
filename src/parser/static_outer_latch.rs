//! The static-outer latch (ADR-12529 amendment A1 (a), #18787): whether the
//! program may read another frame's *lexical* chain by name --
//! `$CALLER::OUTER::x`, `CALLER::UNIT::<$u>`, `$CALLER::($pkg)::x` -- so a
//! block-declared routine's frame and a closure created in an `EVAL`'d unit's
//! mainline must carry their precise static outer.
//!
//! The latch is set from the source text of every unit the parser reads (and
//! of every unit a bytecode-cache hit loads without parsing), before the unit
//! is compiled, so the compiler reads a settled answer for the whole unit. It
//! is process-global and monotonic like `opcode::dispatcher_possible`: a unit
//! compiled after any unit that mentions such a chain is compiled latched, and
//! text that merely contains one (a comment, a string) trips it -- a harmless
//! over-set, which only costs the latched code shape.

use std::sync::atomic::{AtomicBool, Ordering};

static STATIC_OUTER_OBSERVED: AtomicBool = AtomicBool::new(false);

/// Whether some unit read so far may read another frame's lexical chain.
// Cost: O(1).
#[inline]
pub(crate) fn static_outer_observed() -> bool {
    STATIC_OUTER_OBSERVED.load(Ordering::Relaxed)
}

/// Note a unit's source text: set the latch when it mentions a dynamic
/// neighbour's lexical chain (see [`mentions_static_outer_read`]).
// Cost: O(n), n = bytes of `source` (only while the latch is still unset).
#[inline]
pub(crate) fn note_mention(source: &str) {
    if !static_outer_observed() && mentions_static_outer_read(source) {
        STATIC_OUTER_OBSERVED.store(true, Ordering::Relaxed);
    }
}

/// The pseudo-packages after which a chain continues on the static chain.
const STATIC_CHAIN_WORDS: [&str; 5] = ["OUTER", "UNIT", "LEXICAL", "SETTING", "("];

/// Whether `source` holds a read of a dynamic neighbour's lexical chain:
/// `CALLER`, `CALLERS` or `DYNAMIC` followed by `::` and `OUTER`/`OUTERS`,
/// `UNIT`, `LEXICAL`, `SETTING` or a symbolic segment `(...)`; or one of the
/// three spelled as a string (`::('CALLER')::...`, `my $c = "CALLER"`), which a
/// symbolic name may splice into such a chain.
// Cost: O(n), n = bytes of `source`.
pub(crate) fn mentions_static_outer_read(source: &str) -> bool {
    ["CALLER", "DYNAMIC"].iter().any(|word| {
        source.match_indices(word).any(|(at, _)| {
            let before = source[..at].bytes().next_back();
            if matches!(before, Some(b'\'' | b'"' | b'<')) {
                return true;
            }
            let mut rest = &source[at + word.len()..];
            if *word == "CALLER" {
                rest = rest.strip_prefix('S').unwrap_or(rest);
            }
            // `CALLER::CALLER::OUTER::x`: the chain continues past more
            // dynamic hops.
            while let Some(more) = rest
                .strip_prefix("::CALLER")
                .or_else(|| rest.strip_prefix("::DYNAMIC"))
            {
                rest = more.strip_prefix('S').unwrap_or(more);
            }
            rest.strip_prefix("::")
                .is_some_and(|tail| STATIC_CHAIN_WORDS.iter().any(|w| tail.starts_with(w)))
        })
    })
}

#[cfg(test)]
mod tests {
    use super::mentions_static_outer_read as m;

    #[test]
    fn static_chain_after_a_dynamic_neighbour() {
        assert!(m("f({ $CALLER::OUTER::x })"));
        assert!(m("$CALLERS::UNIT::x"));
        assert!(m("CALLER::LEXICAL::<$x>"));
        assert!(m("$DYNAMIC::SETTING::x"));
        assert!(m("$CALLER::($unit)::x110"));
        assert!(m("$CALLER::CALLER::OUTER::x"));
        assert!(m("my $c = 'CALLER'; ::($c)::OUTER::x"));
        assert!(m("::(\"CALLERS\")::('$*foo')"));
    }

    #[test]
    fn plain_caller_reads_do_not_latch() {
        assert!(!m("$CALLER::x; CALLER::<$y>; $CALLERS::z"));
        assert!(!m("$OUTER::x; $UNIT::y; ::($n)"));
        assert!(!m("say 1"));
    }
}
