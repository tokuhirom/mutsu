# The scan prefilter's cluster-start check no longer calls out per rejected position

The negated-class grapheme fix (#10875) taught `FirstSet::admits_at` to offer
the engine every position that starts a multi-codepoint grapheme cluster,
because a negated class matches `x` + U+0301 although it rejects `x`. It did so
by calling `grapheme_end` for *every* position the set rejected — the
per-position reject path of every prefiltered scan — and the deterministic
bench series showed it: `bench-regex-long-subject` +20% Ir and
`bench-regex-split-subst` +11% (#11145).

`admits_at` now looks at the next codepoint first. When it is ASCII, or there is
none, nothing can extend `chars[i]` (no ASCII codepoint is a mark, ZWJ or other
extender), so the only multi-codepoint cluster that can start there is `\r\n`,
answered by one comparison. `grapheme_end` is asked only when a non-ASCII
codepoint follows. The answer is unchanged in every case; new tests pin the
CRLF cluster against a negated class without `\n` and an ASCII base followed by
a mark.

Measured with `scripts/bench-det.sh` against current `main`:
`bench-regex-long-subject` 86.45M → 73.12M Ir, `bench-regex-split-subst`
477.73M → 442.74M Ir; `grapheme_end` itself drops from 99.4M to 2.2M Ir on the
latter's callgrind profile.
