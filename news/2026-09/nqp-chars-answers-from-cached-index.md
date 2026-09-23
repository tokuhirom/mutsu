# `nqp::chars` answers from the cached string index

`nqp::chars` copied its string argument (`to_string_value()`) and then counted
it with `.chars().count()` on every call: O(n) time plus an O(n) allocation.
The idiomatic NQP scanner loop `nqp::while(nqp::islt_i($i, nqp::chars($s)), ...)`
re-evaluates the length every iteration, so it ran in O(n^2) — 4.26x the time
for 2x the input in `scripts/nqp-complexity-check.sh chars` (#9130).

The per-payload `GraphemeIndex` that #9140 added for `.chars`/`.substr` already
does the O(n) walk once per string allocation and caches the result. It now
also records the string's **codepoint** count (for a flat ASCII string that is
just its byte length, so nothing extra is computed), and `nqp::chars` reads it
through a new `grapheme_index::codepoint_count` helper: a long `Str` answers
from the cache, a short one (< 256 bytes, below the cache threshold) is counted
in place from the borrowed payload with no copy. The TRIR fast path's fallback
for `nqp::chars` uses the same helper.

`nqp::chars` keeps counting codepoints rather than NFG graphemes, as the rest of
mutsu's `nqp::` string family (`substr`, `ord`, `index`, the `cclass` ops) is
codepoint-indexed; switching one op alone would make a scanner's bound
disagree with its own indexing. The grapheme-vs-codepoint divergence from
MoarVM is a family-wide question, independent of this cost fix.

Pinned by `t/vm/nqp-chars-scan-is-linear.t` (correctness plus a
4x-input time ratio).
