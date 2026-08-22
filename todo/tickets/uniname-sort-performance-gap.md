# `.sort(*.uniname.chars)` over the full Unicode range is ~18x slower than raku (times out)

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Cool.rakudoc:932`).

## Repro

```raku
say (0..0x1FFFF).sort(*.uniname.chars)[*-1].chr.uniname;
# OUTPUT: «BOX DRAWINGS LIGHT DIAGONAL UPPER CENTRE TO MIDDLE RIGHT AND MIDDLE LEFT TO LOWER CENTRE␤»
```

Both produce the correct output — this is a pure performance finding, not a correctness bug.
The doc-diff harness's default 10s timeout classifies it as a crash (`exit 124`), but it's
really "too slow", not "wrong" or "hangs forever":

- `raku`: completes in **~0.7s** wall clock.
- `mutsu` (`target/debug/mutsu`, debug build): completes in **~12.5s** wall clock — correct
  result, just far too slow. (No release-build measurement was taken; a release build would
  likely still be several seconds, i.e. a real multi-x gap even after accounting for
  debug-vs-release overhead, not just a debug-build artifact.)

## Root cause hypothesis

`.sort(*.uniname.chars)` sorts 131072 (`0x20000`) elements with a block comparator that calls
`.uniname` (a Unicode character-name lookup) and `.chars` on each comparison. If mutsu
re-evaluates the comparator block (and therefore re-computes `.uniname`) on every pairwise
comparison during the sort, rather than computing each element's sort key once up front
(Schwartzian-transform-style decorate/sort/undecorate, which is how `.sort(&block)` should be
implemented for a single-argument block — the argument is a *key extractor*, not a
comparator), that would explain an O(n log n) *comparator calls* × (redundant `.uniname` cost)
blowup relative to raku's presumably-cached-or-key-extracted approach. Worth checking:
1. Whether `.sort(&block)` with a 1-arity block computes the sort key once per element
   (cache) or recomputes it on every comparison.
2. Whether `.uniname` itself has an avoidable per-call cost (e.g., scanning a table linearly
   instead of an indexed/hashed lookup) that would compound either way.

## Affected files (starting point)

- `.sort` implementation for a single-arg (key-extractor) block — likely in
  `runtime/methods.rs` or a dedicated sort helper; check whether it does one key-computation
  pass or repeatedly invokes the block during comparisons.
- `.uniname` implementation in `builtins/unicode.rs` (or similar) — check lookup complexity.
