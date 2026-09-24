# Regex entry points share the subject instead of copying it per call

Every single-match regex entry point built a fresh `MatchTarget::new(text)`
per call: a copy of the whole subject plus its collected chars, about five
bytes per character, even when the match sat at the front. A tokenizer loop
(`while $s.match(rx, :p($p)) { ... }`), a `~~` test in a loop, and
`.contains(rx)` were therefore quadratic in the subject length.
`.comb(rx, :match)` was worse: it built one copy per produced Match. And
`.prematch` / `.postmatch` re-copied `.orig` and collected all its chars to
return a slice (#9144).

## The fix

- `MatchTarget` gained a small per-thread cache keyed on the identity of the
  `Str` payload (`src/runtime/match_target.rs`). An entry point that has the
  subject as a `Value` primes it with `MatchTarget::primed_subject` /
  `of_subject`; the engine's own `MatchTarget::new(&str)` then finds the same
  target when handed that payload's bytes. A hit is an identity test, not a
  content comparison: the cache holds a `Weak<String>` and compares pointer and
  length only after it upgrades, and a `Str` payload is never mutated behind a
  live `Weak` (the in-place `~=` path uses `Arc::get_mut`, which refuses). This
  is the same argument `builtins::grapheme_index` makes, and it uses the same
  256-byte threshold so short accumulators stay on the in-place append path.
  The target's `.orig` is the payload itself, so a Match no longer holds its
  own copy of the subject either. The grammar cursor class stays per engine
  run, so a parse stamping its class cannot relabel a later plain match of the
  same string.
- `Str ~~ /rx/` matches a `Str` topic as its own payload instead of
  re-stringifying it; `.match` and `.contains(rx)` prime their invocant.
- `.comb(rx, :match)` builds one target per call and shares it by refcount.
- `.prematch` / `.postmatch` slice the lazy Match's shared target directly.

## Measured

`scripts/str-complexity-check.sh`, t(2N)/t(N), release build, the two
binaries measured back to back on a 4-core container:

| case | before | after |
|---|---:|---:|
| `comb(regex, :match)` | 3.97 | 1.67 |
| `match :p loop` | 3.13 | 2.00 |
| `Str ~~ /rx/` (N calls) | 3.49 | 2.01 |
| `.contains(rx)` (N calls) | 3.35 | 1.92 |
| `.prematch`, long unrelated suffix (N calls) | 4.02 | 2.01 |

The `~~` case used to time `$s ~~ /b/` on `"a" x N`. That search fails, so
every call scans the whole subject, and N calls are quadratic in rakudo as
well (80k -> 160k: 0.38 s -> 0.93 s, and the ratio is still rising). It measured
the search, not the setup. The case now uses a match at the front, `/a/`,
which isolates the per-call setup that the case name describes. Two new cases,
`contains(regex)` and `prematch`, cover the other two entry points.

Pinned by `t/regex/match/match-subject-shared-target.t`, which checks that the
sharing cannot be observed: repeated matches, appending after a match, a
grammar parse followed by a plain match of the same string, `comb :match`
offsets, and non-ASCII `prematch` / `postmatch`.
