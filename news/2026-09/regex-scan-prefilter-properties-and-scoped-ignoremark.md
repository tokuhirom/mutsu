# The scan prefilter stops declining on `<:prop>` and on a scoped `:ignoremark`

ADR-0099 Stage 1's fifth slice ([#8272](https://github.com/tokuhirom/mutsu/issues/8272)). Two atoms
were still sinking the first-character-set derivation outright. A `<:Lu>` widened it to "anything",
and a scoped `:ignoremark` group declined the whole pattern it sat in — so both shapes entered the
full backtracking engine at every one of a long subject's start positions, which is the cliff the
issue opened with, just spelled differently.

Failing scans over a 135,000-character subject, release, against `MUTSU_REGEX_PREFILTER=off`:

| scan | before | after | warm rakudo |
|---|---:|---:|---:|
| `~~ / <:Lu> <:Lu> <:Lu> <:Lu> /` | 29.9 ms | **1.8 ms** | 40.5 ms |
| `~~ / <:Greek> <:Greek> /` | 24.7 ms | **1.2 ms** | 44.0 ms |
| `~~ / <:Nd> ** 6 /` | 42.4 ms | **19.5 ms** | 47.7 ms |
| `~~ / [:m 'zzzq'] /` | 58.3 ms | **1.0 ms** | 27.2 ms |
| `~~ m:g/ [:m 'zzzq'] /` | 60.2 ms | **0.5 ms** | 26.5 ms |
| `~~ m:g/ <:Lu> /` | 29.6 ms | **0.4 ms** | 40.7 ms |

The `<:Nd>` row is the mechanism working as designed rather than falling short: the subject holds one
digit per 44 characters, so the scan really does have 3,000 positions to try and the remaining cost
is engine entries the prefilter correctly did not skip. `/ <:!Nd> <:Lu> /` moves from 61.1 ms to
62.7 ms for the same reason in the other direction — a negated property admits nearly every
character, so deriving its set precisely buys nothing. Neither is a shape to quote as a win.

## `<:prop>`: call the predicate, do not restate it

The property set is built by calling `check_unicode_property` / `check_unicode_property_with_args`
— the engine's own predicates, unchanged — over the ASCII range, exactly as the character-class
first-set already calls the engine's class evaluator. That is ADR-0099 §4 constraint 1 satisfied the
only way it can be: a second, independently-written table of what `<:Lu>` matches would drift from
the engine's, and the drift would be a silently dropped match rather than an error. Every non-ASCII
character is admitted wholesale, so `<:Greek>` derives an empty ASCII half and nothing else, which
is what makes it the fastest row above on a Latin subject.

Negation is the predicate's answer inverted, not a reason to widen. `:i` does not enter it at all,
because the engine's property arm does not case-fold either.

## A scoped `:ignoremark` is derived from the tree the matcher actually walks

The engine matches a `:m` sub-pattern as `strip_marks_pattern(p)` against the **mark-stripped**
subject. So the analysis walks exactly that derived tree, off the same memo the matcher uses —
there is no second reading of what `:m` means to drift from. What comes out is a statement about
stripped text, and carrying it back onto the original subject is where the whole slice's difficulty
sits.

**Stripping is not the identity on positions, and that is not a detail.** Three things follow, and
getting any of them wrong is a dropped match rather than a slow scan:

- **Every non-ASCII character must be admitted.** A precomposed `é` strips to `e`, so a derived
  `{e}` is a claim about a subject character that may be spelled any number of ways, and the reverse
  mapping is bounded only by Unicode's composition tables. This was caught by the differential
  corpus, not by reasoning: `"\x[0300]x" ~~ / [:m 'x'] /` answered differently with the prefilter on
  until the set was widened.
- **A position that does not start a grapheme cluster must be offered.** Stripping maps such a
  position forward to the next surviving character, so the set says nothing about the character
  actually sitting there. The cheap sufficient test for the safe case is that the previous character
  is ASCII and the pair is not `\r\n` — UAX #29 breaks between every other pair of ASCII characters,
  since Prepend, Extend, ZWJ, regional indicators, Hangul and Extended_Pictographic are all
  non-ASCII. `FirstSet::admits_at` is that one extra branch, taken only on the reject path.
- **The length bound does not survive at all.** Both halves of a `\r\n` cluster map to the cluster's
  start, so a sub-pattern consuming two stripped characters can cover zero original ones. A bound
  derived in stripped space is therefore not a lower bound in the original, and a bound that is not
  a lower bound prunes a viable start. It is dropped outright rather than patched.

An ASCII subject loses none of this precision: no position is inside a cluster, so the bitmap
decides all of them, which is why `/ [:m 'zzzq'] /` is as fast above as a plain literal scan.

## What is still declined, and why it is not an oversight

A `<+a -b>` composite class stays universal. Its `NamedBuiltin` items are not a character set: when
the built-in predicate rejects a character, the engine falls back to resolving a **grammar token** of
that name in the current package and matching it against the *remaining input*. It is a `<subrule>`
in disguise, so answering it needs the package-and-generation-keyed memo (and `mentions_subrule`
would have to report it) — a slice of its own, not a widening of this one.

## Testing

`tests/regex_prefilter_differential.rs` gains 22 cases and now runs 99; `tests/regex_prefilter_engagement.rs`
gains four, pinning that the new derivations engage *and* that the mark-skewed escape hatch still
offers every position it cannot speak for. `t/regex/regex-scan-prefilter-ignoremark-prop.t` pins the
34 observable behaviours, and passes under `raku` unmodified. On top of those, a randomized corpus of
about 32,000 generated pattern/subject pairs — weighted toward `<:prop>` atoms, scoped `:m` groups,
combining marks, Prepend characters, CRLF clusters and all-mark subjects — was run with the
prefilter on and off and diffed; it is what found the non-ASCII widening above.

## Adjacent, filed separately

Two divergences the work surfaced, neither of them the prefilter's (both reproduce with
`MUTSU_REGEX_PREFILTER=off`): a scoped `:ignoremark` match reports a start on a character stripping
removed where rakudo reports the first surviving one
([#8485](https://github.com/tokuhirom/mutsu/issues/8485)), and `<:Nv(1)>` matches in mutsu where
rakudo accepts no such property spelling ([#8486](https://github.com/tokuhirom/mutsu/issues/8486)).
