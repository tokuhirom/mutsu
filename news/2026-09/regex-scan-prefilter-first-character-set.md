# The regex scan prefilter grows a first-character set, and stops entering the engine to find out a position cannot match

ADR-0099 Stage 1's second slice ([#8272](https://github.com/tokuhirom/mutsu/issues/8272)). The
first slice ([#8285](https://github.com/tokuhirom/mutsu/issues/8285)) wired mutsu's existing
substring search to the one pattern shape that could use it — a plain, unconditional, non-`:i`
literal prefix — and left the rest of the scan loop entering the full backtracking engine at every
one of the subject's start positions, at ~983 instructions per position to establish that
`chars[i] != 'z'` (ADR-0099 §2.4). This slice answers that question for the shapes with no such
prefix: an alternation, a leading character class, a `:i` literal.

## What was derived

One pass over a parsed `RegexPattern`, memoized alongside it, now yields three things instead of one:

- a **required literal prefix** (unchanged, from the first slice);
- a **first-character set** — a 128-bit ASCII bitmap plus a non-ASCII policy — so a position is
  rejected by a shift and a mask;
- a **minimum match length**, which truncates the tail of the start range.

The set is derived through alternation (union of the branches), groups, character classes, the
grapheme and newline atoms, and leading zero-width assertions, with `:i` handled by a **fold
closure** rather than a folded needle: multi-character folds (`ß`/`SS`, `ﬁ`/`fi`) make a case-folded
literal variable-length, so searching for a folded needle would be unsound (ADR-0099 §4
constraint 2), but the set of characters that could *begin* such a match is perfectly well defined.

Everything is an over-approximation: the set is a superset of what can start a match and the length
a lower bound, so an undecidable shape widens or declines rather than guessing. Declining is
exactly the status quo — the caller walks every position, as it did before.

## Measured

A failing scan over a 644,000-character subject, release build, prefilter on versus
`MUTSU_REGEX_PREFILTER=off`:

| scan | before | after |
|---|---:|---:|
| `~~ / 'zzzk-not-here' /` | 101.1 ms | 9.4 ms |
| `~~ / :i 'ZZZK' /` | 218.0 ms | 5.0 ms |
| `~~ / 'zzzk' \| 'kkkz' /` | 562.0 ms | 3.8 ms |
| `~~ / \d\d\d /` | 160.7 ms | 12.6 ms |

The `:i` row needed one thing besides the first-set. With positions no longer the bottleneck, the
remaining 57 ms turned out to be `needs_casefold_expansion`, which asks "does any character here
fold to more than one?" by running two case conversions and building a `Vec` for **every character
of the subject, on every match**. No ASCII character has a multi-character fold, so that is now a
range check, and the `:i` scan went 57.4 ms → 5.0 ms — below rakudo's 6.6 ms on the same question.

## Not a second definition of what the engine matches

ADR-0099 §4 constraint 1 is that the prefilter must not become an independently-written statement
of what matches, because the two would drift and the drift would be silent: a dropped valid match,
not an error. The literal prefix satisfies that by recognizing a strict subset of `ltm_litlen_walk`'s
cases. The first-set satisfies it by *calling* the engine's own class evaluator over the ASCII range
instead of restating its table — which meant extracting `regex_match_class` and
`regex_match_class_ignorecase` into free functions, since both were already pure and only the `self`
receiver stood in the way. `\w`, `<:Lu>`, `<[a..z]>` and every negated or `:i` combination of them
therefore cannot disagree with the prefilter by construction.

The remaining constraint 3 shapes decline outright: a `<subrule>` call (a set derived through one
would have to be keyed by invocant package and `TOKEN_DEFS_GEN` to survive `H is G` overriding
`token x`), anything that runs user code before the first character is consumed (a leading `{ … }`
block runs once per start position in both mutsu and rakudo, ADR-0009), backreferences, `<~~>`, and
a *scoped* `:ignoremark`.

## The gate

This is machinery that can be wrong without being incorrect, so the tests are about the two ways it
can be wrong rather than about the answers:

- `tests/regex_prefilter_differential.rs` grew 27 cases covering the new shapes — every one asserts
  that the program's output is byte-identical with the prefilter on and with
  `MUTSU_REGEX_PREFILTER=off`, which is the only shape of test that catches a silently skipped match.
- `tests/regex_prefilter_engagement.rs` is new, and pins the opposite failure: that the prefilter
  keeps *applying*. It asserts on the `MUTSU_VM_STATS` counters rather than on wall-clock, which
  states "sub-linear in subject length" exactly and load-independently — `position_hits` stays at
  zero while `positions_offered` grows 8x with the subject. Those counters now separate
  `literal_prefix=` from `first_char_set=`, so "the first-character set stopped engaging" is visible
  even while the rarer literal-prefix scans keep the aggregate up.

## Still open

[#8272](https://github.com/tokuhirom/mutsu/issues/8272) stays open: a required *inner* literal for
patterns with no usable prefix, subrule-derived prefixes keyed by package + `TOKEN_DEFS_GEN`,
NFD-aware first-sets for a scoped `:m`, and ADR-0099 §5's NFA over the declarative prefix are all
still ahead.

Implementing the fold closure also confirmed the `:i` divergence ADR-0099's constraint 2 predicted
would surface — mutsu answers `False` where rakudo matches `'s'` against U+017F LATIN SMALL LETTER
LONG S, because the engine's `:i` literal test is a case *mapping* (`to_lowercase`) rather than a
case *folding*. Filed separately as [#8440](https://github.com/tokuhirom/mutsu/issues/8440), as the
issue asked.
