# Quantifier candidate enumeration runs embedded code blocks far more often than real raku

Split off from `todo/tickets/regex-embedded-code-block-quantifier-scope.md` while fixing that
ticket (2026-08-25). That ticket's exact repro from `raku-doc/doc/Language/regexes.rakudoc`:

```raku
my $paragraph = "line\nline2\nline3";
$paragraph ~~ rx| :my $counter = 0; ( \V* { ++$counter } ) *%% \n |;
say "Matched $counter lines";
```

- raku: `Matched 3 lines`
- mutsu (after the `:my`/`:constant` persistence fix): `Matched 17 lines`

The persistence half of the original ticket is now fixed (see
`news/2026-08/regex-my-declarator-persists-to-caller-scope.md`): a `:my $counter`'s mutations
now correctly reach the caller's `$counter` after the match. But the *count itself* is wrong —
mutsu runs `{ ++$counter }` 17 times where raku runs it 3 times, because mutsu's regex engine
computes quantifier candidates eagerly (all possible lengths up front) instead of lazily (only
as many lengths as backtracking actually needs), and an embedded code block's side effects are
real and run once per *candidate computed*, not once per candidate *actually walked as part of
an accepted or rejected full-match attempt*.

## Root cause (traced with an env-gated instrumented print, then removed)

`( \V* { ++$counter } ) *%% \n` matched against `"line\nline2\nline3"`:

- `match_separated_quantifier` (`src/runtime/regex/regex_match_sep.rs`) needs the *set* of
  possible end positions for the atom `( \V* { ++$counter } )` at each position, to backtrack
  the separator quantifier (`enumerate_separated_chains` / `extend_separated_chain`).
- It gets that set via `regex_match_atom_all_with_capture_in_pkg`, which enumerates **every**
  length `\V*` can match at that position (0 through the greedy max), lowest-priority first.
  Because the atom's tokens include the `{ ++$counter }` code block, matching to each candidate
  length actually **runs the code block for real** (per the documented "the code runs inline,
  once, where the cursor reaches it, and its side effects are real" contract in
  `eval_regex_inline_code`, which exists on purpose for `advent2013-day18`-style patterns that
  need a block's side effect visible to a later alternative in the *same* attempt).
- For `"line\nline2\nline3"`: at position 0 ("line", 4 chars), `\V*` candidate lengths 0..=4 (5
  total) each run the block. Only length 4 is followed by `\n` and extends the chain; the other
  4 are immediately discarded, but the block already ran for each of them. Same at position 5
  ("line2", 5 chars: 6 candidates) and position 11 ("line3", the last unseparated group, 5
  chars: 6 candidates, no trailing `\n` since it's end of string). `5 + 6 + 6 == 17`.
- Also, `extend_separated_chain` does `out.push((atom_caps.clone(), sep_caps.clone(), cur))` for
  **every** node visited in the recursion, not just terminal/accepted chains — every partial
  chain becomes its own output candidate, compounding the effect.

Confirmed this is not specific to `*%%`: a plain, non-separated `*`/`+` quantifier that needs
real backtracking also over-runs. `raku -e '"aaac" ~~ / :my $c = 0; ( \w* { ++$c } ) c /; say $c'`
prints `2` (greedy `\w*` tries "aaac" once, fails the trailing `c`, backtracks to "aaa" once,
succeeds — 2 real attempts). mutsu prints `5` for the same pattern (it eagerly enumerates and
runs the block for every length 0..=4, not just the two raku actually visits). So the bug lives
in the general "enumerate every candidate length up front, each via
`regex_match_atom_all_with_capture_in_pkg`" strategy used across the backtracking quantifier
matchers, not only in the separator-quantifier path — though the separator path's `enumerate_
separated_chains` DFS additionally records every intermediate node as a candidate, which is why
its over-count is proportionally worse.

By contrast, plain `*`/`+` quantifiers *without* forced backtracking (the atom's greedy longest
match happens to satisfy everything that follows on the first try) count correctly today, because
no extra candidate lengths ever get computed — this is what let the sibling persistence bug's
simpler repro cases (`t/regex-my-embedded-block-persist.t`) pass cleanly.

## Why this is deep, not a ticket-sized fix

Real raku's backtracking regex engine is lazy: it tries the highest-priority candidate first and
only computes/tries the next one when something later in the pattern actually fails against the
current attempt, and it stops as soon as the whole pattern succeeds. mutsu's `regex_match_atom_
all_with_capture_in_pkg` family instead **eagerly materializes every candidate length as a
`Vec`** before the caller picks one, which is architecturally simpler and lets each quantifier
matcher be a nearly-pure function over "the full candidate set" — but it is fundamentally
incompatible with real, once-per-actual-attempt side effects from an embedded code block. Two
directions to consider, both substantial:

1. **True laziness**: turn the "all candidate lengths" enumeration into something demand-driven
   (an iterator, or explicit continuation/generator) so a length past the one that succeeds is
   never computed, and hence its code block never runs. This touches every backtracking
   quantifier matcher (`regex_match_atom_all_with_capture_in_pkg` and all of its callers across
   `src/runtime/regex/`), likely with real perf implications for the (much more common) code-free
   case that currently benefits from batch enumeration.
2. **Two-phase candidate generation**: split "compute the boundary end-position of this
   candidate" (pure, side-effect-free — needed for backtracking bookkeeping) from "run this
   candidate's embedded code for real" (side-effecting — done only once, for the length actually
   adopted along the accepted path, mirroring the ADR-0009 discipline already used for LTM
   declarative-prefix measurement, which explicitly must "never execute user code"). This needs a
   way to compute an atom's candidate end-positions without evaluating any `{ ... }` blocks inside
   it, then re-run the block for real only on the winning candidate — doable per-atom-shape but
   nontrivial to thread through every atom kind (nested groups, alternations, named subrule calls)
   that can contain a code block.

Either direction is a real quantifier-matching architecture change, not a local fix — hence a
`todo/deep/` entry rather than folding it into the (already-shipped) persistence fix.

## Affected files (starting point)

- `src/runtime/regex/regex_match_sep.rs` — `match_separated_quantifier`,
  `enumerate_separated_chains`, `extend_separated_chain` (the `*%%`/`+%` path; proportionally
  worst-affected because it records every intermediate DFS node as a candidate too)
- `src/runtime/regex/regex_match_atom.rs` / `regex_match_atom_simple.rs` (wherever
  `regex_match_atom_all_with_capture_in_pkg` lives) — the eager all-candidate-lengths enumeration
  shared by every backtracking quantifier matcher
- `src/runtime/regex/regex_eval.rs` — `eval_regex_inline_code`, whose "runs inline, once, real
  side effect" contract is exactly what over-fires under the current eager enumeration

## Minimal repro

```raku
my $s = "aaac";
$s ~~ / :my $c = 0; ( \w* { ++$c } ) c /;
say $c;   # raku: 2, mutsu: 5
```

```raku
my $paragraph = "line\nline2\nline3";
$paragraph ~~ rx| :my $counter = 0; ( \V* { ++$counter } ) *%% \n |;
say "Matched $counter lines";   # raku: "Matched 3 lines", mutsu: "Matched 17 lines"
```
