# A regex atom's candidates are produced on demand, driven by the continuation

mutsu's token walk has always been a proper depth-first backtracking machine:
it tries one candidate, descends into the rest of the pattern, and only comes
back for the next candidate when the continuation rejected the current one. Its
*atoms* were not. When a token's atom was itself a sub-pattern — a group, an
alternation, the repeated element of a separated quantifier — the walk asked a
producer for the **complete** candidate set before descending into any of them.

Computing a candidate runs the embedded `{ ... }` blocks inside it for real
(that is ADR-0009's contract, and it exists on purpose), so a block fired once
per candidate *computed* rather than once per candidate *entered*:

```raku
"aaac" ~~ / :my $c = 0; ( \w* { ++$c } ) c /;      # raku 2   mutsu 5
my $p = "line\nline2\nline3";
$p ~~ rx| :my $n = 0; ( \V* { ++$n } ) *%% \n |;    # raku 3   mutsu 17
```

The counter is the symptom; the severity is that a block on a candidate raku
never enters can `die`, and that aborted a match that should have succeeded:

```raku
my $d = 0;
try { "aaac" ~~ / ( \w* { $d++; $d > 3 ?? die('boom') !! 1 } ) c / }
say $! ?? 'died' !! 'lived';   # raku: lived    mutsu: died
```

The decision, the options costed against it, and the full raku-measured table
are [ADR-0073](../../docs/adr/0073-regex-atom-candidates-are-demand-driven.md).

## The original ticket's diagnosis was wrong in two places

This entry replaces `todo/deep/regex-quantifier-eager-candidate-enumeration-overruns-code-blocks.md`.
Re-measuring its claims on 2026-09-07 moved the target substantially:

- It said plain `*`/`+` "without forced backtracking … count correctly today".
  They did not: `"aaa" ~~ / ( \w* { B } ) /` ran the block 4 times against
  raku's 1, with no continuation at all to backtrack from. Backtracking was
  never the trigger.
- It blamed "the general enumerate-every-candidate-length strategy used across
  the backtracking quantifier matchers". It was not the quantifier matchers:
  `walk_quant_chain` already grows its chain one iteration at a time through the
  *singular* matcher, so `[ \w+ {B} ]+`, `( \w+ {B} )*`, `[ \w {B} ] ** 2..3`
  and `( [ \w {B} ]* ) c` all already agreed with raku, and so did a top-level
  `\w* {B} c` outside any group.

The real trigger was narrower and sharper — **an atom that is asked for its
whole candidate set** — which is what made the fix affordable: the walk keeps
its shape, and only the atom boundary stops being a collect-then-pick barrier.

## What changed

`walk_tokens` gained a **sink** (`MatchSink`, `regex_match_core.rs`) in place of
its `&mut Vec<(usize, RegexCaptures)>` output parameter. A sink either collects
(the old behaviour) or hands each completed match straight to the enclosing
walk's continuation. `regex_walk_ends_in_pkg` is the continuation-form entry
point beside the collecting `regex_match_ends_from_caps_in_pkg`.

On top of it, two drivers:

- `for_each_atom_candidate` (`regex_match_lazy.rs`) replaces the "collect, then
  iterate in reverse" idiom at the walk's `One` / `ZeroOrOne` atom sites. For
  `Group`, `CaptureGroup`, `CaptureIsolatedGroup` and `Alternation` it drives the
  sub-pattern's walk through a `Cont` sink; for every other atom kind it falls
  back to the eager producer iterated highest-priority-first, which is
  byte-identical to what the walk did before. `|` alternation ranks its branches
  by declarative prefix *first* (measurement, so ADR-0009 still holds — nothing
  is executed to rank) and only then walks them in rank order, entering branch
  *k+1* once branch *k* has been rejected. That is the same shape
  `walk_seq_alternation` already used for `||`.
- `for_each_separated_candidate` (`regex_match_sep_lazy.rs`) is the CPS form of
  `enumerate_separated_chains` / `extend_separated_chain`. Besides deferring the
  atom's candidates it also stops recording every intermediate DFS node into a
  vector before the walk sees any of them.

The delta-shaping code (`group_merge_delta`, `capture_group_delta`,
`alternation_branch_delta`) now has one definition, shared by the eager producer
and the demand-driven driver, so the two cannot drift.

In LTM declarative mode the eager producer is used unchanged: measurement has no
continuation to be driven by, and ADR-0022/ADR-0046's ranking paths must stay
side-effect free.

## Measured

`t/regex-lazy-candidate-enumeration.t` pins 60 rows, every one of them run
against real `raku` first. The rows that already agreed are pinned too — they
are exactly what a laziness change is most likely to break, and three of them
(`( { B } \w* )`, `( \w* ) { B }`, `:r ( \w* { B } ) c`) discriminate between
"the block moved" and "the count moved".

| family | shape | raku | before | after |
| --- | --- | --- | --- | --- |
| A | `( \w* {B} ) c` on `aaac` | 2 | 5 | 2 |
| A | `( \w* {B} )` on `aaa` | 1 | 4 | 1 |
| A | `[ \w* {B} ]` on `aaa` | 1 | 4 | 1 |
| A | `( ( \w* {B} ) ) c` | 2 | 5 | 2 |
| A | `( \w* {B} ) <?before c>` | 2 | 5 | 2 |
| A | `( \w* { … die … } ) c` | lived | **died** | lived |
| C | `'a' [ 'bc' {+10} \| 'b' {+1} ]` on `abcd` | 10 | 11 | 10 |
| C | three branches, winner first | 100 | 111 | 100 |
| F | `( \V* {B} ) *%% \n` over 3 lines | 3 | 17 | 3 |
| F | `( \V* {B} ) *% \n` | 3 | 17 | 3 |
| B | every control row (14 of them) | = | = | = |

`make test`, the full local `make roast` and `scripts/battery-testsuite.sh` are
green. The batteries are the load-bearing check: `YAMLish`, `JSON::Fast`,
`Cro::HTTP`, `TOML` and the vendored `zef` are all grammar-driven, and a
grammar's hot path is exactly the atom boundary this changed.

## Residue

Three measured rows are still eager, each for its own reason, and each is
recorded rather than hand-waved:

- **The `<subrule>` boundary** (ADR-0073 Slice 2). `regex part { \w* {B} }`
  under `regex TOP { <part> 'c' }` still runs the block 5 times against raku's
  2, and a `||` inside a non-ratcheted `regex` subrule still enters both
  branches. The `Named` producer arm carries the left-recursion seed loop and
  the proto rank-then-match dispatch, both of which genuinely want a candidate
  *set*; separating the plain case is its own slice, and the narrowed record is
  `todo/deep/ordered-alternation-eager-candidate-enumeration.md`.
- **The `:g` / `subst` scan** re-runs the pattern per start position rather than
  committing to a match and resuming after it —
  `todo/tickets/global-match-scan-enumerates-every-end-at-every-start.md`.
- **A non-capturing group with a block under a counted separator**
  (`[ \w+ {B} ] ** 1..3 % ','`) drops the chain after the first element. That
  one is a *pre-existing wrong match*, not a count problem, and was measured to
  behave identically before this change —
  `todo/tickets/noncapturing-group-block-under-counted-separator.md`.
