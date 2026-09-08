# ADR-0073: Regex atom candidates are produced on demand, driven by the continuation

- Status: Accepted (Slices 1 and 3 implemented 2026-09-07; Slice 2 implemented
  in two halves, the ratcheted one 2026-09-07 and the streamed one 2026-09-08.
  What the streamed path still declines — a call with arguments, a proto, several
  resolved candidates, `<::(EXPR)>`, a custom-HOW grammar, `:m`, a program
  declaring any dynamic rule parameter, and a rule that really is part of a call
  cycle — stays on the eager arm and is tracked in
  `todo/deep/ordered-alternation-eager-candidate-enumeration.md`)
- Date: 2026-09-07
- Supersedes: nothing
- Related: [ADR-0009](0009-regex-code-assertion-execution-model.md) (a code assertion runs
  inline in the real interpreter; measurement never executes user code),
  [ADR-0007](0007-grammar-parse-trail-matcher.md) (the cursor + undo-trail matcher),
  [ADR-0022](0022-regex-alternation-ltm-ranking.md) (`|` ranks branches by declarative prefix),
  [ADR-0046](0046-proto-token-ltm-shares-one-ranking-mechanism.md) (rank-then-match for protos)

## Context

mutsu's regex engine is a depth-first backtracking walk over a pattern's tokens
(`walk_tokens`, `src/runtime/regex/regex_match_core.rs`). The walk itself is
demand-driven: it tries one candidate, descends into the rest of the pattern,
and only comes back for the next candidate when the continuation rejected the
current one.

Its *atoms* are not. When a token's atom is itself a sub-pattern — a group
`( … )` / `[ … ]`, an alternation, a `<subrule>` call, a separated quantifier's
repeated element — the walk asks a **producer** for the atom's complete
candidate set as a `Vec<(usize, RegexCaptures)>` before it descends into any of
them (`regex_match_atom_all_with_capture_in_pkg`,
`match_separated_quantifier`). Every candidate is therefore *computed* whether
or not it is ever *walked*.

For a code-free atom that is only a wasted-work question. For an atom
containing an embedded `{ … }` block it is a correctness question, because
ADR-0009 makes such a block run inline, once, where the cursor reaches it, with
real side effects. Computing a candidate runs the block; the walk then discards
the candidate. So a block fires once per candidate *computed* rather than once
per candidate *entered*, which is the count raku produces.

```raku
"aaac" ~~ / :my $c = 0; ( \w* { ++$c } ) c /;   # raku: 2      mutsu: 5
my $p = "line\nline2\nline3";
$p ~~ rx| :my $n = 0; ( \V* { ++$n } ) *%% \n |;  # raku: 3    mutsu: 17
```

The severity is not only a wrong counter. A block on a candidate raku never
enters can `die`, and that aborts a match that should have succeeded:

```raku
my $d = 0;
try { "aaac" ~~ / ( \w* { $d++; $d > 3 ?? die('boom') !! 1 } ) c / }
say $! ?? 'died' !! 'lived';   # raku: lived    mutsu: died
```

Two `todo/deep/` files describe this from opposite ends —
`regex-quantifier-eager-candidate-enumeration-overruns-code-blocks.md` (the
quantifier face) and `ordered-alternation-eager-candidate-enumeration.md` (the
alternation face). They are one defect.

### What the 2026-09-07 re-measurement changed

Both files' diagnoses were re-measured against `raku` and a fresh build. Two of
their central claims are wrong, and correcting them is what makes this ADR's
decision affordable:

1. **The quantifier file claims plain `*`/`+` "without forced backtracking …
   count correctly today".** They do not. `"aaa" ~~ / ( \w* { B } ) /` runs the
   block 4 times in mutsu and once in raku — with no continuation at all to
   backtrack from. Backtracking is not the trigger.
2. **The quantifier file blames "the general enumerate-every-candidate-length
   strategy used across the backtracking quantifier matchers".** It is not the
   quantifier matchers. `walk_quant_chain` already grows its chain one
   iteration at a time through the *singular* matcher
   (`regex_match_atom_with_capture_in_pkg`), so `[ \w+ { B } ]+`, `( \w+ { B } )*`,
   `[ \w { B } ] ** 2..3` and `( [ \w { B } ]* ) c` all already agree with raku.
   Likewise a top-level `\w* { B } c` — not inside any group — agrees.

The real trigger is narrower and sharper: **an atom that is asked for its whole
candidate set**. That is exactly the set of atoms whose producer arm recurses
through `regex_match_ends_from_caps_in_pkg`, plus the separated-quantifier chain
enumerator. Everything else in the engine is already demand-driven.

The alternation file is also understated in one direction: it says the residue
is a *non-ratcheted `regex` subrule* with a `||` inside. Unordered `|`
alternation is over-firing too, in every context, because
`ltm_rank_and_collect_branches` evaluates every branch before the ranking is
used (`'a' [ 'bcd' {C} | 'bc' {B} | 'b' {A} ]` on `"abcd"`: raku runs only the
LTM winner's block, mutsu runs all three).

### Measured table

Every row below was run against `raku` (2026-09-07) and against a fresh
`cargo build`. `B` denotes an embedded block that increments a counter; the
cells are how many times it ran. Rows are grouped by which mechanism produces
the candidates.

**Family A — a group atom asked for all its ends (`RegexQuant::One`/`ZeroOrOne`)**

| # | shape | subject | raku | mutsu (before) |
| --- | --- | --- | --- | --- |
| A1 | `( \w* {B} ) c` | `aaac` | 2 | 5 |
| A2 | `( \w+ {B} ) c` | `aaac` | 2 | 4 |
| A3 | `( 'a'? {B} ) b` | `ab` | 1 | 2 |
| A4 | `( [\w ** 1..4] {B} ) c` | `aaac` | 2 | 4 |
| A5 | `( \w*? {B} ) c` | `aaac` | 4 | 5 |
| A6 | `( \w* {B} )` | `aaa` | 1 | 4 |
| A7 | `( \w+ {B} ) $` | `aaa` | 1 | 3 |
| A8 | `[ \w* {B} ]` | `aaa` | 1 | 4 |
| A9 | `( ( \w* {B} ) ) c` | `aaac` | 2 | 5 |
| A10 | `( [ \w* ] {B} ) c` | `aaac` | 2 | 5 |
| A11 | `( \w* {B} ) <?before c>` | `aaac` | 2 | 5 |
| A12 | `( \w* {B} ) <!before x>` | `aaac` | 1 | 5 |
| A13 | `:my $*Z; ( \w* {B} ) c` | `aaac` | 2 | 5 |
| A14 | `m:g/ ( \w* {B} ) /` | `aa bb` | 4 | 12 |
| A15 | `.subst(/ ( \w* {B} ) /, 'X')` | `aaa` | 1 | 5 |
| A16 | `( \w* {B} & \w* )` (conjunction) | `aaa` | 1 | 4 |
| A17 | `( \w* { … die … } ) c` | `aaac` | lived | **died** |

**Family B — already demand-driven (controls; a laziness change must not move these)**

| # | shape | subject | raku | mutsu |
| --- | --- | --- | --- | --- |
| B1 | `\w* {B}` (top level) | `aaa` | 1 | 1 |
| B2 | `\w* {B} c` (top level) | `aaac` | 2 | 2 |
| B3 | `( {B} \w* )` | `aaa` | 1 | 1 |
| B4 | `( \w* ) {B}` | `aaa` | 1 | 1 |
| B5 | `( 'a' {B} ) b` | `ab` | 1 | 1 |
| B6 | `( 'a' {B} ) ( 'b' {B} )` | `ab` | 2 | 2 |
| B7 | `:r ( \w* {B} ) c` | `aaac` | 5 | 5 |
| B8 | `:r ( \w* {B} )` | `aaa` | 1 | 1 |
| B9 | `[ \w+ {B} ]+ b` | `aab` | 3 | 3 |
| B10 | `( \w+ {B} )* b` | `aab` | 3 | 3 |
| B11 | `[ \w {B} ]? b` | `ab` | 1 | 1 |
| B12 | `[ \w {B} ] ** 2..3 b` | `aaab` | 3 | 3 |
| B13 | `( [ \w {B} ]* ) b` | `aab` | 3 | 3 |
| B14 | `( [ \w {B} ]* ) c` | `aaac` | 4 | 4 |

**Family C — unordered `|` alternation**

| # | shape | subject | raku | mutsu (before) |
| --- | --- | --- | --- | --- |
| C1 | `'a' [ 'b' {+1} \| 'bc' {+10} ] 'cd'` | `abcd` | 11 | 11 |
| C2 | `'a' [ 'bc' {+10} \| 'b' {+1} ]` | `abcd` | 10 | 11 |
| C3 | `'a' [ 'b' {+1} \| 'bc' {+10} ]` | `abcd` | 10 | 11 |
| C4 | `:r 'a' [ 'bc' {+10} \| 'b' {+1} ] 'cd'` | `abcd` | 10 | 11 |
| C5 | `'a' [ 'bcd' {+100} \| 'bc' {+10} \| 'b' {+1} ]` | `abcd` | 100 | 111 |

C1 is the control: raku genuinely enters both branches there (the LTM winner
`'bc'` is rejected by `'cd'`), so a laziness change must keep it at 11.

**Family D — ordered `||` alternation (already fixed by the continuation-driven `walk_seq_alternation`)**

| # | shape | subject | raku | mutsu |
| --- | --- | --- | --- | --- |
| D1 | `'a' [ 'b' {+1} \|\| 'bc' {+10} ] 'cd'` | `abcd` | 1 | 1 |
| D2 | `'a' [ 'bc' {+10} \|\| 'b' {+1} ] 'cd'` | `abcd` | 11 | 11 |
| D3 | `'a' [ 'b' {+1} \|\| 'bc' {+10} ]` | `abcd` | 1 | 1 |

**Family E — `<subrule>` calls**

| # | shape | subject | raku | mutsu (before) |
| --- | --- | --- | --- | --- |
| E1 | `regex TOP { <part> 'c' }` / `regex part { \w* {B} }` | `aaac` | 2 | 5 |
| E2 | `regex TOP { <a> 'c' }` / `regex a { <b> }` / `regex b { \w* {B} }` | `aaac` | 2 | 5 |
| E3 | `token TOP { <part> 'c' }` / `regex part { \w* {B} }` | `aaac` | 1 | 5 (now 1) |
| E4 | `regex TOP { <part> 'c' }` / `token part { \w* {B} }` | `aaac` | 1 | 1 |
| E5 | `token`/`token` twin of E1 | `aaac` | 1 | 1 |
| E6 | `regex part { 'a' [ 'b' {one} \|\| 'bc' {two} ] }` under `regex TOP { <part> 'cd' }` | `abcd` | `one` | `one,two` |
| E7 | `token TOP { :my $*N = 0; <part> 'c' }` / `token part { \w* {B} }` | `aaac` | 1 | 1 |
| E8 | `token part { (\w*) {B; make ~$0} }` under `token TOP { <part> 'c' }` | `aaac` | 1 | 1 |

**Family F — separated quantifiers**

| # | shape | subject | raku | mutsu (before) |
| --- | --- | --- | --- | --- |
| F1 | `( \V* {B} ) *%% \n` | `line\nline2\nline3` | 3 | 17 |
| F2 | `( \V* {B} ) *% \n` | `line\nline2\nline3` | 3 | 17 |
| F3 | `( \w+ {B} ) +% ','` | `a,b,c` | 3 | 3 |
| F4 | `:r ( \w+ {B} ) +% ','` | `a,b,c` | 3 | 3 |
| F5 | `( \w {B} ) +%% ','` | `a,b,c` | 3 | 3 |
| F6 | `[ \w+ {B} ] ** 1..3 % ','` | `a,b,c` | 3 | 6 |
| F7 | `:r ( \V* {B} ) *%% \n` | `line\nline2` | 2 | 2 |
| F8 | `[ \w+ ] +% ',' {B}` | `a,b,c` | 1 | 1 |

**Not part of this ADR (measured, filed separately)**

`regex TOP { <part> 'c' }` / `regex part { \w* }` with an `:actions` class:
raku runs the `part` action twice (`aaac`, then `aaa` after backtracking),
mutsu runs it once. That is mutsu *under*-firing an action, the mirror of this
ADR's problem, and it lives in the reduce walk rather than in candidate
production.

## Decision

**An atom's candidates are produced on demand, one at a time, and the walk's
continuation decides whether the next one is ever computed.** Concretely:

1. The pattern walk gains a **sink** (`MatchSink`) in place of its
   `&mut Vec<(usize, RegexCaptures)>` output parameter. A sink is either
   `Collect` (the existing behaviour: push and keep walking) or `Cont` (a
   callback invoked at each completed match, returning "stop the walk").
   `regex_walk_ends_in_pkg` is the continuation-form entry point beside the
   existing collecting `regex_match_ends_from_caps_in_pkg`.

2. `for_each_atom_candidate` replaces the "collect then iterate in reverse"
   idiom at the walk's atom sites. For the atom kinds that recurse into a
   sub-pattern it drives that sub-pattern's walk through a `Cont` sink, so
   candidate *k+1* of the atom is computed only after candidate *k* has been
   rejected by the real continuation. For every other atom kind it falls back
   to the existing eager producer and iterates it highest-priority-first —
   identical behaviour, so the change is confined to the atoms that can contain
   code.

3. The same treatment is applied to the separated-quantifier chain
   enumerator: `for_each_separated_candidate` walks the chain DFS and reports
   each chain node to a continuation instead of accumulating every node into a
   `Vec`.

4. **Ranking stays separate from walking, and ranking still never runs user
   code (ADR-0009).** `|` alternation ranks its branches by declarative prefix
   *first* (measurement, no execution), then walks the branches in rank order
   through the continuation — the same shape `walk_seq_alternation` already
   uses for `||` and `ADR-0046` uses for protos. What changes is only that
   branch *k+1* is not *evaluated* until branch *k* has been rejected.

5. **In LTM declarative mode the eager producer is used unchanged.**
   Measurement has no continuation to be driven by and must remain
   side-effect-free; ADR-0022/ADR-0046's ranking paths are untouched.

### Options considered

**Option 1 — two-phase: measure candidate ends with code suppressed, then
re-run the winning candidate for real.** This is what the quantifier ticket
proposed as its alternative, and it is the cheaper change: no interface moves,
only a suppression flag around the producer plus a replay of the adopted
candidate. It is rejected. Suppressing a block changes the *match* whenever the
block writes an in-regex `:my`/`:let` lexical that a later part of the same atom
interpolates, so the measured ends can be wrong; and mutsu has already shipped
and then deleted exactly this shape once — `SPECULATIVE_ALT_BRANCH` muted a
speculatively-measured branch's blocks and produced the mirror bug, a block that
never ran even when its branch was the one the match needed
(`news/2026-09/ordered-alternation-driven-by-the-continuation.md`). Per
CLAUDE.md's gain/risk rule, a mechanism that is correct only under an incomplete
static analysis is the risky option even when it is the smaller diff.

**Option 2 — make the whole engine CPS/generator-based.** Rejected as the unit
of work, not as a direction. The walk is already a continuation; only the atom
boundary is not. Converting the whole engine at once would move the hot path
(`regex_match_ends_from_caps_in_pkg` has callers in LTM ranking, `.parse`,
`subst`, token methods) with no separable validation step.

**Option 3 (chosen) — push the continuation across the atom boundary.** The
walk keeps its shape; the atom boundary stops being a collect-then-pick barrier.
This is the minimal mechanism that makes the count exact, it reuses the
`walk_seq_alternation` precedent verbatim, and it cannot go flaky: there is no
predicate to get wrong, because nothing is suppressed and nothing is replayed.
The eager producer survives for the paths that genuinely want a set (LTM
ranking, the `Named` left-recursion seed, the no-capture matcher), which is why
it can be adopted incrementally.

### What each option fixes

| rows | Option 1 (measure + replay) | Option 3 (continuation) |
| --- | --- | --- |
| A1-A17 | fixed, except where a block's write feeds its own atom | fixed |
| B1-B14 | unchanged | unchanged |
| C2-C5 | fixed | fixed |
| A14, A15 (`:g` / `subst`) | not addressed by either — the scan is the mechanism | not addressed |
| A16 (conjunction) | fixed | needs a `Conjunction` arm; not in Slice 1 |
| E3 | fixed only if the subrule boundary is also replayed | fixed by Slice 2's ratcheted half |
| E1, E2, E6 | fixed only if the subrule boundary is also replayed | fixed by Slice 2's streamed half |
| F1, F2 | fixed | fixed by Slice 3 |
| F6 | not a count bug at all — a pre-existing wrong *match* | unchanged, filed separately |
| C1, D1-D3 (controls) | at risk (suppression can mute a needed block) | unchanged by construction |

## Slices

- **Slice 1 — IMPLEMENTED 2026-09-07.** `MatchSink` + `regex_walk_ends_in_pkg`
  (`regex_match_core.rs`) + `for_each_atom_candidate` (`regex_match_lazy.rs`),
  with demand-driven arms for `Group`, `CaptureGroup`, `CaptureIsolatedGroup`
  and `Alternation`, wired into `walk_tokens`' `One` and `ZeroOrOne` arms.
  Fixes A1-A13 and A17, and C2-C5.
- **Slice 3 — IMPLEMENTED 2026-09-07.** `for_each_separated_candidate`
  (`regex_match_sep_lazy.rs`) — the CPS form of `enumerate_separated_chains` /
  `extend_separated_chain`, which additionally stops recording every
  intermediate DFS node as its own candidate. Fixes F1 and F2 (17 -> 3).
- **Slice 2 (ratcheted half) — IMPLEMENTED 2026-09-07.**
  `regex_match_atom_all_with_capture_opts`' `subrule_first_only` knob, fed by
  the calling token's `ratchet` flag from `for_each_atom_candidate`, plus
  `regex_subrule_lazy::pattern_is_rule_call_free`. A ratcheted caller cannot
  backtrack into the subrule at all, so only the subrule's highest-priority end
  can ever be used and each candidate body is walked with `first_only` instead
  of having its whole end set collected and then discarded. Fixes E3
  (`token TOP { <part> 'c' }` / `regex part { \w* {B} }`: 5 -> raku's 1).

  The guard is the load-bearing part. The `Named` arm's growing-seed loop
  discovers left recursion by *evaluating* candidates and then asking whether
  the seed was consulted; a `first_only` walk can return before it ever enters
  the branch that re-enters the rule, and the loop would then wrongly conclude
  "not left-recursive". Measured, not hypothetical:
  `token expr { <term> | <expr> '+' <term> }` ranks `<term>` first, so an
  unguarded `first_only` stops on `1` and `.parse('1+2+3')` fails.
  `pattern_is_rule_call_free` is the sound precondition — a body that cannot
  invoke a named rule cannot re-enter its own key — and it lists the safe
  `RegexAtom` variants explicitly so a new variant is excluded by default. It is
  an over-approximation (leaf rules only); tightening it to "not part of a call
  cycle" needs a rule-call-graph analysis and is residue. The seed loop keeps a
  runtime fallback (redo the iteration with the full set) for the case a `{ … }`
  block re-enters the rule by hand.
- **Slice 2 (streamed half) — IMPLEMENTED 2026-09-08.**
  `drive_named_subrule_candidates` (`regex_match_lazy_subrule.rs`) walks the
  subrule's body through a `MatchSink::Cont`, wrapping each end into the atom's
  capture delta as it is produced, so end *k+1* is computed only once the real
  continuation has rejected end *k*. It takes the separable case named above —
  no arguments, no proto, exactly one resolved candidate, no custom-HOW
  dispatch, no `:m`, no dynamic (`$*`) rule parameters anywhere in the program,
  and this key not LR-active — and everything else falls back to the eager arm
  unchanged. Fixes E1, E2 and E6.

  What made the case decidable is `src/runtime/regex/regex_call_graph.rs`, which
  replaces Slice 2's syntactic guard with the question the seed loop actually
  asks: *can this rule reach a call to its own name?* Edges come from the same
  resolution the matcher uses, `<.ws>` is an edge to `ws`, a name that resolves
  to no rule is a builtin assertion (no edge) unless the grammar has a method of
  that name, and anything unresolvable answers "may re-enter". So `true` means
  proven safe and `false` only means not proven. With re-entry ruled out the
  growing-seed loop is a formality — one iteration, seed unconsulted, first
  result final — and the walk can be streamed. A ratchet on the calling token
  simply stops the stream after the first end, which is what the ratcheted half
  needed `first_only` for, so **non-leaf rules under a ratcheted caller are now
  streamed too** and the leaf-only over-approximation is lifted for them.

  The analysis is memoized per `TOKEN_DEFS_GEN` in three layers (streamable
  verdict, direct edges, and the raw-text staticness that decides whether a
  node's edges are generation-stable at all), because re-deciding it per call
  cost more than the laziness saved — measured at +22% instructions on
  `bench-yaml-parse` before the memoization and +1.6% after. A rule body whose
  text splices a value in is answered "not knowable" *and cached as such*: a
  `Regex`-valued scalar is interpolated as pattern SOURCE
  (`interpolate_bound_regex_scalars`), so such a body really can gain a call
  edge between two attempts. A sigil that appears only inside a `{ … }` code
  block does not, because that pass treats code blocks as opaque — which is what
  keeps the overwhelmingly common `token part { \w+ { $n++ } }` shape eligible.

  The activation is still registered while streaming, so a `{ … }` block that
  re-enters the key by hand reads the empty seed and fails instead of recursing
  forever; if the seed turns out to have been consulted and nothing has
  committed yet, the call is handed back to the eager growing-seed path.

Two rows are deliberately **out of this ADR's scope** and have their own ticket
files, because their mechanism is not candidate production: the `:g` / `subst`
start-position scan (A14/A15), and a pre-existing wrong *match* for a
non-capturing group carrying a block under a counted separator (F6). The
`Conjunction` arm (A16) was in scope and landed the same day
(`news/2026-09/regex-conjunction-candidates-are-demand-driven.md`): the first
branch is driven lazily and the other branches keep the eager per-candidate
yes/no probe.

## Consequences

- The eager producer and the demand-driven driver coexist. That is deliberate,
  not debt to be paid down blindly: LTM ranking, the left-recursion seed and the
  no-capture matcher all legitimately want the whole set, and forcing them
  through a continuation would buy nothing.
- Perf should improve, not regress, on the common grammar path: when the first
  candidate satisfies the continuation, the remaining candidates are never
  computed at all. The risk is the opposite direction — a pattern whose
  continuation rejects *every* candidate now pays the walk's per-candidate
  bookkeeping instead of one batched vector. `make roast` and
  `scripts/battery-testsuite.sh` (every bundled battery is grammar-driven:
  `YAMLish`, `JSON::Fast`, `Cro::HTTP`, `TOML`, the vendored `zef`) are the gate.
- `t/regex-lazy-candidate-enumeration.t` pins every row of the table
  above, including the Family B and C1/D controls and the still-unfixed rows as
  `todo` — so a future laziness change cannot quietly move a row that already
  agreed, and closing a residue row shows up as a test that starts passing
  unexpectedly.
- The walk's stop signal now means two different things at an atom boundary and
  they must not be conflated: `true` from the candidate continuation unwinds the
  WHOLE enclosing DFS, while a `:ratchet` only exhausts *this* atom's
  candidates. Both stop the inner walk, so the drivers record which one happened
  separately (`unwind` in `drive_subpattern_candidates` /
  `drive_alternation_branch`). Returning the inner walk's own `bool` there is
  the obvious bug to write, and it turns a ratcheted group into a match failure.
