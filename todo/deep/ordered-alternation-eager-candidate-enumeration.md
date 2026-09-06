# The `<subrule>` boundary is the last collect-then-pick barrier in the regex engine

**Narrowed 2026-09-07** by ADR-0073 Slice 1+3
(`news/2026-09/regex-atom-candidates-are-demand-driven.md`). The group,
alternation and separated-quantifier boundaries are now continuation-driven; the
subrule boundary is not, and it is the remaining face of the same defect. This
is ADR-0073's **Slice 2**.

## What is left (re-measured 2026-09-07 against `raku` and a current build)

An embedded `{ ... }` block inside a **non-ratcheted (`regex`) subrule** still
runs once per candidate the engine *computes* rather than once per candidate
raku's cursor *enters*.

| # | shape | subject | raku | mutsu |
| --- | --- | --- | --- | --- |
| E1 | `regex TOP { <part> 'c' }` / `regex part { \w* {B} }` | `aaac` | 2 | 5 |
| E2 | `regex TOP { <a> 'c' }` / `regex a { <b> }` / `regex b { \w* {B} }` | `aaac` | 2 | 5 |
| E3 | `token TOP { <part> 'c' }` / `regex part { \w* {B} }` | `aaac` | 1 | 5 |
| E6 | `regex part { 'a' [ 'b' {one} \|\| 'bc' {two} ] }` under `regex TOP { <part> 'cd' }` | `abcd` | `one` | `one,two` |

Controls that already agree and must not move: `regex TOP` calling a `token`
subrule (E4), `token`/`token` (E5), a `$*`-declaring caller (E7), and a
`make`-bearing block (E8). All eight rows are pinned in
`t/regex-lazy-candidate-enumeration.t`, the E-rows as `todo`.

E6 is the row the original 2026-09-05 filing was written around: the match
itself is correct, and every block mutsu runs is one raku *would* run if the
continuation had failed. It only bites a block with a side effect that must not
happen — a `die`, a push, a counter.

**E3 is the cheap half.** The caller is ratcheted, so it cannot backtrack into
the subrule at all; only the subrule's highest-priority candidate can ever be
used. Walking `part` with `first_only` under a ratcheted caller would fix E3
outright and cut a lot of wasted enumeration on the common grammar path (every
bundled grammar is `token`/`rule`). It needs a way to tell the `Named` arm that
the calling token is ratcheted — the arm currently has no such parameter — and
it must not disturb the left-recursion seed loop, which discovers re-entry by
*evaluating* candidates.

## Why the `Named` arm resisted Slice 1

`for_each_atom_candidate` (`src/runtime/regex/regex_match_lazy.rs`) drives
`Group` / `CaptureGroup` / `CaptureIsolatedGroup` / `Alternation` through a
`MatchSink::Cont` and falls back to the eager producer for everything else.
`RegexAtom::Named` was left on the fallback deliberately: its arm in
`regex_match_atom.rs` is not a simple sub-pattern walk. It carries

- the **left-recursion growing-seed loop** (`LR_ACTIVE` / `LR_MEMO` /
  `LR_SEED_READ`), which decides whether the rule is left-recursive at this
  position by evaluating the candidates and then checking whether the seed was
  consulted. It cannot know before evaluating, so a lazy path that skipped the
  bookkeeping would let a genuinely left-recursive rule recurse forever, and one
  that discovers re-entry mid-walk cannot un-run the blocks it already ran;
- the **proto rank-then-match dispatch** (ADR-0046), which ranks candidates by
  measurement and then commits to the first that matches — already
  demand-driven at the candidate level, but it wants the matched candidate's
  whole end set;
- `try_regex_subrule_as_method`, `try_custom_how_subrule_dispatch`, and the
  `<::(EXPR)>` symbolic indirection, each of which returns a `Vec` from a
  different place.

The separable case is: no arguments, no proto, exactly one resolved candidate,
and this `(name, position)` key not currently LR-active. That is the case E1/E2
need. Scoping it means deciding what happens when the seed *is* consulted after
blocks have already run — the honest answer is probably "keep the lazy result,
because raku entered the rule too", but that has to be measured against a
left-recursive grammar carrying a code block before it is asserted.

## Also still eager (measured, different mechanisms)

- **Conjunction** (`( \w* {B} & \w* )` on `"aaa"`: raku 1, mutsu 4). The
  `Conjunction` arm tries the first branch's candidate ends and requires every
  other branch to end exactly there, so it wants the first branch's set. It is a
  small, self-contained addition to `for_each_atom_candidate` — the first
  branch can be driven lazily and the other branches probed per candidate.
- The `:g` / `subst` scan and the counted-separator chain bug have their own
  ticket files (see the news entry's Residue section).
