# The non-ratcheted `<subrule>` boundary is the last collect-then-pick barrier in the regex engine

**Narrowed 2026-09-07** by ADR-0073 Slice 1+3
(`news/2026-09/regex-atom-candidates-are-demand-driven.md`) and again the same
day by Slice 2's ratcheted half
(`news/2026-09/ratcheted-subrule-calls-are-first-only.md`). The group,
alternation, conjunction and separated-quantifier boundaries are
continuation-driven; a `<subrule>` called from a **ratcheted** caller
(`token` / `rule`) now walks its body with `first_only`. What is left is the
**non-ratcheted (`regex`) caller**, which genuinely can backtrack into the
subrule and therefore needs a streamed candidate set rather than a truncated
one. This is the residue of ADR-0073's **Slice 2**.

## What is left (re-measured 2026-09-07 against `raku` and a current build)

An embedded `{ ... }` block inside a subrule called from a **non-ratcheted
(`regex`) caller** still runs once per candidate the engine *computes* rather
than once per candidate raku's cursor *enters*.

| # | shape | subject | raku | mutsu |
| --- | --- | --- | --- | --- |
| E1 | `regex TOP { <part> 'c' }` / `regex part { \w* {B} }` | `aaac` | 2 | 5 |
| E2 | `regex TOP { <a> 'c' }` / `regex a { <b> }` / `regex b { \w* {B} }` | `aaac` | 2 | 5 |
| E6 | `regex part { 'a' [ 'b' {one} \|\| 'bc' {two} ] }` under `regex TOP { <part> 'cd' }` | `abcd` | `one` | `one,two` |

Controls that already agree and must not move: `regex TOP` calling a `token`
subrule (E4), `token`/`token` (E5), a `$*`-declaring caller (E7), a
`make`-bearing block (E8), a quantified subrule under a ratcheted caller (E3g),
and — new with the ratcheted half — the left-recursive `token expr { <term> |
<expr> '+' <term> }` growing-seed rows (E3d/E3e). All of them are pinned in
`t/regex-lazy-candidate-enumeration.t`; the three rows above are still `todo`.

E6 is the row the original 2026-09-05 filing was written around: the match
itself is correct, and every block mutsu runs is one raku *would* run if the
continuation had failed. It only bites a block with a side effect that must not
happen — a `die`, a push, a counter.

## Why the `Named` arm resisted Slice 1, and what the ratcheted half did instead

`for_each_atom_candidate` (`src/runtime/regex/regex_match_lazy.rs`) drives
`Group` / `CaptureGroup` / `CaptureIsolatedGroup` / `Conjunction` /
`Alternation` through a `MatchSink::Cont` and falls back to the eager producer
for everything else. `RegexAtom::Named` is still on the fallback: its arm in
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

The **ratcheted** half sidestepped all of that: when the calling token cannot
backtrack into the atom, only the subrule's highest-priority end can ever be
used, so the arm walks each candidate body with `first_only` instead of
collecting its whole end set. No streaming, no interface change — the arm still
returns a `Vec`, it is just a `Vec` of length ≤ 1. The one hazard is the seed
loop above (a `first_only` walk can return before entering the branch that
re-enters the rule), and it is closed by a *sound* structural precondition:
`regex_subrule_lazy::pattern_is_rule_call_free` — a body that cannot invoke a
named rule cannot re-enter its own key. That is an over-approximation: it admits
leaf rules only.

## The two pieces of residue

1. **The non-ratcheted caller (E1/E2/E6).** A `regex` caller really can
   backtrack into the subrule, so truncation is wrong and the arm has to
   *stream*: `regex_match_atom_all_with_capture_opts` would have to grow a
   `MatchSink::Cont` form so candidate *k+1* of the subrule is computed only
   after *k* has been rejected. That means threading the continuation through
   the seed loop, the proto dispatch and the three `Vec`-returning escape
   hatches — the work Slice 1 deliberately deferred. The separable sub-case
   remains "no arguments, no proto, exactly one resolved candidate, this
   `(name, position)` key not currently LR-active".

2. **Non-leaf rules under a ratcheted caller.** `pattern_is_rule_call_free`
   excludes any body containing `<name>`, `<.ws>`, `<{ … }>` or `<~~>`, so a
   grammar's interior rules keep computing their full end set even though their
   ratcheted caller will use only the first. Lifting this needs a rule-call-graph
   cycle analysis ("can this rule reach itself?") rather than the current
   syntactic "does this rule call anything at all?", and that analysis has to
   cope with inheritance, protos and `<::(EXPR)>` indirection.

## Also still eager (measured, different mechanisms)

- The `:g` / `subst` scan and the counted-separator chain bug have their own
  ticket files (see the Slice 1 news entry's Residue section).
