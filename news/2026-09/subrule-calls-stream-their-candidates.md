# A `<subrule>` call streams its candidates, decided by a rule call graph

ADR-0073's Slice 2 — the `<subrule>` boundary, the last collect-then-pick
barrier in the regex engine — is closed for its **non-ratcheted** half, and the
ratcheted half's leaf-only restriction is lifted at the same time. Both fall out
of one new thing: an analysis that answers *can this rule reach a call to
itself?* over the grammar's rule call graph.

## What was still wrong

An embedded `{ … }` block inside a subrule called from a `regex` caller ran once
per candidate the engine *computed* rather than once per candidate raku's cursor
*enters*:

```raku
my $n = 0;
grammar G { regex TOP { <part> 'c' }; regex part { \w* { $n++ } } }
G.parse('aaac');
say $n;                 # raku: 2     mutsu before: 5
```

The match itself was right; every block mutsu ran is one raku *would* run if the
continuation had failed. It bites a block with a side effect that must not
happen on a path never taken — a `die`, a push, a counter. The nastiest shape was
an ordered alternation inside such a subrule, where the losing branch's block
fired even though the winning branch had already carried the whole parse:

```raku
my @seen;
grammar G {
    regex TOP  { <part> 'cd' }
    regex part { 'a' [ 'b' { @seen.push('one') } || 'bc' { @seen.push('two') } ] }
}
G.parse('abcd');
say @seen;              # raku: [one]     mutsu before: [one two]
```

## Why the `Named` arm resisted the earlier slices

Slice 1 turned `Group` / `CaptureGroup` / `CaptureIsolatedGroup` / `Conjunction`
/ `Alternation` into continuation-driven producers, and Slice 2's ratcheted half
walked a subrule's body with `first_only` when the calling `token` could not
backtrack into it anyway. Neither could touch the general `<subrule>` case,
because the `Named` arm carries the **left-recursion growing-seed loop**
(`LR_ACTIVE` / `LR_MEMO` / `LR_SEED_READ`). That loop decides whether a rule is
left-recursive *at this position* by evaluating its candidates and then asking
whether the seed was consulted — it cannot know before evaluating, and it cannot
un-run the blocks a cut-short walk already ran. The ratcheted half sidestepped it
with a syntactic precondition, `pattern_is_rule_call_free`: a body that cannot
invoke a named rule at all cannot re-enter its own key. Sound, but it admits
**leaf rules only**, so a grammar's interior rules kept paying for their whole
end set even under a ratcheted caller.

## The call graph answers the real question

`src/runtime/regex/regex_call_graph.rs` replaces the syntactic guess with the
question the seed loop actually cares about. A rule's left-recursion key is
re-entered only by a *call* to a rule of the same name, so walking the call graph
from `(package, name)` and asking whether `name` shows up again in the reachable
set decides it exactly:

- edges come from the same resolution the matcher uses
  (`parsed_subrule_candidates`), so a candidate's body resolves its unqualified
  references against the package that *defined* it;
- `<.ws>` is an edge to `ws`; a name that resolves to no rule is a builtin
  assertion or character class (no edge) unless the grammar has a *method* of
  that name, which is arbitrary user code;
- anything unresolvable — a rule call with arguments, `<::(EXPR)>`, `<{ … }>`,
  `<~~>`, a body whose pattern will not resolve — answers "may re-enter".

So the verdict is a sound under-approximation of safety: `true` means proven
safe, `false` means only *not proven*, and the caller falls back to today's eager
path.

## Memoize it, or it costs more than it saves

Re-deciding this per call was measured at **+22% instructions** on
`bench-yaml-parse` (`callgrind`, 804M vs 657M Ir), almost all of it `malloc` and
`free`: every subrule call re-resolved its own rule and then walked the grammar's
whole call cone. Three memo layers keyed on `TOKEN_DEFS_GEN` bring that to
**+1.6%** (and to parity on `bench-grammar-parse` / `-deep`):

- `STREAMABLE` — `package -> subrule atom text -> may this be streamed?`, a
  two-level map so the hot probe borrows `&str` instead of building a
  `(String, String)` key. It is asked *before* the atom's name is parsed, so an
  ineligible call costs two hash lookups and nothing else.
- `DIRECT_CALLS` — one node's edges.
- the raw-text staticness that decides whether a node's edges are
  generation-stable at all.

That last one is the subtle part. A body carrying an interpolation is re-parsed
on every attempt, and a `Regex`-valued scalar is spliced in as pattern *source*
(`interpolate_bound_regex_scalars`), so such a body genuinely can gain a call
edge between two attempts — its edges are answered "not knowable", and **that
verdict is itself cached**, which is what keeps the analysis off the hot path. A
sigil that appears only inside a `{ … }` code block does not count, because that
interpolation pass treats code blocks as opaque; without that distinction the
overwhelmingly common `token part { \w+ { $n++ } }` shape — the very shape this
work exists for — would have been ineligible.

With re-entry ruled out, the growing-seed loop is a formality — one iteration,
seed unconsulted, first result final — and the call can be **streamed**.
`drive_named_subrule_candidates` (`regex_match_lazy.rs`) walks the subrule body
through a `MatchSink::Cont`, wrapping each end into the atom's capture delta as
it is produced, so end *k+1* is computed only after the real continuation has
rejected end *k*. It takes only the shape where none of the `Named` arm's other
machinery is in play: an argument-less call, exactly one non-proto candidate, no
custom-HOW dispatch, no `:m`, no dynamic (`$*`) rule parameters anywhere in the
program, and a key that is not already LR-active. A ratchet on the calling token
simply stops the stream after the first end, which is what the ratcheted half
used `first_only` for — so non-leaf rules under a ratcheted caller are now
streamed too.

The one construct the call graph deliberately treats as harmless is an embedded
`{ … }` block: it is user code and could re-enter the rule by hand. The
activation is still registered (so such a re-entry reads the empty seed and fails
instead of recursing forever), and if the seed turns out to have been consulted
while nothing has committed yet, the call is handed back to the eager
growing-seed path.

## Measured

`B` is an embedded block incrementing a counter; the cells are how many times it
ran. Every row was run against `raku` (2026-09-07) and a fresh `cargo build`.

| shape | raku | before | after |
| --- | --- | --- | --- |
| `regex TOP { <part> 'c' }` / `regex part { \w* {B} }` on `aaac` | 2 | 5 | 2 |
| `regex TOP { <a> 'c' }` / `regex a { <b> }` / `regex b { \w* {B} }` | 2 | 5 | 2 |
| `regex part { 'a' [ 'b' {one} \|\| 'bc' {two} ] }` under `regex TOP { <part> 'cd' }` | `one` | `one,two` | `one` |
| `token TOP { <part> 'c' }` / `regex part { <inner> }` / `regex inner { \w* {B} }` | 1 | 5 | 1 |

The controls that already agreed did not move: a `token` subrule under a `regex`
caller, `token`/`token`, a `$*`-declaring caller, a `make`-bearing block, a
quantified subrule under either kind of caller, a `rule` caller's `<.ws>`, and
the left-recursive `token expr { <term> | <expr> '+' <term> }` growing-seed rows
— the last of which is exactly what the call graph refuses, keeping it on the
full walk. All of them are pinned in `t/regex-lazy-candidate-enumeration.t`,
which also gains a mutual-recursion row (`a` reaches `a` through `b`) and a
backtracking-through-two-subrule-levels row.

## Residue

The eager `Named` arm still owns every call the streamed path declines: one with
arguments, a proto's rank-then-match dispatch, several resolved candidates,
`<::(EXPR)>` indirection, a custom-HOW grammar, `:m`, a program that declares any
dynamic rule parameter, and — by construction — a rule that really is part of a
call cycle. See
`todo/deep/ordered-alternation-eager-candidate-enumeration.md`.
