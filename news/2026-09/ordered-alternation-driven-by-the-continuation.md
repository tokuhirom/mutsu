# Ordered alternation is driven by the continuation, not measured up front

`||` is an *ordered* alternation: raku enters branch *k+1* only after branch *k*
has been entered and the rest of the pattern has rejected every way it could
match. mutsu's matcher instead measured **every** branch the moment the atom was
reached, because a later branch's candidate ends are what let an enclosing
pattern backtrack into it. An embedded `{ … }` block therefore ran on paths
raku's cursor never takes, and the suppression added to hide that
(`SPECULATIVE_ALT_BRANCH`) produced the mirror bug: a block that never ran even
when its branch was the one the match needed.

```raku
my @fired;
grammar G {
    regex TOP { 'a' [ 'bc' || 'b' { @fired.push('second') } ] 'cd' }
}
say G.parse('abcd').defined;   # both: True
say @fired.raku;               # raku: ["second"]   mutsu: []
```

Branch 1 (`'bc'`) matches, but the `'cd'` after the alternation cannot continue
from its end, so the match backtracks into branch 2 and succeeds there. raku's
cursor reaches branch 2's block; mutsu had already measured that branch
speculatively and skipped its block.

## Root cause

`RegexAtom::SequentialAlternation` was handled entirely inside the atom
candidate producer (`src/runtime/regex/regex_match_atom.rs`), which has no access
to the continuation — the rest of the enclosing pattern. It could therefore only
guess at when a later branch was "really" reached, and the guess was a pair of
text scans in `regex_helpers.rs`: `SPECULATIVE_ALT_BRANCH` skipped a block seen
while a later branch was being measured, and `code_block_produces_value` carved
out an exception for blocks whose source mentions `make` (an atom after the
alternation can read a branch's `make` back through `$/.values[0].ast` while the
match is still running, which is how YAMLish's `Schema::JSON` `TOP` resolves a
plain scalar).

## Fix

The token walk already *is* a depth-first backtracking machine with the
continuation in hand, and the parser emits `[ A || B ]` directly as a token atom
rather than wrapping it in a group. So the alternation is now driven from
`walk_tokens` (`walk_seq_alternation`, `src/runtime/regex/regex_match_core.rs`):
branch *k*'s candidates are produced, tried highest-priority first against the
real continuation, and only when every one of them has been rejected is branch
*k+1* evaluated at all. Because evaluating a branch is what runs its blocks, the
blocks now fire exactly when raku's cursor enters the branch — including the
repeats raku makes on backtracking.

`:ratchet` (so: every `token` and `rule`) makes this a hard commit: the first
branch that matches keeps only its highest-priority candidate and no later
branch is evaluated. That is what keeps `Config::TOML`'s
`\\ [ <escape> || . { die "bad escape sequence" } ]` from dying on every valid
escape — the losing branch is never entered, rather than being entered with its
side effects muted.

`SPECULATIVE_ALT_BRANCH` and `code_block_produces_value` are both deleted. The
per-branch candidate packaging moved to `seqalt_branch_candidates`, shared by the
driver and by the residual eager producer (which still serves the alternation
under a list quantifier and in LTM declarative-prefix measurement).

### `Grammar.parse` stops at the first full match

`.parse` needs the match that covers the whole text, and it used to get there by
walking the start rule's entire backtracking tree, ranking every end it found and
picking a full one. The depth-first walk already discovers the highest-priority
full match first, so the rest of that tree was pure waste — and, now that
evaluating a `||` branch is what runs its blocks, actively harmful: the walk kept
entering later branches after the parse had already succeeded through an earlier
one. `regex_match_ends_stop_at_full` stops the walk at the first match reaching
the end of the subject while still collecting the shorter prefix matches, which
is what a *failed* parse needs for its action dispatch.

### Two `:ratchet` leaks the ordering fix exposed

- A top-level `|` / `||` splits the pattern and re-parses each alternative as its
  own pattern. That re-parse re-applied `:i` and `:s` but **not** `:ratchet`, so
  `token TOP { 'z' | \d+ \d }` matched `"12"` in mutsu (raku: no match) and an
  ordered alternation inside such an alternative could backtrack into its later
  branches even in a ratcheted rule.
- The token synthesised to carry a whole-pattern alternation hardcoded
  `ratchet: false`, so `token TOP { || "a" || { die } }` was not committed
  either.

Both now carry the ambient ratchet (`src/runtime/regex_parse_core.rs`).

## Measured against raku

Every expectation below was measured against the reference implementation and is
pinned in `t/ordered-alternation-branch-order.t`; the losing-branch control
`t/ordered-alternation-loser-code-block.t` stays green.

| shape | raku | mutsu before | mutsu after |
| --- | --- | --- | --- |
| `regex TOP { 'a' [ 'bc' \|\| 'b' {B} ] 'cd' }` on `abcd` | `[B]` | `[]` | `[B]` |
| `regex TOP { 'a' [ 'b' {A} \|\| 'bc' {B} ] 'cd' }` on `abcd` | `[A]` | `[A]` | `[A]` |
| `regex TOP { 'a' [ 'b' {A} \|\| 'bc' {B} ] 'd' }` on `abcd` | `[A, B]` | `[A]` | `[A, B]` |
| three branches, only the third completes | `[1, 2, 3]` | `[1]` | `[1, 2, 3]` |
| `token TOP { 'z' \| 'a' [ 'b' \|\| . {L} ] }` on `ab` | `[]` | `[L]` | `[]` |
| `token TOP { 'z' \| \d+ \d }` on `12` | no match | match | no match |

A branch that raku *enters and then abandons* still runs its block in both
implementations (rows 3 and 4) — that is not a divergence to "fix", it is what
ordered alternation does.

## Residue

One shape is still driven eagerly: a **non-ratcheted** `regex` subrule, which is
asked for its whole candidate set so the caller can backtrack into it. raku
genuinely does re-enter such a subrule — `regex part { 'a' [ 'bc' || 'b' {B} ] }`
under `TOP { <part> 'cd' }` reaches `B`, and mutsu now reaches it too — so the
candidate set cannot simply be truncated. Where the caller succeeds through an
earlier branch, mutsu has already evaluated the later ones and their blocks run
when raku's would not. Closing that needs the subrule boundary to be
continuation-driven rather than collect-then-pick; it is recorded in
`todo/deep/ordered-alternation-eager-candidate-enumeration.md`.
