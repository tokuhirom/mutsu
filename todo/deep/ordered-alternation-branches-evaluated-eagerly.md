# Ordered alternation evaluates every branch eagerly, so a later branch's side effects never fire

mutsu's `||` (ordered alternation) matches **every** branch when the atom is
reached, because a later branch's candidate ends are what let an enclosing
pattern backtrack into it (`RegexAtom::SequentialAlternation`,
`src/runtime/regex/regex_match_atom.rs`). raku instead tries branch *n+1* only
after branch *n*'s continuation has failed.

The difference is papered over by `SPECULATIVE_ALT_BRANCH`
(`src/runtime/regex/regex_helpers.rs`): once an earlier branch has matched, later
branches are evaluated with the flag set, and an embedded `{ … }` block seen
while it is set is **skipped** so its side effects do not fire on a branch raku's
cursor never reaches. That is what keeps `Config::TOML`'s
`\\ [ <escape> || . { die "bad escape sequence" } ]` from dying on every valid
escape (`t/ordered-alternation-loser-code-block.t`).

The cost is the mirror image: when the later branch **is** the one the overall
match needs, raku runs its block and mutsu does not.

## Repro (measured 2026-09-05)

```raku
my @fired;
grammar G {
    regex TOP { 'a' [ 'bc' || 'b' { @fired.push('second') } ] 'cd' }
}
say G.parse('abcd').defined;
say @fired.raku;
```

- raku: `True` / `["second"]`
- mutsu: `True` / `[]`

Branch 1 (`'bc'`) matches, but the `'cd'` that follows the alternation then
cannot, so the overall match backtracks into branch 2 and succeeds through it.
raku's cursor therefore does reach branch 2's block and runs it; mutsu had
already evaluated that branch speculatively and skipped the block.

(The rule must be a `regex`, not a `token` — a ratcheting `token` never
backtracks into the second branch at all, and both implementations then fail the
match.) Re-measure before designing.

## Current narrowing (not a fix)

`news/2026-09/grammar-inline-code-block-order.md` moved `make`-bearing blocks
onto the inline path, which turned this from a lost *side effect* into a lost
*value*: an atom after the alternation can read the branch's `make` back through
`$/.values[0].ast` while the match is still running, which is how YAMLish's
`Schema::JSON` TOP resolves a plain scalar. So the skip is now gated by
`code_block_produces_value` — a value-producing block in a speculative branch
runs, a pure side-effect one is still skipped. That is a text scan, and it is
there only because the underlying evaluation order is wrong.

## What the real fix looks like

Either make branch evaluation lazy (produce branch *n+1*'s candidates only when
the engine has exhausted branch *n*'s), or keep the eager measurement but
**re-match the chosen branch for real** at the moment the engine commits to one
of its candidates — `CapStore::merge_delta` in
`src/runtime/regex/regex_trail.rs` is that moment, but it has no `&mut
Interpreter`, so the re-run has to be hoisted into the token loop in
`regex_match_core.rs`.

Either way `SPECULATIVE_ALT_BRANCH` and `code_block_produces_value` both
disappear, and a later branch's blocks fire exactly when raku's cursor reaches
them — including on backtracking, which raku also re-runs.

## Blast radius

The alternation atom is on the hottest path in the engine, and every grammar in
the batteries uses `||`. It lands under full-roast plus
`scripts/battery-testsuite.sh` or not at all.
