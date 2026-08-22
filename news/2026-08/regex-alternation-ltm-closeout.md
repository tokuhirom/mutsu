# `|` alternation LTM ranking: closed out (ADR-0022, all five slices)

`todo/deep/regex-alternation-ltm-longest-literal-prefix.md` recorded that mutsu's `|`
alternation picked the first matching branch instead of applying Rakudo's Longest Token
Matching. The investigation it triggered produced
[ADR-0022](../../docs/adr/0022-regex-alternation-ltm-ranking.md), whose five slices all
merged on 2026-08-11. Re-verified against `main` at `4e4ca30ce` on 2026-08-20, this
finding is fully resolved, so the todo file retires here.

## What the ticket claimed, and what was actually true

The ticket's headline ("`|` picks the first matching branch") was already a correction
target when ADR-0022 was written: mutsu's three engine paths ranked by **longest actual
full-branch match**, ties broken by declaration order. The Cro `http-router.rakutest`
test 61 symptom was a genuine length *tie* (14 vs 14) lost on the tie-break, not a
first-match commit. Rakudo ranks by declarative-prefix length, then longest-literal
(litlen), then declaration order — the triple ADR-0022 implements.

## Verification on current `main`

All four divergence classes the ticket and ADR-0022 §1 named now agree with `raku`:

| case | raku | mutsu |
|---|---|---|
| `"/category/tree" ~~ / "/category/" (\w+) \| "/category/tree" /` (litlen tie-break) | literal branch | literal branch |
| `'abc---' ~~ /<ltm_ws1> \| <ltm_ws2>/` (prefix, not full length) | `abc-` | `abc-` |
| `'food' ~~ / 'foo' \| ['doof' \|\| 'food'] /` (`\|\|` contributes only its first branch) | `foo` | `foo` |
| `"aaab" ~~ / [ a+ \| q ] ab /` (backtracking into shorter ends of the chosen branch) | `aaab` | `aaab` |

`roast/S05-metasyntax/longest-alternative.t` — the acceptance suite the ticket nominated —
runs 62/62 and has been on `roast-whitelist.txt` since Slice 5. The BLOCKERS.md row the
ticket referenced was removed by Slice 4.

## What is not closed here

ADR-0022 §6 deliberately left two gaps open, and neither belongs to this ticket:

- **Side effects of losing branches** — candidate collection still matches every `|`
  branch fully, so a losing branch's plain `{ }` blocks run. Its *proto-dispatch* twin
  turns out to be considerably worse and is now designed for in
  [ADR-0046](../../docs/adr/0046-proto-token-ltm-shares-one-ranking-mechanism.md).
- **A real token NFA** for ranking many-branch alternations in O(1)-ish time — a
  performance follow-up, gated on the bench CI showing the measurement pass hot.

Investigating this closeout also turned up the fact that ADR-0022's carve-out ("protoregex
dispatch already ranks by declarative prefix and is NOT changed here; interpolated-`@array`
alternation already approximates LTM and is not changed here") was wrong on both counts —
see `news/2026-08/proto-token-ltm-one-ranking-mechanism.md` and ADR-0046.
