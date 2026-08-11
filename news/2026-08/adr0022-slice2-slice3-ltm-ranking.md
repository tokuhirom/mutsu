# ADR-0022 Slices 2 and 3: `|` alternation ranks by declarative-prefix LTM

Implemented the remaining core of ADR-0022 (`docs/adr/0022-regex-alternation-ltm-ranking.md`):
Slice 2 (`ltm_litlen_at`, the leading-literal-length tiebreak measurement, PR #6259) and
Slice 3 (the ranking swap in all three alternation-consuming matcher arms, plus plural-ends
collection, PR #6261). `|` branches now rank by `(declarative-prefix length desc, litlen
desc, declaration order asc)` instead of by longest actual full-branch match, matching
Rakudo's NFA-based LTM semantics.

Direct results: `roast/S05-metasyntax/longest-alternative.t` went from 57/62 (stale
BLOCKERS.md figure) to 61/62 — only test 50 (non-constant `$var` interpolation must not
count toward LTM, ADR §2 row, deferred as Slice 5) and the known `#?rakudo todo`
negative-lookahead quirk (line 461) remain. `Cro::HTTP`'s `t/http-router.rakutest` reached
**83/83**, closing out its last remaining failure (the LTM tie-break bug that motivated
this ADR in the first place).

## CI regression found and fixed before merge

Slice 3's first CI run failed `roast/S05-grammar/signatures.t` (previously fully green).
Root cause: a `|`-alternation branch consisting only of a regex comment (e.g. a token body
opening with `token fred($arg) {    #a comment\n | ... }`, a common visual-alignment idiom
where the first `|` is placed on its own line) was parsed into a genuine, always
zero-width-matching empty `RegexPattern` branch. The parser's existing "leading empty
branch is allowed for alignment" elision (`/ | a /`) only recognized a branch as empty via
`.trim().is_empty()`, which does not account for regex comments — a comment-only branch is
therefore *textually* non-empty even though it contributes no atoms once parsed.

This phantom branch was harmless under the old longest-actual-match ranking (a zero-width
match always lost to any real match), but ADR-0022's declarative-prefix ranking can tie it
at `(prefix_len=0, litlen=0)` with a real branch that also terminates its own measurement
early — a leading plain `{ code }` block, per ADR-0009 discipline — and then the
declaration-order tie-break wrongly preferred the earlier-declared phantom branch, making
the whole subrule match spuriously zero-width.

Fixed with a new `regex_branch_is_blank` helper (`src/runtime/regex_parse.rs`) that strips
`#...`/`` #`[...] `` regex comments before checking for blankness, replacing the four
`.trim().is_empty()` call sites that implement the same "elide a leading empty branch"
idiom for `|` alternation, `&` conjunction, and the `(...)`/`[...]` group forms. Pinned by
`t/regex-alternation-leading-comment-branch.t`.

A second, unrelated bug was found (not fixed) while writing that regression test: a literal
`|` character inside a regex comment is misread as a real top-level alternation separator
by `split_top_level_alternation`, which is not comment-aware. Filed as
`todo/tickets/regex-comment-containing-pipe-char-confuses-top-level-alternation-split.md`
— confirmed to predate ADR-0022 (reproduces identically on `main`).

## Remaining work

Slice 4 (BLOCKERS.md ledger refresh) and Slice 5 (marking non-constant `$var`
interpolation as LTM-non-declarative, needed for roast test 50 and full whitelisting of
`longest-alternative.t`) are tracked in the ADR as follow-ups.
