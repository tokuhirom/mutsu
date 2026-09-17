# The scan prefilter gains a small NFA over the declarative prefix — ADR-0099 Stage 1 is complete

ADR-0099 Stage 1's seventh and final slice, closing [#8272](https://github.com/tokuhirom/mutsu/issues/8272).

Every mechanism the six earlier slices built answers one question: "may a match begin with THIS
character?" That rejects a start position using only its very first character, so a pattern like
`/ \d\d\d /` or `/ <:Lu> ** 4 /` still entered the full backtracking engine — ~983 instructions
(ADR-0099 §2.4) — at every position whose FIRST character alone looked plausible: every isolated
digit, every isolated uppercase letter. The second and third character of a three-character
requirement are just as necessary a condition as the first, and nothing before this slice could
check them without walking the whole pattern per candidate position, which is exactly the cost
Stage 1 exists to avoid.

## What landed

A bounded sequence of per-offset character-acceptance sets — `src/runtime/regex/regex_prefilter_chain.rs`
and `regex_prefilter_chain_atom.rs` — derived once per pattern (memoized alongside the existing
`Prefilter`) and checked as an ADDITIONAL layer on top of whichever of the six single-position
mechanisms applies. It extends as far as the pattern's declarative leading run makes a fixed,
exactly-one-character-per-atom promise: a `Literal`, a `CharClass` with no `Grapheme` item and no
`\n` member, a `UnicodeProp`, and a `CompositeClass` whose grammar-token fallback cannot engage all
qualify; concatenation, alternation and a bounded repeat (`** N`, `** N..M`) all compose the way
Thompson NFA fragments do. Anything that cannot make that promise — `.`, a grapheme literal,
`<.ws>`, a nullable quantifier, `:i`/`:m`, or a construct that can run user code — simply stops the
chain there rather than declining the whole pattern, exactly the asymmetry the earlier slices
already established for their own single-position sets.

## Why "layered on top", not "replacing"

The required literal prefix's substring search and the required inner literal's occurrence-window
tracking are both strictly better than a per-character chain walk for the shapes they cover, so
neither is re-derived through the chain — a pattern with a usable literal prefix skips the chain
check entirely. The chain only ever adds rejections a position could not already survive, and a
pattern with no usable chain (fewer than two pinned offsets) pays nothing extra at all: `build_chain`
does not even surface one in that case.

## Deliberately not a correlated NFA simulation

Rakudo's own mechanism for this is an NFA with true subset simulation, correlating which branch's
edge a specific subject character actually satisfied at each offset. This implementation takes a
strictly weaker, sound approximation instead: each offset's accepted set is the union of every live
branch's requirement there, checked independently of what the previous offset's character was. An
alternation of unequal-total-length branches, or two branches that individually pin the same
offsets but with different characters, can therefore admit a candidate position neither branch
alone would accept — which only ever costs a wasted (but still correctly-rejected) engine entry,
never a dropped match. This trade bought a compositionally simple, easy-to-verify construction over
a more precise one that would need genuine per-character correlated state tracking.

## Numbers

Isolated the chain's own marginal contribution by A/B-ing a release build with just the chain's
narrowing wrapper compiled out (every other mechanism — literal prefix, first-character set,
required inner literal, subrule/composite-class derivation — unchanged), on a 3,360,000-character
failing scan built from a repeated unit containing one isolated digit, one isolated uppercase
letter, and one two-character composite-class-eligible digit run per repeat:

| scan | first-character-set alone (pre-chain behavior) | with the chain | speedup |
|---|---:|---:|---:|
| `~~ / \d\d\d /` (three consecutive digits) | 35.7–40.1 ms | **20.7–24.7 ms** | ~1.6–1.8x |
| `~~ / <:Lu> <:Lu> <:Lu> <:Lu> /` (four consecutive uppercase) | 50.7–79.9 ms | **21.9–26.4 ms** | ~2.3–3.6x |
| `~~ / <+digit> <+digit> <+digit> /` (three composite-class digits) | 78.8–106.9 ms | **20.5–25.3 ms** | ~3.8–4.3x |
| `~~ / <G::kw> 'z' /`, `token kw { 'aa' \| 'bb' }` (subrule then literal) | 72.3–154.3 ms | **19.6–21.4 ms** | ~3.7–7.9x |

**No regression on the six existing mechanisms**, same subject and A/B: `~~ / 'zzzq-not-here' /`
(literal prefix) 32.6–32.8 ms before vs 30.8–34.1 ms after; `~~ / :i 'ZZZQ' /` 24.4–24.8 ms vs
23.7–24.9 ms; `~~ / 'zzzq' | 'kkkz' /` (alternation) 18.6–20.9 ms vs 20.5–21.5 ms — all differences
inside this box's run-to-run noise, exactly as expected since none of these patterns has a chain
long enough to change which code path they take (a literal prefix never even reaches the chain
check per `Prefilter::chain`'s doc comment, and the fold-closure alternation here is already fully
decided by its own first-character set).

## Constraint 1, again by sharing the leaf predicates

Every character set the chain composes comes from the exact same functions the single-position
mechanisms already call — `literal_first_set`, `class_first_set`, `unicode_prop_first_set`, and
`composite_class_chain_set` (factored out of the existing composite-class derivation so both the
old single-position analysis and the new chain call the same function rather than two independent
statements of the same fact). Subrule resolution reuses the existing package-and-generation-keyed
resolver too. This module adds no new leaf predicate; it only composes existing ones across more
than one offset, which is what keeps it from being a second, driftable definition of what the
engine matches (ADR-0099 §4 constraint 1).

## The one genuinely subtle case: a `<subrule>` with a bounded, separated repeat

A plain literal or class atom's bounded repeat-with-separator (`'ab' ** 2..4 % ','`) is fully
unrolled by the PARSER into flat, separately-visible tokens — the separator becomes its own literal
token in between — so the chain's ordinary sequence concatenation already handles it correctly with
no special case at all. A `<subrule>`'s length is not known until match time, though, so
`<G::kw> ** 2..3 % ','` stays ONE token whose quantifier carries the separator directly
(`token.separator.is_some()`). Naively concatenating two mandatory copies of the subrule's own chain
back to back would silently assume no comma sits between them, and reject a real "ab,ab" at the
position right after the first copy — the one place in this slice where getting the composition
wrong would have been unsound rather than merely imprecise. The fix is simple once spotted: a
separator-carrying repeated token never unrolls past its first mandatory occurrence.

## Testing

A new differential harness section (18 cases) exercises the chain specifically — three consecutive
digits, a fixed-count Unicode-property repeat, two composite classes in a row, an alternation of
equal- and unequal-length branches, `:i`/`:m` non-extension, a subrule-then-literal chain, the
separator-carrying subrule repeat, a class that can consume more than one codepoint, and the
`MAX_CHAIN_LEN` bound — all comparing the prefilter on against `MUTSU_REGEX_PREFILTER=off`, which
also re-ran the full existing 110-case corpus with zero regressions. `tests/regex_prefilter_engagement.rs`
gains two counters, `chain=(engaged=… rejections=…)`, pinning that `\d\d\d` engages the chain and
that every position surviving the first-character set alone still gets rejected by it (no run of
three digits exists in the fixture), and that a single-step pattern never pays for the extra check.
`t/regex/regex-scan-prefilter-chain.t` (21 assertions) covers the behavior a Raku program can
observe, verified against real rakudo first.

## Stage 1 is complete

All seven pieces have landed: required literal prefix, first-character set, required inner literal,
subrule-derived first-sets, `<:prop>`/scoped-`:ignoremark` first-sets, the `<+a -b>` composite
class, and now the NFA generalization. ADR-0099 §8 records the full slice history. Stage 2 (a
fast-lane compiled matcher) remains a question, not work, until Stage 0 lands and grammars are
re-profiled without its ceremony.
