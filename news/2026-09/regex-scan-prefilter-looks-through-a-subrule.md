# The regex scan prefilter looks through a `<subrule>`

ADR-0099 Stage 1's fourth slice ([#8272](https://github.com/tokuhirom/mutsu/issues/8272)). The
unanchored-scan prefilter now derives a **first-character set through a rule name**, keyed by
invocant package and `TOKEN_DEFS_GEN`, instead of declining on every `<subrule>` it meets.

## What was wrong with declining

Constraint 3 of the ADR does not ban this; it attaches a condition to it — "a prefix derived through
`<subrule>` must be keyed by invocant package and `TOKEN_DEFS_GEN` (dynamic override via `H is G` is
legal), **or decline**". The first three slices ([#8285](https://github.com/tokuhirom/mutsu/issues/8285),
[#8446](https://github.com/tokuhirom/mutsu/issues/8446),
[#8457](https://github.com/tokuhirom/mutsu/issues/8457)) took the decline, and that turned out to be
a cliff rather than a shortfall: a rule name *anywhere in a pattern's leading run* makes the whole
derivation decline, so an alternation of two literals was narrowed to two candidate characters when
written inline and walked at every one of a subject's positions when written behind a rule name.
Same pattern, same engine, 100x apart.

Failing scans over a 135,000-character subject, release, against `MUTSU_REGEX_PREFILTER=off` (the
status quo for every one of these, since all four declined before):

| scan | before | after | warm rakudo |
|---|---:|---:|---:|
| `~~ / <G::kw> /`, `token kw { 'zzzq' \| 'kkkz' }` | 145.8 ms | 5.5 ms | 675.6 ms |
| `~~ / <G::outer> /`, `token outer { <inner> }`, `token inner { 'zzzq' }` | 61.1 ms | 0.7 ms | 892.0 ms |
| `~~ / <G::sigil> <[a..z]> /`, `token sigil { <[$@%&]> }` | 53.4 ms | 1.0 ms | 608.7 ms |
| `~~ m:g/ <G::kw> /` | 153.8 ms | 5.4 ms | — |

The inline form of the first row, `~~ / 'zzzq' | 'kkkz' /`, costs 4.7 ms on the same subject — which
is the point: hiding a pattern behind a rule name no longer changes its complexity class. Rakudo
uses an NFA for start filtering and evidently does not lower a rule call onto it either, so these
are rows where mutsu is not merely competitive but two orders of magnitude ahead.

## Three things worth carrying forward

**Constraint 1 was satisfied by resolving through the matcher's own table.** The resolution goes
through `resolve_parsed_token_candidates_in_pkg` — the same memoized `(pkg, name)` +
`TOKEN_DEFS_GEN` table the matcher resolves `<x>` through — and each candidate body is then walked in
the package that *defined* it, which is the rule `subrule_candidate_ends` matches under. So there is
no second reading of the rule registry to drift from the first, and a redefinition invalidates both
at once. Whoever extends this: re-deriving "which body does `<x>` mean here" independently is exactly
the change that reintroduces the drift risk, and the drift would be a silently dropped match rather
than an error.

**The memo split is a correctness boundary, not a caching detail.** A derivation that looked through
a rule name is not a pure function of the pattern, and patterns are shared by source text through the
regex parse cache — so the two `/ <x> /` in two different packages are *one* cached pattern. Storing
one package's first-set in the pattern-keyed slot would therefore have answered for both and dropped
every match in the second. Patterns that name no rule keep the single `OnceLock` slot and the path
they had before; the rest go to a package-and-generation-keyed memo. The test that picks between
them is deliberately coarse and deliberately exhaustive over `RegexAtom` — over-reporting costs a
mutex acquire, under-reporting costs correctness, so a newly added atom has to fail to compile there.

**Recursion is answered "unknown", and only where it has to be.** A first-set must be a *superset* of
what can match, and a rule reached from itself before anything is consumed can only be walked to a
subset — the one unsound direction — so it declines. But that bites far less often than it sounds:
`token thing { 'a' <thing> | 'b' }` has already fixed its first character by the time the recursive
call is seen, so the decline on the call costs nothing and the set comes out exactly `{a, b}`. Only
genuine *left* recursion loses the derivation.

## What still declines, and why each is unsound rather than imprecise

A call with arguments and `<::(EXPR)>` resolve per call against values the key does not carry. A
reference naming a lexical `Regex` (`<&re>`, `<&$re>`) resolves against the caller's scope, which the
key does not carry either. A body whose text interpolates a runtime value is re-parsed per call, so
no generation-keyed memo can hold it — tested with the same predicate the call-graph analysis uses,
which correctly ignores a sigil appearing only inside a `{ ... }` block, so the common
`token part { \w+ { $n++ } }` shape stays analyzable. A name no rule answers to is a builtin
assertion or a grammar method; a name a grammar method *also* answers to is not this analysis's
dispatch decision to re-derive. And a rule body that opens with a code block declines as it always
did: ADR-0009 makes that block run once per start position in both mutsu and rakudo, so no position
may be skipped ahead of it.

The required literal prefix and the required inner literal still decline on rule names, deliberately.
Both make a claim about *text* rather than about one position's viability, and the inner literal's
decline on anything that can run code is stronger than the first-set's for the reason #8457 recorded:
skipping a position because a literal is missing *later* skips a code block that would have run.

## Testing

`t/regex/regex-scan-prefilter-subrule.t` pins the observable behaviour — 26 assertions, 25 of which
rakudo agrees with verbatim (the exception is the left-recursive rule, which rakudo does not
terminate on at all). `tests/regex_prefilter_differential.rs` gained eleven cases comparing each
shape against `MUTSU_REGEX_PREFILTER=off`, including the two-packages-one-pattern case that a
pattern-keyed memo would fail. `tests/regex_prefilter_engagement.rs` pins both directions on the
`MUTSU_VM_STATS` counters: that a subrule-led failing scan stays sub-linear in subject length, and
that the shapes above still decline — a `subrule_derivations=(resolved=N declined=N)` counter makes
"it stopped engaging" observable rather than merely slower.
