# Grammar re-entrant backtracking no longer re-derives pure per-pattern analyses on every attempt

A regex of the shape `m:g/ (.+) <?{ CODE }> /`, where `CODE` calls back into a
grammar's `.parse`/`.subparse` (a common idiom for "try several sub-parsers"
helpers such as `Text::SubParsers`), re-enters the same grammar once per
backtrack attempt of the greedy `.+` — up to O(n²) times for an n-character
subject (#8510). Two pieces of per-attempt work in that path were pure
functions of static, already-registered data, yet were fully recomputed on
every single attempt instead of once:

- `Interpreter::establish_grammar_dynamic_vars` scanned every token/rule
  definition registered *anywhere in the whole program*, filtering by
  package-prefix string match, to build a package's `:my $*/%*/@*…`
  dynamic-variable declaration table — on every `.parse()`/subparse call, even
  though the answer depends only on the target package (and its MRO
  ancestors) and is invalidated only when a new token/rule is registered.
  It is now memoized per package, keyed by the existing `TOKEN_DEFS_GEN`
  generation counter that every token/rule registration already bumps.

- `atom_contains_backref` and `count_capture_groups` are both pure functions
  of a `RegexAtom`/`RegexPattern`'s shape, but were recomputed by a full
  recursive tree walk on every match attempt of the atom that owns them —
  including every backtrack retry. Both are now memoized in the
  `PatternDerived` slot each `RegexPattern` already carries for exactly this
  purpose (the unanchored-scan prefilter next to it follows the same
  "pure function of the pattern, computed once, shared through the parse
  cache" discipline).

A callgrind profile of the `Text::SubParsers` 0.1.4 `t/04-whatever-code-parsing.t`
reproduction (a 126-character subject, `WhateverCode` sub-parser trying
`DateTime::Grammar` and `JSON::Fast` behind `try`) showed no single dominant
hotspot — the remaining cost is spread broadly across allocation, hashing,
and the general regex-match machinery — but `atom_contains_backref` alone
accounted for ~2.6% of total instructions across 2M+ calls before this
change, and both fixes are now O(1) after the first call per package/pattern
instead of O(program size) / O(pattern size) on every attempt.

This distribution's own test suite already runs in single-digit seconds
against current `main` (down from the ~60s originally measured against an
older `main`, most of which is explained by the unrelated scan-prefilter
work landed the same day — see the `perf(regex): derive a scan first-set …`
series); these two fixes shave a further ~7% off the same measurement and
generalize to any grammar re-entered from a backtracking regex, not just
this one distribution.
