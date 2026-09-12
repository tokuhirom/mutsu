# Sub-pattern parses are memoized, keyed on interpolated text

`Interpreter::parse_regex` memoizes a compiled `RegexPattern` per
`(package, pattern)` under the grammar-token registry generation. The parser is
recursive, though, and its *inner* entry point was not covered: a group, a
lookaround body, an alternation branch, a conjunction part and a `%`-separator
atom each re-enter the parser through `parse_regex_with_mode`, which was a bare
passthrough to `parse_regex_uncached` with no memo of its own. So the outer
pattern was cached while everything inside it was re-parsed on every parse of
the outer pattern.

A `rust-gdb` hit-count sweep over one 60-row variant of
`benchmarks/bench-yaml-parse.raku` measured the result: **10,958 parses of only
255 distinct patterns**, `<.space>` alone 1,005 times, for **597 M of the run's
5.18 Bn instructions (11.5%)** — the site round 12 of
[#7576](https://github.com/tokuhirom/mutsu/issues/7576) handed off.

## Keying on the source text is worth a third of the available win

The obvious memo — the same key `parse_regex` uses, guarded by
`regex_pattern_is_static` — was implemented first and measured **-2.7%**. Then
the same gdb sweep on that build showed why: 4,994 of the parses still missed,
and the missed patterns were dominated by YAMLish rules like

```
[ <properties> <.space>+ ]?  $<value> = [ … ]
[ $<content>= [ <.after .> ] ]
$<kind>=<[\|\>]> $<chomp>=<[+-]>?
```

`regex_pattern_is_static` counts `$<name>` as interpolation. It is
conservative by design — a false "dynamic" costs a cache miss, never
correctness — but `$<name>` *names a capture*, it does not read a variable, and
these are exactly the workhorse rules of a grammar.

## The purity boundary is after interpolation, not before

`Match` mode substitutes `$`/`@`/`%` variable values into the pattern text
first, and the structural parse that follows is a function of the resulting
string plus the token registry. So the memo key is the **interpolated** text,
and the static-pattern predicate drops out of the picture entirely:
`parse_regex_uncached` now interpolates, probes, and on a miss calls the
extracted `parse_regex_structural`, which is the old body minus its
interpolation step.

That also caches patterns that genuinely do interpolate, whenever the result
repeats — a grammar that splices an indent string re-parses once per *distinct*
indent instead of once per call.

## The exclusions have to come from the parser, not from reading it

`Validate` mode is excluded outright: a structurally-questionable pattern pushes
non-fatal diagnostics onto `REGEX_SORROWS` on its way to a *successful* parse
and `validate_regex_structurally` drains them, so a cache hit would report none
the second time. It has nothing to gain either — validation runs once per regex
literal at compile time, 20 times over the whole profiled document.

The `Match`-mode exclusions were harder, and the first attempt got them wrong.
Interpolation is not quite the whole of the impurity: a handful of parse steps
read state the key does not carry. `<$var>` and `<@var>` *assertion* forms take
the variable's value at parse time and recompile it as a regex (unlike a bare
`$var`, which interpolation substitutes into the text first, so the key already
reflects it), and `<~~>` reads `PARSING_TOP_LEVEL_SOURCE`. The first version of
this memo derived its exclusion list by reading the parser, caught `<~~>`,
missed the sigil-assertion forms, and made `roast/S05-metasyntax/litvar.t` reuse
the tree parsed for `$var = '$i'` when matching `$var = '<$i>'` — two
consecutive matches of the identical source pattern `/<$var>/`, one of which
must die.

A list of pattern syntaxes maintained next to the parser cannot stay correct as
the parser grows, so the exclusions are raised **at the reads themselves**:
`Interpreter::note_regex_parse_ambient_read` sets a
`PARSE_CONSULTED_AMBIENT_STATE` flag, `parse_regex_uncached` runs each parse
with the flag cleared, refuses to store a parse that raised it, and ORs it into
the enclosing parse so a pattern containing an impure sub-pattern is not stored
either. Adding a new parse-time read of interpreter state means calling that
one function; forgetting to update a syntax list is no longer possible.

Unlike source text, interpolated text is not bounded by the program — a regex
that splices a loop counter mints a fresh key every iteration — so each
`(package, mode)` bucket is capped at `SUBPATTERN_PARSE_CACHE_MAX` entries and
cleared on reaching it. Dropping a memo costs re-parses and nothing else.

A hit clones the stored tree rather than handing out the `Arc`, because every
caller moves the `RegexPattern` into a `RegexAtom`. The clone is a memory copy
of one sub-tree; the parse it replaces re-scans the source, re-resolves grammar
tokens against the registry, and re-runs LTM expansion over it.

## Effect

60-row document, `valgrind --tool=callgrind` (deterministic and
load-independent, per the method note rounds 10-12 added to the ticket):

| | instructions | `parse_regex` inclusive | structural parses |
| --- | ---: | ---: | ---: |
| before | 5,176,849,912 | 597,362,519 (11.54%) | 10,958 |
| source-keyed | 5,038,201,950 | 460,379,018 (9.14%) | 4,994 |
| interpolated-keyed | 4,673,371,023 | 82,442,085 (1.76%) | 269 |
| | **-9.7%** | **-86%** | **-98%** |

269 structural parses over 247 distinct patterns is within rounding of optimal:
one parse per distinct pattern, plus the dozen `<$var>`/`<~~>` parses the
ambient-read flag correctly refuses to store.

`t/regex/regex-subpattern-parse-memo.t` pins the key's three axes and both
exclusion mechanisms: a variable whose value changes between calls (including
inside a nested group), a `$<name>` capture form repeated across matches, the
`<$var>` assertion form under two different values of the same pattern text,
`<@var>`, two grammars whose `TOP` bodies are character-for-character identical
but whose `<+digit +thing>` folds must resolve in their own package, a grammar
declared after an earlier parse, and `<~~>` recursion. All eighteen assertions
were verified against rakudo v2026.07.

One thing the test deliberately does *not* pin: a `<@var>` regex literal
evaluated before the array is reassigned later in the same scope already sees
the later value, on `main` with this change both applied and stashed. That is a
separate pre-existing bug, filed as
[#8040](https://github.com/tokuhirom/mutsu/issues/8040).
