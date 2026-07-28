# YAMLish's upstream test suite passes in full

The YAML battery candidate, [`YAMLish`](https://github.com/Leont/yamlish), now
passes **81/81** of its upstream tests under mutsu — every subtest of all five
files (`anchor-alias`, `basic`, `p5-tests`, `roundtrip`, `test-harness`), the
same score `raku` gets. At the start of the session it was 28/81, with block
scalars, anchors, flow collections and the whole round-trip file failing.

None of the six fixes below is YAMLish-specific; each is a general regex or
grammar bug that the module happened to be the first thing to exercise.

## A lookaround's body is part of the same regex

`interpolate_bound_regex_scalars` and `bake_bound_params_into_regex_code_blocks`
rewrite a token's pattern text to substitute the values bound to its parameters,
because the pattern is matched later, by an interpreter whose env no longer has
them. Both skipped everything between `<` and `>` — correct for `<[…]>` character
classes, `<{code}>` interpolations and subrule calls, but wrong for a lookaround,
whose body is a sub-pattern of the same regex. So `<?before $indent …>` matched a
literal `$indent`, and YAMLish's `block` (which measures its indent exactly that
way) never matched a `---`-prefixed document. Both functions now descend into a
lookaround body and leave everything else opaque.

The keyword may also be separated from the body by a **newline**, not just a
space — YAMLish's `block-string` writes its lookahead across four source lines.
The parser required a literal `"before "`, so those spellings were not recognised
as lookarounds at all. `lookaround_keyword` is now the single place that decides,
and both the parser and the text rewriters use it.

## `:my` lexicals reach the sub-patterns of their own regex

A group, an alternative or a quantified group is parsed by a *nested*
`parse_regex_uncached` call, which knew nothing about the `:my` declarations of
the pattern containing it. A bare `$new-indent` inside `[ $indent $new-indent … ]+
% <.line-break>` was therefore pre-substituted from the outer env (usually to
`Nil`) instead of being lowered to a match-time `VarInterp` atom. A new
`ENCLOSING_REGEX_VARS` thread-local publishes those names for the duration of the
enclosing parse, and a guard restores it so a sub-pattern's own declarations do
not leak back out. The separated-quantifier matcher was also passing an *empty*
capture store as the baseline for its atom matches, which withheld the lexicals
from them; it now threads the live store through.

## A mid-pattern `$` is end-of-string

Raku's `$` is always end-of-string; only `$$` is end-of-line. A trailing `$` set
the pattern's `anchor_end`, but a `$` followed by anything else (`^ .* $ { make …
}`) was compiled to the end-of-*line* atom, which deliberately does not match at
the end of a string that ends in a newline. YAMLish's `Schema::Core`
`token plain` is exactly that shape, so every multi-line scalar failed to resolve
and died with `Invalid value …`. A new `RegexAtom::EndOfString` covers the
mid-pattern `$`; `$$` keeps `EndOfLine`.

## A goalpost takes the greedy inner match

`A ~ B C` matches `A`, then `C`, then `B`. The candidate list the goalpost
produced was in highest-priority-first order while the atom contract is
lowest-priority-first, so the engine tried the *shortest* inner match first and
stopped at the first position where the closer could match. `'ab''cd'` under
`"'" ~ "'" [ <single-bare> | "''" ]*` matched only `'ab'`. The list is now
reversed, exactly as the sibling `Group` arm already did.

## A qualified subrule is relative to its package

`<Schema::Core::element>` written inside `module YAMLish` names
`YAMLish::Schema::Core::element`. Both subrule resolvers looked the qualified name
up verbatim and gave up, so `to-yaml`'s `where /^ <!Schema::Core::element> …/`
constraint failed — and left its `X::Method::NotFound` in the pending-error slot,
where the *next* `load-yaml` picked it up. Both resolvers now retry the name under
each enclosing package.

## Zero iterations still mark their captures as quantified

A separated quantifier that matched nothing returned bare default captures, so
`$/<pair>` was a single empty `Match` rather than an empty list and
`load-yaml("{}")` produced `{"" => Any}` instead of `{}`. Relatedly, a
non-suppressing alias (`<tags=tag-directive>`) captures under *both* names, but
only the alias was marked quantified — so `@<tag-directive>` was a bare Match and
YAMLish's `%TAG` directive handling died with "Odd number of elements found where
hash initializer expected".

## Pins

`t/regex-lookaround-bound-param.t`, `t/regex-my-var-in-subpattern.t`,
`t/regex-end-of-string-anchor.t`, `t/regex-goal-match-greedy.t`,
`t/regex-qualified-subrule-relative-package.t`,
`t/regex-separated-quantifier-zero-matches.t`,
`t/regex-alias-subrule-quantified.t` — 50 assertions, all green under `raku` too.
