# `S:g/@(EXPR)/.../ ` doesn't interpolate an array as a regex alternation

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
2290).

## Repro

```
my %h = a => 1, b => 2;
my @a = %h.keys;
say S:g/@(%h.keys)/%h{$/}/ given 'abc';
say S:g/@a/%h{$/}/ given 'abc';
```

- raku: `12c` (both forms) — `@(...)`/`@array` interpolated directly into a regex pattern is
  treated as an alternation over the array's elements (matching `a` or `b`), and each match is
  replaced by looking it up in `%h`)
- mutsu: `%ha%hbc` (both forms) — the interpolation is being stringified as literal text
  (`%h{...}`-looking output) rather than either (a) building an alternation from the array, or
  (b) evaluating the replacement's `%h{$/}` hash lookup

Two possible independent bugs bundled in one symptom: array-as-regex-pattern interpolation, and
`%h{$/}` (hash-lookup-on-the-match-object) in the replacement.

## Root cause guess

`@(...)`/bare `@array` interpolated inside a regex pattern presumably isn't recognized as
"expand to an alternation of the array's stringified elements" at all — it's likely being
inserted as a literal stringification of the array (hence `%h`-looking noise appearing in the
output, which looks like the *replacement* side's `%h{$/}` wasn't evaluated either and got
stringified/escaped literally).

## Affected files (starting point)

- `src/runtime/regex_parse.rs` — array interpolation into a regex pattern (`@(...)`, bare
  `@array`)
- `src/vm/vm_string_regex_ops.rs` — `S:g/.../` replacement-string evaluation, specifically a
  hash-subscript expression (`%h{$/}`) as the replacement

## Suggested next step

Split into two smaller repros to isolate which side is actually broken: (a) `'abc' ~~ S:g/@a//`
(empty replacement) to check if the array-as-alternation match itself works at all; (b) a
simpler replacement like `S:g/a/%h<a>/` to check if a hash-lookup replacement expression
evaluates correctly outside of array-pattern interpolation.
