# Regex scalar interpolation escapes every metachar (Text::CSV 55_combi 17009/17009)

An interpolated scalar in a regex matches its value literally — raku never
re-parses the value as regex source. mutsu's `escape_regex_scalar_literal`
(src/runtime/regex_parse_modifier.rs) enforced this with an ENUMERATED
metachar list, which leaked whichever char it forgot: `~` (the goal-match
marker) survived to the structural parser as a bare `TildeMarker` atom and
hit the matcher's `unreachable!()` — a hard Rust panic. Text::CSV's
`t/55_combi.t` aborted at test 77 exactly there: its `@special` char pool
includes `"~"`, and the module's

```raku
$t.subst-mutate (/( $q | $e )/, { "$e$0" }, :g);
```

interpolates the quote/escape chars into an alternation. Minimal repro:
`my $e = "~"; "a~b" ~~ m/ $e /`.

The list had already grown `'`/`"` and `%`/`&` entries from earlier
Text::CSV rounds — the enumeration itself was the bug. It is now a blanket
rule: every char that is not alphanumeric/`_` gets a backslash (whitespace
keeps its `\x[..]` codepoint form; a backslash before any non-alphanumeric
char is always a literal in regex slang, while alphanumerics must stay bare
so escaping cannot CREATE class shorthands like `\d`).

`t/55_combi.t` now runs all **17009/17009** tests green (it exercises the
full sep/quote/escape combination matrix). Pin:
`t/regex-scalar-interp-metachar-literal.t`.

Residue filed as
`todo/tickets/quantified-scalar-regex-interpolation-broken.md`: a
QUANTIFIED interpolation (`$s?`) never matches — a pre-existing,
independent defect of the text-splicing design.
