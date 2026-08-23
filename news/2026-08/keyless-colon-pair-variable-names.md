# Key-less colon pairs in extended identifiers (`$take-me:<home>`)

Raku lets a variable's name carry adverbial pair components, and the pair's key
may be omitted so that the quoted value alone spells it
(`raku-doc/doc/Language/syntax.rakudoc:354`):

```raku
my $take-me:<home> = 'Where the glory has no end';
say $take-me:['home'];      # Where the glory has no end
```

mutsu only accepted the *keyed* spelling (`$foo:bar<baz>`). The key-less form
was a hard parse error -- "Confused. expected statement" -- which made it the
worst-shaped failure of the three gaps recorded by the doc-diff harness against
that section.

## Root cause

Two separate colon-suffix loops implement this: `parse_var_name_adverb_suffixes`
in `src/parser/primary/var/adverb.rs` (the expression side) and an inline loop
in `var_name` in `src/parser/stmt/idents.rs` (the declaration side). Both
required an identifier immediately after the `:` before they would look for a
bracketed value. `:<home>` has no identifier, so the loop broke out immediately
and left `:<home>` unconsumed, and the statement parser then failed on it.

## Fix

Both loops now first try a key-less colon pair via a new shared
`parse_anon_adverb_value`, which reuses the existing bracket canonicalization so
`:<home>`, `:«home»` and `:['home']` all normalize to the same `:<home>` name
component. That is what makes a declaration in one spelling readable through any
other, matching the rule the keyed form already followed.

`(...)` is deliberately excluded from the key-less form. `:(...)` is a signature
literal, and raku rejects `my $t:("home")` outright with "You can't adverb $t" --
so accepting it would have been a divergence, not a convenience.

The same change also fixed `<<...>>` as an adverb value spelling (`$t:foo<<a>>`).
The angle-bracket branch matched the first `>`, so `<<a>>` yielded the name
component `<<a>` instead of `<a>`; it now recognizes the doubled brackets, for
the keyed and key-less forms alike.

## Verification

Measured against real raku first: key-less pairs stack (`$t:<a>:<b>`), mix with
keyed pairs (`$t:foo<a>:<b>`), work for the `@` and `%` sigils, and interpolate
inside strings (`"$t:<home>"`). `t/adverbial-pair-variable-name.t` (23
assertions) covers all of that plus the previously-working keyed forms as
regression pins, and passes identically under `raku` and mutsu.
`roast/S02-names-vars/varnames.t` (29 tests, the only roast file exercising
adverbial variable names) still passes, as does the full `t/` suite (3367 files,
31711 tests).

## Not fixed here

The other half of the original ticket -- compile-time interpolation of the
adverb *value* (`$a:foo«$c»` for a `constant $c`, and `$foo:bar(1+1)`) -- turned
out to be a much deeper problem than the ticket assumed, and is now tracked
separately in `todo/deep/begin-time-adverb-value-interpolation.md`. In short:
those spellings need BEGIN-time evaluation against the compile-time constant
environment, and there is no normalization choke point for variable names to
hang it on (the name is a bare `String` on ~8 AST variants, read at ~104
`local_map` lookup sites across 20 compiler files and ~133 more sites in
`runtime`/`vm`/`rakuast`, plus two AST-level collectors that never reach the
compiler). The parser cannot do it either, because its results are memoized. The
real fix is a whole-AST name-rewriting pre-pass plus a relaxation of ADR-0006
§2.2's constant-inlining guard -- ADR-level work, for a feature with no roast
coverage.
