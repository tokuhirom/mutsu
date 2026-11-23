# `CALLER::UNIT::` sees a later write of an `EVAL`'d unit

A closure created in an `EVAL`'d unit's mainline records that unit's
lexicals for `CALLER::UNIT::` (ADR-12529 amendment A1). They are now
recorded as the variables themselves (shared cells), not as snapshots of
their values, so a write the unit makes after creating the closure is seen
(#19009):

```raku
my $f = EVAL 'my $u is dynamic = 1; my $g = -> $fn { $fn() }; $u = 2; $g';
say $f({ $CALLER::UNIT::u });   # 2, as on rakudo (was 1)
```

A name that a nested block of the unit redeclares resolves to the unit's
outermost variable, which is the one `UNIT::` names.

The unit's arrays, hashes and code objects are recorded as themselves, so
the unit's own in-place writes keep working. A unit run by
`Perl6::Compiler.eval` without an outer context no longer sees a caller
lexical just because a closure captured it: it dies as undeclared, as on
rakudo.
