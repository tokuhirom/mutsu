# `CALLER::UNIT::` sees a later write of an `EVAL`'d unit

A closure created in an `EVAL`'d unit's mainline records that unit's
lexicals for `CALLER::UNIT::` (ADR-12529 amendment A1). Until now it took a
snapshot of each value when the closure was created. Now a write the unit
makes after creating the closure is seen too (#19009):

```raku
my $f = EVAL 'my $u is dynamic = 1; my $g = -> $fn { $fn() }; $u = 2; $g';
say $f({ $CALLER::UNIT::u });   # 2, as on rakudo (was 1)
```

Each scalar gets a cell of its own, which the unit fills with the
variable's final value when its mainline ends. The unit's own variables are
left as they are. Arrays, hashes and code objects are recorded as
themselves, so in-place writes are shared. A name that a nested block of the
unit redeclares resolves to the unit's outermost variable, which is the one
`UNIT::` names.

A unit run by `Perl6::Compiler.eval` without an outer context also no longer
sees a caller lexical just because a closure captured it: it dies as
undeclared, as on rakudo.
