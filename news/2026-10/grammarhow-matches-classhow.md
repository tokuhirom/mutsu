# A grammar's HOW smartmatches `Metamodel::ClassHOW`

In Rakudo, `Metamodel::GrammarHOW` is a subclass of `Metamodel::ClassHOW`,
the only such relationship among the built-in metaclasses. A grammar's
metaobject therefore takes a `when Metamodel::ClassHOW` arm.
AttrX::Mooish's attribute trait does exactly that with `$*PACKAGE.HOW`, so
an attribute of a grammar can be made lazy.

mutsu reported the grammar's HOW as `Perl6::Metamodel::GrammarHOW` but
matched it only against `GrammarHOW`. So Config::BINDish's grammar died
while loading with `Unsupported typeobject Config::BINDish::Grammar: it is not
a Raku role or class`.

The static type check now treats `GrammarHOW` as a `ClassHOW`, under both the
`Metamodel::` and `Perl6::Metamodel::` spellings, and in that direction only.
