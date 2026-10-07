# A proto candidate reference is not a call to an undeclared routine

`<value:sym<number>>` in a rule body was scanned by the CHECK-time undeclared-routine pass, which
read the `sym` of the `sym<number>` tail as a call to a routine named `sym`. The scan is skipped
for units that `use` anything, so the failure only showed for a grammar module without imports,
such as `ASN::Grammar`; `ASN::META`'s `t/01-recursive-type.t` died at compile time. The scan now
leaves a `sym<...>` subrule tail alone, and the test passes under mutsu as under rakudo.
