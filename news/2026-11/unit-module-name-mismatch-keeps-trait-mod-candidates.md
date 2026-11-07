# A `unit module` loaded under another name keeps its `trait_mod` candidates

`use Test; use Intl::CLDR::Core` died with "Can't use unknown trait 'is' ->
'aliased-by'": `Core.rakumod` declares `unit module Core` but is loaded as
`Intl::CLDR::Core`, so its `multi sub trait_mod:<is>` candidates register in
`GLOBAL` and are removed again when the load finishes. The re-import then took
the `GLOBAL` family (which by now held only `Test`'s candidate) and never
consulted the module's own `EXPORT::ALL` alias family. The unit-module import
path now adds the candidates that family carries and the global one lost.

Intl::CLDR's three test files load and pass.
