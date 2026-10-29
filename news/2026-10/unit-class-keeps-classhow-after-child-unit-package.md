# `unit class Foo;` keeps its ClassHOW when a child module says `unit package Foo;`

Async::Workers declares `unit class Async::Workers` and then `use`s
`Async::Workers::Msg`, whose file begins `unit package Async::Workers;`. Running
that child module's package statement recorded `Async::Workers` as a bare
package again, so `$*PACKAGE.HOW` was a `PackageHOW` while the class body
registered and AttrX::Mooish's `trait_mod:<is>` died with "it is not a Raku role
or class". The package statement now leaves a class that is already being
declared (`user_declared_classes`), not only one already registered, alone.
