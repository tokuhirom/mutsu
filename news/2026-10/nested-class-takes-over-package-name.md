# A nested `class Foo::Bar` inside `unit class Foo::Bar` takes the name over

Rakudo installs a compound-named class declared inside a package into the
already existing `Foo` stash, so after `unit class Foo::Bar; class Foo::Bar { }`
the name `Foo::Bar` denotes the nested class (named `Foo::Bar::Foo::Bar`).
mutsu kept resolving the name to the outer, empty class. The registry now
records such overrides and the bareword lookup follows them. Found through the
Marrow distribution, whose `t/02-db.t` now passes.
