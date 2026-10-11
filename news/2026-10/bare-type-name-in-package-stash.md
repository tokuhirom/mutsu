# A bare type name resolves through the enclosing package's stash

Inside `unit class Foo`, a bare `Bar` now names the `Foo::Bar` that
`use Foo::Bar` loaded, as in Rakudo. The scope walk used to treat every
compound-declared name as invisible from its parent package, a guard that only
makes sense for core names (`Foo::List` must not shadow `List` inside `Foo`).
The guard now applies to core names alone. Found with the Selkie::UI
distribution, whose six remaining test files now pass.
