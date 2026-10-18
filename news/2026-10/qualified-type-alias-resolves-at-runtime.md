# A package-qualified `constant` type alias is settled at run time

Since #13850, the compiler resolves a `constant` type alias in a signature
or a typed declaration (`constant T = int64; sub f(T $x)`) to its target
when it compiles the declaration, as rakudo does. When the target was a
package-qualified name, the compiler used the spelling as written. What
`P::S` names depends on `P`'s stash at run time: BDD::Behave's
`BDD::Behave::SpecTree` declares `our constant Suite =
BDD::Behave::SpecTree::Suite::Suite` alongside the nested module
`BDD::Behave::SpecTree::Suite`. So a test's `constant Suite =
BDD::Behave::SpecTree::Suite; sub run(Suite $suite)` checked arguments
against the module and rejected every suite.

An alias chain that ends at a package-qualified name is now left to the run
time, where the alias resolves to the constant's value. BDD::Behave's
`t/hooks/hook-failure.rakutest`, `t/hooks/around-not-continued.rakutest` and
`t/dry-run/filter-matching.rakutest` now pass.
