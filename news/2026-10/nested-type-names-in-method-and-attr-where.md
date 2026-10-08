# Nested type names resolve in methods and attribute `where` predicates

`G::Mt` written inside a method of `Outer` (with `class G { class Mt }` nested in
`Outer`) is now resolved against the running method's class, as rakudo does.
An attribute `where` predicate that names a role or class declared next to its
class (`all($_) ~~ Term`) now resolves it in the declaring class even when the
object is built from a method of an unrelated class. Found by
`Fortran::Grammar`, whose `IO::Glob` dependency hit both; its single test file
now passes 42/42.
