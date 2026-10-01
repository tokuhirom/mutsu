# EVAL's compile-time checks judge every position

The checks an `EVAL`'d snippet goes through before it runs — undeclared
routines, names and variables, illegally post-declared types, `BEGIN` calls to
later routines, duplicate `our sub`s, parameter types, type arguments,
`trusts` targets and inheritance from a type capture — used to be 25
hand-rolled AST walkers, each with a `_ =>` arm that skipped whatever it did
not list. Most of them only looked at the snippet's top level, so
`EVAL 'class C { has $.foo; method m { foo() } }'` ran without complaint where
rakudo (and mutsu's own mainline check) reject the undeclared `foo`.

They are now typed AST visitors (ADR-0137), and each judges a name wherever
rakudo does: routine and method bodies, closures, conditions, operands and
parameters. Rakudo was consulted for every newly reached position.

- The `EVAL` undeclared-routine check is now the mainline CHECK-time analysis
  (`runtime/undeclared_routines.rs`) in an `EVAL` mode — one implementation
  instead of two. In that mode a `use` does not switch the check off (the
  parser already knows the imported names) and a capitalised callee is
  judged too, as before.
- The undeclared-variable check keeps a stack of lexical scopes, so a `my` is
  visible from its own initializer on, a loop header declares into the
  enclosing scope, and a block, routine or closure scope ends with its body.
  It still judges uses only in the positions it judged before: a free
  variable may be a lexical the *caller* declares later in its source (its
  pad already holds it in rakudo), which the runtime environment cannot
  tell yet, so judging more positions would invent errors. It no longer
  mistakes `say C` (a constant), `say x` (a sigilless variable)
  or `say A` (an enum value) for an undeclared `$C`/`$x`/`$A`, and no longer
  rejects a sub that reads `@_` or `%_`.
- The type checks keep the type captures in scope (`::T` of a routine or
  block signature, a role's type parameters), so
  `sub f(::T $a, Array[T] $b)` and `role R[::T] { my Array[T] $x }` are no
  longer rejected, while `role R[::T] { my class C is T {} }` now is.

The ratchet count for the four files fell from 25 to 1; the one left decodes a
`use lib` argument list and is a spine, not an analysis. Pinned by
`t/lang/eval-compile-checks-nested-positions.t`.
