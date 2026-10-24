# A routine's `$_` no longer inherits its caller's typed `$_` constraint

`sub outer(Str $_) { inner() }` followed by `sub inner { $_ = 5 }` died in `inner` with
"Type check failed in assignment to $_; expected Str but got Int". A routine's topic is its
own variable, but the constraint a `Foo $_` parameter registered lives in an env entry the
callee's env inherited, so the callee's assignment was checked against it. Routine entry
(subs and methods) now drops that inherited entry; a `$_` parameter of the callee itself
binds afterwards and registers its own, and the caller keeps its constraint on return.

Red's `CommonSQL.translate` has `multi method translate(Red::AST::Infix $_, ...)` calling
`translate(Red::AST::Select $ast, ...)`, which does `$_ = $entry.key` in a `.map` block
(Red `t/19-in-sql`, `t/100-concurrent-query-projections`).

Refs #14094.
