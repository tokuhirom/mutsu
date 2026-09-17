# A role body's own `use` statement imported into the wrong package

Composing a parameterized role that itself imports a routine (most visibly a
custom `infix:<...>` operator) via a `use` statement in its own body could
leave that routine unreachable from the role's own methods.

Root cause: `run_role_body_for_composition` and
`run_composed_role_deferred_body` (the two functions that run a role's
non-declaration body statements at composition time — attribute/method
declarations aside) executed every "plain" statement with `current_package()`
left at whatever the *composing* class happened to be, rather than the
role's own declaring package. That is deliberate for most statements (a bare
`&helper()` call from a role method needs to resolve in the surrounding
lexical scope), but wrong specifically for a `use`/`need` statement: import
registration (`import_module`) always targets "the current package", so a
`use` positioned among a role's methods landed its imports under the
composing class's package instead of the role's own — and a bare call to
the imported routine from one of that role's *own* methods could then never
find it, since `bare_name_packages`'s enclosing-package search walks from
the role's own package, not the composer's.

`Algorithm::Kruskal` (an ecosystem distribution) hit this directly:
`Algorithm::MinMaxHeap`'s private `!bubble-up` method calls
`Algorithm::MinMaxHeap::CmpOperator`'s custom `minmaxheap-cmp` infix, which
that role imports via its own body's `use Algorithm::MinMaxHeap::CmpOperator;`
— composing the role from `Algorithm::Kruskal`'s `BUILD` registered the
operator under `Algorithm::Kruskal` instead of `Algorithm::MinMaxHeap`,
so the very first comparison inside `!bubble-up` died with
`Two terms in a row` (the generic "no such routine" fallback message for an
unresolved infix call).

Fixed by special-casing a `Stmt::Use`/`Stmt::Need` body op to run with the
role's own package current, exactly as a `TypeDecl` op already does.
Pinned by `t/modules/role-body-use-imports-into-role-package.t`.

This is a partial fix: two further, related trigger shapes (a role whose
name has no `::` in it at all, and a role's first-ever composition happening
inside a bare `{ }` block) still reproduce the same symptom and are tracked
in [#8646](https://github.com/tokuhirom/mutsu/issues/8646) — `Algorithm::Kruskal`'s
own `t/01-basic.t` still dies partway through (it calls its `do-it` sub from
inside a bare block four times), so the distribution's ecosystem record
stays `red` for now.
