# `PROCESS::<$x> := $var` makes `$*x` the variable's container

`PROCESS::<$x> := SOURCE` binds the process-level dynamic `$*x` to the source's container: a later
write to either side is seen through the other. mutsu stored the source's current value instead,
so the dynamic was a frozen snapshot. IO::CatHandle::AutoLines, used by the LN distribution,
publishes its line counter as `PROCESS::<$LN> := $!ln` in a `TWEAK`, so `$*LN` stayed `0`.

A `PROCESS::` bind now compiles its source the way an element bind does (`%h<k> := $!attr`), so
an attribute arrives as the instance's own slot cell. The stash keeps that cell; a plain variable
source is promoted to a shared cell first. A later `$*x = v` assigns through the cell instead of
replacing the binding. Binding a value (`PROCESS::<$x> := 42`) still yields a read-only dynamic.

A second fix comes from the same module: an `IO::CatHandle` whose `&!on-switch` holds a Proxy (the
module binds one with `nqp::bindattr`) now FETCHes it before calling it, so the callable runs on
every handle switch.

LN's `t/01-ARGFILES.t` now matches raku. `t/02-imports.t` still differs because `$*ARGFILES` is
not yet an `IO::CatHandle` with a per-file switch hook (#17754).
