# A role's own imported custom operator is now reachable from any call site

A parameterized role that imports a custom `infix:<...>` operator via its own
body's `use` statement, and then calls that operator from one of its own
methods, could get a runtime "Two terms in a row" — the operator sub simply
could not be found — even after an earlier fix made the import land in the
role's own package. Two independent bugs caused this, both fixed here
(#8646):

## Shape 1: a flat (non-namespaced) role name

Method dispatch only anchored `current_package` to the receiver's own class
when that class name was `::`-qualified (`owner_class.contains("::")`, among
a couple of other narrow conditions). A role-pun whose whole name was flat
(`FlatHolder[FlatComparable]`, no `::` anywhere) never matched any of those
conditions, so `current_package` stayed whatever the caller happened to have
— GLOBAL for a call from the mainline — and `bare_name_packages()`'s search
list never included the role's own package, where the imported operator was
actually registered. A private method calling that operator died on its very
first call.

Fixed by tracking which packages received a routine import from a role's own
deferred `use`/`need` body statement (`run_role_deferred_use_stmt`), and
consulting that as an extra reason to anchor `current_package` to the
receiver's class during method dispatch.

## Shape 2: the role's first-ever composition happens inside a bare block

A role's `use` statement runs exactly once, memoized by role composition —
semantically the role's compunit doing its own one-time import, no different
from an ordinary module body's `use`. But it runs lazily, wherever the role
happens to be composed for the first time, which is often deep inside an
ordinary bare `{ ... }` block several calls down the stack. That block
compiles to `OpCode::BlockScope`, which unconditionally snapshots and
restores the whole routine registry around its body
(`Interpreter::restore_routine_registry`) — a mechanism completely separate
from the `use`-triggered `PushImportScope`/`PopImportScope` bracket, and
blind to the fact that the import it was rolling back was never lexically
scoped to begin with. The imported operator was reachable only for the
remainder of whichever block first triggered composition, and silently
vanished the next time the role's own methods tried to call it from a
different call stack.

Fixed by persisting the package-qualified function keys a role's `use`/`need`
statement installs into `module_registered_functions`, the same set that
already protects a genuine module's own top-level declarations from this
exact kind of scope rollback.

Both shapes reproduced the exact runtime error `Algorithm::Kruskal` (an
ecosystem distribution) hit via its `Algorithm::MinMaxHeap` dependency, whose
`t/01-basic.t` calls its `do-it`-shaped helper from inside bare `{ }` blocks.
Pinned by `t/oo/role/role-body-use-flat-role-name.t` and
`t/oo/role/role-body-use-survives-block-scope-rollback.t`.
