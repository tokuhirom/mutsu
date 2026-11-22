# A role-body `sub`'s traits run once

A `sub` declared in a role body (`role R { sub g is probe { } }`) is
re-registered every time the role body runs -- at the role's declaration, at
each composition and at a pun -- and its custom traits ran again each time
(NativeCall's `is native` sanity check warned once per composition). Rakudo
applies them once, at compile time, and clones the traited routine per
composition (#18097).

The first registration that runs a role-body sub's traits now records what
they left (the mixin a trait put on `&g`) under (role, routine); a later
registration of the same declaration skips the handlers, keeps the wrap chain
the traits built (a bound `$!do`, how `is native` replaces the body), and gets
the traited routine cloned over its own composition's scope.
