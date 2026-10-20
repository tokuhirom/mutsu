A scalar name bound directly to an object now records the immutable value
binding on its own binding cell. The object retains its writable attributes,
while assigning a different value through that name is refused.

Container reflection asks the binding cell before the dynamically scoped
readonly registry, so an inner declaration with the same name does not change
the outer binding's `.VAR` identity.

Named method calls carry the receiver slot resolved by the compiler. Both
literal and dynamic `.VAR` calls publish that binding before the call's
writeback boundary, rather than selecting the first same-named local slot.

The separate scalar-bind container-identity namespace, its global latch and
its per-slot key memo are retired. Value binds use their readonly binding kind;
Proxy bindings retain their assignable container semantics.

Applying a role to a value-bound routine updates the object through its
existing binding cell, preserving aliases and the binding's readonly kind.
