# `@!x.shift` in a `start` block acts on the instance's array

Since `@!x` / `%!x` method calls go through an attribute slot (ADR-18356 D2,
#18394), the call site refreshes the receiver from `self` before the call.
Inside a spawned thread the mutating-method dispatch then still routed a
removal (`shift`, `pop`, `splice`) through the env binding of the name
`@!x`, which in a thread clone held the spawn-time snapshot `clone_for_thread`
had seeded (a `BUILD` parameter `:@!x` puts one there). A queue filled after the spawn read as empty there: the Mux
distribution's controller, a `start` block spawned in `BUILD`, died with
"Cannot shift from an empty Array" and its drain test hung.

The prologue that refreshes the attribute slot from `self` now also rebinds
a visible env binding of the attribute's name that holds another container,
so the receiver place the mutator writes through is the instance's container
in every thread (#18438).
