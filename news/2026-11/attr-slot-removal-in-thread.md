# `@!x.shift` in a `start` block acts on the instance's array

Since `@!x` / `%!x` method calls go through an attribute slot (ADR-18356 D2,
#18394), the call site refreshes the receiver from `self` before the call.
Inside a spawned thread the mutating-method dispatch then still routed a
removal (`shift`, `pop`, `splice`) through the name-keyed thread-shared array
lane, which held the spawn-time snapshot `clone_for_thread` had seeded under
`@!x`. A queue filled after the spawn read as empty there: the Mux
distribution's controller, a `start` block spawned in `BUILD`, died with
"Cannot shift from an empty Array" and its drain test hung.

A private attribute name (`@!x`) no longer takes the thread-shared array
lane: its one home is the instance's attribute cell, which every thread
reaches through `self`, as the slot read (`GetLocal`) already assumed
(#18438).
