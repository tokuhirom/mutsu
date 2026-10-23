# Methods with closures write no `self` / `?CLASS` / `?ROLE` env keys either

ADR-14633 slice 3 (as re-scoped by ADR-14792): the key omission of slice 2 now also covers a
compiled method whose body creates closures. A closure created in such a frame takes the fixed
names it reads from the frame's record into its own capture at creation
(`materialize_frame_self_into_capture`), so calling it later sees its creator's `self`,
`?CLASS` and `?ROLE`, never those of whichever method it runs under.

A method that declares a routine, class or role still writes its keys: a named `sub` captures the
env by another path than a closure creation and reads `self` from it after the method returned.
As in slice 2, a frame under a caller that writes the keys writes its own, so no analysis decides
correctness; the chain probe does.
