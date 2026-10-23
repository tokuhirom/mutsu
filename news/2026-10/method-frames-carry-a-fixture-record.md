# Method frames carry a fixture record

ADR-14633 slice 1: a method call now pushes a `FrameFixtures` record (the invocant, `?CLASS` and
`?ROLE`) next to its routine frame. The records live in `RoutineStack`, which cuts them back in
`pop` and `truncate` together with the frames, so no call site can leave one behind; a frame names
its record through the new `Copy` field `RoutineFrame::fixtures`.

Nothing reads the record yet and the `self` / `?CLASS` / `?ROLE` env keys are still written, so
behavior is unchanged. It costs about 236 Ir per `$o.m()` (14,552 to 14,788, callgrind, second run)
until slice 2 stops writing the env keys. Answering `self` from the record before that is not safe:
about 20 helpers rebind the env `self` for the duration of a call, and a closure called inside a
method pushes no record of its own.
