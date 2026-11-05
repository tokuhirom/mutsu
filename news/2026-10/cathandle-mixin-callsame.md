# `callsame` from a role mixed into an `IO::CatHandle` instance

A role mixed into an `IO::CatHandle.new(...)` object (`IO::CatHandle+{R}`) had
no native base behind `callsame`: the role's `get` dispatched back to itself
until the stack overflowed. The plain-handle view used as that base now covers
every `IO::*` handle class with a mixin, not only `IO::Handle` (#15825).
