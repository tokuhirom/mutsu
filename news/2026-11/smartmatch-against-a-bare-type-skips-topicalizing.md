# `$x ~~ SomeType` skips the topic machinery

A smartmatch runs its right side with `$_` aliased to the left side, syncs
the locals a regex there might interpolate, runs the right side as a nested
range and afterwards checks whether it modified the topic. For a bare type
name on the right (`$obj ~~ SomeRole`, `self ~~ Monad::Maybe::Some`) none of
that is observable: once the bare word's site has resolved it to a type
object, evaluating it runs no code. Such a match now goes straight to the
same match core and epilogue (`vm_smartmatch_type_lane.rs`), taking it from
~13K to ~8K instructions; a bare word that a block-scoped `use` binds, or
that resolves to anything but a type object, keeps the general op.

An exact `ObjAt` operand of `~` (`M.WHICH ~ '|'`) is also stringified before
the dozen operand probes none of which can apply to it.

Found on the way: #17594 (a user `method ACCEPTS` on a class is ignored
when the type object is the right side). Refs #17331.
