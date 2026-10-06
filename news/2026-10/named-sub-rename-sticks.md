# Renaming a named sub through `&foo` sticks

`sub foo {}; &foo.set_name("x"); say &foo.name` printed `foo`: every `&foo` read of a named
routine builds a fresh code object from the routine's registry entry, so the rename written
into one of them was gone on the next read. The same held for
`nqp::setcodename(nqp::getattr(&foo, Code, '$!do'), 'baz')`.

The rename now belongs to the routine: `RoutineCell`, the identity cell every code object of
a routine shares (ADR-11827), records it, and `Code.name` / `nqp::getcodename` read it back.
An alias taken before the rename sees it too, a `.clone` of the routine (and
`nqp::freshcoderef`) forks it so renaming the copy leaves the original alone, and the
routine still runs and dispatches under its declared name, as in Rakudo (#11844).
