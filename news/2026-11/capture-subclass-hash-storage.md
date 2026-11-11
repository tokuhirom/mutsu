# A `Capture` subclass shares its `%!hash` with nqp code

An instance of a user `Capture` subclass now keeps its nameds in a hash node
of its own, and `nqp::getattr` / `nqp::bindattr` of `Capture`'s `%!hash` and
`@!list` read and install it. EEL 0.3's `Event is Capture` binds a `Hash`'s
`$!storage` there and reads it back into a fresh `Hash` its actions write
captures into (#16888); both directions now share one store, so a capture an
action records is visible through the event. When the capture's `%!hash` is
installed into a `Map`, the capture follows the map's node (ADR-12666 §9
re-points the raw handle variable; here the handle is held by the capture).

On the way: assigning an element through a `has Hash $.h` accessor checked the
element against `Hash` (the attribute's own type), as if the attribute were a
`%`-sigil one; a typed `%`/`@` attribute's element check now raises
`X::TypeCheck::Assignment` naming `%!h`, as rakudo does; and `<key>` of a
`Capture` subclass is `Nil` for a missing key rather than a "does not support
associative indexing" Failure.
