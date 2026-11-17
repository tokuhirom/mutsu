# `Attribute.set_value` shares an unboxed scalar's container

`Attribute.set_value($object, $source)` now promotes the caller's scalar to a
shared cell when the built-in method binds it. Writes through the attribute or
the source variable reach the same value, including captured outer lexicals.
Literal and directly bound values keep their existing binding behavior.
