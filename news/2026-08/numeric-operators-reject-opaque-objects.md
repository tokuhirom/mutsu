# Numeric operators reject opaque objects

Numeric infix operators now raise `X::Multi::NoMatch` when an object has no
`Numeric` method, matching Rakudo's generic numeric candidates. The error names
the unresolved `Numeric(Class:D:)` call instead of silently using mutsu's
structural or floating-point fallback.

The numeric bridge still accepts native numeric-capable values and user-defined
`Numeric` methods. `DateTime` remains available to its dedicated temporal
operators, while its existing equality fallback continues to numify it.
