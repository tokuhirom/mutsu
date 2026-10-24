# `CALLERS::<&sub>` sees a sub declared at the top of the calling role's file

`::CALLERS::<&name>` answers the routine `name` that the calling routine's lexical scope
declares. A method composed from a role runs with the composing class as its package, so the
lookup walked the class's packages and never reached the role's own, where the sub the role's
file declares at its top level is registered: Red's `Red::ResultSeqMethods` (`sub
__RED_OPERATOR_LOADED__ { True }` next to `method !agg { self.map(...) }`) was told the
operators were not loaded. The role the method is lexically inside now counts too.

A role with type parameters still misses it: its body is re-declared for each composition and
that sub is not in the routine table; filed separately.

Refs #14094.
