# A non-multi method's subset predicate runs once per call

The first call of a plain (non-multi) method with a subset-typed or
`where`-constrained parameter ran the predicate twice: once while the method
resolver speculatively matched the arguments against the candidate, and once
in the binder. With a single visible candidate the resolver's answer does not
depend on that match -- a failed match still selects the same method, and the
binder raises the type error -- so both resolvers (the legacy MRO walk and the
cached sequence picker) now return the lone candidate without matching. A
side-effecting predicate (a counter, a log) now runs once per call, as in
Rakudo (#10935).
