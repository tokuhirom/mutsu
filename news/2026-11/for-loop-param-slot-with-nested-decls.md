# A `for` parameter keeps its slot when the body re-declares its name

A pointy `for` parameter (`for @a -> $x { ... }`) is a local slot of the loop
body since #17530, so a body read is a slot read instead of a by-name scope
walk. A body that declared the same name in a nested block
(`{ my $x = ...; }`) or declared a lexical `sub` fell back to the old by-name
binding, because the nested `my` would have reused the parameter's slot.

The parameter is now declared in the body's scope frame, so a nested block's
`my $x` gets a shadow slot of its own, and a lexical `sub` captures the
parameter like any other local. Only a body that declares an `our` routine, a
proto or a `use` keeps the by-name binding, since `RegisterSub` persists that
binding for an `our` sub that outlives the body (ADR-12529).
