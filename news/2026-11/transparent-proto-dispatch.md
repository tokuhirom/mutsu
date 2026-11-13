# A `{*}`-only `proto f(|)` dispatches like an implicit proto

`my proto sub insert(|) {*}` declares exactly the dispatcher a `multi` family
gets implicitly: it binds any argument list, runs no code and has no trait.
mutsu still treated it as a proto, so every call took the trivial-proto bypass,
which runs the winner through the general binder, while the same family
without the explicit proto takes the multi path: the winner's positional light
bind and the type-keyed multi light cache.

Such a proto is now transparent to dispatch (`src/vm/vm_transparent_proto.rs`,
memoized per proto generation). A proto with a body beyond `{*}`, a real
signature or a routine trait is unchanged. A call through
`proto a(|) {*}` with a sigilless candidate parameter now costs what the
implicit family costs (52k vs 49k instructions, from ~85k). Together with the
trivial-proto resolver change this is -7.4% instructions per
Concurrent::PriorityQueue push (#15786).
