# A trivial proto's winner takes the light bind

A call through a `{*}`-only proto (`my proto sub insert(|) {*}`) resolves its
winner with the proto's own candidate set and then ran it through
`compile_and_call_function_def`. That entry has no caller `CompiledCode`, and
the positional light bind needs it to promote a caller variable passed to a
sigilless parameter, so every such winner (`insert(@a, $pos, \value)`) fell
back to the general binder.

The call site has its code, so the winner now binds the way a multi winner on
the ordinary multi arm does: the dispatch frame for `nextsame`/`callsame`, the
samewith context and the light bind, with the candidate choice still the
proto's. A call through `proto a(|) {*}` with a sigilless parameter now costs
what the same family without the explicit proto costs (47k instructions, from
~85k). Concurrent::PriorityQueue's push loop: 83.1M to 75.9M instructions per
64 pushes together with #17643 (#15786).

An earlier attempt routed such protos through the implicit-proto path
wholesale; it is wrong, because an explicit proto bounds the candidate set (an
inner `proto` hides the outer candidates) and treats a named slip differently.
