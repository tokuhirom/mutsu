# An exported method's sub form no longer runs its body in an rw context

`method run(...) is export` is importable as a sub that forwards to the method.
That synthetic sub has no compiled body, so `returns_container()` answered its
"unknown means container" default, and the dispatcher set `rw_return_context`
for the whole call: every local the method read came back as its Scalar
container, and a native method such as `.from`/`.caps` on a stored grammar
`Match` found no such method (CSS::Selector::To::XPath's `selector-to-xpath`).
A candidate without a compiled body now answers from its own declaration
(`candidate_returns_container`). Fixes #15094.
