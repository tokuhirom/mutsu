# A routine's type capture is local to it

`sub outer(::Type) { inner(Int); Type }` answered `Int` when `inner` also
captured `::Type`: a type capture is bound by name in the routine's env, and
the call's return merge treated that name as the caller's, so the callee's
binding replaced the caller's. A type capture (and its bound marker) now
counts as the routine's own declaration, like its parameters (#13568). The
Deps distribution hit this through `register(Any:U ::Type)` calling
`prepare-args(::Type)` calling `get(::Type)`.
