# `await` consumes an `Awaitable::Handle`

`await` on an `Awaitable` whose `get-await-handle` returns an `Awaitable::Handle` (ADR-15519, second
unit) now settles from the handle instead of returning the handle object: an already-successful
handle yields its `result`, an already-failed one raises `X::Await::Died` carrying its `cause`, and
a pending one is settled by the callback passed to its `subscribe-awaiter`, including from another
thread. The handle is turned into a Promise by a small prelude routine, so settlement and the
`X::Await::Died` wrapping stay on the one native Promise path. A native `Promise`/`Channel` still
awaits directly; making them instances of the role is left open.
