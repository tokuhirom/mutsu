# A class-scoped routine call takes its statics from the per-class plan

A class-scoped `sub` (and a class's multi dispatch) runs with its class's
class-body `my` statics in scope, injected into the env on entry and put
back afterwards. On every call that path cloned each static's name as a
`String`, re-interned it, and rebuilt the statics' typed and stale metadata
lanes. It now reads the class's own writes from the per-class statics plan
that method calls already use, and saves and restores the displaced bindings
by `Symbol` (#18274).
