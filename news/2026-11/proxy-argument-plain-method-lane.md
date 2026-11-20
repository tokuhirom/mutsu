# A method call with a Proxy argument replays its call plan

The plain-method lane skips the stretch of speculative probes a `CallMethodMut` walks
before it dispatches, keyed by the receiver's class, the method and the arguments' type
keys. A `Proxy` argument had no key, so any call that passed one never entered the lane:
every call walked the whole probe stretch and the dispatch tail. That held even when the
parameter kept the Proxy itself (`Mu $x is raw`) and never FETCHed it.

A positional Proxy now keys the lane with a marker of its own, never with what its
FETCH answers. None of the probes the lane skips reads a Proxy argument's value. The
method-call plan admits it too. The plan's reading of a Proxy argument is fixed by the
parameter it binds: a `Mu` container parameter keeps the Proxy, a typed one FETCHes it on
every call, and the lean frame refuses it. Multi-dispatch caches still refuse a Proxy,
since a candidate set dispatches on its value.

The #18321 repro (`$c.m($p)` with `Mu $p is raw`) now dispatches through the tail once and
replays the plan on every later call: 25.3k → 17.0k instructions per call. AttrX::Mooish
passes its `AttrProxy` this way to `build-attr` and `store-with-cb` on every lazy build.
