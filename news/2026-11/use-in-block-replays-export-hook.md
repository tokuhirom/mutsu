# A routine-body `use` of a module with `sub EXPORT` is replayed too

A `use` written inside a routine body runs on every call of the routine. After
its first run, a later run replays what the first one recorded instead of
importing again (#13906). A module whose import went through a `sub EXPORT`
hook was never replayed, so `sub g { use JSON::Fast; ... }` re-imported on
every call: 78.9k instructions a call, against 14.7k for `use Test` (#19102).

Rakudo runs the hook once, at compile time, so the values of its first run are
the ones every later run sees, and the replay reuses them. Those values stay in
the block's import set. The replay also records the names that run hid from
routines written outside the block, and restores them on each call. A `use`
that passes arguments to the hook still imports for real.

`sub g { use JSON::Fast; 1 }` now costs 13.4k instructions a call on a release
build, the same as a module without a hook.
