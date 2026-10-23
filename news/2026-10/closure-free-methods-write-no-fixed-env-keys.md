# Closure-free methods write no `self` / `?CLASS` / `?ROLE` env keys

ADR-14633 slice 2: a compiled method whose body creates no closure no longer inserts `self`,
`__ANON_STATE#` and `?CLASS` (nor removes `?ROLE`) in its frame env. The frame's fixture record,
added in slice 1, answers them instead: every runtime reader of those names now goes through
`Interpreter::env_or_fixture_sym` (an env key first, then the innermost method record), and the
by-name `GetGlobal` path falls back to the record the same way.

The frame still writes the keys when one of the four names is visible in its env chain (a caller
that writes its keys, a class body, a closure's lexical `self`): an env-first reader would find
that key instead of this call's. So no analysis decides correctness -- the chain probe does, and a
frame under a key-writing caller behaves as before. Methods with closures, `EVAL`, or the slow
path are unchanged and are the next slices (closures carrying their own record, lazy frame env).
