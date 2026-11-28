# A routine-nested sub reads its outer variables through a frame slot

A `sub` declared inside a routine reads the routine's lexicals through
per-activation alias cells (ADR-0114), but every read still resolved the name
by walking the whole by-name chain -- attribute, gather and package-store
probes, then the environment cascade -- before it reached the alias. That was
about 3k instructions per read, and it is the shape of JSON::Fast's `to-json`
helpers (`jsonify` and the `pretty-*` subs push to the routine's `@out` and
read `$comma` / `$pretty`) and of Web's `tokenize` scanners (`$html`, `$len`,
`%memo`).

The compiler now gives such a sub one hidden local per aliased free variable
and compiles each read (`$x`, `@x`, `%x`) to `GetLexFree`, a slot op. The
first read of a call takes the alias cell straight from the environment and
keeps it in the slot; every later read of the call loads the slot. Anything
that could make the by-name answer differ during a call (an rw-return
context, an atomic variable, a sigilless attribute, an escaping `our`
capture, a running `for` parameter of the same name, a call through a code
object that captured its own activation) sends the read down the by-name
path as before, and a debug build checks every first read against it.
