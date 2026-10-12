# A grammar's action walk stops paying for the general call funnels

One warm `load-yaml` of the 60-row `bench-yaml-parse-big` document now runs
in ~290M instructions instead of ~359M (-19%, `--profile profiling`,
`MUTSU_JIT=off`, four calls minus one; ~414M before #13012, which shared the
role scope described first below at the same time). Four costs in the
post-parse action walk had nothing to do with the actions themselves (#12917,
ADR-12917 direction 4):

- **Role scopes copied per construction.** A role whose attribute has a
  default captures its declaring scope, and every attribute default copied
  onto a composing class carried its own copy of that map. The attribute
  list is cloned per `.new`, so each construction of YAMLish's `Quoted`
  copied and dropped the module's whole scope per attribute — ~84k `String`
  clones per parse. The scope is now one shared `Arc` (#13012 landed the same
  sharing for the attribute defaults concurrently; #13022 adds the role's own
  copy).
- **Action methods through `call_method_with_values`.** Every action call
  walked the general by-value method funnel's name-keyed interceptors before
  reaching the compiled user method it always ended at. The walk now resolves
  the method directly and keeps the funnel as the fallback.
- **`make` through the function-call funnel.** An unshadowed one-argument
  `make` now goes straight to the builtin, checking the same shadowing
  sources (`sub make`, a lexical `&make`, multis, wraps) as the
  `callsame`/`nextsame` lane: ~15,000 instructions per call to ~4,700.
- **`$<name>` through `AT-KEY`.** Reading a named capture of an action's `$/`
  asked `AT-KEY` through the method funnel (~45,000 instructions) instead of
  reading the capture the way the `$/<name>` subscript does (~7,600). Both
  now share one helper.

The deferred-repeat flush, which rebuilt its pending list at every node of
the walk even when nothing was ready, now answers a miss with a scan.
