# A typed `my` declaration no longer costs five untyped ones

`my Str $c = "a"` in a loop body cost **14,534** instructions per iteration
against **2,857** for `my $c = 1` (5.1x, #11467). It now costs **5,585**
against 2,875 (1.94x), measured with callgrind on the `profiling` build as
`(Ir(2000) - Ir(1000)) / 1000` of `for ^N { my Str $c = "a" }`. The
`my Str $chunk := @ch[$i]` shape Text::CSV runs per parsed chunk went from
22,761 to 18,349, and `my Int $c = 1` from 13,600 to 4,941.

Almost none of the old cost was the type check. It was the declaration
re-learning, on every execution, a constraint the source fixed at compile time:

- **The constraint was registered twice.** `hoist_typed_var_decls` emits a
  block-entry `SetVarTypeHoisted` for every typed `my` so that code running
  *before* the declaration sees its type. When nothing can run before it — the
  declaration is preceded only by literal-initialized `my`s and the block has
  no phaser — the declaration's own registration, which precedes its
  initializer, is already first, and the hoist is no longer emitted.
- **It was cleared in between.** `SetVarDynamic` drops a stale same-named
  constraint before every declaration. For a typed declaration whose
  registration is the very next op (`type_follows`), the clear only removed an
  entry the next op re-inserted.
- **The spelling was resolved from scratch.** Capture substitution, package
  and constant aliases, package qualification and three registry probes for a
  shadowing `subset`/`enum`/`role` all answer "unchanged" for a core name nothing
  shadows. `TypeDeclSiteCaches` remembers that answer per constant for one
  registry write generation (the `GetBareWord` memo's pattern); the
  env-dependent steps are two loads re-checked on every hit. The registration
  then stores the op's own constant-pool string as the metadata value, so a
  re-registration finds that very allocation already bound and skips the env
  write.
- **The type check ran twice and interpreted.** A known constraint was matched
  in its own arm and again by the general check below it. `TypeCheck` was also
  not on the JIT's step list, so every loop body with a typed `my` ran in the
  interpreter. Both are fixed, and a plain builtin `$` scalar takes a short path
  that skips the preamble's repeated `value.view()` calls (each a refcount round
  trip on a `Str`). The store skips the coercion and native wrap a vouching
  `TypeCheck` already applied.

Smaller items on the same path: `loop_local_saved_env` hashed its `String`
keys with SipHash (now `FxHashMap`), hash-key metadata is only touched for `%`
names (no other name can carry it), and the typed store asks the one-probe
"is there a same-named slot" question before the env-walking constraint probe
that only mattered when there is.
