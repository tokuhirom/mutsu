# `PROCESS::<$name> := ...` installed inside a nested block/module/sub now survives scope exit

`PROCESS::<$name> := value` (the `Rakudo::Internals.REGISTER-DYNAMIC` idiom
several CPAN/zef modules use to install a process-level dynamic default, e.g.
`$*AUTH-USER`, `$*DBI-DEFS`) used to survive only when it ran at the outermost
flat scope of a compilation unit. `store_process_dynamic` wrote the value
through `self.env_mut()`, which is whatever `Env` is currently live —
including a nested bare block's, a `module { ... }` body's, or a sub/closure
call's *scoped-child* frame. That frame's overlay is dropped the moment the
frame exits, so a later `$*name` read or write from a sibling or enclosing
scope threw `X::Dynamic::NotFound`, even though `raku` installs the default
globally regardless of nesting depth.

Fixed by adding `Interpreter::process_dynamics`, a small mutable
per-interpreter store (mirroring the existing `our_vars` store for `our`
variables) that lives outside the `Env` chain and therefore outlives every
frame:

- `store_process_dynamic` now mirrors every `PROCESS::<$name> := ...` write
  into `process_dynamics`, and marks the name as a declared dynamic
  (`set_var_dynamic`) so a later `$*name = ...` from ANY frame passes
  `CheckDynamicVarDeclared` without needing its own `my $*name` first — which
  is exactly what installing a process-level default means.
- `GetGlobal`'s fallback chain now consults `process_dynamics` as a last
  resort, after every live `env`/dynamic-scope route, so a read from an
  unrelated later frame resolves to the installed value.
- The `SetGlobal` Proxy write-through lookup (the code path that makes a
  plain `$*name = value` fire a `Proxy`'s `STORE` instead of rebinding the
  variable) now also checks `process_dynamics`, so a `PROCESS::<$name> :=
  Proxy.new(...)` install — Object::Permission's `$*AUTH-USER` pattern — keeps
  routing through the SAME Proxy container across frames, not just within the
  frame that installed it.
- A subsequent plain `$*name = value` (not another `PROCESS::<...>`) to a
  name already present in `process_dynamics` also updates the durable copy,
  so a third, completely unrelated frame sees the fresh value rather than the
  one recorded at install time.

Pinned with `t/vm/scope/process-dynamic-survives-nested-frame-exit.t`,
covering a Proxy install from a bare block, a Proxy install from a `module {
... }` body, and a plain-value install/write/read chain across three
different sub frames.

Closes [#8682](https://github.com/tokuhirom/mutsu/issues/8682), found via the
`ecosystem-dist-roulette` sweep on the `Object::Permission` distribution,
whose `lib/Object/Permission.rakumod` installs `$*AUTH-USER` exactly this way
from inside its own `module Object::Permission:ver<...> { ... }` block.
