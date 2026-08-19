# `.VAR.WHICH` identity does not survive a closure boundary

## Repro

```raku
{ my $v = 1; my $mk = -> { $v.VAR.WHICH }; say $mk() eq $v.VAR.WHICH }
```

raku: `True`. mutsu: `False`.

## Root cause

`.VAR` on a scalar variable target (`compile_expr_method_on_var` in
`src/compiler/expr_method.rs`, compiled as `CallMethodMut` with the plain
variable NAME baked in as `target_name_idx`) is dispatched by
`call_method_mut_with_values` (`src/runtime/methods_mut_dispatch.rs`, the
`method == "VAR"` branch around line 123). It builds a reflection `Instance`
(class `Scalar`/`Array`/`Hash`) the first time `.VAR` is called for a given
variable name, and CACHES it via `set_var_meta_value(target_var, meta)`
(`src/runtime/runtime_var_meta.rs`), which is a raw `self.env.insert` under
the synthetic key `__mutsu_var_meta::<name>`. A second `.VAR` call for the
same name reads the cache back (`var_meta_value`) and reuses the SAME
`Instance` (so `.WHICH`, which is keyed off the instance's own monotonic
`id`, compares equal) — but only within the SAME frame's env.

The `target` value itself carries no identity: `compile_expr(target)` for a
plain `Expr::Var` emits `GetGlobal`, which (per its own doc comment)
INTENTIONALLY dereferences a captured `ContainerRef` cell for an ordinary
value read — so even a boxed `$v` arrives at the `.VAR` dispatch as a plain
value, not a cell. Identity is therefore carried ENTIRELY by the
`__mutsu_var_meta::` env-cache convention, and that cache write is a
runtime-only side effect with a synthetic key the compiler's free-variable
analysis (`free_var_writes`, the only thing that drives closure
env-writeback) never sees, because the key does not correspond to any
name that appears in the source text. So a `.VAR` call made *inside* a
closure caches its `Scalar` instance into the CLOSURE's own env, and that
write never propagates back out to the declaring frame — a later `.VAR`
call in the outer frame (or a second, independent closure) creates its own
fresh `Instance` with a different `id`.

Confirmed independent of ADR-0032 (`docs/adr/0032-wrapvarref-container-capture-across-closure-boundaries.md`):
the ADR generalized the UNRELATED `WrapVarRef` container-capture mechanism
(`key => $v`, `Pair.new`, `\($v)`, rw-arg/`:=` — see that ADR), and `.VAR`
does not go through `WrapVarRef` at all. As a control, the OLDER named-sub
mechanism this ADR's D1-D3 generalizes (which predates the ADR and already
worked for `key => $v` in a named sub) fails the identical `.VAR.WHICH`
shape:

```raku
{ my $v = 1; sub f() { $v.VAR.WHICH }; say f() eq $v.VAR.WHICH }
```

mutsu: `False` (raku: `True`) — proving this is a pre-existing, orthogonal
bug, not something ADR-0032's slice 1 could have fixed.

## Why it is not a quick fix

The correct behaviour is that a variable's `Scalar` container reflection
object should have ONE stable identity for the life of the variable,
regardless of which frame reads `.VAR`. That requires either:

- Deriving `.VAR`'s identity from something ALREADY frame-independent (the
  `ContainerRef` cell's own `Arc`/lock address) whenever the variable is
  boxed — but most variables are NOT boxed (boxing is deliberately rare and
  syntactically triggered, per ADR-0025's `#2749` perf gate), so this only
  covers a subset.
- Making the `var_meta_value` cache write for a captured/free variable
  reach the DECLARING frame's env, mirroring the write-back machinery
  `free_var_writes` drives for genuine source-level variable writes — but
  this synthetic key is invisible to that compile-time analysis by
  construction, so it would need its own dedicated mechanism (e.g., always
  writing the cache into the declaring frame's slot instead of the
  executing frame's env, which requires knowing at runtime which frame owns
  the name — the exact kind of by-name cross-frame resolution ADR-0032 §3
  alternative 2 rejects for `WrapVarRef`).

Either direction is a new, undesigned mechanism, not a natural extension of
ADR-0032's WrapVarRef-capture-edge decision — it deserves its own design
pass rather than a bolt-on fix inside this ticket.

## Affected files

- `src/compiler/expr_method.rs` (`compile_expr_method_on_var`)
- `src/runtime/methods_mut_dispatch.rs` (`call_method_mut_with_values`,
  `method == "VAR"` branch)
- `src/runtime/runtime_var_meta.rs` (`set_var_meta_value` / `var_meta_value`)

## Pin

`t/closure-container-capture-alias.t` probe X (marked `todo` with a
reference to this file).
