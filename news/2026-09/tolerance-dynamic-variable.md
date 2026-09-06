# `$*TOLERANCE` is now defined, so `≅` compares against the documented tolerance

`$*TOLERANCE` — the relative tolerance `infix:<=~=>` / `infix:<≅>` compare
against — was never seeded in mutsu. Reading it gave `Nil`, so the example
`raku-doc/doc/Language/operators.rakudoc` uses to *demonstrate* the operator
silently degenerated:

```raku
my $x = 1;
say ($x + $*TOLERANCE) ≅ $x;   # raku: False   mutsu (before): True
```

With `$*TOLERANCE` undefined the left operand was `1 + Any`, i.e. `1`, so the
comparison was the trivially-true `1 ≅ 1`. The asymmetry raku shows is real and
is the point of the example: `1 + 1e-15` is a different `Num` from `1` while
`1 - 1e-15` rounds back to it.

## What changed

`$*TOLERANCE` now materializes on first read via
`Interpreter::lazy_magic_dynamic_var` (`src/runtime/io_env.rs`), alongside
`$*DISTRO` / `$*COLLATION` and friends. It is a plain `Num` rather than an
expensive instance, but it belongs in the lazy table for the same reason those
do: a program that never compares with `≅` should not carry the entry in every
per-frame env overlay. Because the lazy table is consulted only *after* a direct
env miss, a `my $*TOLERANCE = 0.1` still shadows it exactly as in rakudo,
including for a sub called from that scope.

The default value moved into one named constant, `runtime::DEFAULT_TOLERANCE`,
which the four operator sites that previously spelled `1e-15` inline
(`vm_comparison_ops.rs`, `vm_comparison_order_ops.rs`, `builtins_coerce.rs`,
`methods_native_bypass.rs`) now share with the declaration, so the variable and
its fallback cannot drift apart.

`cmp-ok $a, '=~=', $b` had its own private copy of the comparison with a
hard-coded `1e-15` and a relative-only formula. In rakudo `cmp-ok` resolves the
real `infix:<=~=>` routine, so it now delegates to `approx_eq_values` — which
means it observes a caller's `$*TOLERANCE` override and the operator's exact
zero/`Inf`/`NaN` handling instead of approximating them.

The comparison formula itself was already correct and is unchanged.

Pinned by `t/tolerance-dynamic.t` (20 assertions, all verified against raku
v2026.07): the default and its type, the doc example's asymmetry, both
spellings, relative-vs-absolute behaviour, zero/`Inf`/`NaN`, `Complex` and `Rat`
operands, a lexical override seen by a called sub and not leaking past its
scope, and `cmp-ok`.
