# Argument-list interpolation is now decided by call-site syntax everywhere

Raku's `f(|EXPR)` interpolates `EXPR`'s elements into the argument list; every
other argument is exactly one argument, whatever its runtime type. A `Slip`
(e.g. the `Empty` a non-firing tail `if` returns) is an ordinary `List`
subtype, not a spread request. mutsu used to get this right on exactly two
call opcodes (`ExecCall`/`ExecCallPairs`, via a compile-time
`slip_positions_idx` constant) and wrong everywhere else: `CallFunc`,
`CallOnValue`, `CallOnCodeVar`, `CallMethod`, `CallMethodMut`,
`CallMethodDynamic[Mut]`, and `HyperMethodCall[Dynamic]` all flattened *any*
Slip-shaped argument value at runtime, regardless of whether `|` was used.
That meant a very common Raku shape — passing along the result of a routine
whose tail conditional did not fire — died with a bogus arity error:

```raku
sub maybe($x) { if $x { 42 } }
sub show($a)  { say "show got: ", $a.raku }
show(maybe(0));   # raku: "show got: Empty"; mutsu: Too few positionals passed
```

[ADR-0054](../../docs/adr/0054-argument-list-interpolation-is-a-call-site-property.md)
records the full investigation (a `raku`-verified argument-shape matrix, the
rejected alternatives, and why "argument-list interpolation is a call-site
property" is the only sound fix — the same principle
[ADR-0021](../../docs/adr/0021-argument-namedness-is-a-call-site-property.md)
already established for named-ness). It landed in six slices:

- **Slices 1-3** extended the `arg_sources` per-argument-position side table
  (which already tracked `is rw` source names) with a third entry shape: a
  `|EXPR` position is marked `TRUE`, decoded by `decode_arg_slip_positions`.
  Every call op now spreads only the positions the compiler recorded as
  `|EXPR`, never a value's runtime Slip-shape. This fixed the actual
  language-compatibility bug across every call path (function, method,
  code-variable, hyper), including a genuine bug in `HyperMethodCallDynamic`
  which previously did not flatten a Slip argument at all, by shape or
  syntax.
- **Slice 4** collapsed `ExecCallPairs`'s separate `slip_positions_idx`
  constant (a bare array of integer positions) into the same `arg_sources_idx`
  descriptor every other call op uses, so a call site carries exactly one
  syntax descriptor instead of two. This also fixed a light-call/OTF-cache
  perf bug: the cache-bypass check (`stack_args_have_slip`) used to probe the
  stack for Slip-shaped values, so a call whose ordinary argument merely
  *evaluated to* a Slip (`f(@a.Slip)`) forfeited those caches on every single
  call, forever. It now decides this once from the compile-time descriptor,
  so such a call stays cache-eligible.
- **Slice 5** was compiler-comment cleanup: `compile_tail_stmt_call_value`'s
  named/slip routing through `ExecCallPairs { keep_value: true }` no longer
  needed to justify itself by Slip-tracking accuracy (that's now uniform
  across every call op) — only by needing to keep the tail call's value on
  the stack.
- **Slice 6** audited every `Value::slip`/`slip_arc` construction in
  `src/runtime/` and `src/builtins/` for an internal (non-compiled) caller
  relying on the old blind-flattening behavior. None were: every site builds
  a Slip as a return value (`.Slip`/`Slip.new`/`slip()` coercions, the
  `andthen`/`succeed`/`leave`/`make` "return `Empty`" signal, or a recursive
  value-transform), never as a synthesized call argument expecting
  auto-spread.

Two long-standing band-aids are gone rather than extended: `preserve_empty_slip_arg`
(a callee-name allow-list that only ever patched the empty-Slip half of the
bug) and the `val`-name special case that existed purely to opt out of blind
flattening. The mutual exclusion between a `|` argument and `is rw`-source
tracking is gone too — the two side tables were unified, so a call with both
now cooperates instead of one defensively dropping the other.

Regression coverage: `t/slip-value-argument-is-one-argument.t` (the
fixed-arity-callee matrix across every call shape, Slices 1-3) and
`t/slip-value-argument-warm-cache.t` (50 repeated calls per shape, pinning
that a Slip-valued argument and a genuine `|EXPR` spread stay distinguishable
once their light-call cache entries are warm, Slice 4), alongside the
existing 29-case `t/slip-arg-flatten.t` net, which stayed green throughout.
