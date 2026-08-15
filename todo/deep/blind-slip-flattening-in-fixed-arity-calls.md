# Blind Slip-value flattening corrupts fixed-arity call argument lists

## Root cause

mutsu has two independent mechanisms for handling Raku's Slip-flattening
(`|EXPR` / an `Empty`-valued term):

1. **Explicit-marker-based** (architecturally correct): `ExecCallPairs`
   consults `slip_positions_idx`, a compile-time constant built by
   `add_slip_positions_constant` that records only the *syntactic* `|EXPR`
   argument positions. A Slip-shaped value that was NOT introduced with `|`
   (e.g. a sub call that merely *evaluates to* `Empty`) is left as an
   ordinary single argument.

2. **Blind value-shape-based** (architecturally wrong):
   `append_flattened_call_arg` (`src/vm/vm_call_helpers.rs:65`) flattens
   *any* Slip-valued argument it sees at runtime, regardless of whether `|`
   was used at the call site. This is the argument-gathering path for plain
   `CallFunc` / `CallOnValue` / `CallOnCodeVar` (`vm_call_func_ops.rs`,
   `vm_call_method_ops.rs`, `vm_call_method_mut_ops.rs`,
   `vm_hyper_method_ops.rs`).

Per real Raku semantics (verified with `raku -e`), a `Slip` (e.g. `Empty`)
flattens into a **slurpy** (`*@a`) parameter list, but must **not** flatten
into a **fixed-arity** positional parameter list — there it is passed
through as a single (empty-list-valued) argument, same as any other Iterable
term would be in a non-slurpy position.

Mechanism 2 cannot tell the difference: by the time it sees the value, the
syntactic `|` marker is gone. It flattens unconditionally, so a plain
positional argument that merely *happens to evaluate to* `Empty`/a Slip gets
silently dropped from a fixed-arity call's argument list instead of being
passed as one (empty) argument.

## Existing narrow mitigation (not a fix)

`preserve_empty_slip_arg` (`vm_call_helpers.rs:84`) special-cases the
*callee name* for operator forms (`prefix:<...>`, `postfix:<...>`,
`infix:<...>`) and the `andthen`/`notandthen`/`__mutsu_andthen_finalize`
family, so an `Empty` operand reaching one of *those specific* dispatch
sites is preserved as a single value instead of vanishing. This is a
targeted patch for the `andthen`/`orelse` compiled-operator call shape (see
`news/2026-08/` entry for the `helpers_sub_body.rs` tail-call fix, PR
#6499) — it does **not** generalize. Any other fixed-arity user sub/method
that receives a Slip-shaped value as a plain positional argument still
silently drops it.

## Minimal repro

```raku
sub g($a) { say $a.raku }
g (Empty andthen 42);   # should print `()`  (an empty Slip/List, one arg)
                         # currently: wrong arg count / arg silently dropped,
                         # because `g` is not in `preserve_empty_slip_arg`'s
                         # allow-list.
```

Confirmed against real `raku`: `g` receives exactly one argument, the
(empty) result of `Empty andthen 42` — `andthen` short-circuits on `Empty`
and returns `Empty` itself, which coerces to `()` when captured by a
non-slurpy `$a`. mutsu's blind-flattening path instead treats the `Empty`
Slip as "nothing to pass," changing `g`'s effective arity.

## Why this is deep / out of scope for a quick ticket

Fixing this properly means replacing mechanism 2 with mechanism 1 (or
teaching mechanism 2 to consult the callee's actual signature arity/
slurpiness before flattening) across every `CallFunc`/`CallOnValue`/
`CallOnCodeVar` site — `vm_call_func_ops.rs`, `vm_call_method_ops.rs`,
`vm_call_method_mut_ops.rs`, `vm_hyper_method_ops.rs`. That is a
cross-cutting VM change, not a local patch, and needs its own design pass
(most likely: migrate these call sites onto the same `slip_positions_idx`
compile-time tracking `ExecCallPairs` already uses, rather than trying to
recover slurpiness information at the value-flattening call site where the
callee's signature is not yet resolved).

## Affected files

- `src/vm/vm_call_helpers.rs` (`append_flattened_call_arg`,
  `preserve_empty_slip_arg`)
- `src/vm/vm_call_func_ops.rs`
- `src/vm/vm_call_method_ops.rs`
- `src/vm/vm_call_method_mut_ops.rs`
- `src/vm/vm_hyper_method_ops.rs`
