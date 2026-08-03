# `$x does R` inside a block invoked as a *callable* never reaches the caller

`does` rebinds its left-hand variable to the mixed-in value. When the `does`
runs inside a block that is passed to a routine and invoked there, the rebinding
is lost — the caller's variable still holds the un-mixed value.

## Minimal repro

```raku
sub call1(&b) { b() }

my $p = {:x};
{ $p does role { has $.k = 1 } }
say "bare block: ", (try $p.k) // 'LOST';      # raku 1     mutsu 1

my $r = {:x};
call1 { $r does role { has $.k = 3 } };
say "via sub:    ", (try $r.k) // 'LOST';      # raku 3     mutsu LOST

my $u = {:x};
$u does role { has $.k = 2 };
say "no block:   ", (try $u.k) // 'LOST';      # raku 2     mutsu 2
```

Only the middle case is wrong. A **plain assignment** in the same position
(`call1 { $r = 5 }`) *does* reach the caller, so this is specific to the `does`
rebinding, not to the block shape.

## Where it hits

`roast/S14-roles/anonymous.t` aborts on its 4th assertion and loses 10 of its 13
tests under `MUTSU_REAL_TEST=1`:

```raku
my $a = {:x};
lives-ok { $a does role { has $.cool = "yeah" }}, "anonymous role mixin";
is $a.cool, "yeah", "anonymous role gave us an attribute";
# No such method 'cool' for invocant of type 'Hash'
```

`lives-ok` is exactly `call1` above — a routine that invokes the block it was
handed. Under mutsu's native `Test` provider the same file passes, because the
native `lives-ok` runs the block inline rather than through a routine call, so
the whole file is whitelisted today and only the real module exposes it.

## What is NOT the cause (measured, 2026-08-04)

- **Not the closure-capture cell.** At the `DoesVar` op the env binding for the
  variable is a plain value, not a `ContainerRef` — the lexical was never boxed,
  because `call1 { ... }` is an immediately-invoked call argument and the
  boxing gate (`needs_cell_locals`) deliberately excludes those for perf.
- **Not the by-name write classification alone.** Adding `OpCode::DoesVar` to
  `op_name_const_idx` / `op_name_write_const_idx` in `src/opcode.rs` — so the
  block records the variable in `free_var_syms` / `free_var_writes` exactly as
  an `AssignExpr` would — changes nothing observable. That patch was measured
  and reverted rather than shipped; whatever carries an `AssignExpr` write back
  out of such a block is not keyed only on that set, so the next step is to find
  the runtime writeback path an assignment takes (`vm_call_fast.rs:317`,
  `vm_call_light.rs:421`, `vm_call_named_inner.rs:566` all drain
  `free_var_writes`; one of them is presumably not reached for this shape) and
  make `exec_does_var_op`'s rebinding travel the same road.
- **`.^name` is a red herring.** `say $p.^name` prints `Hash` rather than
  `Hash+{<anon|1>}` even in the cases that work, so it cannot be used to detect
  the loss. Assert on the mixed-in attribute instead. (The `.^name` rendering of
  a mixin is a separate, cosmetic gap.)

## Files

- `src/vm/vm_mixin_does_ops.rs` — `exec_does_var_op` (`env.insert` +
  `write_local_slot_or_name`; the slot is `None` for a free variable).
- `src/opcode.rs` — `op_name_const_idx` / `op_name_write_const_idx` and the
  free-var classification loop around line 3778.
