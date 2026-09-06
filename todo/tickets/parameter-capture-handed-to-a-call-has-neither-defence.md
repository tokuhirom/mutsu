# A closure over the frame's own parameter is still hijackable when that parameter was handed to a call

Measured 2026-09-06 while landing ADR-0055's `needs_cell_unvouched_locals`
(`news/2026-09/adr0055-unvouched-escaping-captures-get-a-cell.md`). This is the
one shape that mechanism deliberately leaves open.

## Repro

```raku
sub noop($v) { 1 }
sub outer($p) { noop($p); my $f = { $p }; return $f }
my $f = outer("OUTER");
sub collide() { my $p = "CALLER"; my $g = { $p }; $g.(); $f.() }
say collide();          # raku: OUTER    mutsu: CALLER
```

Drop the `noop($p)` line and mutsu answers `OUTER` (the vouch then holds, so `$p`
is authoritative and the merge force-installs it). Drop the `my $g = { $p }` line
and mutsu also answers `OUTER` — the caller's `$p` stays in a local slot, so the
merge's chain probe never sees it. Both lines are load-bearing, exactly as in
ADR-0055 §1.2(b).

## Why neither defence applies

* **Not authoritative.** `noop($p)` puts `$p` in `own_call_arg_sources`, and
  `CompiledCode::compute_free_vars` refuses to vouch for such a name: an
  `is rw` parameter in the callee could write it back, making a by-value
  overwrite-install go stale.
* **Not a cell.** `needs_cell_unvouched_locals` (the vouch's complement within
  the escaping-captured set) explicitly filters out `CompiledCode::param_locals`.
  Boxing a parameter made two invocations of one routine share a binding, which
  is what dropped six Cro::HTTP suites when the mechanism was first prototyped
  (`todo/deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md`,
  now closed by that exclusion).

So the exclusion traded this narrow shape for the Cro regression. Both cannot be
had until a parameter binding can create a genuinely fresh binding on top of a
slot/env that may still hold the previous invocation's cell.

## What the fix probably is

The Cro ticket's option 2, unblocked: **make parameter binding reset a stale
cell the way a vardecl does**. `exec_set_local_op_inner`'s `is_vardecl` arm
already does exactly this for `my` (clear a `ContainerRef` left in the slot and
overwrite the env key, so the redeclaration is a fresh binding, guarded by
`self_capture_decl_locals` and `local_bind_pairs`). A parameter binding does not
go through that arm. With the reset in place, `param_locals` could be dropped
from the `needs_cell_unvouched_locals` filter and this repro closes.

Not done here because it needs the same freshness rules
`t/for-loop-param-start-sibling-isolation.t` and the loop-body per-iteration
boxing depend on, and because the Cro leak's mechanism was inferred from the
August investigation's trace rather than re-measured under a debugger.

## Acceptance

The repro above prints `OUTER`; it joins
`t/closure-capture-cell-dichotomy.t` as an 18th assertion; the batteries gate
(`scripts/battery-testsuite.sh`) stays at its whitelisted count.
