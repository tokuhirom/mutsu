# An `is raw`/`is rw` closure parameter boxes a fresh cell instead of reusing the caller's

## Repro

```raku
{ my $v = 1; my $mk = -> $x is raw { key => $x }; my $p = $mk($v); $v = 2; say $p.value }
```

raku: `2`. mutsu: `1` (re-measured 2026-08-19 on the ADR-0032 D1-D3 branch —
unaffected by that change, as expected; this is ADR-0032 §2.1's explicitly
out-of-scope probe `O`).

## Root cause (per ADR-0032 §2.1)

Here `$x` (the pointy-block PARAM, not a free variable) genuinely IS a local
of the running frame at the `key => $x` `WrapVarRef` site, so
`capture_var_cell_inner` (`src/vm/vm_data_ops.rs`) takes its "found a slot"
branch rather than the `slot == u32::MAX` branch ADR-0032 addresses. That
branch boxes the PARAM SLOT into a fresh `ContainerRef` cell — but the
caller's argument (`$v`) was passed as a plain value (or, if already boxed,
its own cell was not threaded through the `is raw` bind), so the freshly
minted cell does not alias the caller's container at all. This is a
parameter-BINDING identity defect, not a capture-edge problem — a different
branch of the same function than ADR-0032 touches.

## Direction

Per ADR-0032 §2.1: "a `is raw` param slot that already holds the caller's
cell would then take the `is_container_ref` early return" — i.e. the real
fix is upstream of `capture_var_cell_inner`, at the `is raw`/`is rw`
parameter BIND site: when binding `$x is raw` from `$v`, the callee's param
slot must receive the CALLER's actual container (boxing the caller's
declaration into a cell first if it is not one already), not a plain-value
copy that gets independently boxed later. Investigate the `is raw`/`is rw`
bind path (parameter binding in `src/runtime/`) rather than
`capture_var_cell_inner`.

## Affected files

- `src/vm/vm_data_ops.rs` (`capture_var_cell_inner`)
- The `is raw`/`is rw` parameter bind path (not yet located precisely —
  start from how a pointy-block param with `is raw`/`is rw` receives its
  argument value).

## Pin

None yet — no dedicated test exists for this specific shape. Add one when
fixing.
