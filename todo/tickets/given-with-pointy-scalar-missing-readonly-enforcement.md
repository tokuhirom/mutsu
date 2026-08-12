# `given`/`with EXPR -> $v { ... }` (no `is rw`) does not enforce readonly on `$v`

Split out from
`todo/deep/pointy-scalar-param-final-value-untracked-by-writeback.md` (now
resolved) — this is the separate, still-open gap that ticket's "Symptom"
section flagged but did not investigate.

## Symptom

```raku
my $x = 1;
given $x -> $v { $v = 99 }
say $x;
```

Real Raku dies at the assignment: `Cannot assign to a readonly variable or a
value`. mutsu instead silently allows the assignment — and, now that
`given`/`with` pointy-scalar writeback is fixed (news/2026-08/...), the
mutated value actually propagates back to `$x`, printing `99`.

## Where this lives

`exec_given_op` (`src/vm/vm_given_when_ops.rs`) computes `mark_ro` for the
*topic* (`$_`) as:

```rust
let mark_ro = topic_readonly && pointy_param.is_none() && !self.is_readonly("_");
```

`pointy_param.is_none()` unconditionally excludes readonly marking whenever
ANY pointy param is present — this is correct for `-> @p`/`-> %p` (Raku binds
those rw regardless of an explicit `is rw`), but wrong for `-> $v` without
`is rw`: a scalar pointy param should be readonly unless `is rw` is given.

The parser already accepts and records `is rw` in the pointy param's
`ParamDef.traits` (`parse_pointy_param`, consumed via `pd.traits` in
`pointy_topic_bind`, `src/parser/stmt/control.rs`), but `pointy_topic_bind`
only branches on `is copy` — the `rw` trait is parsed and then dropped on the
floor; there is no runtime signal distinguishing a `-> $v` from a `-> $v is
rw` scalar pointy param at all.

## Suggested fix shape (not implemented)

1. In `pointy_topic_bind`, detect `pd.traits.iter().any(|t| t == "rw")` for a
   scalar (non-`@`/`%`/`&`) pointy param.
2. Thread that boolean through to the compiled `Given`/`With` op (a new
   `pointy_param_rw: bool` alongside the existing `pointy_param_idx`
   constant-pool index), mirroring how `pointy_param_idx` is threaded today.
3. In `exec_given_op`, mark the pointy param's own slot readonly (via
   `self.mark_readonly(p)`, the same mechanism `scalar_bind` literal binds use
   — see `vm_var_assign_set_local.rs` around the `scalar_bind &&
   bind_source.is_none()` block) when the param is scalar and NOT `is rw`, so
   `$v = ...` inside the body dies through the normal readonly-assignment
   check instead of silently succeeding.
4. Verify against `raku -e '...'` that `@p`/`%p` (always rw, no trait needed)
   and `-> $v is rw` (rw) are unaffected.

## Reproduce

`my $x = 1; given $x -> $v { $v = 99 }; say $x;` — no fixtures needed.
