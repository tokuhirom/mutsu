# A two-hop sigilless bind chain, and a sigilless bind to a typed array element, are wrongly rejected as read-only

## Symptom

Two separate `\x := ...` bind shapes reject a subsequent write with
`X::Assignment::RO` ("Cannot modify an immutable value (x)") in mutsu, where
real Raku accepts the write and propagates it to the ultimate source. Neither
involves any type constraint — plain untyped variables reproduce both.

### 1. A two-hop bind chain

```raku
my $a = 5;
my \y := $a;
my \x := y;
x = 42;
say "a=$a";
```

Raku: `a=42`.

mutsu: dies immediately with `Cannot modify an immutable value (x)`.

### 2. A sigilless bind to a typed array element

```raku
my Int @arr = 1, 2, 3;
my \x := @arr[0];
x = 1000;
say @arr;
```

Raku: `[1000 2 3]`.

mutsu: dies immediately with `Cannot modify an immutable value (x)`.

Both were found while verifying the scope of
`news/2026-08/sigilless-alias-write-now-type-checked.md` (which fixes the
*type-check* gap for a sigilless alias write) — these are unrelated,
pre-existing bugs about read-only status, not type-checking; the write is
rejected before any type check would even run.

## Root cause (not yet investigated)

For shape 1, the bind-time readonly computation in
`src/vm/vm_var_assign_set_local.rs` (around line 1306-1340, the `bind_source`
handling inside `exec_set_local_op_inner`) resolves the chain's ultimate
source via `resolve_sigilless_alias_source_name` and then calls
`self.is_readonly(&resolved_source)` to decide whether to
`self.mark_readonly(name)`. For `\x := y` where `y` itself came from
`\y := $a`, `resolved_source` should resolve all the way to `"a"`, and
`$a` (a plain `my $a = 5`) should not be readonly — so on a first read of
that code this looks like it SHOULD work. Either `is_readonly` is answering a
different question than expected (e.g. checking `y`'s own readonly flag
rather than the resolved root's), or the alias-chain resolution used at bind
time differs subtly from `resolve_sigilless_alias_source_name`'s walk (see
the `alias_target` vs `resolved_source` distinction a few lines above, which
deliberately keeps the *immediate* source for env-write-chain-walking
purposes in some cases). Needs a `rust-gdb` breakpoint at the
`mark_readonly`/`is_readonly` calls in that function to see what `resolved_source`
and `is_readonly`'s answer actually are for this repro, per CLAUDE.md's
debugging guidance (do not guess the key name; break and read the real
value).

For shape 2, this may be a related but distinct code path — binding to an
`Expr::Index` target (`scalar_elem_bind` in `src/compiler/stmt.rs` around
line 2021, and the `ContainerRef`-cell promotion machinery in
`src/vm/vm_var_assign_set_local.rs` for `my \x := @a[i]`-shaped binds) which
may have its own, separately-wrong readonly determination. Confirm whether
the SAME `is_readonly`/`mark_readonly` call is reached, or whether this is a
second, independent bug, before fixing either.

## Minimal repros

```raku
# 1. chained bind
my $a = 5;
my \y := $a;
my \x := y;
x = 42;
say "a=$a";   # raku: a=42; mutsu: dies "Cannot modify an immutable value (x)"
```

```raku
# 2. array-element bind
my Int @arr = 1, 2, 3;
my \x := @arr[0];
x = 1000;
say @arr;   # raku: [1000 2 3]; mutsu: dies "Cannot modify an immutable value (x)"
```

## Re-verified 2026-09-01 (TRIAGE regeneration)

Both shapes still die, but the message changed: mutsu now says `Cannot modify
an immutable Int (5)` / `Cannot modify an immutable Int (1)` rather than
`... immutable value (x)`. The alias apparently resolves to the *value* now
instead of being flagged read-only by name, so the `is_readonly` /
`mark_readonly` hypothesis above may be stale — break with `rust-gdb` at the
site that raises it and read the real path before fixing.
