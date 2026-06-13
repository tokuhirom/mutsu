# Parameter aliasing for array/hash params — design plan (handoff)

> **Status: NOT STARTED. This is a handoff for a fresh session.** It is the
> largest remaining Track B / first-class-container correctness gap. It changes
> core function-call argument binding (high blast radius), so it deserves its
> own session with CI as the safety net.
>
> Read alongside `docs/container-identity.md` (Phase 2 element cells) and the
> memory file `project_track_b_phase2_element_cells.md`. The cell machinery this
> builds on landed in #2990 (whole-container `:=` bind sharing).

---

## 1. The bug

In Raku, a plain positional **array/hash parameter binds the caller's container
by alias** — mutating it through the parameter is visible in the caller:

```raku
sub a1(@x) { @x.push(9) }
my @a = 1,2,3;  a1(@a);  say @a;     # raku: [1 2 3 9]   mutsu: [1 2 3]   ✗
sub a2(@x) { @x[0] = 99 }
my @b = 1,2,3;  a2(@b);  say @b;     # raku: [99 2 3]    mutsu: [1 2 3]   ✗
```

**mutsu copies plain array parameters** (mutations don't propagate), i.e. it
treats plain `@x` like `is copy`. Raku treats plain `@x` like a read-only *alias*
(the container binding is readonly, but element mutation propagates because it's
bound, not copied).

### What already works in mutsu (verified 2026-06-13)

| form | raku | mutsu | note |
|------|------|-------|------|
| `sub f(@x){ @x.push }` | propagates | **copy ✗** | the gap |
| `sub f(@x){ @x[0]=v }` | propagates | **copy ✗** | the gap |
| `sub f(%h){ %h<k>=v }` | propagates | **propagates ✓** | hash params already alias! |
| `sub f(@x is copy){…}` | copy | copy ✓ | correct |
| `sub f(@x is raw){@x.push}` | propagates | **propagates ✓** | the alias mechanism exists |

**Two crucial facts that scope the work down:**
1. **Hash parameters already alias correctly** — only *array* params copy. So the
   fix is array-param-specific; the hash path is a working reference.
2. **`is raw` array params already alias correctly** in mutsu. The aliasing
   machinery (sigilless alias + writeback) already exists at
   `src/runtime/types/binding.rs:814-842` (the `is_rw || is_raw` branch). Plain
   params skip it and fall through to the copy path.

So the task is essentially: **make a plain `@`/`%` positional param bind by alias
(as `is raw` does) rather than by copy** — while preserving `is copy` (copy) and
the read-only-container semantics (you may mutate elements but not rebind).

---

## 2. Where it lives

- `src/runtime/types/binding.rs` — `bind_function_args_values` (line ~51), the
  positional-param loop at ~803. The `is_rw || is_raw` branch (814) sets up the
  caller alias (`rw_bindings` + `__mutsu_sigilless_alias::{name}`); plain params
  fall through to `let mut value = unwrap_varref_value(raw_arg)` (~863) and are
  stored as a (COW-shared) copy.
- The hyper writeback fix from this session (`#2999`) deliberately made
  `sub f(@x){ @x>>++ }` *not* propagate, to be consistent with `.push` (which
  doesn't propagate). **Once array params alias, both `.push` and `>>++` will
  propagate to the caller automatically** (the hyper lvalue-precise writeback in
  `write_back_hyper_target_var` writes through a cell, which the aliased param
  will hold) — so revisit `t/hyper-nested-writeback.t`'s param expectation and
  `docs`/PR-2999 note when this lands.

## 3. Approach (candidate)

Reuse the #2990 whole-container `:=` bind machinery: a plain array param should
hold a shared `ContainerRef` cell aliasing the caller's `@`-variable, exactly as
`my @b := @a` does. Then:
- `@x.push` / `@x[0]=v` / `@x>>++` write through the cell → caller sees it.
- the caller's `@a` must also become / already be that same cell.
- `is copy` continues to copy (no cell).
- readonly-container: rebinding `@x := ...` inside the sub is a fresh bind, not a
  caller mutation (already handled by rebind logic).

Key questions to resolve first (why this needs its own session):
- Does the caller's `@a` need to be promoted to a cell at the call site (like
  `@b := @a` promotes the source)? The arg is passed by value through the VM
  stack; the `arg_sources` table (used by the `is raw` branch) carries the
  caller variable name, so the binder can reach `@a` to promote it.
- Blast radius: **every** function/method call with an array param. Many tests
  may rely on the current copy behavior. Expect CI iterations.
- Interaction with: multi-dispatch, slurpy `*@`, `is copy`/`is rw`/`is raw`,
  signatures with sub-signatures, `@a[*]` flattening, native-typed arrays.

## 4. Verification protocol

- New `t/param-array-alias.t`: push/index-assign/`>>++`/splice through a plain
  array param propagate to the caller; `is copy` does not; nested/`%h` params.
- `make test` + **release `make roast`** (the broad blast radius means roast is
  the real safety net; expect to fix-forward).
- Watch `roast/S06-*` (signatures/params) and `int.t` perf (call path is hot).

---

## 5. This session's completed Track B work (context)

All MERGED unless noted (2026-06-13):
- **#2990** — whole-container `:=` bind sharing (`my @b := @a`, `my %g := %h`):
  push/pop/shift/unshift/splice/index/slice/delete propagate both ways via a
  shared `ContainerRef` cell. GetArrayVar/GetHashVar decont top-level cells.
- **#2993** — scalar `:=` whole-container bind (`my $r := @a`): same sharing for
  `$`-targets (compiler routes the bind through `compile_call_arg`).
- **#2994** — `.VAR.^name` reflects the bound container type (List/Array/Hash…)
  for `:=`-bound scalars, not `Scalar`.
- **#2996** — nested-element hyper writeback (`@b[0]>>++`, `%h<a>>>++`,
  `$r>>++`): in-place through the element's shared Arc.
- **#2999** (CI in flight) — hyper lvalue-precise writeback: `@a>>++` no longer
  corrupts a copy `my @x = @a`; bound aliases still see it (cell-write). Made
  `>>++` on an array param consistent with `.push` (both copy — until §1 lands).

Remaining Track B after param aliasing: write-autoviv `HashSlotRef` (concluded
**not removable** — deferred-token + `is raw` reduce lvalue-read semantics);
that campaign is effectively complete.
