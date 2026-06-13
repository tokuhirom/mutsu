# Parameter aliasing for array/hash params — design plan (handoff)

> **Status: DONE (2026-06-13).** Implemented via the writeback approach, not the
> cell approach sketched in §3 below. Plain `@`/`%` positional params now bind
> the caller's container by alias: `.push`, element assign, `>>++`, `splice`,
> and whole-container `=` assignment all propagate; `is copy` copies; scalar `$`
> params stay readonly. See §6 for what shipped.
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

---

## 6. What shipped (2026-06-13)

Chose the **writeback approach over the cell approach** of §3. The caller's
variable is not reachable at bind time (it lives in the saved call frame /
caller VM locals, restored only at return), so promoting it to a live shared
cell during binding is impractical. The proven `is rw`/`is raw` writeback
machinery already propagates a param's final container value to the caller at
return — and that handles `.push`, element assign, `splice`, `>>++`, and
whole-container `=` uniformly, because they all leave the param holding the
correct final value. Two small edits in `src/runtime/types/binding.rs`:

1. In the positional-param loop, push plain `@`/`%` positional params (not
   `is copy`/`is rw`/`is raw`, not slurpy/`onearg`/invocant, no sub-signature,
   not an attribute twigil) with a caller lvalue source onto `rw_bindings`, so
   `apply_rw_bindings_to_env` writes the final value back at return.
2. In the readonly-marking loop, exclude `@`/`%` params from `readonly_vars`
   (scalar `$` stays readonly). Raku array/hash params are writable containers:
   `@x = (...)` / `%h = (...)` / `.push` are allowed; only `:=` rebinding is
   forbidden (a separate signature-bound check).

Propagates through every dispatch path (VM, `dispatch.rs`, `calls.rs`,
`class.rs`, closures, …) because they all call `apply_rw_bindings_to_env`.
Tests: `t/param-array-alias.t` (15). `make test` clean; whitelisted S06/S12/S02/
S03/S29/S17 roast samples clean.

**Known limitations (writeback ≠ live cell), both pre-existing, not regressions:**
- `f(@z, @z)` — same caller var bound to two params, mutate one: the
  unmutated param's writeback clobbers the mutated one (last write wins). raku:
  live alias → both see it.
- `return-rw @x` of a param, then mutate the returned alias after the call: the
  writeback already fired at return, so later mutations don't propagate.

Both need true shared-cell aliasing (caller var promoted to a cell at the call
site, in the VM arg-eval path) and are extremely rare; deferred.
