# ADR-0057: `.VAR` reflection identity is the shared cell's address, not a per-frame cache

- Status: Accepted (implemented)
- Date: 2026-08-20
- Related: ADR-0025 (captured scalar cells, value-kind-blind), ADR-0032
  (`WrapVarRef` container capture across closure boundaries)
- Addresses: `todo/tickets/var-which-identity-across-closure-boundary.md`

## Context

`.VAR` on a scalar variable (`$v.VAR`) returns a reflection `Instance` (class
`Scalar`) representing the variable's container. Raku guarantees this object
has one stable identity for the life of the variable:

```raku
{ my $v = 1; my $mk = -> { $v.VAR.WHICH }; say $mk() eq $v.VAR.WHICH }  # True
```

mutsu printed `False`. The same shape failed for the older named-sub capture
mechanism too (`sub f() { $v.VAR.WHICH }`), proving the bug predates and is
independent of ADR-0032.

### Root cause

`.VAR` (`compile_expr_method_on_var` in `src/compiler/expr_method.rs`,
dispatched at runtime by the `method == "VAR"` branch of
`call_method_mut_with_values` in `src/runtime/methods_mut_dispatch.rs`) builds
a fresh reflection `Instance` the first time it is called for a given
variable name in a frame, then caches it via `set_var_meta_value` — a raw
`self.env.insert` under the synthetic key `__mutsu_var_meta::<name>`
(`src/runtime/runtime_var_meta.rs`). A second `.VAR` call for the same name
in the SAME frame hits the cache and reuses the same `Instance`, so its
`.WHICH` (keyed off the instance's own `id` field, format
`"{class_name}|{id}"` — see the `WHICH` dispatch in
`src/builtins/methods_0arg/dispatch_core_coerce.rs`) compares equal. But the
cache key is synthetic (`__mutsu_var_meta::v`, never a name that appears in
source text), so the compiler's free-variable analysis
(`free_var_writes`/`compute_free_vars`, the only mechanism that drives
closure env write-back) never sees it. A `.VAR` call made inside a closure
writes its cache entry into the CLOSURE's own env; that write never
propagates back to the declaring frame or to a sibling closure, so a second,
independent `.VAR` call anywhere else builds its own `Instance` with a fresh
monotonic `id` from `next_instance_id()`.

`.VAR`'s target itself carries no identity to fall back on: `compile_expr`
for a plain `Expr::Var` emits `GetGlobal`/`GetUpvalue`, both of which
INTENTIONALLY dereference a captured `ContainerRef` cell for an ordinary
value read (documented behavior) — so even when the variable happens to be
boxed, `.VAR`'s dispatch only ever sees the plain dereferenced value, never
the cell.

### Two candidate fixes considered in the originating ticket, and why neither is taken as designed

1. **Derive identity from the `ContainerRef` cell's own address whenever the
   variable is boxed.** Correct in principle, but most variables are never
   boxed at all — boxing is deliberately rare and syntactically triggered
   (ADR-0025's `#2749` perf gate: broad closure boxing took
   `roast/S32-num/int.t` from ~1s to 150s+). Taken alone this only covers the
   subset of variables some OTHER mechanism happened to box for an unrelated
   reason — which does not include a plain read-only capture like the
   ticket's own repro (`$v` is read, never written, inside the closure; per
   ADR-0025 only captured-AND-MUTATED locals get decl-site boxed today).
2. **Make the `var_meta_value` cache write for a captured/free variable reach
   the DECLARING frame's env at runtime**, mirroring `free_var_writes`'s
   write-back machinery. Rejected: this is exactly the shape of ADR-0032 §3
   alternative 2 ("runtime name search of the creating frame from
   `capture_var_cell_inner`"), which that ADR rejected because a by-name
   cross-frame guess at RUNTIME can pick up a same-named SHADOW slot instead
   of the binding actually in scope — the precise failure `slot: u32::MAX`
   was introduced to prevent (`t/list-alias-shadowed-name.t`). The compiler
   already knows, at compile time, whether a name is free relative to the
   emitting frame; re-deriving that answer by guessing at runtime reintroduces
   a bug class ADR-0032 spent real effort eliminating.

## Decision

Neither candidate alone is sufficient, but **composing them** — reusing
ADR-0032's already-shipped, already-perf-gated machinery to make direction 1
apply to exactly the cases direction 2 was trying to reach — closes the gap
with no new cross-frame runtime mechanism at all:

### D1 (compile time): treat a free `.VAR` target as a container-capture edge

`compile_expr_method_on_var` now calls a new
`Compiler::register_container_ref_capture_if_free` helper — factored out of
`emit_wrap_var_ref`'s existing ADR-0032 D1 registration, byte-identical logic
— whenever `.VAR` is called with no args/modifier on a bare `Expr::Var`
target whose name does not resolve to a local of the emitting frame (the same
`slot == u32::MAX` condition `emit_wrap_var_ref` already uses, restricted to
`is_plain_lexical_name` — plain `$`-sigil lexicals only, matching ADR-0032
D1's own restriction).

This registers the variable's name into the reading `CompiledCode`'s
`container_ref_capture_syms`, which ADR-0032's D2 (`bubble_container_ref_capture_syms`,
already wired into every nested-code attachment site: pointy blocks, anon
subs, bare blocks, named subs, class/role methods, `start`/`supply` bodies)
already bubbles to whichever ancestor frame owns the name, requesting
decl-site boxing there (`needs_cell_ref_capture_slots` → `box_decl_local_cell`
at `exec_set_local_op`). No new compiler mechanism is added — `.VAR` simply
becomes one more consumer of an existing, validated edge, exactly the same
way `key => $v` and `Pair.new($k, $v)` already are.

The effect: by the time ANY closure/named-sub/method that calls `.VAR` on a
captured name actually runs, that variable is GUARANTEED to be a shared
`ContainerRef` cell — narrowly, only for names actually read via `.VAR`
across a frame boundary, not broadly for every capture (`.VAR` is a rare,
deliberate reflection call, so this trigger fires far less often than
ADR-0032's own `key => $v`/`Pair.new` triggers already do, and inherits their
perf gates for free — see "Verification" below).

### D2 (runtime): derive the reflection Instance's `id` from the cell's address

In the `VAR` dispatch (`call_method_mut_with_values`), when building a fresh
meta `Instance` (cache miss), peek the RAW env entry for `target_var`
(`self.env.get(target_var)`, bypassing whatever dereferenced value
`compile_expr(target)` produced). If it is currently a
`ValueView::ContainerRef(cell)`, build the Instance with
`Value::make_instance_with_id(class_sym, attributes, id)` where
`id = Gc::as_ptr(&cell) as usize as u64` — the cell's own stable heap address
— instead of `Value::make_instance`'s process-global monotonic counter.
Otherwise (the overwhelmingly common, non-`.VAR`-captured case), behavior is
byte-identical to before: `Value::make_instance` with the monotonic counter,
same per-frame `var_meta_value` cache.

Since `.WHICH` is purely `"{class_name}|{id}"`, two DIFFERENT `Instance`
objects built in two different frames — the declaring frame's own `.VAR`
call and a nested closure's `.VAR` call, or two sibling closures' calls —
independently compute the SAME `id` whenever they resolve the SAME shared
cell, because a `Gc` pointer is stable for the cell's lifetime (mutsu's
Bacon-Rajan collector is non-moving; see ADR-0001 §7 and ADR-0013). No
cross-frame cache write-back of any kind is needed: identity falls out of
already-shared structure (the cell IS the same object in every frame that
holds it, by construction) rather than a synthetic side-channel trying to
publish an already-built object to frames that cannot see it.

`Gc::as_ptr` returning a `u64` identity is an existing idiom in this codebase
(`src/runtime/builtins_multidim_subscript.rs`'s `temp_id`,
`src/runtime/attr_build_defaults.rs`'s `cell_addr`, and the `Array|{:p}` /
`Hash|{:p}` / `Regex|{:p}` pointer-formatted `WHICH` arms already in
`dispatch_core_coerce.rs`), not a new pattern introduced by this ADR.

### Why this narrower composition works where the two standalone directions did not

- It is not "boxing on first `.VAR` call" in the blunt sense (force-box
  unconditionally whenever `.VAR` executes) — that would still need a
  cross-frame answer for the DECLARING frame's slot from inside the callee,
  which is exactly the by-name guess ADR-0032 rejected. Registering the
  capture edge at COMPILE TIME, before any code runs, sidesteps the guess
  entirely: the compiler already resolved, unambiguously and per ADR-0032's
  shadow-safety invariant, which declaration a free `.VAR` target refers to.
- It is not "derive identity from the cell only when already boxed for some
  unrelated reason" (candidate 1 alone) — the read-only capture in the
  ticket's own repro would never be boxed by any pre-existing trigger.
  Registering `.VAR` itself as a trigger is what makes the boxing happen
  precisely when — and only when — cross-frame identity is actually needed.

## Verification

- Both repros from the ticket (closure and named-sub) now print `True`,
  matching `raku`.
- Additional shapes verified against `raku`: three nested closure levels all
  reading `.VAR.WHICH` on the same outer variable agree; two independently
  captured variables never collide.
- `t/closure-container-capture-alias.t` probe X (previously `todo`, citing
  this ticket) now passes unconditionally; all 19 tests in the file pass.
- `t/captured-outer-pair-container-alias.t`, `t/closure-capture-instance-cell.t`,
  `t/for-loop-param-start-sibling-isolation.t`, `t/named-sub-lexical-scope.t`,
  `t/lock-protect-shared-scalar.t`, `t/expr-decl-lexical-no-leak.t`,
  `t/hash-attr-map-default-element-assign.t`, `t/list-alias-shadowed-name.t`,
  `t/pair-new-container-alias.t`, `t/captured-outer-cell-sharing.t`,
  `t/captured-outer-container-cell-sharing.t`, `t/varref-binding.t`,
  `t/which-method.t`, `t/var-name*.t` — the full set of ADR-0025/ADR-0032
  canaries this change's blast radius could plausibly touch — all pass.
- Perf gate: `roast/S32-num/int.t` on a release build runs in 0.043s wall
  clock (165/165 tests), no trace of the ADR-0025 `#2749` blowup this gate
  exists to catch — expected, since `.VAR` calls do not appear in that file
  at all, and the new boxing trigger only fires for a `.VAR` call on a free
  variable, a shape that is rare by construction.
- `make test` run: no regressions attributable to this change (one
  pre-existing, environment-specific failure in
  `t/compunit-can-install.t` — a filesystem-permission probe that assumes
  `/` is not writable by the current user, which does not hold in this
  container — unrelated to closures/`.VAR`/reflection).

## Alternatives rejected

See "Two candidate fixes considered" above for why neither direction 1 nor
direction 2 alone was taken. A third alternative — force-boxing a variable's
container unconditionally on every first `.VAR` call, regardless of whether
the target is free relative to the emitting frame — was also considered and
rejected: it would box even a purely local, non-escaping `.VAR` use (the
overwhelming majority of `.VAR` calls, e.g. reflection on a sub-local
variable never touched by any closure), paying an unnecessary cell
allocation on a hot reflection path for no identity benefit, since a
same-frame `.VAR` call already gets correct identity from the existing
per-frame cache with zero boxing. Gating registration on "free relative to
the emitting frame" (the same gate `emit_wrap_var_ref` already uses) keeps
the trigger exactly as narrow as it needs to be.
