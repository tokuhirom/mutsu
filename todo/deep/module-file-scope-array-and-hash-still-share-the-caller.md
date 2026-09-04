# A module's file-scope `my @a` / `my %h` is still the caller's variable

**Design: [ADR-0039](../../docs/adr/0039-container-lexicals-resolve-lexically.md).
Slice 1 (§4.1) LANDED 2026-08-20** — the module shape, the module-free mainline
shadow shape, and the sub-local-consumer shape from this file's repros are all
fixed and pinned (`t/module-file-scope-lexical.t`,
`t/named-sub-lexical-scope-container.t`). **This file stays open** because
slice 2 (§4.2 — containers resolve by slot/upvalue at the compiler, not by
name, retiring `unit_lexicals`'s container special-casing entirely) is the
ADR's stated architectural end state, not merely a follow-up; the exclusion
list slice 1 carries (`our`, `state`, `is export`, `$*dynamic`, `::`-qualified,
type-constrained, anonymous-container names) is explicitly "the list of things
slice 2 must subsume" (ADR-0039 §4.3). `our @arr` colliding (§1.2's third
instance) was ALSO still broken after slice 1; it is **FIXED as of 2026-08-23**
by a resolution-only change (see "Remaining open scope" item 1). Move this file
to `news/2026-08/` only once slice 2 lands and closes the remaining by-name
container resolution path.

`news/2026-08/module-file-scope-lexical-is-not-the-callers.md` fixed this for
**scalars**: a `unit` compunit's file-scope `my $x` now lives in a shared cell in
`Interpreter::unit_lexicals`, keyed by the unit package. `@`/`%` were
deliberately left out of that store until slice 1 landed:

```raku
# UFL.rakumod
unit module UFL;
my @items = <a b>;
sub peek-items() is export { @items.join(",") }
sub push-item($v) is export { @items.push($v) }
```
```raku
use UFL;
my @items = <x y z>;
push-item("c");
say peek-items();        # raku: a,b,c    mutsu (slice 1): a,b,c  (was: x,y,z,c)
say @items.join(",");    # raku: x,y,z    mutsu (slice 1): x,y,z  (was: x,y,z,c)
```

Verified fixed on the slice-1 branch, 2026-08-20 — see ADR-0039 §6.1 for the
implementation notes (which write chokepoints needed fixing beyond the two
skips) and the full acceptance-criteria verification.

## Two corrections the 2026-08-20 investigation made (read before touching this)

Both of the previous version's load-bearing claims were wrong. The full
argument is in ADR-0039 §1.2 and §2; the summary:

1. **It is not a module bug.** The identical divergence reproduces with no
   module involved, at plain mainline scope, when an ordinary inner block
   declares a same-named `my @a` while a named sub mutates the outer one. The
   byte-identical *scalar* program is correct, because ADR-0024 fixed it. So
   this ticket is exactly the `@`/`%` follow-up that ADR-0024 ("Known
   limitations") and ADR-0025 (slice 3) both named and deferred — module
   loading is merely the shape that makes the collision most likely. `our @a`
   in a module collides too, for the same reason.

2. **The "~120+ call sites" sizing rested on an obsolete premise.** The
   previous version argued that container mutation goes through `Gc::make_mut`
   (copy-on-write), so a sound fix would need a new
   write-through-the-canonical-slot primitive at every site. mutsu does not do
   that: `Value::with_array_inplace` (`src/value/view.rs:769-789`) writes
   through the **shared** node via ADR-0013's `gc_contents_mut`, and its doc
   comment states outright that `Gc::make_mut` is *wrong* here (Raku `=` copy
   semantics are enforced at copy time by `detach_shared_container` instead).
   Verified empirically: two distinct `env` keys holding one container observe
   each other's `push` / element-assign / key-set. **In-place mutation needs no
   write handle — it only needs to read the right container.** The previous
   version's dismissal of ADR-0013 as "orthogonal" was the mis-attribution, not
   the citation it replaced.

What survives is the *routing* diagnosis, which ADR-0039 §4.1 scopes: the
residual hazard is the small set of sites that, on an `env` miss, **build a
fresh container and insert it** under the bare name — `push_to_shared_var`'s
tail (`src/runtime/runtime_thread.rs:929-957`) being the sharpest — plus
whole-container reassignment, which must preserve container identity through
the cell.

## What slice 1 changed, and what slice 2 still owns

- `collect_unit_lexical_names` (`src/runtime/run_modules.rs`) and ADR-0024's
  mainline capture (`src/vm/vm_register_sub_ops.rs`) no longer skip `@`/`%` —
  see ADR-0039 §4.1/§6.1 for the full list of write-side chokepoints
  (`env_root_descended_mut`, `push_to_shared_var`, `exec_delete_index_named_op`)
  that had to learn to consult `unit_lexicals` first.
- **`compute_upvalues`** (`src/opcode.rs:6124-6130`, still excludes
  `@`/`%`/`&`) and **`is_plain_lexical_name`**'s `@%&` exclusion
  (`compiler/mod.rs:1712-1721`) are UNCHANGED by slice 1 — a container
  free variable still resolves by NAME at the compiler, not by slot/upvalue.
  This is exactly slice 1's scope boundary: it makes a compunit's OWN
  file-scope containers safe by giving them a name-keyed cell store that
  wins over `env`, but does not make container scoping lexical in general
  (an ordinary inner block's `@a` is still name-resolved). Closing that
  asymmetry — deleting `unit_lexicals`'s container special-casing along with
  it — is slice 2 (ADR-0039 §4.2), not this file's concern any more once
  slice 2 lands.
- `box_captured_lexicals` (`src/vm/vm_register_ops.rs:927`) is the scalar-only
  box-on-capture mechanism (lever C slice 2, unrelated numbering to this
  ADR's slice 2) — still scalar-only, not touched by ADR-0039 slice 1.

## Stale: the `roast/integration/99problems-41-to-50.t` instance

The previous version recorded that file aborting at 1 of 9 under
`MUTSU_REAL_TEST=1` because `Test.rakumod`'s `my @vars` collided with the
test's own. It now runs 9/9 clean under `MUTSU_REAL_TEST=1`, although the
collision setup is unchanged — see ADR-0039 §7. It is no longer a usable
measure; ADR-0039 §6's divergence matrix replaces it.

## Repros

`tmp/ufl/` (regenerate from ADR-0039 §1.1/§1.2, or from ADR-0039 §6.1's fixed
examples): `matrix.raku` (15-assertion `@`/`%` operation matrix through a
module — now pinned as `t/module-file-scope-lexical.t`'s container half),
`namedsub-mainline.raku` (the module-free mainline shadow shape — now pinned
as `t/named-sub-lexical-scope-container.t`), `repro-sub.raku` (consumer
declares the shadow inside a sub — now fixed too), `ourtest.raku` (`our @a` —
fixed 2026-08-23, now pinned as `t/our-container-bare-name-resolution.t`; only
its trailing `our $s` scalar line still diverges, tracked separately),
`alias.raku` (the write-through control, unaffected by this ADR either way).

## Remaining open scope (why this file is not fully closed)

1. ~~**`our @arr` colliding**~~ — **FIXED 2026-08-23**, see
   `news/2026-08/our-container-bare-name-prefers-package-mirror.md`. The
   resolution fix landed as `src/vm/vm_our_package_vars.rs`: a bare `@`/`%`
   name is resolved to the package-qualified `our` mirror of the package the
   running routine belongs to, wired into the read chokepoint
   (`get_env_with_main_alias`), the container-mutation chokepoint
   (`env_root_descended_mut`), and `:delete`'s own by-name dance
   (`exec_delete_index_named_op`). A name the running frame declares as its
   own local is never redirected, so lexical shadowing inside the module
   still wins. Pinned by `t/our-container-bare-name-resolution.t` (28
   assertions) with fixture `t/lib/UnitOurContainer.rakumod`. No store change
   was needed, exactly as ADR-0039 §4.1 predicted.

   The **scalar** twin (`our $x` written from a module routine landing on the
   caller's `my $x`) is a different mechanism — a scalar has no shared node,
   so it needs `SetGlobal`'s bare env store suppressed rather than resolution
   redirected — and is tracked separately in
   `todo/tickets/our-scalar-write-leaks-to-the-callers-lexical.md`.
2. **Slice 2** (ADR-0039 §4.2): container free variables in ordinary inner
   blocks, closures, and any non-compunit-file-scope declaration still
   resolve by name at the compiler (`Expr::ArrayVar`/`Expr::HashVar` emit
   `GetArrayVar`/`GetHashVar` unconditionally, never `GetLocal(slot)`). Slice
   1 is a name-keyed cell store that shadows `env` for exactly the compunit
   file-scope case; slice 2 is the architectural fix that makes container
   scoping lexical everywhere, the way scalar scoping already is, and lets
   the container special cases slice 1 introduced be deleted rather than
   extended further.

## Re-verified 2026-09-01 (TRIAGE regeneration): two slice-2 acceptance rows

The module shape and the mainline named-sub shadow shape agree with raku
(slice 1 holds). Two non-file-scope shapes still resolve the container by
name, and are usable as slice 2's acceptance rows (raku answers first):

```raku
# (a) a sub-local @a captured by an inner anonymous sub, shadowed by an inner block's @a
sub f { my @a = 1, 2; my $push = sub { @a.push(9) }; { my @a = 3; $push(); say "inner=", @a }; @a }
say f();      # raku: inner=[3] / [1 2 9]    mutsu: inner=[3 9] / [1 2]  -- the push lands on the shadow

# (b) a closure declaring @c that shadows a mainline @c and calls a named sub
my @c = 1; sub g { @c.push(7) }
my $h = { my @c; g(); @c }; $h();
say @c;       # raku [1 7]     mutsu []     -- the mainline @c is emptied
```

(Both measured 2026-09-01 on `target/debug/mutsu` vs raku v2026.06.)

## Slice 2's read side measured, and withdrawn (2026-09-04) — see ADR-0039 §9

Both acceptance rows above still reproduce. §4.2's first bullet (container reads
emit `GetLocal(slot)`) was implemented and measured: **7 `t/` files, 12
assertions**, and row (b) is fixed by it alone. Every one of the 12 is a *store*
site that leaves the slot and `env` naming different containers — an
expression-position declaration that allocates no slot (and a monotonic
`local_map` that still holds a popped sibling's slot), or a genuine same-named
shadow producing two `code.locals` entries with one name for an env-centric
`IndexAssignExprNamed` to resolve between.

So the first bullet is not independently landable, and the blocker is §1.3 of
`docs/lexical-scope-slot-campaign.md`, not anything internal to this ADR. ADR-0039
§9 records the enumeration, the withdrawn diff's shape, and the inverted order
slice 2 should now take (store sites first, read flip last). Resource slice 2
with §1.3, alongside
`todo/tickets/same-named-loop-params-in-one-unit-interfere.md`, which reached the
same conclusion from the scalar side.
