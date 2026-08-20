# A module's file-scope `my @a` / `my %h` is still the caller's variable

**Design: [ADR-0039](../../docs/adr/0039-container-lexicals-resolve-lexically.md)
(Proposed, 2026-08-20).** That ADR owns the decision and the slicing; this file
is now just the open-finding marker. It stays in `todo/deep/` until slice 1
lands, at which point it moves to `news/2026-08/` per the todo lifecycle.

`news/2026-08/module-file-scope-lexical-is-not-the-callers.md` fixed this for
**scalars**: a `unit` compunit's file-scope `my $x` now lives in a shared cell in
`Interpreter::unit_lexicals`, keyed by the unit package. `@`/`%` were
deliberately left out of that store, so for them the bug stands:

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
say peek-items();        # raku: a,b,c    mutsu: x,y,z,c
say @items.join(",");    # raku: x,y,z    mutsu: x,y,z,c
```

Re-verified on `bd34751d3` (2026-08-20), `%h` included.

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

## Where the `@`/`%` exclusion is enforced

- `collect_unit_lexical_names` — `src/runtime/run_modules.rs:983-1014`
  (doc comment "**Scalars only.**"; the filter itself at `:1002-1008`). Its
  doc comment also cites this file under the wrong directory
  (`todo/tickets/`, not `todo/deep/`) — worth correcting when slice 1 lands.
- ADR-0024's mainline capture — `src/vm/vm_register_sub_ops.rs:464-470`.
- `compute_upvalues` — `src/opcode.rs:6124-6130`.
- `box_captured_lexicals` — `src/vm/vm_register_ops.rs:927`.

## Stale: the `roast/integration/99problems-41-to-50.t` instance

The previous version recorded that file aborting at 1 of 9 under
`MUTSU_REAL_TEST=1` because `Test.rakumod`'s `my @vars` collided with the
test's own. It now runs 9/9 clean under `MUTSU_REAL_TEST=1`, although the
collision setup is unchanged — see ADR-0039 §7. It is no longer a usable
measure; ADR-0039 §6's divergence matrix replaces it.

## Repros

`tmp/ufl/` (regenerate from ADR-0039 §1.1/§1.2): `matrix.raku` (15-assertion
`@`/`%` operation matrix through a module), `namedsub-mainline.raku` (the
module-free mainline shadow shape), `repro-sub.raku` (consumer declares the
shadow inside a sub — this one *loses* the module's mutation entirely),
`ourtest.raku` (`our @a`), `alias.raku` (the write-through control).

Pin when fixed: extend `t/module-file-scope-lexical.t` and
`t/lib/UnitFileLexical.rakumod` with the array/hash cases that were written and
then removed when the scalar slice was scoped down (no recoverable git history
— the add-and-scope-down happened inside one squashed commit, `c5bf19e2e`).
