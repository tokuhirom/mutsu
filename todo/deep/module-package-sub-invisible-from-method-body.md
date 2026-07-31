# A module's package-scoped sub is invisible from a method body

mutsu resolves an unqualified routine name by walking **packages**
(`resolve_function` → `bare_name_packages`: the current package, then each
enclosing one, then `GLOBAL`). Raku resolves it **lexically**: a `sub` declared
anywhere in a compunit is visible to every block in that compunit, including the
bodies of classes declared in it — whatever package those classes live in.

Where the two models agree, nothing is wrong. Where they diverge, a module's own
routine simply cannot be called from a method of a class that is not nested in
the module's namespace, which is an entirely ordinary thing to write.

## Minimal repro

```raku
# lib/Probe.rakumod
unit module Probe;
our sub helper($x) { $x * 2 }
class GLOBAL::T {
    has $.v = 5;
    method doubled() { helper($!v) }        # dies: Unknown function: helper
}
T.^add_method('added', method () { helper($.v) });   # dies the same way
```

```raku
use Probe;
say T.new.doubled;          # Unknown function: helper
say Probe::helper(5);       # 10  -- the routine is registered, just unreachable
say T.new.doubled-if-you-qualify;   # `Probe::helper($!v)` in the body works
```

The routine is present; only the bare-name lookup misses it. Three shapes were
measured:

| call site | result |
| --- | --- |
| another `sub` in the module | works |
| a block/closure the module exports (`our $blk = -> $x { helper($x) }`) | works |
| a **method body** of a class the module declares | **fails** |
| the same body written `Probe::helper(...)` | works |

The class does not have to be `GLOBAL::`-rooted for this to bite; anything whose
package is not under the module's is affected. `NativeHelpers::Pointer` is the
real-world case: it `^add_method`s pointer arithmetic onto
`NativeCall::Types::Pointer`, so the added method runs under `Pointer` and cannot
see `NativeHelpers::Pointer`'s own routines.

## Why it has not been noticed more

Because most of what module method bodies call is either a builtin (always
resolvable, whatever the package) or an `is export` routine (which the importer
then installs globally, so the `GLOBAL` step of the walk finds it). Only a
package-scoped, non-exported routine falls in the hole — and `is export` is the
workaround every affected place has reached for so far.

mutsu's own NativeCall prelude used it and was bitten by exactly the predicted
side effect: re-exporting the routine to whoever `use`s the module, which Raku
would not. That broke every DBIish SQLCipher file with an `X::Redeclaration`
(the re-exported copy collided with the importer's own spliced copy) and has
since been replaced by a narrower mechanism — the `__mutsu_prelude` marker
trait, which registers the routine under `GLOBAL` directly instead of routing
global visibility through an export
([`news/2026-08/nativecall-helpers-are-not-reexported.md`](../../news/2026-08/nativecall-helpers-are-not-reexported.md)).
That marker is available only to preludes mutsu splices in itself, so an
ordinary module still has `is export` (or an explicit `Module::name(...)`) as
its only spelling, and a prelude-injected helper is still visible process-wide
once any compunit loads it — both of which this ticket's fix removes.

## Why this is large

The fix is not a new entry in `bare_name_packages`: the missing scope is not a
*package* at all, it is the **compunit's lexical scope**. Doing it properly means
a routine carrying the lexical scope it was compiled in, and the bare-name
resolver consulting that scope before falling back to packages. `CompiledFunction`
already carries a `package` (`vm_call_eligibility::enter_routine_package`), so
the plumbing point exists, but:

- the resolution order has to stay compatible with the several package-based
  mechanisms layered on top of it (`resolve_hidden_owned_export`'s tagged-export
  un-hiding, grammar action dispatch switching packages, the `Pkg::&name/2`
  mangled state scopes `bare_name_packages` already special-cases);
- a bare name that currently resolves to a *different* routine of the same name
  in an enclosing package would start resolving to the compunit's one, which is
  correct but is a behaviour change with roast-wide blast radius;
- it overlaps the lexical-scope-slot work: the same "block-scoped `my` lives in
  `env`, routine-scoped `my` lives in a slot" split is what makes "the lexical
  scope a routine was compiled in" not a thing mutsu can currently name.

So this belongs with the lexical-scope campaign rather than being bolted onto a
feature PR. Until then, `is export` (or an explicit `Module::name(...)`) is the
available spelling.

## Affected files

- `src/runtime/resolution.rs` — `resolve_function`, the bare-name walk
- `src/runtime/accessors_stack.rs` — `bare_name_packages`
- `src/vm/vm_call_eligibility.rs` — `enter_routine_package` / `leave_routine_package`
