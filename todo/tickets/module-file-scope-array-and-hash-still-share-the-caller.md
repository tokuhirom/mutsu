# A module's file-scope `my @a` / `my %h` is still the caller's variable

`news/2026-08/module-file-scope-lexical-is-not-the-callers.md` fixed this for
**scalars**: a `unit` compunit's file-scope `my $x` now lives in a shared cell in
`Interpreter::unit_lexicals`, keyed by the unit package, and the module's own
routines read and write it there instead of under the plain `env` key the loading
scope uses. `@`/`%` file-scope lexicals were deliberately left out of that store,
so for them the original bug stands:

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
say peek-items();        # raku: a,b,c
say @items.join(",");    # raku: x,y,z
```

**Re-verified 2026-08-14 on current `main` (514cddc24):** both lines now print
`x,y,z,c` — the module's own `peek-items()` no longer even sees `a,b` (as the
2026-08-05 write-up recorded), it reads the *caller's* array outright. The exact
symptom drifted (whatever previously made `push-item` land on the caller's copy
without also making `peek-items` read it now makes both routines converge on the
same, wrong, storage cell), but the root cause and its fix are unchanged from
below. This is a genuine name collision, not a roast artifact: `raku` passes the
repro as written.

## Correction (2026-08-14): the ADR-0001/ADR-0013 dependency this ticket cited was wrong

The previous version of this ticket said the fix was "cheapest after the
`arc_contents_mut` chokepoint consolidation ADR-0001 fuses with GC," and
CLAUDE.md's GC/JIT status section now says that fusion rule was superseded by
[ADR-0013](../../docs/adr/0013-container-interior-mutability-cellvalue.md) §7 —
so the natural question is whether this ticket is now unblocked. **It was never
actually blocked by that campaign; the citation was a mis-attribution.**

ADR-0013 §7/§8 is about whether taking `&mut` through a shared `Gc<T>` pointer is
*sound* (provenance UB under Miri) — it moved the `UnsafeCell` into `GcBox` so
`gc_contents_mut` derives a valid mutable pointer at **every** existing call site,
unchanged, with no Value-layer rewrite. That is orthogonal to this ticket's
problem, which is **routing**: which `env` (or store) entry a mutating method's
receiver, or an element-assignment opcode, resolves `target_var`/`var_name`
*from*, when two different scopes want the same plain name. Making the
existing `gc_contents_mut` sites sound does nothing to consolidate how many of
them there are or how they pick their target. `todo/deep/element-itemization-lost-in-scalar-binding.md`
(store-side Scalar-container wrapping of array/hash *elements*, a genuinely
different problem) is the ticket ADR-0013 §7 actually unblocked; this one was
never coupled to GC/Miri soundness at all.

So: **the premise needs correcting, not the code.** The real blocker — call-site
fragmentation — is exactly as large today as when this ticket was first written,
verified by re-reading the current sources (see inventory below) and confirming
no consolidation landed in between (`git log` on the affected files/functions
shows only unrelated changes: ADR-0019's E1-E7 method-*dispatch* resolver slices,
which pick fast-path vs. native vs. generic-fork by receiver `TypeId` — a
different axis from by-name *storage* resolution, and untouched by this ticket's
problem).

## Why the scalar fix does not extend to them (current inventory, 2026-08-14)

A scalar is read and written through two by-name chokepoints,
`get_env_with_main_alias` / `set_env_with_main_alias_sym`
(`src/vm/vm_env_helpers.rs`), which check `unit_lexicals` first and fall back to
`env`. **These two chokepoints are already sigil-agnostic** —
`get_env_with_main_alias_inner` has explicit `@`/`%`-prefixed branches (the
`__mutsu_atomic_arr::`/`__mutsu_atomic_hash::` thread-shared lookups), and
`GetArrayVar`/`GetHashVar` (`src/vm/vm_exec_dispatch.rs:607`, `:718`ish) already
call `get_env_with_main_alias(name)` as their first candidate. **The read side
would work today if `collect_unit_lexical_names` simply stopped skipping `@`/`%`
names** — that filter is the only thing keeping a container out of the
`unit_lexicals` cell in the first place.

The problem is entirely on the **write-back** side, and it is structurally
different from the scalar case, not just bigger. A scalar write is a value
replace (`set_env_with_main_alias_sym` swaps what the key denotes). A container
"write" is usually an **in-place mutation through a `Gc` pointer** obtained from
a *prior* read — `push`/`pop`/`splice`/element-assign call `Gc::make_mut` on the
array's `Gc<ArrayData>`, which silently **reallocates** (breaks aliasing with any
other holder) whenever the strong count is above 1, which it always is here
(the read that produced `target` and whatever `env`/cell copy is left behind are
two holders). After that reallocation, the *only* way the mutation becomes
visible to a later read is if the mutator writes the new `Gc` back into
whatever storage location later reads will consult — so unlike scalars, a
sound fix needs a **write-through-the-canonical-slot** primitive for
containers, not just a value get/set pair, and every one of the following
sites has to use it instead of touching `env` directly:

- **`call_method_mut_with_values`** (`src/runtime/methods_mut_dispatch.rs`,
  2808 lines): **53** direct `self.env.get`/`get_mut`/`insert(target_var, ...)`
  call sites. Examples: the array push/append/pop/shift/unshift/splice/prepend
  block (~line 680-950) reads `self.env.get(&key)` for type-constraint checks
  and calls `self.push_to_shared_var(&key, ...)` for `push`, but `append`
  (~line 762-779) does its own `self.env.get_mut(&key)` / falls back to
  `self.env.insert(key.clone(), Value::real_array(items))` directly — two
  different write-back shapes for two sibling methods in the same match arm.
  The hash push/append block (~line 1332+) reads `self.env.get(&key)` for
  key/value type constraints before mutating. `self.env.insert(target_var...)`
  recurs at (at least) lines 341, 378-379, 498, 597 for other mutating
  operations.
- **`push_to_shared_var`** (`src/runtime/runtime_thread.rs:834`, the array-push
  fast path most of the above delegates to): **7** direct `self.env`
  accesses. Its final fallback (~line 927-952) explicitly does
  `self.env.get(key)` / `self.env.get_mut(key).unwrap().with_array_mut(...)` —
  reads and mutates *whatever `env[key]` currently holds*, which after a
  container is moved out of `env` (the scalar-fix pattern) would either find
  nothing (silently dropping the push into a stray `env.insert` nobody reads
  back, since `unit_lexicals` is checked *before* `env`) or find the wrong
  array (a same-named consumer variable) if one exists — the exact "worse than
  the original bug" failure mode this ticket has always flagged.
- **The `env_mut().get_mut(name)` / `env_mut().insert(name, ...)` sites in
  `src/vm/vm_var_*.rs`** — element assignment and autovivification opcode
  handlers, i.e. `@a[0] = x`, `%h{k} = v`, `delete @a[0]`, `@a[0]++`, and
  friends. Current counts by file: `vm_var_assign_index_named.rs` **17**
  (e.g. lines 1466/1488/1510/1609 `self.env_mut().get_mut(&var_name)`, lines
  1571/1884/2041/2085/2424/2892/3287 `self.env_mut().insert(var_name, ...)`,
  and two raw-pointer aliases at 3161/3293 —
  `self.env_mut().get_mut(var_name)? as *mut Value` — used for in-place
  multi-step autoviv, which is its own hazard to redirect safely),
  `vm_var_index_tracking.rs` **15**, `vm_var_delete_ops.rs` **10**,
  `vm_var_assign_set_local.rs` **6**, plus smaller counts (1-5 each) across
  `vm_var_assign_element.rs`, `vm_var_assign_local.rs`,
  `vm_var_assign_local_get.rs`, `vm_var_assign_ops.rs`,
  `vm_var_assign_post_incdec.rs`, `vm_var_multidim_ops.rs`,
  `vm_var_assign_coerce.rs`, `vm_var_assign_computed_attr.rs`. These are hot,
  correctness-sensitive opcode arms (every indexed assignment in the
  interpreter goes through some of them), not a contained corner.

That is on the order of **120+ call sites** across at least a dozen files, each
of which would need to change from "read/write `env[key]` directly" to
"resolve through whatever canonical slot owns `key` right now" — and a wrong
redirect at any one of them reproduces the exact failure this ticket exists to
fix, just relocated (mutating the wrong array, or a mutation that silently
never becomes visible). Threading that consolidation through by hand, file by
file, is real, substantial work with a correctness-landmine failure mode that
`make roast` will not reliably catch (a stray env write that nothing reads
back does not throw — the test would just see a stale value, indistinguishable
from "the feature was simply never implemented").

**Not the same thing:** `module_scope_lexicals` (`src/runtime/mod.rs`,
distinct from `unit_lexicals`) already stores a module's bare `@`/`%` names as a
read-only last-resort snapshot, consulted only *after* `env` misses (see
`GetArrayVar`/`GetHashVar` in `vm_exec_dispatch.rs`). It exists to keep a
module's `constant`/sigilless names reachable after the loading frame that
declared them is gone (`NativeHelpers::Blob`'s `MoarVM::Guts::REPRs` case), not
to solve this collision — being last-resort and read-only, it never even
triggers when a consumer declares the same name (which is exactly this bug's
precondition), and it would go stale on the first mutation regardless.

## What it would take

A single canonical-slot abstraction that every one of the ~120+ sites above
calls instead of touching `env` directly — something like a
`get_container_slot_mut(name) -> &mut Value` that checks `unit_lexicals` first
(promoting/writing through the cell) and falls back to `env.get_mut`, mirroring
`get_env_with_main_alias`/`set_env_with_main_alias_sym` but returning a mutable
handle instead of a value, since containers are mutated via `Gc::make_mut`
write-back rather than value replacement. Then: extend
`collect_unit_lexical_names` to include `@`/`%` declarations, and migrate each
call site above off raw `env` access. This is a bold, high-blast-radius,
CLAUDE.md-"refactor boldly"-shaped change in principle, but the specific risk
(silently wrong receiver identity) is not one `make test`/`make roast` reliably
surfaces as a red build — it surfaces as a quietly-wrong value, which is why
this has been deferred past a single session twice now (2026-08-05 and
2026-08-14) rather than forced through piecemeal.

Note the exposure is narrower than the scalar case was: it needs a module with a
file-scope `my @a`/`my %h` **and** a consumer that declares the same name.
`Test.rakumod`'s `@vars` is the one that matters for
`todo/deep/vendor-real-test-module.md`; the other eight of its file-scope
lexicals are scalars and are fixed. As of 2026-08-14, `vendor-real-test-module.md`
is still unmerged (mutsu's default `Test` provider stays native/`test_functions.rs`),
so nothing whitelisted depends on this fix today — it only bites the moment
that vendoring cutover lands.

## A measured instance: `roast/integration/99problems-41-to-50.t` (2026-08-05)

That predicted collision is real and it costs a whole roast file — **but only
under the experimental real-`Test` module** (`MUTSU_REAL_TEST=1`); the file is
in `roast-whitelist.txt` today and passes under mutsu's default native `Test`
provider, since that provider has no `@vars` lexical to collide with. Under
`MUTSU_REAL_TEST=1` the file aborts after 1 of its 9 assertions with
`unknown variable: A`, raised from the test's own grammar action:

```raku
method truth-table($expr, $actions) {
    my @vars = @( $/.ast<vars> );          # <-- same name as Test.rakumod's
    sub the-truth(@vals) {
        our %*VAR = @vars Z=> @vals;       # built from the WRONG @vars
        ...
    }
}
```

`%*VAR` therefore comes out empty and the `term:sym<var>` closure's
`%*VAR{$id} // die "unknown variable: $id"` fires. Renaming the *test's*
`@vars` to `@varz` makes the file pass, which is the confirmation — the file is
otherwise unmodified and `raku` passes it as written.

Bisecting `Test.rakumod` converges on `sub _push_vars` (`@vars.push: item [...]`),
i.e. the declaration alone is not enough; it takes a routine that mutates the
array by name. Two notes on doing that bisect, since the obvious method does not
work:

- **Do not truncate `Test.rakumod` at a line number.** File scope calls
  `_init_vars()` at line 41 and that routine is declared at line 867, so every
  prefix cut either fails to parse or dies on `Unknown function: _init_vars`
  long before it changes behaviour.
- Split the file into brace-balanced top-level chunks (248 of them) and always
  keep chunks 0-47 plus the `_init_vars` chunk (220). Then a keep-range or
  drop-range bisect converges in about six runs.

So the file is blocked on this ticket, not on anything in `Test`.

Pin when fixed: extend `t/module-file-scope-lexical.t` (and
`t/lib/UnitFileLexical.rakumod`) with the array/hash cases that were written and
then removed when the slice was scoped to scalars (there is no recoverable git
history for that removed content — the add-and-scope-down happened inside one
squashed commit, `c5bf19e2e` — so it needs writing fresh from this ticket's
repro).
