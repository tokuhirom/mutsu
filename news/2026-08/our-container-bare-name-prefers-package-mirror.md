# A module's `our @arr` / `our %h` is no longer the caller's container

`todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md` item 1 —
the `our` instance of ADR-0039 §1.2, deliberately left unfixed by slice 1 — is
closed. A module that declares a package-scoped container and mutates it from
its own routines now mutates *its own* container, even when the loading script
happens to declare a same-named `my`:

```raku
# UnitOurContainer.rakumod
unit module UnitOurContainer;
our @arr = <a b>;
sub arr-push($v) is export { @arr.push($v) }
sub arr-read()   is export { @arr.join(",") }
```
```raku
use UnitOurContainer;
my @arr = <x y z>;
arr-push("c");
say arr-read();       # raku: a,b,c    was: x,y,z,c    now: a,b,c
say @arr.join(",");   # raku: x,y,z    was: x,y,z,c    now: x,y,z
```

## Root cause

`our @arr` inside `unit module UFL3` publishes its container under the
package-qualified mirror `@UFL3::arr`: the declaration compiles to
`Dup; SetLocalDecl(slot); SetGlobal("@UFL3::arr")`, and the `SetGlobal` arm
mirrors every package-qualified write into `our_vars`. That store was always
correct — `@UFL3::arr` read back the right value throughout the bug's lifetime.

The module's own routines, however, reference the container by its **bare**
name. A sub body compiles in a fresh `Compiler` whose `current_package` has
been overwritten with the mangled state-scope name `UFL3::&arr-push/1`
(`compiler/helpers_sub_body.rs`), and `qualify_variable_name`
(`compiler/mod.rs`) returns any name verbatim when the package contains `::&`
— a guard that exists so `state`-variable scope names never leak into variable
qualification. So `Expr::ArrayVar` emitted a bare `GetArrayVar("@arr")`, which
resolved against `env`, whose `@arr` key belongs to whatever scope loaded the
module. Every read and every mutation the module made went to the consumer's
array while the correct container sat in the mirror, unconsulted.

This was a **resolution** bug against an already-correct store, which is why
ADR-0039 §4.1 excluded `our` from slice 1's `unit_lexicals` work: `our`
variables are externally visible (`@UFL3::arr` must stay readable and writable
from outside the module), so the qualified mirror — not a private per-compunit
cell — is their canonical store.

## The fix

New `src/vm/vm_our_package_vars.rs` reconstructs the qualified key from the
package the running routine belongs to and prefers the mirror.
`our_package_container_key` mirrors `unit_lexical_slot`'s candidate order
exactly (the frame's `lexical_package`, the method class, the frame package,
then `current_package`), each walked up its `::` chain, and reuses
`package_qualified_candidate` so it applies precisely the twigil /
positional-capture exclusions the compiler's `qualify_variable_name` does —
reads and writes therefore reconstruct exactly the key the declaration stored.

It is wired into three chokepoints:

- `get_env_with_main_alias` (`vm_env_helpers.rs`), the by-name **read**
  chokepoint, immediately after `unit_scope_lexical` — so a compunit's
  file-scope `my` still wins over a same-named package variable.
- `env_root_descended_mut` (`vm_var_assign_index_named.rs`), the container
  **mutation** chokepoint every element-assign / `push` / `pop` / key-set
  funnels through, immediately after `unit_lexical_slot_mut`.
- `exec_delete_index_named_op` (`vm_var_delete_ops.rs`), which resolves its
  container out of `env` by name rather than through `env_root_descended_mut`
  and so needed its own seed / run / write-back / restore dance — the same one
  ADR-0039 §6.1 records for the unit-lexical case, and for the same reason.
  Its write-back goes through a new `our_mirror_store_preserving_identity`,
  which copies the mutated contents **into** the mirror's existing `Gc` node
  rather than swapping it, so the module mainline's own slot and the
  `env["%Pkg::h"]` entry are not orphaned on the first `:delete`.

Nothing else was needed on the write side: container mutation in mutsu is
write-through-the-shared-node (ADR-0013 / ADR-0039 §2), so once a chokepoint
resolves to the mirror's node the mutation lands there by itself.

A container the **running frame declares itself** shadows the package variable,
so a name present in the frame's own `CompiledCode::locals` is never
redirected. That keeps the change a resolution *preference* rather than a
hijack in the opposite direction: `sub f { my @arr = <p q>; @arr.push('r') }`
inside the very module that declares `our @arr` still uses its own lexical, and
leaves the package container untouched.

## Verification

New pin `t/our-container-bare-name-resolution.t` (28 assertions, all verified
against real `raku` first) with fixture `t/lib/UnitOurContainer.rakumod`: read /
`push` / element-read / element-assign / `pop` / nested-block push for `@`, and
read / key-set / element-read / `:delete` for `%`, each paired with an assertion
that the script's same-named container is untouched and one that the
package-qualified mirror agrees — plus the two lexical-shadowing rows above.

Slice 1's pins (`t/module-file-scope-lexical.t`,
`t/named-sub-lexical-scope-container.t`) and
`t/anon-container-cell-inplace-reassign.t` stay green, as does the full local
suite (3362 files, 31590 assertions).

## Still open

ADR-0039 slice 2 (containers resolve by slot/upvalue at the compiler, retiring
by-name container resolution outright) remains the architectural end state and
keeps the deep ticket open. The **scalar** twin of this bug — a module's
`our $x` write landing on the caller's `my $x` — is a genuinely different
mechanism (a scalar has no shared node, so it needs the bare env store
*suppressed*, not just resolution redirected) and is recorded separately as
`todo/tickets/our-scalar-write-leaks-to-the-callers-lexical.md`.
