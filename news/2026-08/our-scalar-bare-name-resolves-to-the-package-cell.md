# A module's `our $s` no longer lands on the loading script's `my $s`

`our @arr` / `our %h` got bare-name resolution through the package-qualified
mirror on 2026-08-23 (`src/vm/vm_our_package_vars.rs`, ADR-0039 §4.1's
excluded case). Its **scalar** twin stayed broken, and in both directions:

```raku
# t/lib/UnitOurScalar.rakumod
unit module UnitOurScalar;
our $s = 'S';
sub s-set($v) is export { $s = $v }
sub s-read()  is export { $s }
```
```raku
use UnitOurScalar;
my $s = 'mine';
s-set('changed');
say s-read();   # raku: changed   mutsu: changed
say $s;         # raku: mine      mutsu: changed   <-- the module clobbered the caller
```

and, with the `s-set` call removed, the module's own read yielded `Nil` rather
than either value.

## Root cause, as measured (the ticket got half of it wrong)

The ticket named two mechanisms. Breakpoints on the live binary confirmed the
first and refuted the second.

**Write side — confirmed.** A breakpoint on the `SetGlobal` arm's bare `env`
store fired exactly once for the whole program, with `name = "s"`, from
`s-set` in package `UflScalar`. That store is the leak: the bare key belongs to
whatever scope loaded the module, and the consumer's `my $s` is what lives
under it.

**Read side — refuted.** The ticket blamed `GetGlobal`'s `name.contains("::")`
guard on the `our_vars` fallback. Breakpoints on that guard *and* on
`package_chain_var_fallback` never fired: the cascade never gets that far. The
real cause is one line up the stack. `exec_declare_our_scalar_op` publishes the
`our` cell under the BARE `env` key too, so the consumer's later `my $s = ...`
hits `exec_set_local_op_inner`'s redeclaration guard, which replaces a stale
`ContainerRef` in `env` with `Nil` to stop the fresh binding inheriting it.
That `Nil` is a live *positive* env hit, so the module's bare read stopped
there — and the `our_vars` guard the ticket accused was never consulted at all.

## The fix

`our $x` already has a canonical home: `OpCode::DeclareOurScalar` installs ONE
`ContainerRef` cell into the declaring slot, `env[bare]`, `env[qualified]` and
`our_vars[qualified]`. Everything below routes to that cell, so read and write
cannot disagree and no alias is ever severed.

- **Resolution** (`vm_our_package_vars.rs`): `our_package_container_key`'s
  candidate-package search is now the sigil-agnostic `our_package_var_key`, and
  a new `our_package_scalar_cell` reconstructs the qualified key for a bare
  scalar name. It fires only when the `our_vars` entry actually *is* a cell —
  the precise signature of a plain `our $x` declaration — so a bareword, a type
  object, an `our constant` or an `our`-scoped sub that happens to share the
  reconstructed key is never mistaken for a package scalar.
- **Read**: `get_env_with_main_alias` consults the cell before `env`, in the
  same position and for the same reason as the container redirect.
- **Write**: the `SetGlobal` arm gains an `our_scalar_write` gate that writes
  through the cell and *suppresses* the bare `env` / `our_vars` / shared-var
  stores — the exclusivity `unit_lexical_write` (ADR-0039 slice 1) already has.
  The three read-modify-write chokepoints (`++`, `--`, and the fused
  `AtomicCompoundVar` compound assignment) share one new
  `store_scalar_by_name` tail so `$s ~= '+'` and `$n += 10` gate identically.
- **`writeback_package_scope_var`** now writes THROUGH a cell it finds in
  `our_vars` instead of replacing it with a plain value, which had been quietly
  severing the store from the cell every other name still points at.
- **The redeclaration guard** removes the bare key for a package `our` cell
  instead of poisoning it with `Nil`. The cell stays reachable through its
  qualified key and the `our` store, and the fresh `my` binding is no longer
  overwritten by a `Nil` the env↔locals reconciliation pulls back.

## Keeping a captured lexical from being hijacked the other way

Preferring the package cell must not outrank a genuine lexical. The existing
`running_frame_declares_local` gate only looked at the running frame's own
`locals`, which a closure body does not have for the variables it captures — so
`module M { our $x = 'our'; sub f { my $x = 'lex'; sub { $x ~= '!' }(); $x } }`
would have read and written the package variable. The gate now also treats a
name as lexical when the running code has an upvalue slot for it
(`compute_upvalues`, pure reads) or when `free_var_parent_slots` records the
creating frame declaring it (assignments, which get no upvalue). Both tables
are built only for closure bodies, so a named routine's reference to its own
package variable — the case the redirect exists for — is untouched. The same
gate fixes the container twin, which had the identical hijack.

## Pins

`t/our-scalar-bare-name-resolution.t` (33 assertions,
`t/lib/UnitOurScalar.rakumod` + `t/lib/UnitOurScalarTwin.rakumod`) mirrors
`t/our-container-bare-name-resolution.t`'s shape: both directions, `++`/`+=`,
nested blocks and closures, routine-local and parameter shadowing, closures
capturing a shadowing lexical, interpolation, two modules owning the same bare
name, a class-body `our`, and a nested module's `our`. It passes under `raku`
and mutsu alike. `$*dyn`, `$_`, plain globals, a caller-side `our`, and an
external `$Pkg::x = ...` write were probed against `raku` and are unchanged.

One neighbouring divergence found while probing is *not* fixed here and is
recorded separately in
`todo/tickets/in-file-package-our-var-clobbered-by-mainline-my.md`: a
`module M { our $x }` in the SAME file as a mainline `my $x` loses `$M::x`.
It predates this work and affects `our @`/`our %` identically.
