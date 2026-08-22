# A module's `our $x` write lands on the caller's same-named `my $x`

Found while fixing the `our @arr` / `our %h` half of
`todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md`
(item 1). The container half is fixed; the **scalar** twin is not, and it is a
different enough mechanism to deserve its own ticket rather than being folded
into the container fix.

## Repro

```raku
# t-lib/UFL4.rakumod
unit module UFL4;
our $s = 'S';
sub s-set($v) is export { $s = $v }
sub s-peek() is export { $s }
```
```raku
use UFL4;
my $s = 'mine';
s-set('changed');
say s-peek();     # raku: changed   mutsu: changed
say $s;           # raku: mine      mutsu: changed   <-- WRONG
say $UFL4::s;     # raku: changed   mutsu: changed
```

The pure-read direction is wrong too, and differently — it yields `Nil` rather
than either the module's or the caller's value:

```raku
use UFL4;
my $s = 'mine';
say s-peek();     # raku: S         mutsu: Nil ("Use of Nil in string context")
say $UFL4::s;     # raku: S         mutsu: S
say $s;           # raku: mine      mutsu: mine
```

## Why the container fix does not cover it

`our @arr` and `our $s` compile to different shapes:

- `our @arr = <a b>` emits `Dup; SetLocalDecl(slot); SetGlobal("@UFL3::arr")`.
  The container is one `Gc` node published under several names, and container
  mutation is write-through-the-shared-node (ADR-0013 / ADR-0039 §2). So
  redirecting *resolution* to the package-qualified mirror is sufficient: once
  a read or write chokepoint resolves to the mirror's node, `push` /
  element-assign / `:delete` land on the right container with no store change.
  That is what `src/vm/vm_our_package_vars.rs` now does.
- `our $s = 'S'` emits `DeclareOurScalar { slot, qualified_idx }`
  (`src/compiler/stmt.rs`, gate `use_our_cell`), and a write inside a module
  routine is a bare `SetGlobal("s")`. A scalar has no shared node: the write
  must *replace* a value. `SetGlobal` already calls
  `writeback_package_scope_var` (`src/vm/vm_env_helpers.rs`), which does update
  `our_vars["UFL4::s"]` correctly — but it *also* writes the bare `env["s"]`
  key unconditionally, and that key is the caller's `my $s`. So fixing the
  scalar means **suppressing the bare env store** when the name resolves to an
  `our` of the running routine's package, the way `unit_lexical_write`
  suppresses it for ADR-0039 slice 1's compunit lexicals — a store/write-gating
  change, not just a resolution preference.

The read side needs matching work: `GetGlobal`'s cascade consults `our_vars`
only when `name.contains("::")` (an explicit comment at
`src/vm/vm_exec_dispatch.rs` says "Bare variable names should NOT fall back to
our_vars"), so the bare `$s` read inside the module never reaches
`our_vars["UFL4::s"]`. Whatever relaxes that has to keep the reason the
restriction was added in the first place.

## Affected files

- `src/vm/vm_exec_dispatch.rs` — `GetGlobal` cascade (bare-name `our_vars`
  exclusion) and the `SetGlobal` arm (unconditional bare env store).
- `src/vm/vm_env_helpers.rs` — `writeback_package_scope_var`,
  `package_qualified_candidate`.
- `src/vm/vm_our_package_vars.rs` — the container-side resolution helper; its
  `our_package_container_key` already builds the right key for a scalar too
  (it is only gated to `@`/`%` because the scalar needs the write-gating above
  before a redirect would be correct).

## Why this is not trivial

`SetGlobal` is the single hottest by-name write chokepoint in the VM and its
bare env store is load-bearing for a great deal besides `our` (dynamic vars,
plain globals, `$_`, package-qualified writes, the `:=`-rebind path). Gating it
needs the same care ADR-0039 slice 1's `unit_lexical_write` gate needed, plus a
matching relaxation of the deliberate bare-name `our_vars` read exclusion. Pin
against `t/our-container-bare-name-resolution.t`'s shape (a `@`/`%` matrix that
already passes) extended with the scalar rows.
