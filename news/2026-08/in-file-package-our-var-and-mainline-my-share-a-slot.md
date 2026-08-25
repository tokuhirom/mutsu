# An in-file `module M { our $x }` no longer shares storage with a mainline `my $x`

```raku
module M {
    our $x = 'our';
    our @y = 'oury';
}
my $x = 'top';
my @y = 'topy';
say $M::x;              # raku: our    mutsu: top
say @M::y.join(",");    # raku: oury   mutsu: topy
```

Both sigils failed identically, so this was never the scalar/container
asymmetry the neighbouring `our`-resolution work
(`news/2026-08/our-scalar-bare-name-resolves-to-the-package-cell.md`) was
about — and it predated that fix, which is why it was filed separately.

## Root cause, as measured (the ticket had the mechanism, not the chokepoint)

The ticket guessed the shared slot correctly and the chokepoint wrongly. It
pointed at `exec_declare_our_scalar_op` and `exec_set_local_op_inner`'s
redeclaration guard. Breakpoints proved the guard **never fires at all** here
(the package-block exit has already removed the bare `env` key, so the guard's
`ContainerRef` probe finds nothing) and that `exec_declare_our_scalar_op`
behaves correctly.

The shared slot is real, and `--dump-bytecode` shows it directly: an in-file
`module M { ... }` body compiles INLINE into the enclosing frame, and
`Compiler::declare_local` allocates one slot per NAME per compiled code object,
so the block's `our $x` and the mainline's `my $x` are both `slot: 0` of one
`locals: ["x", "@y"]`.

What leaked through that slot is `code.our_locals` — a compile-time
slot -> package-qualified-name map that three runtime helpers consult **by slot
alone**. A breakpoint on the reverse sync fired three times on the repro:
once legitimately for the `our @y` declaration, and then twice more for the
MAINLINE `my $x` and `my @y`, pushing `'top'` / `'topy'` straight out to
`M::x` / `@M::y`. Probing further turned up two more leaks through the same
map, in the other direction and at block exit.

## The fix

An `our` variable's bare lexical alias exists only inside its own package's
lexical scope, which `PackageScope` brackets exactly, and a *declaration* is
never a write to a package variable. Both gates are lexical facts, not value
guesses or name special-cases:

- **`sync_our_package_var_from_local`** (`vm_misc_scope.rs`) now takes the
  `vardecl` flag and returns early for a declaration. A `my`/`state` is a fresh
  lexical that is not a package variable at all, and an `our` declaration
  publishes itself explicitly — `DeclareOurScalar`, or the
  `SetGlobal`/`SetGlobalRaw` the compiler emits immediately after the declaring
  `SetLocal`, at the very place the `our_locals` entry is pushed — so the sync
  was redundant there in every shape. `exec_set_local_op` snapshots
  `vardecl_context` before the inner handler consumes it.
- A new **`our_link_owner_in_scope`** gates both syncs on the owning package
  being the one executing: `M::x` is honoured while `current_package` is `M`
  (or a package nested inside it), and ignored once `module M { ... }` has
  exited. An entry with no `::` is a file-scope `our` — `GLOBAL::x` collapses
  to a bare `x` — whose scope is the whole compilation unit, so it stays always
  live. The reverse sync also stopped taking the *first* entry for a slot,
  which was wrong for nested in-file packages (`module M { module N { our $x };
  our $x }` puts two entries on slot 0); it now takes the one whose owner is in
  scope.
- **`sync_our_local_from_qualified`** got the same gate. Without it, an
  external `@M::l = ...` write after the block closed was pulled back onto the
  mainline `my @l` — the mirror image of the reported bug, and the one the
  scalar cell happened to hide.
- **`exec_package_scope_op`** (`vm_misc_reduction_scan.rs`) no longer
  re-exports a bare `env` key that the block introduced as its own `our`
  variable. That loop exists so a write the block made to an *enclosing*
  lexical (`my $x = 1; module M { $x = 2 }`) survives the scope exit, but an
  `our $x` declaration also leaves a bare key — its alias — and copying that
  out made `my $x = 'top'; module M { our $x = 'our' }` leave the mainline
  `$x` reading `'our'`. The two are told apart by the package-qualified twin
  every `our` declaration publishes alongside the alias; requiring the twin to
  be **new** (present after the body, absent before it) is what makes the test
  "this block declared it" rather than "a package variable of this name
  exists", so an `$M::x = ...` performed before the block still lets a genuine
  write-through out.

## Pins

`t/in-file-package-our-var.t` — 38 assertions passing identically under `raku`
and mutsu: both collision orders, all three sigils, later plain assignment to
the mainline lexical, external writes to `$M::x` after the block closed,
`class`/`package` as well as `module`, a nested in-file package, two modules
owning one bare name, a module routine reading and writing its own `our $s`
(the neighbouring fix's shape, unregressed), an assignment to the `our`
variable from inside its own block, mainline `our`/`$GLOBAL::`, and `$*dyn` /
`$_` / a plain package global as controls.
`t/our-scalar-bare-name-resolution.t` and
`t/our-container-bare-name-resolution.t` are unchanged and still pass.

One neighbouring divergence surfaced while probing is *not* fixed here — a
`module`/`package` block resets an outer `my` that no earlier statement has
flushed into `env`, which is the `locals`/`env` dual-store debt rather than
anything to do with `our`. It is recorded in
`todo/tickets/package-block-resets-an-outer-lexical-declared-before-any-env-flush.md`,
and is why the test file opens with a `class Warm { }`.
