# A module's `my` variable passed to an `is rw` parameter is written back

A module routine that handed its module-level `my` variable to an `is rw`
parameter lost the write (#10372):

```raku
unit module ModH;
my IO::Handle $fmtfile;
sub wopenin(IO::Handle $f is rw) { $f = open('data.txt', :r) }
our sub go() is export { wopenin($fmtfile); say $fmtfile.defined }  # False, raku: True
```

and when the call was the routine's whole body it died outright with
"Parameter '$f' expects a writable container".

Such a variable outlives the scope that declared it in a dedicated store —
`unit_lexicals` for a `unit module`'s file-scope `my`, `package_lexicals` for a
`module M { my $x; ... }` block, `escaped_our_lexical_cells` for a bare block's
`my` captured by an `our sub` — and every read from the routines that close over
it resolves through that store before `env`. Passing it as an argument did not:

- `WrapVarRef` tagged the argument with the value it had read and left the
  container to a by-name write-back into the calling routine's env, a copy no
  later read consults. It now binds the store's own cell
  (`outer_store_lexical_cell`). A package-block lexical recorded as a plain
  value (no routine *assigns* it, so its block never boxed it) is promoted to a
  cell in place, which every reader already derefs.
- A routine run as a TRIR chunk evaluated a free-variable argument to its value,
  so the callee's `is rw` parameter had no container at all. A generic call
  site now passes a free variable by variable (`TrArg::Outer`), and the
  parameter binds the same store cell (`trir_outer_rw_cell`); the reseed after
  the call picks up the write.

The fix keys on the store the read path already uses, so two modules' same-named
`my $x` stay separate.
