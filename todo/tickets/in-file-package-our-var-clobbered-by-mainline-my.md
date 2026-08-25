# An in-file `module M { our $x }` is clobbered by a mainline `my $x`

Found while fixing
`news/2026-08/our-scalar-bare-name-resolves-to-the-package-cell.md`. It is a
*neighbouring* defect, not the one that fix addresses: it predates that work
(verified against the pre-fix binary) and it is unchanged by it.

## Repro

```raku
module M {
    our $x = 'our';
    our @y = 'oury';
}
my $x = 'top';
my @y = 'topy';
say $M::x;              # raku: our    mutsu: top
say @M::y.join(",");    # raku: oury   mutsu: topy
say $x;                 # raku: top    mutsu: top
say @y.join(",");       # raku: top    mutsu: topy
```

Both sigils fail identically, so it is not the scalar/container asymmetry the
`our`-resolution work is about — the package-qualified mirror itself ends up
holding the mainline lexical's value.

## Why it is not the same bug

The fixed bug was about a *module in a separate compunit* whose routines
reference their `our` variable by the bare name: the loading script's
same-named `my` hijacked that bare name in `env`. Here there is no separate
compunit and no module routine involved at all — the mainline `my $x = 'top'`
declaration itself reaches the package's storage. The likely mechanism is that
an in-file `module` block's `our` declaration and the mainline share one frame
(the package block runs inline), so the `my` declaration's slot/env write lands
on the same slot the `our` declaration published its cell into. That is a
different chokepoint from the bare-name resolution the fix changed, and the
`our`-cell redirect deliberately does not fire at mainline scope (the running
frame's `current_package` is GLOBAL there).

## Notes for whoever picks this up

- Verify first: this file records behaviour as of 2026-08-25 on the fix branch;
  re-run the repro before designing anything.
- `t/our-scalar-bare-name-resolution.t` and
  `t/our-container-bare-name-resolution.t` are the pins for the neighbouring
  behaviour — neither must regress.
- Start at `exec_declare_our_scalar_op` (`src/vm/vm_misc_scope.rs`) and
  `exec_set_local_op_inner`'s redeclaration handling
  (`src/vm/vm_var_assign_set_local.rs`), comparing the slot each one writes
  when the `module` body is inline versus loaded from a separate file.
