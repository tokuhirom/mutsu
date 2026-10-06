# A plain assignment through a `:=`-bound element obeys the variable's own container

```raku
my Int $s = 1; my %g; %g<k> := $s;
%g<k> = "x";     # X::TypeCheck::Assignment (was: stored "x" into $s)
my Int $t = 1; my %h; %h<k> := $t;
%h<k> = Nil;     # $t is Int, the type object (was: Any)
```

After `%h<k> := $y` the element IS `$y`'s container (#11810). A plain element
assignment through it must apply `$y`'s declared constraint and `is default`; the
element store decayed `Nil` against the *hash's* default and wrote the raw value
into the shared cell, which checks nothing.

The cell a `:=` element bind promotes already carries `$y`'s `of` constraint and
`is default` (#11618). The named element assignment now looks for such a cell
under a single `@a[i]` / `%h{k}` target and, when it finds one, takes its `Nil`
reset and its type check from the cell (`Interpreter::element_store_through_cell`,
the cell store `coerce_container_cell_store` already gives a scalar write). The
`Nil` reset is the one the sigilless-alias store already used, now shared as
`cell_nil_reset_value`. The probe sits behind a flag that is set only once some
cell has been given a constraint or a default, so a program that never makes one
pays a single relaxed load per element store. A bound untyped scalar, a cell with
no metadata, a slice and an object-hash key are covered by the same test file.
