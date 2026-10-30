# `+@a is raw` binds the caller's containers like `*@a is raw`

```raku
sub bump(+@v is raw) { for @v -> $x is raw { $x++ } }
my ($p, $q) = 1, 2;
bump($p, $q);          # raku: 2 3, mutsu used to leave 1 2 for an anonymous container
```

A `*@a is raw` slurpy already kept each argument's container, but its
single-argument-rule twin `+@a is raw` (and `is rw`) bound plain values, so
`nqp::isrwcont` was false for every element and a later assignment through a
bound slot was silently dropped. The `+@a` binder now reuses the `|c` capture
container helper: an argument that is already a cell (`$ = 1`) is kept, one
that names a plain caller variable becomes that variable's shared cell with a
return-time writeback, and a lone non-itemized array source aliases its
elements. `annotations::containers::Buffer`'s `STAMP` (`+@values is raw`) relies
on exactly this, and its `t/01-containers.rakutest` "can assign to a slot by
position if mutable" now passes.
