# List destructuring keeps an Array element's itemization

`my ($y, %r) = @a` stages its right-hand side in a hidden temp before it
reads one value per target. That temp is the RHS list, not a user `Array`,
so it must not itemize its elements. mutsu enforced this by stripping every
element's itemization, which also stripped the `Scalar` holder an `Array`'s
elements really are:

```raku
my %h = a => 1, b => 2;
my @a = 1, %h;
my ($y, %r) = @a;   # raku: dies "Odd number of elements ... Only saw: ${...}"
                    # mutsu: %r was a copy of %h
```

The temp now neither adds nor removes itemization (#9898, ADR-0040 §11,
ADR-0079 §6). It is built by `coerce_to_staging_array`, which is
`coerce_to_array` without the itemizing tail, so a `List` literal's bare
hash still flattens into a `%` target, and an `Array`'s element arrives as
the one item it already was. The strip pass, `deitemize_real_array_elements`,
is gone.
