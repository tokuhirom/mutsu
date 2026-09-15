# A `Slip` read out of a container now flattens (half 1 of #8465)

A `Slip` is the one value that flattens out of whatever container it was
read from — that is its whole purpose:

```raku
my $x = slip(5, 6);
say (1, $x, 2).elems;    # rakudo: 4        mutsu (before): 3
my @a = 1, $x, 2;
say @a.raku;             # rakudo: [1, 5, 6, 2]   mutsu (before): [1, slip(5, 6), 2]

my %h;
%h<a> = slip(5, 6);
say (1, %h<a>, 2).elems; # rakudo: 4        mutsu (before): 3
```

`exec_make_array_op` (`src/vm/vm_data_ops.rs`), the list-construction path
shared by every list/array/argument literal, handled a container-aliasing
wrapper *first* — a `WrapVarRef`-tagged scalar variable read, or a hash/array
element read arriving as a live `ContainerRef` cell — to alias the source
container so a later mutation is visible through the list, and `continue`d
before ever reaching the `ValueView::Slip(items) => elems.extend(...)` arm
below it. So a `Slip` read out of either kind of container never got the
chance to flatten, while the same `Slip` written inline, returned from a
sub, read through `.self`, wrapped in parens, or consumed by a `for` loop
already worked (none of those go through the aliasing wrapper).

Fixed by checking, in both wrapper arms, whether the wrapped value is a
`Slip`; if so, flatten its items directly instead of preserving the
container alias — a `Slip` splices its contents into the list right now, so
there is nothing left to alias afterward.

This is half 1 of #8465 ("List construction"); half 2 ("Scalar store") —
`my $x = slip(...); $x.raku` still renders `slip(5, 6)` instead of rakudo's
`$(slip(5, 6))` — is a purely cosmetic `.raku`-rendering gap in
`itemize_scalar_store_value` that does not affect flattening/`.elems`
correctness, and is left open on #8465 to get its own change and full-suite
run, per the issue's own scoping.

Refs #8465.
