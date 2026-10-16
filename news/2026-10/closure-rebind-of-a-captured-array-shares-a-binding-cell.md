# A closure's `:=` of a captured array reaches its creator through a binding cell

```raku
sub run-it(&code) { code() }
{
    my @y = 1, 2;
    my &peek = { @y.elems };
    run-it({ @y := [3, 4, 5] });
    say @y;       # [3 4 5]
    say peek();   # 3
}
```

A closure that rebinds an `@` or `%` variable of the frame that created it
used to rebind only its own copy of the name; the call's return merge then
copied the new binding back to the block, and a sibling closure made earlier
kept reading the old array. The variable now gets a binding cell at its
declaration whenever a closure rebinds it, so the block and every closure that
captured it see the rebind, while a name bound to the old array keeps it
(ADR-12529 §3, slice 25).
