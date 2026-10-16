# A substitution inside a closure updates the captured variable everywhere

```raku
sub run(&c) { c() }
my $s = 'aba';
my &peek = { $s };
run { $s ~~ s/b/X/ };
say peek();   # aXa
```

`$x ~~ s///` or `$x ~~ tr///` inside a closure now updates the captured `$x`
for the block that declared it and for every other closure that captured
it, at once. The closure used to read `$x` as a private copy, and the
declaring block only caught up when the routine that ran the closure
returned (ADR-12529 §3, slice 33).
