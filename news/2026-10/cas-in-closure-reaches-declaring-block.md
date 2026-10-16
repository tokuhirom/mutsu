# A first `cas` inside a closure is seen by the declaring block

```raku
sub run(&c) { c() }
my @a = 1, 2;
run { cas(@a[1], 2, 20) };
my &peek = { @a[1] };
cas(@a[1], 20, 30);
say peek();   # 30
```

The first `cas` on an element of an Array or Hash variable moves the variable
onto a celled atomic store. When that first `cas` ran inside a closure, the
move was stored into the closure's own copy of the variable, and the block
that declared it only caught up when the routine that ran the closure
returned. The move now goes through the variable's capture cell, so the
declaring block and every closure see it at once (ADR-12529 §3, slice 31).
