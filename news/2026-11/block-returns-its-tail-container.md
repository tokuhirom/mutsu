# A plain Block returns the container of its last statement

A `Block` / pointy block (unlike a `sub`) hands back the container its last
statement denotes, in Raku and now in mutsu (#17094):

```raku
my $x = 5; my &b = { $x };  b() = 7;      # $x is 7
my @a = 1, 2; my &c = { @a[0] }; c() = 9; # @a is [9 2]
my &f = { $_ }; f($z).VAR.^name;          # Scalar -- the topic container
```

Block bodies now compile their bare variable / single-element subscript / `$_`
tail like an `is rw` routine's (`rw_tail_block`); `sub_is_rw_capable` accepts a
non-routine block so `&block() = v` writes through. A slice, range or closure
index, an assignment, a ternary and `if`/`given` tails stay plain values.

Consumers that would otherwise keep a live cell read the value at the right
time: a `.map` callback whose tail is a call, a non-rw routine whose tail is a
call to a block, and each `{ ... }` / call part of an interpolated string
(`"{$c()},{$c()}"`).

`t/vm/writeback/dualstore-slot-local-gate.t` expected `(1, 2, 3)` from three
calls of `-> { $n = $n + 1; $n }`; raku prints `(3 3 3)` (the list holds the
same cell), so the expectation is corrected.
