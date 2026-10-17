# Writes through a `$*x is rw` parameter or a sigilless slice alias from a nested call

```raku
sub assign() { $*a = 666 }
sub by-rw($*a is rw) { assign(); $*a }
my $c = 42;
say by-rw($c), ' ', $c;      # 666 666
sub uses-eval() { EVAL '1' } # never called

my %h = a => { b => { c => 1, d => 2 } };
sub set(\target, \values) { { target = values }() }
set(%h{"a";"b";<c d>}, (7, 8));
say %h;                      # {a => {b => {c => 7, d => 8}}}
```

An `EVAL` anywhere in a program puts the program's captured lexicals in
cells. A callee's store to `$*a` then wrote only the cell of the variable
bound to it. `$*a` itself kept the old value, which the `is rw` parameter
copied back over the variable when `by-rw` returned. The store now updates
the dynamic as well. A callee's `$*b++` or `$*b += 1` is now also carried
back to the frame that binds `$*b` (#13701).

A closure's assignment to a sigilless parameter bound to a multi-dimensional
slice is now written through the slice, as the same assignment in the routine
body already was (#13730).
