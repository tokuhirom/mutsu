# A block-final `my $*x` no longer outlives its block

`do { my $*y = 3; }`, `given 1 { my $*z = 3; }` and `if True { my $*x = 3; }` left the declared
dynamic in force after the block. A declaration in the final position of a block was compiled by
a hand-inlined path that stored into the enclosing scope's slot instead of declaring a fresh
one; a dynamic declaration now goes through the ordinary statement compile and is read back for
the block's value. A constant-condition `if` branch (folded away at compile time) also runs as a
real block when it declares a dynamic. Closes #16199.
