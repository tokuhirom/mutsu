use Test;

# An unbounded closure sequence (`... *`) must not spend its eager warm-up
# prefix on elements that are expensive to compute. Found with Math::Sequences
# (Sylvester's sequence, at module load time). #15306

plan 2;

my @sylvester = 2, { $_**2 - $_ + 1 } ... *;
is-deeply @sylvester[^5].List, (2, 3, 7, 43, 1807), 'a doubly-exponential generator does not hang';

my @fib = (1, 1, * + * ... *).lazy;
is @fib[40], 165580141, '.lazy keeps the generator history past the eager prefix';
