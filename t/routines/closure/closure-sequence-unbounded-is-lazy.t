use Test;

# An unbounded closure sequence (`... *`) runs its generator only as elements
# are demanded: no prefix is pre-computed, so an expensive or side-effecting
# generator is not run ahead. Found with Math::Sequences (Sylvester's sequence
# at load time). #15306

plan 5;

my @sylvester = 2, { $_**2 - $_ + 1 } ... *;
is-deeply @sylvester[^5].List, (2, 3, 7, 43, 1807), 'a doubly-exponential generator is lazy';

my $calls = 0;
my @s = 1, { $calls++; $_ * 2 } ... *;
is $calls, 0, 'creating the sequence does not run the generator';
is @s[3], 8, 'an element is computed on demand';
is $calls, 3, 'only the elements asked for were computed';

my @fib = (1, 1, * + * ... *).lazy;
is @fib[40], 165580141, '.lazy keeps the generator history (no pre-computed prefix to lean on)';
