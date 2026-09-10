use Test;

plan 6;

# `constant @x` keeps an unreifiable lazy list lazy, exactly as `my @x` does.
# It used to be wrapped as a SINGLE element, so `@primes[^8]` read
# `((...) Nil Nil …)` — which broke Digest::SHA2's
# `constant @primes = grep *.is-prime, 2 .. *`.

constant @primes = grep *.is-prime, 2 .. *;
is @primes[^8], (2, 3, 5, 7, 11, 13, 17, 19), 'an infinite constant list reifies on demand';
is @primes[8..15], (23, 29, 31, 37, 41, 43, 47, 53), 'and further slices keep reifying';
is @primes[0], 2, 'a single index reifies';

constant @squares = (1 .. *).map(* ** 2);
is @squares[^5], (1, 4, 9, 16, 25), 'a lazy map behind a constant';

# Eager values keep their existing `constant @` semantics.
constant @three = 1, 2, 3;
is @three.elems, 3, 'an eager comma list is unchanged';

constant @one = 42;
is @one.elems, 1, 'a scalar becomes a one-element list';
