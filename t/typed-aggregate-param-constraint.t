use v6;
use Test;

# On an @/% parameter the type constraint applies to the ELEMENTS, not to
# the aggregate: a default array value and a passed array must both bind
# (Base64's `Str:D :@alpha = @chars64std` and its `samewith(:alpha(@u))`).

plan 4;

my Str @letters = 'a', 'b';

sub with-default(Str:D :@alpha = @letters) { @alpha.elems }
is with-default(), 2, 'array default binds under an element type constraint';

multi typed-named(Str:D :@alpha) { "array:{@alpha.elems}" }
multi typed-named(Bool:D :$uri!) { typed-named(:alpha(@letters)) }
is typed-named(:alpha(@letters)), 'array:2',
    'multi named @ param accepts a typed array';
is typed-named(:uri), 'array:2', 'redispatch with :alpha(...) reaches the array candidate';

my Int %ones = a => 1;
sub hash-default(Int:D :%counts = %ones) { %counts.elems }
is hash-default(), 1, 'hash default binds under an element type constraint';
