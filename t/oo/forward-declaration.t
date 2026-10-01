use Test;

plan 2;

sub add($a, $b);
is add(1, 2), 3, 'forward declaration resolves to later body';
sub add($a, $b) { $a + $b }

# The body cannot read its arguments through `@_`: a routine with a signature
# rejects `@_` ("Placeholder variable '@_' cannot override existing
# signature"), in a destructuring `my (...) = @_` as anywhere else.
sub sum4($$$$);
is sum4(1, 2, 3, 4), 'four', 'compact anonymous-sigil signature works in forward declaration';
sub sum4($$$$) { 'four' }
