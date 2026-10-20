use Test;

# From the PDF distribution (t/indobj-name.t): `enum « :Baz<Baz> »`.
plan 3;

enum « :Baz<Baz> :Qux<Q> »;
is Baz.value, 'Baz', 'colonpair value';
is Qux.value, 'Q', 'second variant';
my $e = enum « a b »;
is b.value, 1, 'bare words in expression context';
