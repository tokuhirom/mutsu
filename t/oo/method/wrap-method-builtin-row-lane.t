use Test;

# The method-table lanes answer a built-in method call from its row. They
# must step aside whenever a `.wrap` chain could run for the receiver. A
# chain owned by a user class never can: a built-in type does not inherit
# from one. So a program that wraps a user method keeps the lanes for
# built-in receivers, while a wrap of a built-in method still runs its
# wrapper (#12875, #12506).

plan 8;

class Frob {
    method uc() { 'frob' }
    method elems() { 42 }
}
Frob.^lookup('uc').wrap(my method (|c) { 'wrapped ' ~ callsame });
Frob.^lookup('elems').wrap(my method (|c) { callsame() + 1 });

is Frob.new.uc, 'wrapped frob', 'the user class runs its wrapper';
is Frob.new.elems, 43, 'and for a second method';
is 'abc'.uc, 'ABC', 'a Str .uc of the same name runs the built-in';
is [1, 2, 3].elems, 3, 'an Array .elems runs the built-in';
my @a = 1, 2;
is @a.elems, 2, 'a named Array .elems runs the built-in';

# A wrap of a built-in method itself still runs.
my $h = Str.^lookup('lc').wrap(my method (|c) { 'lc-wrapped:' ~ callsame });
is 'ABC'.lc, 'lc-wrapped:abc', 'a wrapped built-in method runs its wrapper';
my $s = 'XY';
is $s.lc, 'lc-wrapped:xy', 'on a named receiver too';
$h.restore;
is 'ABC'.lc, 'abc', 'and the built-in again after restore';
