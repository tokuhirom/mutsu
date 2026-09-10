use v6;
use Test;

plan 8;

# Assigning a lazy sequence (gather / map) to a % container must reify it
# before splitting into key/value pairs. Previously the unreified LazyList
# stringified into a single bogus key ({ => Nil}).

my %h = gather { take 'foo' => 1; take 'bar' => 2 };
is %h<foo>, 1, 'gather of pairs into a Hash: foo';
is %h<bar>, 2, 'gather of pairs into a Hash: bar';

my %kv = gather { take 'a'; take 1; take 'b'; take 2 };
is %kv<a>, 1, 'gather of alternating key/value into a Hash: a';
is %kv<b>, 2, 'gather of alternating key/value into a Hash: b';

my %m = (1..3).map({ $_ => $_ * $_ });
is %m{3}, 9, 'map producing pairs into a Hash';

my $seq = gather { take 'x' => 10; take 'y' => 20 };
my %s = $seq;
is %s<y>, 20, 'lazy Seq bound to a scalar then assigned to a Hash';

# The lazy seq must not lose elements.
my %big = gather { take $_ => $_ for 1..10 };
is %big.elems, 10, 'all gathered pairs survive the assignment';

# An empty gather yields an empty hash.
my %e = gather { };
is %e.elems, 0, 'empty gather is the empty hash';
