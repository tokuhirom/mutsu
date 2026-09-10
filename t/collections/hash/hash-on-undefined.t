use v6;
use Test;

# `.hash` on an undefined invocant is the empty hash (it has no contents), and
# `.Hash` coerces a Map to a fresh mutable Hash while returning a plain Hash by
# identity. Previously `my $d; $d.hash` threw "Odd number of elements".

plan 13;

# `.hash` on an undefined scalar / type object is `{}`.
my $d;
is $d.hash.gist, '{}',       'undefined scalar .hash is empty';
is $d.hash.^name, 'Hash',    '.hash returns a Hash';
ok $d.hash.defined,          'the empty hash is defined';
my Int $x;
is $x.hash.gist, '{}',       'typed undefined .hash is empty';
is Any.hash.gist, '{}',      'Any type object .hash is empty';

# `.hash` on a Map returns a Map (immutable, keeps its type).
my %m is Map = a => 42, b => 666;
is %m.hash.^name, 'Map',                 'Map.hash stays a Map';
is %m.hash.raku, 'Map.new((:a(42),:b(666)))'.subst(/\s/, '', :g),
    'Map.hash renders as Map.new(...)';

# `.Hash` on a Map is a fresh mutable Hash (drops the Map-ness).
is %m.Hash.^name, 'Hash',     'Map.Hash is a plain Hash';
is %m.Hash.gist, '{a => 42, b => 666}', 'Map.Hash renders as a hash';

# `.Hash` on a plain Hash is the same hash (identity).
my %h = a => 1, b => 2;
is %h.Hash.^name, 'Hash',     'Hash.Hash is a Hash';
ok %h.Hash =:= %h,            'Hash.Hash is identity';

# A defined non-Associative scalar still throws on odd hash coercion.
throws-like { my $y = 42; $y.hash }, X::Hash::Store::OddNumber,
    'defined scalar .hash is an odd-count error';

# `.Hash` on an undefined scalar is also empty.
is $d.Hash.gist, '{}',        'undefined scalar .Hash is empty';
