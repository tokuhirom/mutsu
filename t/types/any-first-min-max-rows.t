use Test;

# `.first` with no matcher and `.min`/`.max` with no argument answer from the
# `Any` rows (ADR-11276 §9.85): `.first` is the first element of the
# invocant's list, whatever the invocant is, and `min`/`max` read a `Pair` as
# its one element.

plan 17;

class P { }

is (4, 5, 6).first, 4, 'List.first';
my @a = 7, 8;
is @a.first, 7, 'Array.first';
my @holes; @holes[1] = 3;
nok @holes.first.defined, 'a hole at the front is undefined';
ok ().first === Nil, 'an empty List.first is Nil';
is %(a => 1).first.key, 'a', 'Hash.first is a Pair';
is set(<a>).first.key, 'a', 'a Set reads as its Pairs';
is \(1, 2, :a).first, 1, 'a Capture reads its positionals';
is Blob.new(9, 8).first, 9, 'a Blob reads its bytes';
is (1, 2, 3).map(* * 3).first, 3, 'a Seq.first';
is ((1, 2) but role { }).first, 1, 'a list mixin lists what it wraps';
is 42.first, 42, 'a scalar is its own one-element list';
my $p = P.new;
ok $p.first === $p, 'a user-class instance is its own one-element list';
is (1, 2, 3).first(* > 1), 2, 'a matcher still applies';

is-deeply (a => 1).min, (a => 1), 'Pair.min is the Pair';
is-deeply (a => 1).max, (a => 1), 'Pair.max is the Pair';
is [3, 1, 2].min, 1, 'Array.min';
is <b a c>.max, 'c', 'List.max';
