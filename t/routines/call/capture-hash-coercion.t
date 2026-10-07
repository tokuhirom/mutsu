use Test;

# From the Proc::Q distribution (t/01-basic.t): `.Capture.Hash` is a mutable Hash,
# while `.hash` stays an immutable Map.
plan 6;

class R { has $.a = 1; has $.b = 2; }

my $c = R.new.Capture;
isa-ok $c.Hash, Hash, 'Capture.Hash is a Hash';
isa-ok $c.hash, Map, 'Capture.hash stays a Map';
ok $c.hash !~~ Hash, 'Capture.hash is not a Hash';

my @res = R.new, R.new;
for @res { $_ = .Capture.Hash; }
@res[0]<a>:delete;
is-deeply @res[0].keys.sort.List, ('b',), ':delete works on the coerced Hash';
is @res[1].keys.sort.join(','), 'a,b', 'a second Hash is independent';
dies-ok { $c.hash<a>:delete }, '.hash is immutable';
