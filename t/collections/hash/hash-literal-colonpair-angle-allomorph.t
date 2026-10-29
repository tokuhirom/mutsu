use Test;

plan 8;

my %h = :k<4>;
isa-ok %h<k>, IntStr, 'my %h = :k<4> is an IntStr';

isa-ok {:k<4>}<k>, IntStr, ':k<4> in a hash literal is an IntStr';
isa-ok {:j<4.1>}<j>, RatStr, ':j<4.1> in a hash literal is a RatStr';
isa-ok {:s<abc>}<s>, Str, ':s<abc> in a hash literal stays a Str';

my $h = { x => 1, :k<4>, :j<4.1> };
isa-ok $h<k>, IntStr, 'mixed with fat-arrow pairs: IntStr';
isa-ok $h<j>, RatStr, 'mixed with fat-arrow pairs: RatStr';

my @l = :k<4>;
is-deeply { :k<4> }, { k => <4> }, 'hash literal :k<4> equals { k => <4> }';
is-deeply { k => <4> }, @l.Hash, 'a pair list agrees with the hash literal';
