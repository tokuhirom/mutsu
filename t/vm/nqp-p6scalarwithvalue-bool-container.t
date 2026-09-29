use Test;

# From the JSON::Hjson ecosystem suite: JSON::Fast builds decoded objects with
# nqp::bindkey($hash, $key, nqp::p6scalarwithvalue($descriptor, $value)), so a
# decoded Bool is held in a Scalar and renders `:a(Bool::False)` inside a Hash,
# exactly like a Bool stored by ordinary assignment.
plan 5;

use JSON::Fast;

my %assigned = a => False, b => True;
my $decoded = from-json('{"a":false,"b":true}');

is $decoded.raku, '${:a(Bool::False), :b(Bool::True)}', 'decoded Bool values render as contained';
is $decoded.raku.subst('$', ''), %assigned.raku, 'same shape as an assigned Hash';

my $h = from-json('{"a":false}');
$h<a> = 5;
is $h<a>, 5, 'a decoded Bool slot stays assignable';

is from-json('[true,false]').raku, '[Bool::True, Bool::False]', 'array elements unchanged';
my $n = from-json('{"a":{"d":true}}');
is $n.raku, '${:a(${:d(Bool::True)})}', 'nested object';
