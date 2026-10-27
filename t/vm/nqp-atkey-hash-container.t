use v6;
use nqp;
use Test;

# A Hash's `$!storage` holds Scalar containers in Rakudo, so nqp::atkey hands
# out the value's container until nqp::bindkey replaces it with a bare value.
# Hash2Class (zef:lizmat) relies on exactly this to convert each value once.

plan 8;

my %h = a => "42", b => 7;
my $st := nqp::getattr(%h, Map, '$!storage');

is nqp::iscont(nqp::atkey($st, 'a')), 1, 'atkey on a Hash storage is a container';
is nqp::iscont(my $v := nqp::atkey($st, 'a')), 1, 'iscont over a declaring bind asks about the bound variable';
is $v.Int + 1, 43, 'the container reads as its value';

nqp::bindkey($st, 'a', $v.Int);
is nqp::iscont(nqp::atkey($st, 'a')), 0, 'a bound bare value is no longer a container';
is %h<a>.raku, '42', 'the bound value is visible through the Hash';
is nqp::iscont(nqp::atkey($st, 'b')), 1, 'an untouched key is still a container';

is nqp::iscont(nqp::atkey(nqp::hash('x', 1), 'x')), 0, 'a raw nqp::hash holds bare values';

my %m := Map.new((k => 1));
is nqp::iscont(nqp::atkey(nqp::getattr(%m, Map, '$!storage'), 'k')), 0, 'a Map holds bare values';
