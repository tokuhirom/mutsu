use v6;
use nqp;
use Test;

# nqp::getlex / bindlex (and the native _i/_n/_s forms) name a lexical of the
# running frame by a runtime string; getlexrel / getlexreldyn / curlexpad read
# one through a context. A bindlex write must reach the cell captured closures
# share (checked against Rakudo).
plan 12;

my $x = 5;
my &g = { $x };
is nqp::getlex(q/$x/), 5, 'getlex reads a captured lexical';
nqp::bindlex(q/$x/, 9);
is g(), 9, 'bindlex is seen by a closure capturing the lexical';
is $x, 9, 'bindlex is seen by the declaring frame';

my int $i = 3;
my &gi = { $i };
is nqp::getlex_i(q/$i/), 3, 'getlex_i reads a native int lexical';
nqp::bindlex_i(q/$i/, 4);
is gi(), 4, 'bindlex_i writes through to the closure';

my num $n = 1.5e0;
my &gn = { $n };
is nqp::getlex_n(q/$n/), 1.5e0, 'getlex_n';
nqp::bindlex_n(q/$n/, 2.5e0);
is gn(), 2.5e0, 'bindlex_n';

my str $s = 'a';
my &gs = { $s };
nqp::bindlex_s(q/$s/, 'b');
is nqp::getlex_s(q/$s/), 'b', 'bindlex_s then getlex_s';

is nqp::getlexrel(nqp::ctx(), q/$x/), 9, 'getlexrel through the current context';
is nqp::getlexreldyn(nqp::ctx(), q/$x/), 9, 'getlexreldyn through the current context';
ok nqp::existskey(nqp::curlexpad(), q/$x/), 'curlexpad holds the lexical';
is nqp::atkey(nqp::curlexpad(), q/$x/), 9, 'curlexpad value';
