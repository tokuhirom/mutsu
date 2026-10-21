use Test;
use lib 't/lib';
use TopUseMultiRw;

plan 4;

my Int $i = 1;
bump($i);
is $i, 2, 'a top-level imported multi family reaches an rw parameter';
my Str $s = 'a';
bump($s);
is $s, 'a!', 'the other candidate writes through too';
my $c = 5;
is peek($c), 'Scalar', 'a raw parameter sees the caller container';
is peek('x', 1), 'two', 'a two-argument candidate dispatches';
