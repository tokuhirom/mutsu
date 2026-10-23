use v6.d;
use Test;

plan 6;

# A chained element store through an undefined scalar captured by a sub
# vivifies every level (#14647).
my $nest;
my sub deep { $nest<a><b> = 1 }
deep();
is $nest.raku, '${:a(${:b(1)})}', 'a two-level keyed chain vivifies a Hash of Hashes';

my $n2;
my sub d2 { $n2<a><b><c> = 1 }
d2();
is $n2.raku, '${:a(${:b(${:c(1)})})}', 'a three-level chain does too';

my $n3;
my sub d3 { $n3[1][2] = 7 }
d3();
is $n3.raku, '$[Any, [Any, Any, 7]]', 'a positional chain vivifies Arrays';

my $n4;
my sub d4 { $n4<a>[1] = 7; $n4.raku }
is d4(), '${:a($[Any, 7])}', 'a mixed chain: the sub sees its own store';
is $n4.raku, '${:a($[Any, 7])}', '... and so does the declaring scope';

my $n5;
my $anon = sub { $n5<a><b> = 1 };
$anon();
is $n5.raku, '${:a(${:b(1)})}', 'an anonymous sub works too';
