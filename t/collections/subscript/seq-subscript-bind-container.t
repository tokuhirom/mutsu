# Indexing a Seq whose elements are live containers (a `.map` block ending in a
# subscript) hands a `:=` bind the stored cell, so the bind writes through (#17063).
use Test;
plan 5;

my %h = a => 1;
my $r = ('a',).map(-> $k { %h{$k} });
my $x := $r[0];
$x = 9;
is %h<a>, 9, 'a bound Seq subscript writes through to the hash element';

my @arr = 1, 2, 3;
my $s = (0, 2).map({ @arr[$_] });
my $y := $s[1];
$y = 30;
is-deeply @arr, [1, 2, 30], 'a second element of a mapped Seq binds to its array cell';

my $z := $s[0];
$z = 10;
is-deeply @arr, [10, 2, 30], 'the first element too';

my $plain = (1, 2).map({ $_ * 2 });
my $p := $plain[0];
is $p, 2, 'a Seq of plain values still binds (read-only)';
dies-ok { $p = 5 }, 'and stays immutable';
