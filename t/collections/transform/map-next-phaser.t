use Test;

plan 7;

# A NEXT phaser in a map block fires after every iteration.
my $a = 0; (1..5).map({ NEXT $a++; $_ }).eager;
is $a, 5, "NEXT fires once per element of an inline map block";

my atomicint $n;
(1..5).map({ NEXT $n⚛++; $_ * 2 }).eager;
is $n, 5, "NEXT with an atomicint counter";

my $b = 0; (1..6).map(-> $x, $y { NEXT $b++; $x }).eager;
is $b, 3, "NEXT fires once per call of a two-parameter block";

my $c = 0; (1..6).map(-> $x { NEXT $c++; next if $x == 2; $x }).eager;
is $c, 6, "NEXT also fires for an iteration ended by next";

my $d = 0; (1..6).map(-> $x { NEXT $d++; last if $x == 3; $x }).eager;
is $d, 2, "NEXT does not fire for the iteration ended by last";

my $e = 0; (1..4).map({ NEXT $e += $_; $_ }).eager;
is $e, 10, "NEXT sees the topic of the iteration";

my @log;
(1..3).map({ FIRST @log.push("F"); NEXT @log.push("N"); LAST @log.push("L"); $_ }).eager;
is @log.join, "FNNNL", "FIRST, NEXT and LAST order";
