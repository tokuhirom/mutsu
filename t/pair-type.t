use Test;
plan 4;

my $p = pair("name", "Alice");
is $p.key, "name", "pair key";
is $p.value, "Alice", "pair value";
is $p.WHAT, "(Pair)", "pair WHAT";

my $p2 = pair("age", 30);
is $p2.value, 30, "pair with int value";
