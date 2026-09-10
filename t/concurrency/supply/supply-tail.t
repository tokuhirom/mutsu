use Test;

plan 6;

my @last_one;
Supply.from-list(1..10).tail.tap(-> $v { @last_one.push($v) });
is-deeply @last_one, [10], "Supply.tail without argument returns last value";

my @last_five;
Supply.from-list(1..10).tail(5).tap(-> $v { @last_five.push($v) });
is-deeply @last_five, [6, 7, 8, 9, 10], "Supply.tail(5) returns last five values";

my @all_inf;
Supply.from-list(1..10).tail(Inf).tap(-> $v { @all_inf.push($v) });
is-deeply @all_inf, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], "Supply.tail(Inf) returns all values";

my @drop_three;
Supply.from-list(1..10).tail(*-3).tap(-> $v { @drop_three.push($v) });
is-deeply @drop_three, [4, 5, 6, 7, 8, 9, 10], "Supply.tail(*-3) computes size from length";

my @drop_thirteen;
Supply.from-list(1..10).tail(*-13).tap(-> $v { @drop_thirteen.push($v) });
is-deeply @drop_thirteen, [], "Supply.tail(*-13) clamps to empty supply";

dies-ok { Supply.from-list(1..10).tail("foo") }, "Supply.tail dies on non-numeric argument";
