use Test;

plan 5;

sub foo(*@a) { "foo(" ~ @a.join(",") ~ ")" }
sub bar(*@a) { @a.map(* * 10) }

is-deeply (1, 2 ==> foo()), "foo(1,2)", '==> is looser than the comma (two items)';
is-deeply (1, 2, 3 ==> bar()).List, (10, 20, 30), '==> feeds the whole comma list (three items)';
is-deeply (foo() <== 1, 2), "foo(1,2)", '<== feeds the whole comma list on its right';
is-deeply (foo() <== 1, 2, 3), "foo(1,2,3)", '<== with three items';
my @u = 1, 2 ==> foo();
is-deeply @u, [1, 2], 'item assignment is tighter than the feed';
