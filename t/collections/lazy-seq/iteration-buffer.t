use Test;

plan 9;

isa-ok (my $ib := IterationBuffer.new), IterationBuffer;
is $ib.elems, 0, "new IterationBuffer starts empty";
is-deeply $ib.AT-POS(0), Mu, "out-of-range element is Mu";

is-deeply $ib.push(10), 10, "push returns pushed value";
is-deeply $ib.unshift(5), 5, "unshift returns prepended value";
is-deeply $ib.BIND-POS(2, 20), 20, "BIND-POS returns bound value";
is-deeply $ib.List, (5, 10, 20), "List coercion reflects contents";

is-deeply (my $ib2 := IterationBuffer.new(<a b c>)).List, <a b c>, "new accepts list arg";
is-deeply $ib.append($ib2).List, (5, 10, 20, "a", "b", "c"), "append takes items, not object";
