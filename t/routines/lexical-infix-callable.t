use Test;

plan 12;

# A lexical `&infix:<op>` (a signature parameter or `my &infix:<op>`) shadows
# the builtin operator: `$a op $b` and `* op *` call it (#16967).
sub t1(&infix:<+>, $a, $b) { $a + $b }
is t1({ $^a * $^b }, 3, 4), 12, 'parameter &infix:<+> is used by the operator syntax';

sub t2(&infix:<cmp>, $a, $b) { $a cmp $b }
is t2({ $^b <=> $^a }, 1, 2).raku, 'Order::More', 'parameter &infix:<cmp>';

sub t3(&infix:<cmp>, $a, $b) { &infix:<cmp>($a, $b) }
is t3({ $^b <=> $^a }, 1, 2).raku, 'Order::More', 'the explicit call form agrees';

{
    my &infix:<cmp> = { $^b <=> $^a };
    is (1 cmp 2).raku, 'Order::More', 'my &infix:<cmp> is used by the operator syntax';
}
is (1 cmp 2).raku, 'Order::Less', 'the builtin is back outside that block';

{
    sub infix:<cmp>($a, $b) { Order::More }
    is (1 cmp 2).raku, 'Order::More', 'a local sub infix:<cmp> is honoured';
}

sub t4(&infix:<cmp>, @x) { @x.sort(* cmp *) }
is t4({ $^b <=> $^a }, [3, 1, 2]).join(','), '3,2,1', 'WhateverCode * cmp * in a sort';

sub t5(&infix:<cmp>, @x) { @x.sort({ $^a cmp $^b }) }
is t5({ $^b <=> $^a }, [3, 1, 2]).join(','), '3,2,1', 'a sort block is not run on the inline cmp fast path';

sub t6(&infix:<cmp>, @x) { @x.sort({ $^b cmp $^a }) }
is t6({ $^b <=> $^a }, [3, 1, 2]).join(','), '1,2,3', 'a reversed sort block calls the lexical too';

sub t7(&infix:<cmp>, $a, $b) { my &inner = { $^x cmp $^y }; inner($a, $b) }
is t7({ $^b <=> $^a }, 1, 2).raku, 'Order::More', 'a closure inside the sub sees the lexical';

is ([3, 1, 2].sort({ $^a cmp $^b })).join(','), '1,2,3', 'no lexical: the sort block is unchanged';
is (2 cmp 1).raku, 'Order::More', 'no lexical: the builtin cmp';
