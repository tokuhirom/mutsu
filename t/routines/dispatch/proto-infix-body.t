use Test;

plan 3;

proto sub infix:<proto-answer>($a, $b) { 7 }
multi sub infix:<proto-answer>(Int $a, Int $b) { 8 }
is 1 proto-answer 2, 7,
    'an infix proto body answers before its matching multi candidate';

proto sub infix:<proto-around>($a, $b) { {*} + 1 }
multi sub infix:<proto-around>(Int $a, Int $b) { $a + $b }
is 2 proto-around 3, 6,
    'an infix proto body can dispatch to its candidate';

proto sub infix:<proto-list>($a, $b, $c) is assoc<list> { 7 }
multi sub infix:<proto-list>(Int $a, Int $b, Int $c) { 8 }
is 1 proto-list 2 proto-list 3, 7,
    'a list-associative infix enters its proto body';
