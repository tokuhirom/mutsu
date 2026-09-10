use Test;

plan 5;

sub infix:<rr>($a, $b) is assoc<right> {
    "($a|$b)"
}
is 1 rr 2 rr 3, '(1|(2|3))', 'assoc<right> parses and evaluates as right-associative';

sub infix:<ls>(*@a) is assoc<list> {
    '(' ~ join('|', @a) ~ ')'
}
is 1 ls 2 ls 3, '(1|2|3)', 'assoc<list> passes all operands to the routine';

sub infix:<nn>(*@a) is assoc<non> {
    '(' ~ join('|', @a) ~ ')'
}
dies-ok { EVAL '1 nn 2 nn 3' }, 'assoc<non> rejects chained use';

sub infix:<cg>($a, $b) is assoc<chain> {
    so $a == $b + 1
}
ok 5 cg 4 cg 3 cg 2 cg 1, 'assoc<chain> evaluates pairwise comparisons';

is (so 2 == 3), False, 'loose so applies to comparison result';
