use Test;
plan 10;

# Unicode multiplication ×
is 3 × 4, 12, '× multiplication';

# Unicode division ÷
ok 10 ÷ 2 == 5, '÷ division';

# Unicode minus −
is 5 − 2, 3, '− subtraction';

# Unicode less-than-or-equal ≤
ok 3 ≤ 5, '≤ less-than-or-equal (true)';
ok !(5 ≤ 3), '≤ less-than-or-equal (false)';

# Unicode greater-than-or-equal ≥
ok 5 ≥ 3, '≥ greater-than-or-equal (true)';
ok !(3 ≥ 5), '≥ greater-than-or-equal (false)';

# Unicode not-equal ≠
ok 3 ≠ 5, '≠ not-equal (true)';
ok !(3 ≠ 3), '≠ not-equal (false)';

# Unicode equality ⩵
ok 3 ⩵ 3, '⩵ equality';

done-testing;
