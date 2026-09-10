use Test;

# A parameter's `where` constraint is scoped left-to-right: referencing a param
# declared LATER in the same signature is X::Undeclared at compile time, but
# referencing the param itself or an earlier param (or an outer lexical) is fine.
# S12-subset/subtypes.t test 77.

plan 5;

throws-like 'sub foo($x where { $x == $y }, $y) { }', X::Undeclared,
    'where referencing a later param is X::Undeclared';

lives-ok { EVAL 'sub f($a, $b where { $b > $a }) { }' },
    'where referencing an earlier param is fine';

lives-ok { EVAL 'sub g($x where { $x > 0 }) { }' },
    'where referencing the param itself is fine';

# The whole thing still works at runtime.
{
    sub h($a, $b where { $b > $a }) { $a + $b }
    is h(1, 5), 6, 'earlier-param where enforced and value returned';
}
{
    sub h2($a, $b where { $b > $a }) { $a + $b }
    dies-ok { h2(5, 1) }, 'earlier-param where rejects a bad argument';
}
