use v6;
use Test;

# In an EVAL'd unit, a routine declared in a block nested in the unit shadows
# an outer routine of the same name instead of redeclaring it, as the same text
# does outside an EVAL (#15869). The unit's own top level still may not
# redeclare a name twice.

plan 6;

is EVAL(q[sub helper() { 1 }
{
    sub helper() { 2 }
    helper();
}]), 2, 'the nested block sees its own routine';

is EVAL(q[sub helper() { 1 }
{ sub helper() { 2 } }
helper()]), 1, 'the outer routine is back after the block';

is EVAL(q[sub infix:<op> ($a, $b) { $a - $b }
{
    sub infix:<op> ($a, $b) { $a ** $b }
    2 op 3;
}]), 8, 'an operator declared in a nested block shadows the outer one';

is EVAL(q[sub infix:<op> ($a, $b) { $a - $b }
{ sub infix:<op> ($a, $b) { $a ** $b } }
2 op 3]), -1, 'the outer operator is back after the block';

throws-like { EVAL q[sub twice() { 1 }; sub twice() { 2 }] },
    X::Redeclaration, 'a redeclaration at the unit top level is still refused';

{
    use experimental :rakuast;
    is Q[sub helper() { 1 }
{ sub helper() { 2 } }
helper()].AST.EVAL, 1, 'the same through EVAL($ast)';
}
