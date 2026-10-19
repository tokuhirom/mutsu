use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

for <∪ ∩ ⊎ ⊍ ∖ ⊖> -> $op {
    my $tree = "1 $op 2 $op 3".AST;
    my $expression = $tree.statements[0].expression;
    is $expression.^name, 'RakuAST::ApplyListInfix', "$op stays list associative";
    is $expression.infix.operator, $op, "$op preserves its spelling";
    is $expression.operands.elems, 3, "$op retains all operands";
    is EVAL($tree).^name, ($op eq '⊎' || $op eq '⊍' ?? 'Bag' !! 'Set'),
        "$op lowers to its native set operation";
}
done-testing;
