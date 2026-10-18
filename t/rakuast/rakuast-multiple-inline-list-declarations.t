use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

my $root = Q[(my $y = 2, my $z = 3)[1] = 99].AST.statements[0].expression;
is $root.^name, 'RakuAST::ApplyPostfix', 'the subscript store applies to the list';
my $list = $root.operand;
is $list.^name, 'RakuAST::ApplyListInfix', 'the inline declarations form a comma list';
is $list.operands.elems, 2, 'the list has both declaration operands';
isa-ok $list.operands[0], RakuAST::VarDeclaration::Simple, 'the first operand is a declaration';
isa-ok $list.operands[1], RakuAST::VarDeclaration::Simple, 'the second operand is a declaration';
is EVAL(Q[my $b = 2; my $c = 3; (my $y = $b, my $z = $c)[1] = 99; "$y $z"].AST),
    '2 99', 'lowering keeps adjacent declarations in one scope';
