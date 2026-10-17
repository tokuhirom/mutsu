use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

my $tree = Q[my @a = ["a", "b"], ["c", "d"]; @a[0]>>.=uc].AST;
my $assignment = $tree.statements[1].expression;
is $assignment.^name, 'RakuAST::ApplyDottyInfix',
    'nested hyper call assignment is an ApplyDottyInfix';
is $assignment.infix.^name, 'RakuAST::DottyInfix::CallAssign',
    'the assignment uses the call-assignment infix';
is $assignment.right.^name, 'RakuAST::MetaPostfix::Hyper',
    'the method call retains its hyper modifier';
is $assignment.right.postfix.dispatch, '.=',
    'the hyper method retains call-assignment dispatch';
is EVAL($tree), 'A B', 'round-tripping performs the hyper writeback';
is EVAL(Q[my @a = ["a", "b"], ["c", "d"]; @a[0]>>.=uc; @a[0].join].AST), 'AB',
    'round-tripping mutates the nested array element';
