use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 5;

my $bare = Q[my @a; @a[]:k].AST.statements[*-1].expression.postfix;
is $bare.index.statements.elems, 0, 'a zen slice has no index statements';
is $bare.colonpairs[0].^name, 'RakuAST::ColonPair::True',
    'a bare :k is a true colonpair';

my $conditional = Q[my @a; @a[]:k(True)].AST.statements[*-1].expression.postfix;
is $conditional.colonpairs[0].^name, 'RakuAST::ColonPair::Value',
    'an explicit condition keeps its value colonpair';

is EVAL(Q[my @a = <a b>; @a[]:k].AST).join(' '), '0 1',
    'a zen key slice evaluates after lowering';
is EVAL(Q[my @a = <a b>; @a[]:k(True)].AST).join(' '), '0 1',
    'a conditional zen key slice evaluates after lowering';
