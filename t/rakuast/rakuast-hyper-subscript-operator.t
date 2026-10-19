use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

my $tree = Q{[[1, 2], [3, 4]]».[0]}.AST;
my $expression = $tree.statements[0].expression;
is $expression.postfix.^name, 'RakuAST::MetaPostfix::Hyper', 'hyper postfix node';
is $expression.postfix.postfix.^name, 'RakuAST::Postcircumfix::ArrayIndex', 'inner node is an index operator';
is-deeply EVAL($tree), (1, 3), 'hyper index round trip';
done-testing;
