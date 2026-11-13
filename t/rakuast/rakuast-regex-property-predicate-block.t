use Test;
use experimental :rakuast;

# A property predicate is an expression of any kind in the `.AST` tree: a
# block, a dynamic variable or a list, without running it (Red's regex
# compiler rejects the non-literal ones itself, t/121).

plan 3;

my $ran = False;
sub predicate($source) {
    $source.AST.statements[0].expression.body.elements[0].predicate.semilist.statements[0].expression.^name
}
is predicate(q[/<:Block({ $ran = True })>/]), 'RakuAST::Block', 'a block';
is predicate(q[/<:Block($*PROGRAM-NAME)>/]), 'RakuAST::Var::Dynamic', 'a dynamic variable';
is predicate(q[/<:Block('Basic Latin', 'Greek')>/]), 'RakuAST::ApplyListInfix', 'a list';
