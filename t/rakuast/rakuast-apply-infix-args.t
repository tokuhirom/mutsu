use Test;

plan 4;

# RakuAST::ApplyInfix.args answers the operands as a RakuAST::ArgList.
my $ast = 'my $m = 1 + 2;'.AST;
my @found;
sub walk($n) {
    return unless $n ~~ RakuAST::Node;
    @found.push($n) if $n ~~ RakuAST::ApplyInfix && $n.infix.operator eq '+';
    $n.visit-children(-> $c { walk($c) });
}
walk($ast);
is @found.elems, 1, 'found the + application';
is @found[0].args.^name, 'RakuAST::ArgList', 'args is an ArgList';
is @found[0].args.args.map(*.^name).join(','), 'RakuAST::IntLiteral,RakuAST::IntLiteral',
    'args lists left and right';
is @found[0].args.args.map(*.value).join(','), '1,2', 'in written order';
