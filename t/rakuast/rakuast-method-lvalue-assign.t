use Test;

# `$o.attr = value` across the RakuAST boundary: the parser lowers it to an
# internal rw-accessor writeback call, which `.AST` must render as the plain
# ApplyInfix(Assignment) over a method call that rakudo produces. Found via
# Code::Coverage (t/01-basic.rakutest). Passes under both mutsu and raku.

plan 8;

{
    my $e = Q[my $foo; $foo.c = ()].AST.statements[1].expression;
    isa-ok $e, RakuAST::ApplyInfix, '$foo.c = () is an ApplyInfix';
    isa-ok $e.infix, RakuAST::Assignment, 'with an Assignment infix';
    isa-ok $e.left, RakuAST::ApplyPostfix, 'whose left is a method call';
    isa-ok $e.left.postfix, RakuAST::Call::Method, 'a Call::Method';
    is $e.left.postfix.name.gist, 'RakuAST::Name.from-identifier("c")', 'named c';
    isa-ok $e.right, RakuAST::Circumfix::Parentheses, 'right is the value';
}

{
    my $e = Q[my $foo; $foo.d(1) = 2].AST.statements[1].expression;
    like $e.left.postfix.gist, /'IntLiteral.new(1)'/, 'method arguments survive';
    isa-ok $e.right, RakuAST::IntLiteral, 'right is 2';
}
