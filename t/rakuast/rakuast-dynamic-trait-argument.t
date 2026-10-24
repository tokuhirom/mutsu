use Test;

# A variable's `is dynamic(ARG)` keeps its argument in the RakuAST tree
# (rakudo 2026.09); it was dropped, and a hand-built one with an argument was
# refused. This file passes under both mutsu and raku.

plan 5;

my $t = Q[my $x is dynamic(42)].AST.statements[0].expression.traits[0];
is $t.name.canonicalize, 'dynamic', 'the trait name';
is $t.argument.^name, 'RakuAST::Circumfix::Parentheses', 'the argument is kept';
nok Q[my $x is dynamic].AST.statements[0].expression.traits[0].argument.defined,
    'a bare `is dynamic` has none';
is EVAL(Q[my $y is dynamic(1) = 5; $y].AST), 5, 'the parsed tree round-trips';

my $decl = RakuAST::VarDeclaration::Simple.new(
    sigil       => '$',
    desigilname => RakuAST::Name.from-identifier('q'),
    traits      => (RakuAST::Trait::Is.new(
        name     => RakuAST::Name.from-identifier('dynamic'),
        argument => RakuAST::Circumfix::Parentheses.new(RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1))))),));
is EVAL(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => $decl),
        RakuAST::Statement::Expression.new(expression => RakuAST::Var::Lexical.new('$q')))).raku,
    'Any', 'a hand-built one lowers';
