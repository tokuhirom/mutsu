use Test;

plan 10;

my $typed = Q|class Foo { has $.x }; my Foo $u .= new(x => 5)|.AST.statements[1].expression;
is $typed.initializer.^name, 'RakuAST::Initializer::CallAssign',
    'typed declaration retains the call-assignment initializer';
ok $typed.initializer.raku.contains('RakuAST::Call::Method.new('),
    'the initializer holds a method call without a synthesized invocant';
ok $typed.initializer.raku.contains('key   => "x"'),
    'named arguments remain on the method call';

is Q|my @a .= new(1, 2)|.AST.statements[0].expression.initializer.^name,
    'RakuAST::Initializer::CallAssign',
    'untyped aggregate declaration retains .= spelling';
is Q|my $x .= uc|.AST.statements[0].expression.initializer.^name,
    'RakuAST::Initializer::CallAssign',
    'untyped scalar declaration retains .= spelling';
ok !Q|my $x .= uc|.AST.statements[0].expression.initializer.raku.contains('ArgList'),
    'an argument-free method omits the argument list';

is EVAL(Q|class Foo { has $.x }; my Foo $u .= new(x => 5); $u.x|.AST), 5,
    'a typed call-assignment declaration round-trips';
is EVAL(Q|my @a .= new(1, 2); @a.join(",")|.AST), '1,2',
    'an untyped aggregate declaration round-trips';

my $initializer = RakuAST::Initializer::CallAssign.new(
    RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('new'))
);
my $declaration = RakuAST::VarDeclaration::Simple.new(
    sigil => '@',
    desigilname => RakuAST::Name.from-identifier('items'),
    initializer => $initializer,
);
is EVAL(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => $declaration)
)).^name, 'Array', 'a hand-built CallAssign initializer lowers';

{
    my Int $x;
    (my Int $y .= new).=new(42);
    is $y, 42, 'an inline declaration remains a writable lvalue';
}
