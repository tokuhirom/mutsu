use Test;

# RakuAST::Node.needs-sink-call, measured on rakudo 2026.09: `True` by default,
# `False` for declarations and the nodes that are never sunk as a call, the
# Int `1` for a named call, and for ApplyInfix `0` (assignment, `:=`, `~~`,
# `does`, meta-assignment) / `1` (`xx`) / `True` otherwise.

plan 35;

my $pre = 'my $a; my $b; sub foo(|) { }; ';

# The answer of the first node of class $class in the tree of $code.
sub answer(Str $code, Str $class) {
    my $ast = ($pre ~ $code).AST;
    my @found;
    sub walk($n) {
        @found.push($n.needs-sink-call) if $n.^name eq $class;
        $n.visit-children(&walk);
    }
    walk($_) for $ast.statements[3 .. *];
    @found[0];
}

# default: True
for ('RakuAST::IntLiteral' => '1',
    'RakuAST::Var::Lexical' => '$a',
    'RakuAST::ApplyPrefix' => '-$a',
    'RakuAST::ApplyListInfix' => '1, 2',
    'RakuAST::Call::Method' => '$a.foo',
    'RakuAST::Block' => 'if 1 { }',
    'RakuAST::Statement::If' => 'if 1 { }',
    'RakuAST::StatementList' => 'if 1 { }')
    -> (:key($class), :value($code)) {
    is answer($code, $class), True, "$class needs a sink call";
}

# declarations and never-sunk nodes: False
for ('RakuAST::Statement::Expression' => '1',
    'RakuAST::Ternary' => '1 ?? 2 !! 3',
    'RakuAST::Sub' => 'sub f { }',
    'RakuAST::Class' => 'class C { }',
    'RakuAST::Module' => 'module M { }',
    'RakuAST::Role' => 'role R { }',
    'RakuAST::Statement::Loop::While' => 'while 1 { }',
    'RakuAST::Statement::Loop::Until' => 'until 1 { }',
    'RakuAST::Statement::Loop::RepeatWhile' => 'repeat { } while 1',
    'RakuAST::VarDeclaration::Simple' => 'my $x = 1',
    'RakuAST::VarDeclaration::Constant' => 'constant C = 1',
    'RakuAST::ApplyDottyInfix' => '$a .= foo',
    'RakuAST::Type::Enum' => 'enum E <a b>',
    'RakuAST::Statement::Require' => 'require Foo')
    -> (:key($class), :value($code)) {
    is answer($code, $class), False, "$class does not need a sink call";
}

# Call::Name answers the Int 1
is-deeply answer('foo()', 'RakuAST::Call::Name'), 1, 'Call::Name answers Int 1';
is-deeply answer('foo 1', 'RakuAST::Call::Name::WithoutParentheses'), 1,
    'Call::Name::WithoutParentheses answers Int 1';

# ApplyInfix
is-deeply answer('1 + 2', 'RakuAST::ApplyInfix'), True, 'plain infix: True';
is-deeply answer('$a ~ $b', 'RakuAST::ApplyInfix'), True, '~: True';
is-deeply answer('$a = 1', 'RakuAST::ApplyInfix'), 0, '=: Int 0';
is-deeply answer('$a := 1', 'RakuAST::ApplyInfix'), 0, ':=: Int 0';
is-deeply answer('$a ~~ 1', 'RakuAST::ApplyInfix'), 0, '~~: Int 0';
is-deeply answer('$a += 1', 'RakuAST::ApplyInfix'), 0, '+=: Int 0';
is-deeply answer('$a xx 2', 'RakuAST::ApplyInfix'), 1, 'xx: Int 1';
is-deeply answer('$a >>~~<< $b', 'RakuAST::ApplyInfix'), 0, 'hyper ~~: Int 0';
is-deeply answer('$a >>+<< $b', 'RakuAST::ApplyInfix'), True, 'hyper +: True';
is-deeply answer('$a R~~ $b', 'RakuAST::ApplyInfix'), 0, 'reversed ~~: Int 0';
is-deeply answer('$a !~~ $b', 'RakuAST::ApplyInfix'), True, 'negated ~~: True';

done-testing;
