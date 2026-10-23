use Test;

plan 8;

# `visit-children` walks the same child shape as Rakudo (#14696).
sub shape($node) {
    my @names;
    $node.visit-children(-> $c { @names.push($c.^name) });
    @names
}
sub find-first($node, $class) {
    return $node if $node.^name eq $class;
    my $found;
    $node.visit-children(-> $c { $found //= find-first($c, $class) });
    $found
}

my $ast = q[sub f($x) { $x + 1 }; class C { has $.a }].AST;

my $infix = find-first($ast, 'RakuAST::ApplyInfix');
is shape($infix).join(' '), 'RakuAST::Infix RakuAST::ArgList',
    'ApplyInfix visits its Infix, then an ArgList';
is shape(find-first($infix, 'RakuAST::ArgList')).join(' '),
    'RakuAST::Var::Lexical RakuAST::IntLiteral', 'the ArgList holds both operands';

my $target = find-first($ast, 'RakuAST::ParameterTarget::Var');
is shape($target).join(' '), 'RakuAST::VarDeclaration::Simple',
    'a ParameterTarget::Var visits its VarDeclaration::Simple';
is shape(find-first($target, 'RakuAST::VarDeclaration::Simple')).elems, 0,
    'an untyped parameter declaration has no children';

my $typed = q[sub g(Int $y) { }].AST;
my $typed-target = find-first($typed, 'RakuAST::ParameterTarget::Var');
is shape(find-first($typed-target, 'RakuAST::VarDeclaration::Simple')).join(' '),
    'RakuAST::Type::Simple', 'a typed parameter declaration carries the type';

my $attr = find-first(q[class D { has $.a }].AST, 'RakuAST::VarDeclaration::Simple');
is shape($attr).elems, 0, 'an attribute declaration visits no Name';

my $lex = find-first(q[my $v = 1].AST, 'RakuAST::VarDeclaration::Simple');
is shape($lex).join(' '), 'RakuAST::Initializer::Assign',
    'a lexical declaration visits its initializer, not its name';

lives-ok { $target.raku }, 'the visited target still renders';
