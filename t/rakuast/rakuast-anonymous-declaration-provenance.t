use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

for '__ANON_STATE__', '__ANON_STATE_9000__', '__ANON_STATE_PC_9000__' -> $name {
    for 'my', 'state' -> $scope {
        my $source = $scope ~ ' $' ~ $name ~ ' = 9; $' ~ $name;
        my $ast = $source.AST;
        my $decl = $ast.statements[0].expression;
        is $decl.^name, 'RakuAST::VarDeclaration::Simple',
            "$scope retains the explicit name $name";
        is $decl.name, '$' ~ $name, 'the declaration accessor keeps its name';
        is $decl.initializer.^name, 'RakuAST::Initializer::Assign',
            'the named declaration keeps its initializer';
        is EVAL($ast), 9, 'the named declaration evaluates after a round trip';
        is EVAL($source), 9, 'the ordinary frontend agrees on the named declaration';
    }
    my $state = 'sub f { state $' ~ $name ~ ' = 9; ++$' ~ $name ~ ' }; f(); f()';
    is EVAL($state.AST), 11, 'named state survives repeated routine calls';
    my $lexical = 'sub f { my $' ~ $name ~ ' = 9; ++$' ~ $name ~ ' }; f(); f()';
    is EVAL($lexical.AST), 10, 'named my is initialized on each routine call';
    my $uninitialized = 'state $' ~ $name ~ '; $' ~ $name ~ ' = 12; $' ~ $name;
    is $uninitialized.AST.statements.elems, 3,
        'an uninitialized named state is not removed as an implicit declaration';
    is EVAL($uninitialized.AST), 12, 'uninitialized named state remains assignable';
    my $mixed = 'sub f { state $' ~ $name ~ ' = 9; ++$' ~ $name ~ ' + $++ }; f(); f()';
    is EVAL($mixed.AST), 12, 'a named state and a bare scalar own different cells';
}

is EVAL(Q[my \alias = my $__ANON_STATE__ = 3; alias = 7; $__ANON_STATE__].AST), 7,
    'a sigilless alias does not rename an explicitly named initializer';
ok EVAL(Q[(my $__ANON_STATE__ = Nil) === Any].AST),
    'an inline named scalar applies ordinary Nil initialization';
is EVAL(Q[(my Int $__ANON_STATE__ = 3); $__ANON_STATE__ = 4; $__ANON_STATE__].AST), 4,
    'an inline named scalar retains its type constraint';
throws-like { EVAL(Q[(my Int $__ANON_STATE__ = 3); $__ANON_STATE__ = 'x'].AST) },
    X::TypeCheck::Assignment, 'the inline named scalar enforces its type';
is EVAL(Q[class C { has $.x = 7; method value { my $__ANON_STATE__ = 9; $.x + $__ANON_STATE__ } }; C.new.value].AST),
    16, 'an explicitly named scalar does not replace the method invocant';

for Q[my $ = 9], Q[state $ = 9] -> $source {
    is $source.AST.statements[0].expression.^name,
        'RakuAST::VarDeclaration::Anonymous', 'a bare declaration remains anonymous';
    is EVAL($source.AST), 9, 'an anonymous initializer evaluates';
}
for Q[my Int $ = 3], Q[state Int $ = 3], Q[my int $ = 3],
    Q[my Int $ := 3], Q[my Int $ .= new(3)] -> $source {
    my $node = $source.AST.statements[0].expression;
    is $node.^name, 'RakuAST::VarDeclaration::Anonymous',
        'a typed bare declaration remains anonymous';
    is $node.type.^name, 'RakuAST::Type::Simple', 'the anonymous declaration keeps its type';
    is EVAL($source.AST), 3, 'a typed anonymous initializer evaluates';
}
is Q[my Int $ := 3].AST.statements[0].expression.initializer.^name,
    'RakuAST::Initializer::Bind', 'an anonymous declaration preserves its binding initializer';
is Q[my Int $ .= new(3)].AST.statements[0].expression.initializer.^name,
    'RakuAST::Initializer::CallAssign', 'an anonymous declaration preserves its call-assignment initializer';
my $typed = RakuAST::VarDeclaration::Anonymous.new(
    sigil => '$', type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
    initializer => RakuAST::Initializer::Assign.new(RakuAST::IntLiteral.new(3))
);
is $typed.scope, 'my', 'an anonymous constructor defaults to my scope';
is EVAL($typed), 3, 'a constructed typed anonymous declaration lowers';
my $defaults = RakuAST::VarDeclaration::Anonymous.new(
    sigil => '$', type => RakuAST::Type, initializer => RakuAST::Initializer
);
nok $defaults.gist.contains('type'), 'an explicit default type is omitted from constructor text';
nok $defaults.gist.contains('initializer'), 'an explicit default initializer is omitted from constructor text';
is EVAL(Q[my $pair = a => my Int $ = 3; $pair.value = 4; $pair.value].AST), 4,
    'a typed anonymous scalar retains its writable container';
throws-like { EVAL(Q[my $pair = a => my Int $ = 3; $pair.value = 'x'].AST) },
    X::TypeCheck::Assignment, 'the typed anonymous container enforces its type';
is EVAL(Q[sub f { state $ = 9; ++$ }; f(); f()].AST), 2,
    'a separate bare scalar retains its own state cell';
is EVAL(Q[sub f { $++ }; f(); f()].AST), 1,
    'a bare scalar retains its implicit state declaration';

my $constructed = RakuAST::VarDeclaration::Simple.new(
    scope => 'state', sigil => '$',
    desigilname => RakuAST::Name.from-identifier('__ANON_STATE_9000__'),
    initializer => RakuAST::Initializer::Assign.new(RakuAST::IntLiteral.new(9))
);
is EVAL($constructed), 9, 'a constructed named declaration lowers with its initializer';
my $state_ast = Q[state $x = 1; ++$x].AST;
is EVAL($state_ast), 2, 'the first AST evaluation initializes its state';
is EVAL($state_ast), 2, 'another evaluation of the same AST owns fresh state';
done-testing;
