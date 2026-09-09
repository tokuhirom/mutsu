use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST type captures. The parser and binder already preserve and execute a
# basic `::T` capture; this slice exposes that representation in both RakuAST
# directions without adding another execution path.

plan 17;

my $ast = Q[sub f(::T $x) { $x.^name }].AST;
my $parameter = $ast.statements[0].expression.signature.parameters[0];
my $capture = ($parameter.type-captures)[0];

is $parameter.type-captures.elems, 1,
    'a type-capture parameter exposes one type capture';
is $capture.^name, 'RakuAST::Type::Capture',
    'the type capture has the measured RakuAST class';
is $capture.name.gist, RakuAST::Name.from-identifier('T').gist,
    'the type capture exposes its captured name';
ok $ast.gist.contains('type-captures => (')
    && $ast.gist.contains('RakuAST::Type::Capture.new('),
    'the read direction renders Parameter.type-captures';
ok 'name' (elem) RakuAST::Type::Capture.^methods(:local)>>.name,
    'Type::Capture introspection exposes its name accessor';
ok 'type-captures' (elem) RakuAST::Parameter.^methods(:local)>>.name,
    'Parameter introspection exposes type-captures';

is EVAL(Q[sub f(::T $x) { $x.^name }; f(42)].AST), 'Int',
    'a scalar argument binds through a read-direction type capture';
is EVAL(Q[sub f(::T $x) { $x.^name }; f('text')].AST), 'Str',
    'a string argument binds through a read-direction type capture';
is EVAL(Q[sub f(::T :$x) { $x.^name }; f(:x(42))].AST), 'Int',
    'a named argument retains its type capture';

my $bare = Q[sub f(::T) { 1 }].AST;
ok !$bare.statements[0].expression.signature.parameters[0].gist.contains('target =>'),
    'a bare type capture has no fabricated parameter target';
is EVAL(Q[sub f(::T) { 1 }; f(Int)].AST), 1,
    'a bare type capture lowers through EVAL';

my $pointy = Q[-> ::T $x { $x.^name }].AST;
is (($pointy.statements[0].expression.signature.parameters[0]
    .type-captures)[0]).^name,
    'RakuAST::Type::Capture',
    'pointy-block type captures use the same model node';
is EVAL(Q[my $f = -> ::T $x { $x.^name }; $f(42)].AST), 'Int',
    'a pointy-block type capture lowers through EVAL';

my $tc = RakuAST::Type::Capture.new(
    RakuAST::Name.from-identifier('T'),
);
is $tc.name.gist, RakuAST::Name.from-identifier('T').gist,
    'Type::Capture.new constructs the measured node';

my $constructed-parameter = RakuAST::Parameter.new(
    type-captures => [$tc],
    target => RakuAST::ParameterTarget::Var.new(name => '$x'),
);
is ($constructed-parameter.type-captures)[0], $tc,
    'Parameter.new accepts and exposes a type-capture list';
ok $constructed-parameter.gist.contains('type-captures => ('),
    'constructed type-capture parameters render their field';

my $statements = RakuAST::StatementList.new;
$statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$x'),
    ),
);
my $constructed = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('constructed-type-capture'),
    signature => RakuAST::Signature.new(parameters => [$constructed-parameter]),
    body => RakuAST::Blockoid.new($statements),
);
my $callable = EVAL($constructed);
is $callable(42).^name, 'Int',
    'a constructed type-capture parameter lowers and executes';
