use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST positional/destructuring sub-signatures. The parser already keeps
# these in ParamDef.sub_signature, so this slice covers the model conversion,
# construction, and lowering without changing the execution pipeline.

plan 11;

my $ast = Q[sub f($x ($a, $b)) { $a + $b }].AST;
my $gist = $ast.gist;
ok $gist.contains('sub-signature => RakuAST::Signature.new('),
    'a positional sub-signature renders as a nested Signature';
is $ast.statements[0].expression.signature.parameters[0].sub-signature.^name,
    'RakuAST::Signature',
    'the sub-signature accessor returns a Signature node';
is $ast.statements[0].expression.signature.parameters[0].sub-signature.parameters.elems,
    2,
    'the nested Signature exposes its parameters';
my @inner-parameters = $ast.statements[0].expression.signature.parameters[0]
    .sub-signature.parameters;
is @inner-parameters[0].type.^name,
    'RakuAST::Type::Setting',
    'nested routine parameters retain the implicit Any type';
my @parameter-methods = RakuAST::Parameter.^methods(:local)>>.name;
ok 'sub-signature' (elem) @parameter-methods,
    'Parameter introspection exposes the sub-signature accessor';

is EVAL(Q[sub f($x ($a, $b)) { $a + $b }; f([10, 32])].AST),
    42,
    'a positional sub-signature round-trips through EVAL';

my $array-ast = Q[sub g(@x [$a, $b]) { $a + $b }].AST;
is $array-ast.statements[0].expression.signature.parameters[0].target.name,
    '@x',
    'an array destructuring sub-signature retains its array target';
ok $array-ast.gist.contains('sub-signature => RakuAST::Signature.new('),
    'an array destructuring sub-signature renders recursively';
is EVAL(Q[sub g(@x [$a, $b]) { $a + $b }; g([10, 32])].AST),
    42,
    'an array destructuring sub-signature executes after EVAL';

sub target($name) {
    RakuAST::ParameterTarget::Var.new(name => $name)
}

sub parameter($name) {
    RakuAST::Parameter.new(target => target($name))
}

my $inner = RakuAST::Signature.new(
    parameters => [parameter('$a'), parameter('$b')],
);
my $outer = RakuAST::Parameter.new(
    target => target('$x'),
    sub-signature => $inner,
);
is $outer.sub-signature, $inner,
    'Parameter.new accepts and exposes a constructed sub-signature';

my $sum = RakuAST::ApplyInfix.new(
    left => RakuAST::Var::Lexical.new('$a'),
    infix => RakuAST::Infix.new('+'),
    right => RakuAST::Var::Lexical.new('$b'),
);
my $statements = RakuAST::StatementList.new;
$statements.add-statement(
    RakuAST::Statement::Expression.new(expression => $sum),
);
my $body = RakuAST::Blockoid.new($statements);
my $constructed = RakuAST::Sub.new(
    signature => RakuAST::Signature.new(parameters => [$outer]),
    body => $body,
);
my $callable = EVAL($constructed);
is $callable([10, 32]), 42,
    'a constructed sub-signature lowers and executes';
