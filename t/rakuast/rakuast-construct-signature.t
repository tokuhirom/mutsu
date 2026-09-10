use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# RakuAST Phase 4 slice 8: construction of a plain positional signature.

plan 15;

my $target = RakuAST::ParameterTarget::Var.new(name => '$x');
isa-ok $target, RakuAST::ParameterTarget::Var,
    'ParameterTarget::Var.new constructs a variable target';
is $target.name, '$x', 'the target exposes its name';

my $parameter = RakuAST::Parameter.new(target => $target);
isa-ok $parameter, RakuAST::Parameter, 'Parameter.new constructs a parameter';
is $parameter.target, $target, 'the parameter exposes its target';

my $signature = RakuAST::Signature.new(parameters => [$parameter]);
isa-ok $signature, RakuAST::Signature, 'Signature.new constructs a signature';
is $signature.parameters.elems, 1, 'the signature contains one parameter';
is $signature.parameters[0], $parameter,
    'the signature exposes the constructed parameter';
is $signature.gist, q:to/END/.chomp, 'the signature renders like Rakudo';
    RakuAST::Signature.new(
      parameters => (
        RakuAST::Parameter.new(
          target => RakuAST::ParameterTarget::Var.new(
            name => "\$x"
          )
        ),
      )
    )
    END

my @signature-methods = RakuAST::Signature.^methods(:local)>>.name;
ok 'new' (elem) @signature-methods && 'parameters' (elem) @signature-methods,
    'Signature introspection exposes its constructor and accessor';
my @parameter-methods = RakuAST::Parameter.^methods(:local)>>.name;
ok 'new' (elem) @parameter-methods && 'target' (elem) @parameter-methods,
    'Parameter introspection exposes its constructor and accessor';
my @target-methods = RakuAST::ParameterTarget::Var.^methods(:local)>>.name;
ok 'new' (elem) @target-methods && 'name' (elem) @target-methods,
    'ParameterTarget::Var introspection exposes its constructor and accessor';

my $statements = RakuAST::StatementList.new;
$statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
    )
);
my $routine = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('answer'),
    signature => $signature,
    body => RakuAST::Blockoid.new($statements),
);
is $routine.signature, $signature, 'Sub.new accepts and exposes a signature';
lives-ok { EVAL($routine) }, 'a constructed Sub with a signature lowers through EVAL';

my $empty = RakuAST::Signature.new;
is $empty.parameters.elems, 0, 'Signature.new defaults to no parameters';
lives-ok {
    RakuAST::Sub.new(
        name => RakuAST::Name.from-identifier('empty-signature'),
        signature => $empty,
    )
}, 'an empty constructed signature can be attached to a Sub';
