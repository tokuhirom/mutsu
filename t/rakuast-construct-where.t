use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# RakuAST Phase 4 slice 11: programmatic construction of where-constrained
# parameters.

plan 6;

sub target($name) {
    RakuAST::ParameterTarget::Var.new(name => $name)
}

sub body-for($name) {
    my $statements = RakuAST::StatementList.new;
    $statements.add-statement(
        RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new($name),
        )
    );
    RakuAST::Blockoid.new($statements)
}

sub install($name, $parameter) {
    EVAL(
        RakuAST::Sub.new(
            name => RakuAST::Name.from-identifier($name),
            signature => RakuAST::Signature.new(parameters => [$parameter]),
            body => body-for('$x'),
        )
    )
}

my $constraint = RakuAST::IntLiteral.new(1);
my $parameter = RakuAST::Parameter.new(
    target => target('$x'),
    where => $constraint,
);

isa-ok $parameter, RakuAST::Parameter,
    'Parameter.new constructs a where-constrained parameter';
is $parameter.where, $constraint, 'the where accessor exposes the constraint';
is $parameter.where.value, 1, 'the exposed constraint remains walkable';
ok $parameter.gist.contains('where  => RakuAST::IntLiteral.new(1)'),
    'a where-constrained parameter renders like Rakudo';
ok RakuAST::Parameter.^methods(:local).grep(*.name eq 'where'),
    'Parameter model introspection advertises the where accessor';
lives-ok { install('constructed-where', $parameter) },
    'a where-constrained parameter lowers through EVAL';
