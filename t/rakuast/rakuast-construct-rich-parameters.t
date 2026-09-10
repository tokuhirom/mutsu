use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# RakuAST Phase 4 slice 10: programmatic construction of typed, defaulted,
# optional, named, and slurpy parameters.

plan 14;

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

sub install($name, $parameter, $body-name) {
    EVAL(
        RakuAST::Sub.new(
            name => RakuAST::Name.from-identifier($name),
            signature => RakuAST::Signature.new(parameters => [$parameter]),
            body => body-for($body-name),
        )
    )
}

my $int-type = RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier('Int')
);
isa-ok $int-type, RakuAST::Type::Simple, 'Type::Simple.new constructs a type';
is $int-type.name.gist, RakuAST::Name.from-identifier('Int').gist,
    'a constructed simple type exposes its name';

my $typed = RakuAST::Parameter.new(
    type => $int-type,
    target => target('$x'),
);
is $typed.type, $int-type, 'a typed parameter exposes its type';
is $typed.target.name, '$x', 'a typed parameter exposes its target';
ok $typed.gist.contains('type   => RakuAST::Type::Simple.new('),
    'a typed parameter renders like Rakudo';
lives-ok { install('constructed-typed', $typed, '$x') },
    'a typed parameter lowers through EVAL';

my $defaulted = RakuAST::Parameter.new(
    target => target('$x'),
    default => RakuAST::IntLiteral.new(7),
);
is $defaulted.default.value, 7, 'a defaulted parameter exposes its default';
lives-ok { install('constructed-default', $defaulted, '$x') },
    'a defaulted parameter lowers through EVAL';

my $optional = RakuAST::Parameter.new(
    target => target('$x'),
    optional => True,
);
is $optional.optional, True, 'an optional parameter exposes its marker';
lives-ok { install('constructed-optional', $optional, '$x') },
    'an optional parameter lowers through EVAL';

my $named = RakuAST::Parameter.new(
    names => ['value'],
    target => target('$value'),
);
is $named.names[0], 'value', 'a named parameter exposes its accepted name';
lives-ok { install('constructed-named', $named, '$value') },
    'a named parameter lowers through EVAL';

my $slurpy = RakuAST::Parameter.new(
    target => target('@values'),
    slurpy => RakuAST::Parameter::Slurpy::Flattened,
);
is $slurpy.slurpy.^name, 'RakuAST::Parameter::Slurpy::Flattened',
    'a slurpy parameter exposes its marker';
lives-ok { install('constructed-slurpy', $slurpy, '@values') },
    'a slurpy parameter lowers through EVAL';
