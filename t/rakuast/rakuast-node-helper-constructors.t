use Test;

# RakuAST helpers CSS::Specification::Compiler builds its grammars with.

plan 6;

my $t = RakuAST::Type::Simple.from-identifier-parts('A', 'B');
isa-ok $t, RakuAST::Type::Simple, 'Type::Simple.from-identifier-parts';
is $t.name.canonicalize, 'A::B', 'names the joined parts';
is RakuAST::Type::Simple.from-identifier('A').DEPARSE, 'A', 'Type::Simple.from-identifier';

# The ArgList type object is no arguments (`my RakuAST::ArgList $args .=
# new(|@args) if @args` leaves it when there are none).
my $call = RakuAST::Call::Method.new(
    name => RakuAST::Name.from-identifier('WHY'),
    args => RakuAST::ArgList,
);
is $call.DEPARSE, '.WHY', 'Call::Method takes the ArgList type object';

is RakuAST::Var::Compiler::Routine.new.name, '&?ROUTINE', 'Var::Compiler::Routine.name';
is RakuAST::Var::Compiler::Distribution.new.name, '$?DISTRIBUTION',
    'Var::Compiler::Distribution.name';
