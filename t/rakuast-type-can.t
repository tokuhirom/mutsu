use Test;
use experimental :rakuast;

plan 10;

is RakuAST::IntLiteral.^can('new').elems, 1,
    'type object can find its supported constructor';
is RakuAST::IntLiteral.^can('value').elems, 1,
    'type object can find its positional accessor';
is RakuAST::ApplyInfix.^can('left').elems, 1,
    'type object can find a named-field accessor';
is RakuAST::StatementList.^can('add-statement').elems, 1,
    'type object can find its model mutator';
is RakuAST::Name.^can('from-identifier').elems, 1,
    'type object can find its named constructor';

is RakuAST::Assignment.^can('new').elems, 0,
    'can omits an unsupported model constructor';
is RakuAST::IntLiteral.^can('missing').elems, 0,
    'can rejects an unknown model method';

my $literal = RakuAST::IntLiteral.new(42);
is $literal.^can('value').elems, 1,
    'node value can find its accessor';
is $literal.^can('new').elems, 1,
    'node value shares its type object constructor metadata';
is $literal.^can('missing').elems, 0,
    'node value rejects an unknown model method';
