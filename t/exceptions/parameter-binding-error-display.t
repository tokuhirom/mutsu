use Test;

plan 4;

sub can-turn-into(Str $, Any:U $) { }
try can-turn-into("a string", 123);
my $error = $!;
isa-ok $error, X::Parameter::InvalidConcreteness,
    'anonymous type-object parameter raises the expected exception';
is $error.message,
    "Parameter '<anon>' of routine 'can-turn-into' must be a type object of\ntype 'Any', not an object instance of type 'Int'. Did you forget a\n'multi'?",
    'the message names the anonymous parameter and suggests multi';

sub double(Int(Cool) $x) { 2 * $x }
try double Any;
$error = $!;
isa-ok $error, X::TypeCheck::Binding::Parameter,
    'coercion source mismatch raises a binding type error';
is $error.message,
    "Type check failed in binding to parameter '\$x'; expected Int(Cool) but got Any (Any)",
    'the coercion error names the parameter and shows the type object';
