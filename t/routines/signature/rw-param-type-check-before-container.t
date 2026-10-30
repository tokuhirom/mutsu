use v6;
use Test;

plan 9;

# rakudo checks a parameter's nominal type before the `is rw` container
# requirement: a literal of the wrong type is a type error, not X::Parameter::RW.
class A { method h(Str:D $key is rw) { 1 } }
try A.new.h(123);
isa-ok $!, X::TypeCheck::Binding::Parameter, 'method: wrong literal type is a binding type error';
is $!.message, 'Type check failed in binding to parameter \'$key\'; expected Str but got Int (123)',
    'its message names the expected type';

sub f(Int $x is rw) { 1 }
try f("a");
isa-ok $!, X::TypeCheck::Binding::Parameter, 'sub: wrong literal type';
is $!.message, 'Type check failed in binding to parameter \'$x\'; expected Int but got Str ("a")',
    'sub message';

try f(5);
isa-ok $!, X::Parameter::RW, 'a right-typed literal is still X::Parameter::RW';

my $s = "a";
try f($s);
isa-ok $!, X::TypeCheck::Binding::Parameter, 'a variable of the wrong type';

sub m(Int :$x! is rw) { 1 }
try m(:x("s"));
isa-ok $!, X::TypeCheck::Binding::Parameter, 'named: wrong literal type';
try m(:x(1));
isa-ok $!, X::Parameter::RW, 'named: right-typed literal is X::Parameter::RW';

my Int $i = 3;
is f($i), 1, 'a right-typed variable binds';
