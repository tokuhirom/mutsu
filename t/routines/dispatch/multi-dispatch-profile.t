use v6;
use Test;

plan 4;

multi f(Int $x) { }
multi f(Str $x, :$y!) { }
try f(Any, :z(1));
is $!.message,
    "Cannot resolve caller f(Any:U, :z(Int)); none of these signatures matches:\n"
    ~ "    (Int \$x)\n"
    ~ "    (Str \$x, :\$y!)",
    'a multi dispatch error includes the type object, named argument, and required named parameter';

multi g(Int $x, :$y = 5) { }
multi g(Str $x?) { }
try g(Any, :z(1));
is $!.message,
    "Cannot resolve caller g(Any:U, :z(Int)); none of these signatures matches:\n"
    ~ "    (Int \$x, :\$y = 5)\n"
    ~ "    (Str \$x?)",
    'candidate signatures retain defaults and optional markers';

try f('text', :z(1));
like $!.message, /'f(Str:D, :z(Int))'/,
    'defined positional arguments retain their :D smiley';

my $value = 'text';
try f($value);
like $!.message, /'f(Str:D)'/,
    'a variable argument reports the type of its value';
