use Test;

# A definite/undefined type object term (`Str:D`, `Int:U`, `Str:_`) inside
# EVAL'd code is a known name exactly when its base type is; EVAL's
# undeclared-name check used to reject it outright. mutsu#10814.

plan 6;

is EVAL(q[Str:D.raku]), 'Str:D', 'a core :D type object';
is EVAL(q[Int:U.raku]), 'Int:U', 'a core :U type object';
is EVAL(q[Str:_.raku]), 'Str', 'a :_ type object is the plain type';
is EVAL(q[class K { }; K:D.raku]), 'K:D', 'a type the snippet declares';
is EVAL(q[my $x = Str:D; $x.^name]), 'Str:D', 'as an initializer';
throws-like q[EVAL q[Nope:D]], X::Undeclared::Symbols,
    'an undeclared base type is still rejected';
