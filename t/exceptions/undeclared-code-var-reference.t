use Test;

# `&name` of a routine nobody declared is rakudo's compile-time
# "Undeclared routine", as a call is; declared, imported, built-in, lexical
# `&` variables and parameters, and the non-identifier forms still resolve.

plan 10;

throws-like 'say &nosuch-routine', X::Undeclared::Symbols,
    message => /'Undeclared routine:' .* 'nosuch-routine'/, 'an undeclared &name';
throws-like 'class C { method m { &nosuch-two } }', X::Undeclared::Symbols,
    'in a method body too';

is EVAL('sub f { 2 }; &f()'), 2, 'a declared sub';
is EVAL('my $r = &h(); sub h { 4 }; $r'), 4, 'declared later in the unit';
is EVAL('my &v = { 1 }; &v()'), 1, 'a lexical & variable';
is EVAL('sub g(&cb) { &cb() }; g({ 3 })'), 3, 'a & parameter';
is EVAL('&uc("a")'), 'A', 'a built-in';
is EVAL('&infix:<+>(1, 2)'), 3, 'an operator name';
is EVAL('sub o { &?ROUTINE.name }; o()'), 'o', '&?ROUTINE';
is EVAL('module M { our sub q { 5 } }; &M::q()'), 5, 'a qualified name';
