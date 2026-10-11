use Test;

# The name of an `anon class C { }` is installed nowhere, not even by the
# compile-time shell that installs an `our` class declared inside code
# (#10524): a later reference to `C` is an undeclared name, at the top level
# and inside EVAL alike.

plan 3;

throws-like q[anon class C { }; C.WHAT; ], X::Undeclared::Symbols,
    'the anon class name is undeclared in EVAL';
throws-like q[sub f { class Inner { } }; anon class C { }; C.WHAT; ], X::Undeclared::Symbols,
    'also next to a class nested in a routine';
my $c = anon class Named { method m { 'm' } };
is $c.m, 'm', 'the anon class itself still works';
