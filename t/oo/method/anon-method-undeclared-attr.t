use v6;
use Test;

plan 8;

# An undeclared attribute used inside an *anonymous* method (`method { $!x }`)
# must raise X::Attribute::Undeclared at composition time, just like a named
# method does. And X::Attribute::Undeclared does X::Comp (it is a compile error).

# (1) anonymous method, undeclared private attribute
{
    my $ex;
    try { EVAL q[ unit class A; method { $!x } ]; CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::Attribute::Undeclared, 'anon method: undeclared $!x is X::Attribute::Undeclared';
    is $ex.message, 'Attribute $!x not declared in class A', 'anon method: right message';
    ok $ex ~~ X::Comp, 'anon method: X::Attribute::Undeclared does X::Comp';
}

# (2) named method still detects it (unchanged) and also does X::Comp
{
    my $ex;
    try { EVAL q[ class B { method m { $!y } } ]; CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::Attribute::Undeclared, 'named method: undeclared $!y is X::Attribute::Undeclared';
    ok $ex ~~ X::Comp, 'named method: does X::Comp';
}

# (3) a *declared* attribute inside an anonymous method must NOT error
{
    class C {
        has $.v = 42;
        method { $!v }   # anonymous method referencing a declared attribute
    }
    is C.new.v, 42, 'anon method referencing a declared attribute is fine';
}

# (4) an anonymous method with no attribute reference must NOT error
{
    lives-ok { EVAL q[ class D { method { 99 } } ] },
        'anon method with no attribute reference composes cleanly';
}

# (5) a runtime error does NOT do X::Comp (sanity for the hierarchy change)
{
    my $ex;
    try { die 42; CATCH { default { $ex = $_ } } }
    nok $ex ~~ X::Comp, 'a plain runtime die does not do X::Comp';
}
