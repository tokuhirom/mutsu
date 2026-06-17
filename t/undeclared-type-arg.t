use Test;

plan 5;

# An undeclared type used as a type-parameter argument is X::Undeclared::Symbols.
throws-like Q/my Array[Numerix] $x;/, X::Undeclared::Symbols,
    'undeclared type arg in a variable declaration',
    gist => /Numerix/;
throws-like 'sub f(Array[NoSuchT] $x) { }', X::Undeclared::Symbols,
    'undeclared type arg in a parameter';

# Declared / built-in type args are fine (forward references too).
{
    lives-ok { EVAL 'my Array[Int] @a;' }, 'built-in type arg is allowed';
    lives-ok { EVAL 'class C { }; my Array[C] @a;' },
        'declared-class type arg is allowed';
    lives-ok { EVAL 'my Array[D] @a; class D { }' },
        'forward-referenced type arg is allowed';
}
