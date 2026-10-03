use v6.d;
use Test;

# Before v6.e, `last` / `next` / `redo` take no argument or a `Label` only
# (#11073). A literal argument no candidate can bind is rakudo's compile-time
# `X::TypeCheck::Argument`; any other non-Label argument reaches the routine
# at run time and is `X::Multi::NoMatch`.

plan 6;

throws-like 'last(5)', X::TypeCheck::Argument,
    message => /'Calling last(Int) will never work with any of these multi signatures'/,
    'last(Int literal) fails at compile time';
throws-like 'for ^2 { next "a" }', X::TypeCheck::Argument,
    message => /'Calling next(Str) will never work'/,
    'next Str-literal fails at compile time';
throws-like 'redo(1.5)', X::TypeCheck::Argument,
    'redo(Rat literal) fails at compile time';

my $v = 5;
throws-like { for ^2 { last($v) } }, X::Multi::NoMatch,
    message => /'Cannot resolve caller last(Int:D)'/,
    'last($non-label) is X::Multi::NoMatch at run time';
throws-like { for ^2 { next $v } }, X::Multi::NoMatch,
    'next $non-label is X::Multi::NoMatch at run time';

my $seen = 0;
LBL: for ^3 { for ^3 { $seen++; next(LBL) } }
is $seen, 3, 'next(LABEL) still works';
