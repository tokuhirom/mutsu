use Test;
plan 3;

my $capturing = role CaptureRole[::T] {};
is $capturing.^name, 'CaptureRole',
    'a parametric role with a type capture parses as an expression term';

my $typed = role TypedRole[Int $n] {};
is $typed.^name, 'TypedRole',
    'a parametric role with a typed value parameter parses as an expression term';

ok $typed.HOW.defined,
    'an expression-position parametric role produces a role type object';
