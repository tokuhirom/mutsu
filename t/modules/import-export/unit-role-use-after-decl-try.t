use v6;
use Test;
use lib 't/lib-unit-role-use-after-decl';
use UnitRoleR;

plan 3;

class C does UnitRoleR { }

# A `use` written after `unit role` binds a routine the role's methods call,
# also from inside a `try` body (an anonymous block frame).
is C.new.in-try, 7, 'imported routine resolves in `try EXPR` of a unit role method';
is C.new.in-try-block, 7, 'imported routine resolves in `try { }` of a unit role method';
is C.new.in-catch, 7, 'imported routine resolves in a try body that has a CATCH';
