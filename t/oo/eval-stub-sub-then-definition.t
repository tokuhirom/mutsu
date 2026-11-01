use v6;
use Test;

# A routine forward-declared as a `{...}` stub is completed, not redeclared, by
# the definition that follows, also in an EVAL'd unit; a block (an `if` body
# included) may shadow a `my &name` of the enclosing scope.

plan 4;

is EVAL(q[sub lightning {...}; sub lightning { 42 }; lightning()]), 42,
    'a stub is completed by its definition';
throws-like q[sub twice { 1 }; sub twice { 2 }], X::Redeclaration,
    'two definitions are still a redeclaration';
is EVAL(q[my &h = sub { 'outer' }; my $r; if True { sub h { 'inner' }; $r = h() }; $r ~ h()]), 'innerouter',
    'an if body sub shadows an outer my &h';
is EVAL(q[{ proto sub s($) {*}; multi sub s(Int $x) { 'o' }; { proto sub s($) {*}; multi sub s(Int $x) { 'i' }; } }; 1]), 1,
    'a proto+multi family in an inner block shadows the outer one';
