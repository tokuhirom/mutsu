use v6;
use Test;

# A routine forward-declared as a `{...}` stub is completed, not redeclared, by
# the definition that follows, also in an EVAL'd unit.

plan 2;

is EVAL(q[sub lightning {...}; sub lightning { 42 }; lightning()]), 42,
    'a stub is completed by its definition';
throws-like q[sub twice { 1 }; sub twice { 2 }], X::Redeclaration,
    'two definitions are still a redeclaration';
