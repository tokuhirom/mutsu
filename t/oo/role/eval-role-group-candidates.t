use v6;
use Test;

# Roles of one name with different parameter lists are the candidates of one
# role group, also in an EVAL'd unit.

plan 3;

is EVAL(q[role R[::T] { method m { 1 } }; role R { method m { 2 } }; R.new.m]), 2,
    'a parametric role and a plain role of one name coexist';
is EVAL(q[role S[Int $x] { method m { 'int' } }; role S[Str $x] { method m { 'str' } }; S[Int].new.m ~ S[Str].new.m]), 'intstr',
    'two parametric roles with different parameter types coexist';
is EVAL(q[role U[::T] { method m { 1 } }; role U[::T, ::V] { method m { 2 } }; U[Int].new.m ~ U[Int, Str].new.m]), '12',
    'parametric roles of different arity coexist';
