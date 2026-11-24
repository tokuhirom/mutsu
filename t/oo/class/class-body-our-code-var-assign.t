use Test;

# A class body's `our &t` assigned in a later body statement fills the
# package variable `&S::t`; it is not a read-only routine name (#19152).

plan 5;

class A { our &t; &t = sub { 3 } }
is &A::t(), 3, 'our &t declared then assigned: &A::t() calls it';
is A::t(), 3, '...and so does A::t()';

class B { our &t = sub { 1 }; &t = sub { 2 } }
is &B::t(), 2, 'reassigning an initialized our &t replaces it';

class C { our &t; &t = sub { 4 }; method m { &C::t() } }
is C.m, 4, 'a method sees the assigned package variable';

throws-like { EVAL 'class D { &nope = sub { 5 } }' }, Exception,
    'assigning an undeclared &name in a class body is still refused';
