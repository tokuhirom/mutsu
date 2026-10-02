use Test;

# #10935: the first call of a non-multi method with a subset-typed (or
# `where`-constrained) parameter used to run the predicate twice -- once while
# resolving the method, once in the binder. Rakudo checks it once per bind.

plan 7;

my $c = 0;
subset P of Int where { $c++; True };

class Q { method m(P $x) { } }
Q.m(1);
is $c, 1, 'first type-object call runs the predicate once';
Q.new.m(1);
is $c, 2, 'first instance call runs it once';
Q.m(1);
is $c, 3, 'a later call runs it once';

class A { method m(P $x) { }; method w($x where { $c++; True }) { } }
class B is A { }
B.m(1);
is $c, 4, 'inherited method: predicate runs once';
B.new.w(1);
is $c, 5, 'inherited method with a where clause: predicate runs once';

role R { method r(P $x) { } }
class C does R { }
C.r(1);
is $c, 6, 'role method: predicate runs once';

# The binder still enforces the constraint.
throws-like { A.m("x") }, X::TypeCheck::Binding::Parameter,
    'a failing argument still dies in the binder';
