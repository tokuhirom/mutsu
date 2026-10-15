use Test;

# `$?CLASS` names a class only inside its own body (ADR-12529 phase 3). A
# class body used to leave it bound in the enclosing scope, so the mainline
# read the last declared class, and every call's return merge carried a
# callee's `$?CLASS` back into its caller.
plan 7;

class A {
    our sub f { $?CLASS.^name }
    method m { $?CLASS.^name }
    our &h = -> { $?CLASS.^name };
}
class B { method m { $?CLASS.^name } }

is A::f(), 'A', 'an our sub of the class body sees its class';
is A.m, 'A', 'so does a method';
is A::<&h>(), 'A', 'and a closure made in the body';
is B.m, 'B', 'a later class body binds its own';

nok ::('$?CLASS').defined, 'the mainline has no $?CLASS after the class bodies';

sub plain { 1 }
class C { method m { plain(); $?CLASS.^name } }
is C.m, 'C', 'a sub call in a method leaves the method\'s $?CLASS alone';
nok (try ::('$?CLASS')).defined, 'nor do the calls give the mainline one';
