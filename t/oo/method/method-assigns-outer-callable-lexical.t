use v6.d;
use Test;

# A method body may assign through an enclosing `my &k` Callable container;
# `&k = ...` is only a read-only error when `k` names a routine.
# Found via MVC::Keayl t/job/job-features.rakutest (Job.reset-clock).

plan 4;

my &c = sub { 1 };
class A {
    method reset() { &c = sub { 3 }; }
}
A.reset;
is c(), 3, 'method assigns an outer file-scope my &c';

class B {
    my &d = sub { 1 };
    method set(&n) { &d = &n; }
    method get() { &d }
}
B.set(sub { 7 });
is B.get.(), 7, 'method assigns a class-body my &d';

throws-like { EVAL 'sub foo() { 1 }; class C { method m() { &foo = sub { 2 } } }; C.m' },
    Exception, 'assigning to a routine name from a method still fails';

sub plain() { 1 }
throws-like { EVAL '&plain = sub { 2 }' }, Exception, 'assigning to a routine name still fails';
