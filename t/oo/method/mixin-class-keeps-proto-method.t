use Test;

# `Class.^mixin(Role)` keeps the class's `proto method` body (rakudo's
# RakuAST::Deparse sets its dynamics in `proto method deparse` and is
# specialised with L10N roles through `.^mixin`).
plan 3;

class A {
    proto method d($x) { my $*I := 7; {*} }
    multi method d(Int:D $x) { $*I }
    multi method d(Str:D $x) { "s" ~ $*I }
}
role R { method k { 1 } }
my $c := A.^mixin(R);
is $c.d(5), 7, 'the proto body runs for a mixed-in type object';
is $c.d("a"), 's7', 'and the multi candidate it dispatches to sees its dynamic';
class B is A { }
is B.d(5), 7, 'a subclass keeps working';
