use Test;

# A `where` clause that dies while dispatch matches candidates fails the call:
# the exception is returned through the matchers to the dispatch that ran
# them (ADR-10779 D3), not parked on the interpreter for a later op to raise.
# Outputs checked against rakudo.

plan 7;

multi f(Int $x where { die "fboom" }) { 1 }
multi f(Str $x) { 2 }
is f("s"), 2, 'a candidate that does not reach the dying where still wins';
throws-like { f(3) }, Exception, message => 'fboom', 'a multi sub where that dies fails the call';

sub g(Int $x where { die "gboom" }) { 1 }
throws-like { g(3) }, Exception, message => 'gboom', 'an only sub where that dies fails the call';

class C {
    multi method m(Int $x where { die "mboom" }) { 1 }
    multi method m(Str $x) { 2 }
    method !p(Int $x where { die "pboom" }) { 1 }
    method go { self!p(1) }
}
throws-like { C.m(3) }, Exception, message => 'mboom', 'a multi method where that dies fails the call';
throws-like { C.go }, Exception, message => 'pboom', 'a private method where that dies fails the call';

throws-like { &f.cando(\(1)) }, Exception, message => 'fboom', 'cando runs the where and dies with it';

my $after = try { f(3) };
is f("t"), 2, 'a caught where death leaves nothing behind for the next call';
