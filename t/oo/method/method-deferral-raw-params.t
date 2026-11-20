use Test;
plan 9;

# A deferral frame of a method whose candidates take `is raw` / sigilless
# parameters is built when a deferral builtin first asks (#18267); every
# deferral form must still see the call's arguments and the next candidate.
class A {
    method m(Mu $x is raw) { "A:" ~ $x }
    method s(\x) { "A:" ~ x }
    method w(Mu $x is raw) { $x = 'set-by-A'; 'A' }
}
class B is A {
    method m(Mu $x is raw) { "B>" ~ callsame() }
    method s(\x) { "B>" ~ nextsame }
    method w(Mu $x is raw) { 'B>' ~ callsame() }
}
class C is B {
    method m(Mu $x is raw) { "C>" ~ callwith($x ~ '!') }
    method s(\x) { "C>" ~ nextwith(x ~ '?') }
}
is B.new.m(1), 'B>A:1', 'callsame forwards a raw argument';
is B.new.s(2), 'A:2', 'nextsame forwards a sigilless argument';
is C.new.m(3), 'C>B>A:3!', 'callwith, then callsame from the next candidate';
is C.new.s(4), 'A:4?', 'nextwith, then nextsame from the next candidate';
my $v = 'orig';
is B.new.w($v), 'B>A', 'callsame reaches the next raw candidate';
is $v, 'set-by-A', 'the next candidate writes the caller variable through is raw';

class M {
    multi method d(Mu $x is raw where Int) { 'int>' ~ callsame }
    multi method d(Mu $x is raw) { 'any:' ~ $x }
}
is M.new.d(5), 'int>any:5', 'a raw multi defers to the next matching candidate';

class R {
    method r($x is rw) { $x = 'R' }
}
class R2 is R {
    method r($x is rw) { callsame }
}
my $w = 'w';
R2.new.r($w);
is $w, 'R', 'an is rw candidate still matches a variable argument';
sub helper { callsame }
class H is A { method m(Mu $x is raw) { 'H>' ~ helper() } }
is H.new.m(7), 'H>A:7', 'a deferral from a helper sub reads the method frame';
