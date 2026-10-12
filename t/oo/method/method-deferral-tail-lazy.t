use Test;

# The candidates after a resolved method's winner are built only when a
# deferral builtin reads the frame (#12875). Every shape below must still see
# exactly the deferral chain an eager build would have given it.

plan 12;

# A multi candidate's `callsame` reaches the parent's matching candidate.
class A1 { multi method m(Int $x) { "A1-Int($x)" } }
class B1 is A1 {
    multi method m(Int $x) { "B1-Int," ~ callsame }
    multi method m(Str $x) { "B1-Str($x)" }
}
is B1.new.m(3), 'B1-Int,A1-Int(3)', 'callsame from a multi reaches the next matching candidate';
is B1.new.m('s'), 'B1-Str(s)', 'a non-matching candidate is not in the way';

# `callsame` with nothing left in its own chain falls back to Nil.
class C1 {
    multi method m(Int) { 'C1-Int' ~ (callsame() // '<nil>') }
    multi method m(Str) { 'C1-Str' }
}
is C1.new.m(1), 'C1-Int<nil>', 'callsame past the last matching candidate answers Nil';

# `nextcallee` hands back the next candidate as a callable.
class A2 { method m($x) { "A2($x)" } }
class B2 is A2 { method m($x) { my &next = nextcallee; "B2," ~ next(self, $x + 1) } }
is B2.new.m(1), 'B2,A2(2)', 'nextcallee returns the parent candidate';

# A frame whose tail is never read, nested inside one that is.
class A3 { method outer() { 'A3-outer' } }
class B3 is A3 {
    multi method inner(Int) { 'inner-Int' }
    multi method inner(Str) { 'inner-Str' }
    method outer() { self.inner(1) ~ ',' ~ callsame }
}
is B3.new.outer, 'inner-Int,A3-outer', 'an unread nested frame leaves the outer chain intact';

# `lastcall` empties the chain.
class A4 { method m() { 'A4' } }
class B4 is A4 { method m() { lastcall; 'B4' ~ (callsame() // '<nil>') } }
is B4.new.m, 'B4<nil>', 'lastcall drops the lazily built tail';

# callwith hands the next candidate new arguments.
class A5 {
    multi method m(Int $x) { "A5-Int($x)" }
    multi method m(Str $x) { "A5-Str($x)" }
}
class B5 is A5 { multi method m(Int $x) { 'B5,' ~ callwith($x * 10) } }
is B5.new.m(4), 'B5,A5-Int(40)', 'callwith re-matches the tail against new arguments';

# Deferral from inside a method wrapper.
class A6 { method m() { 'A6' } }
class B6 is A6 { method m() { 'B6,' ~ callsame } }
B6.^find_method('m').wrap(method (|c) { 'w,' ~ callsame });
is B6.new.m, 'w,B6,A6', 'callsame from a wrapper and then from the method';

class A7 { method m($x) { "A7($x)" } }
class B7 is A7 { method m($x) { my &n = nextcallee; 'B7,' ~ n(self, $x) } }
B7.^find_method('m').wrap(method (|c) { my &o = nextcallee; 'w,' ~ o(self, |c) });
is B7.new.m(5), 'w,B7,A7(5)', 'nextcallee from a wrapper and then from the method';

# A wrapped proto method whose wrapper calls nextcallee, under a lock.
my $lock = Lock.new;
class G {
    has %.seen;
    proto method pick(|) {*}
    multi method pick(Int $x) { %!seen{"I$x"}++; "Int($x)" }
    multi method pick(Str $x) { %!seen{"S$x"}++; "Str($x)" }
}
G.^lookup('pick').wrap(method (|c) { my &o = nextcallee; $lock.protect: { o(self, |c) } });
my $g = G.new;
is $g.pick(1), 'Int(1)', 'wrapped proto dispatches an Int';
is $g.pick('a'), 'Str(a)', 'wrapped proto dispatches a Str';
await (^3).map: -> $t { start { $g.pick($t) for ^50 } };
is $g.seen<I0> + $g.seen<I1> + $g.seen<I2>, 151, 'wrapped proto under a lock from several threads';
