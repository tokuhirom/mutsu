use Test;

# A program that mentions a deferral builtin gives every method call a lazily
# built dispatch frame. These calls must still find the right frame after the
# pending entry reuses the call's own class and method symbols.
plan 8;

class Base {
    multi method m(Int $x) { "Base-Int:$x" }
    multi method m(Str $x) { "Base-Str:$x" }
    method plain($x) { $x + 1 }
}
class Derived is Base {
    multi method m(Int $x) { "Derived-Int:" ~ callsame() }
    method plain($x) { callsame() * 2 }
}
my $d = Derived.new;
is $d.m(3), "Derived-Int:Base-Int:3", 'callsame from a multi method reaches the parent candidate';
is $d.m("s"), "Base-Str:s", 'a candidate with no override is reached directly';
is $d.plain(4), 10, 'callsame in a single-candidate method reaches the parent';

my $t = 0;
for ^50 { $t += Base.new.plain(1) }
is $t, 100, 'repeated plain calls next to deferral-capable classes';

class Mid is Derived {
    method plain($x) { callwith($x + 10) }
}
is Mid.new.plain(1), 24, 'callwith passes new arguments to the next candidate';

my @seen;
class A1 { method n { @seen.push('A1'); self } }
class B1 is A1 { method n { @seen.push('B1'); callsame() } }
B1.new.n for ^3;
is @seen.join(','), 'B1,A1,B1,A1,B1,A1', 'the frame of each call is its own';

class Wrap { method w($x) { $x } }
my $w = Wrap.new;
is (^5).map({ $w.w($_) }).sum, 10, 'a loop of plain calls';
is $w.w(7), 7, 'and one more afterwards';
