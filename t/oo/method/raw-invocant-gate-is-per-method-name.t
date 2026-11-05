use v6.e.PREVIEW;
use Test;

# ADR-0067 slice 3b arms its arrival channel only for a call whose method name
# some registered method declares a raw invocant under. A program that has one
# such method must still treat every other method call as an ordinary call, and
# a call of the raw method itself must still reach the caller's container
# whatever else is declared around it (#9170).

plan 11;

class Plain {
    method m($x) { $x * 2 }
    method name-only { 'plain' }
}
class Raw {
    method touch(\S:) { S = 7 }
    method m(\S:) { S = 9 }
}
class Other { method touch($x) { $x + 1 } }

my $p = Plain.new;
my $sum = 0;
$sum += $p.m($_) for ^5;
is $sum, 20, 'plain method calls are unaffected by a raw-invocant method elsewhere';
is $p.name-only, 'plain', 'a method no class declares raw';

my $r = Raw.new;
$r.touch;
is $r, 7, 'a raw-invocant method writes the caller container';

# The same name on another class that is NOT raw must not write through.
my $o = Other.new;
is $o.touch(1), 2, 'same method name, non-raw class: ordinary result';
ok $o ~~ Other, 'the receiver is untouched';

# A name shared by a raw and a non-raw method: the call picks by class.
my $p2 = Plain.new;
is $p2.m(4), 8, 'Plain.m (not raw) keeps its value';
my $r2 = Raw.new;
$r2.m;
is $r2, 9, 'Raw.m (raw) writes through';

# Repeated calls keep the answer stable (no state from the first call).
for ^3 {
    my $c = Raw.new;
    $c.touch;
    is $c, 7, "raw call $_ writes through";
}

# A method declared later in the program (after earlier calls) is gated in.
{
    my class Late { method late(\S:) { S = 'late' } }
    my $l = Late.new;
    $l.late;
    is $l, 'late', 'a raw-invocant method declared after other calls arms on its first call';
}
