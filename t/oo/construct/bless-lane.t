use Test;

# The bless lane (`vm_bless_lane`): once `self.bless(named...)` on a class's
# type object has walked the whole `CallMethodMut` chain into the native
# `bless`, later calls go straight there. Every replay must build what the
# chain builds, and anything the memo does not prove must take the chain.

plan 13;

class P {
    has $.a;
    has @.l;
    has %.h;
    method new(*%_) { self.bless(|%_) }
}
for ^3 {
    my $p = P.new(a => $_, l => (1, 2), h => %(k => $_));
    is-deeply ($p.a, $p.l, $p.h), ($_, [1, 2], %(k => $_)), "replay $_ builds the attributes";
}

class M {
    has $.x;
    has %.meta;
    method new(*%_) { self.bless(|%_, :meta(%_)) }
}
my $m;
$m = M.new(x => 1) for ^3;
is-deeply $m.meta, { x => 1 }, 'a spread plus an extra named argument';

# A TWEAK that writes the caller's lexical: the dispatch stays impure.
my $seen = 0;
class T {
    has $.v;
    method new(*%_) { self.bless(|%_) }
    submethod TWEAK { $seen += $!v }
}
T.new(v => $_) for 1..3;
is $seen, 6, 'a TWEAK writing an outer lexical is seen on every replay';

# A positional argument is outside the lane: bless still refuses it.
class Pos { method new(*@a) { self.bless(|@a) } }
Pos.new() for ^3;
dies-ok { Pos.new(1) }, 'a positional argument to bless still dies';

# A junction value is outside the lane too.
class J { has $.j; method new(*%_) { self.bless(|%_) } }
J.new(j => 1) for ^3;
isa-ok J.new(j => 1|2).j, Junction, 'a Junction named argument binds as it did';

# A `.map` Seq argument is reified as the stretch does.
class S { has @.s; method new(*%_) { self.bless(|%_) } }
S.new(s => (1, 2).map(* + 1)) for ^2;
is-deeply S.new(s => (1, 2).map(* * 10)).s, [10, 20], 'a lazy .map argument fills the array';

# A class with its own `bless` never enters the lane: it runs every time.
my $custom = 0;
class U {
    has $.u;
    method new(*%_) { self.bless(|%_) }
    method bless(*%a) { $custom++; callsame }
}
U.new(u => $_) for ^3;
is $custom, 3, 'a user bless runs on every construction';
is U.new(u => 7).u, 7, 'and builds through Mu.bless';

# Inheritance: a subclass blessed through the parent's new.
class Base { has $.b; method new(*%_) { self.bless(|%_) } }
class Kid is Base { has $.k }
my $kid;
$kid = Kid.new(b => 1, k => 2) for ^3;
is ($kid.WHAT, $kid.b, $kid.k).raku, (Kid, 1, 2).raku, 'a subclass blesses itself';

# bless on an instance is outside the lane (the key is a type object).
my $inst = P.new(a => 1);
is $inst.bless(a => 5).a, 5, 'bless on an instance still builds';

# A `where`-less required attribute is still enforced.
class R { has $.r is required; method new(*%_) { self.bless(|%_) } }
R.new(r => 1) for ^3;
dies-ok { R.new }, 'is required is still enforced on a replay';
