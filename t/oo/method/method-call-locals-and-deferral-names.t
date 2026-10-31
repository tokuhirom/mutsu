use Test;

# ADR-15737 D2b: a method call seeds and syncs its locals through the chunk's
# pre-interned names, and the deferral expansion interns the method name once.
# Behaviour must be unchanged: locals, attributes, callsame/nextsame across the
# MRO, multi candidates with a proto, and a method added at run time.

plan 12;

class Base {
    has Int $.n = 1;
    method locals($x) {
        my $a = $x + $!n;
        my @list = $a, $a + 1;
        my %h = k => $a;
        "{$a}:{@list.join(',')}:{%h<k>}";
    }
    multi method m(Int $i) { "Base.Int($i)" }
    multi method m(Str $s) { "Base.Str($s)" }
    method who { "Base" }
}

class Mid is Base {
    multi method m(Int $i) { "Mid.Int($i)>" ~ callsame }
    method who { "Mid>" ~ callsame }
}

class Leaf is Mid {
    multi method m(Int $i) { "Leaf.Int($i)>" ~ callsame }
    multi method m(Str $s) { "Leaf.Str($s)>" ~ callsame }
    method who { "Leaf>" ~ callsame }
}

my $b = Base.new(n => 10);
is $b.locals(5), '15:15,16:15', 'locals and attributes seeded and synced';
is $b.locals(1), '11:11,12:11', 'second call does not see the first call locals';

# A call that leaves a named argument visible through the env must not leak it.
sub outer-leak { my $a = 'outer'; my $r = Base.new.locals(2); "$a/$r" }
is outer-leak(), 'outer/3:3,4:3', 'caller lexical of the same name is untouched';

my $l = Leaf.new;
is $l.who, 'Leaf>Mid>Base', 'callsame chain through the MRO';
is $l.m(3), 'Leaf.Int(3)>Mid.Int(3)>Base.Int(3)', 'multi chain with callsame';
is $l.m('s'), 'Leaf.Str(s)>Base.Str(s)', 'multi Str candidate defers past Mid';
is Mid.new.m(4), 'Mid.Int(4)>Base.Int(4)', 'mid-level receiver';

# A method added after the first dispatch is seen by the next call.
my $before = Base.new.who;
Base.^add_method('late', method () { 'late' });
is Base.new.late, 'late', 'method added at run time is dispatched';
is $before, 'Base', 'earlier call unaffected';

# Recursion: each frame keeps its own locals.
class Fact { method f(Int $n) { my $r = $n <= 1 ?? 1 !! $n * self.f($n - 1); $r } }
is Fact.new.f(6), 720, 'recursive method keeps per-frame locals';

# Role-provided method with the same name as a class method.
role R { method who { "R>" ~ callsame } }
class WithRole is Base does R { }
lives-ok { WithRole.new.who }, 'role method with callsame resolves';
is Base.new(n => 2).locals(0), '2:2,3:2', 'plain call after all of the above';
