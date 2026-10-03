use Test;

# A typed attribute with no initializer is seeded with its declared type
# object, and the native constructor memoizes that seed per class (#9494).
# Each class, subset and role parameterization must still get its own.

plan 9;

subset Pos of Int where * > 0;
class A { has Pos $.p; has Str $.s; has Int $.i is rw; has $.u }
my @names;
for ^3 {
    my $a = A.new;
    @names.push: ($a.p.^name, $a.s.^name, $a.i.^name, $a.u.^name).join(',');
}
is @names[0], 'Pos,Str,Int,Any', 'seeds are the declared type objects';
is @names[2], 'Pos,Str,Int,Any', 'and stay so on later constructions';

role R[::T] { has T $.v }
class B does R[Int] { }
class C does R[Str] { }
is B.new.v.^name, 'Int', 'a role-parameterized seed, first class';
is C.new.v.^name, 'Str', 'the same role, another parameterization';
is B.new.v.^name, 'Int', 'the first class keeps its own seed';

my $x = A.new;
$x.i = 5;
is $x.i, 5, 'a seeded attribute is still writable';
is A.new.i.^name, 'Int', 'and a fresh object is seeded again';

class D { has Str $.t; method set { $!t = 'z'; self } }
is D.new.set.t, 'z', 'a method writes over the seed';
is D.new.t.^name, 'Str', 'without touching the next seed';
