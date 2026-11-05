use Test;
plan 8;
class A {}; class B is A {}; class C is B {}; class D is C {}; class E is D {}
role R1 {}; role R2 does R1 {}
class F is E does R2 {}
my class Fam {
    multi method m(A $x, A $y) { "AA" }
    multi method m(B $x, A $y) { "BA" }
    multi method m(A $x, B $y) { "AB" }
    multi method m(B $x, B $y) { "BB" }
    multi method m(C $x, B $y) { "CB" }
    multi method m(B $x, C $y) { "BC" }
    multi method m(C $x, C $y) { "CC" }
    multi method m(D $x, C $y) { "DC" }
    multi method m(C $x, D $y) { "CD" }
    multi method m(D $x, D $y) { "DD" }
    multi method m(E $x, D $y) { "ED" }
    multi method m(D $x, E $y) { "DE" }
    multi method m(E $x, E $y) { "EE" }
    multi method m(R1 $x, A $y) { "R1A" }
    multi method m(R2 $x, A $y) { "R2A" }
    multi method m(F $x, A $y) { "FA" }
    multi method m(Int $x, Int $y) { "II" }
    multi method m(Numeric $x, Int $y) { "NI" }
    multi method m(Cool $x, Int $y) { "CoI" }
    multi method m(Any $x, Any $y) { "anyany" }
}
my $f = Fam.new;
my @got;
for 1..3 -> $round {
    @got.push: ($f.m(C.new, C.new), $f.m(E.new, E.new), $f.m(F.new, A.new), $f.m(5, 6), $f.m(5e0, 6), $f.m("a", 6), $f.m(B.new, E.new)).join(",");
}
is @got[0], "CC,EE,FA,II,NI,CoI,BC", "first round";
is @got[1], @got[0], "second round (index reused) agrees";
is @got[2], @got[0], "third round agrees";
# A type declared after the index was built changes the ranking.
class G is A {}
multi sub extra(A $a) { "A" }
is $f.m(G.new, G.new), "AA", "a late subclass dispatches";
class H is G {}
is $f.m(H.new, E.new), "AB", "a deeper late subclass";
is $f.m(C.new, C.new), "CC", "earlier winner unchanged";
{
    my constant B2 = B;
    is $f.m(B2.new, B.new), "BB", "alias in an inner scope";
}
is $f.m(E.new, E.new), "EE", "after the scope";
