use Test;

# Ranking a multi's candidates asks whether one declared type conforms to
# another once per ordered candidate pair; the answers are memoised across
# calls and must not change what wins (#14924).

plan 17;

class A { }
class B is A { }
class C is B { }
role R { }
class D does R { }
class E is D { }
class U { }

multi method-less(A $x) { 'A' }
multi method-less(B $x) { 'B' }
multi method-less(C $x) { 'C' }
multi method-less(R $x) { 'R' }
multi method-less(Mu:D $x) { 'mu' }

# The same tuple twice (the second call is served from the memo), then others.
is method-less(C.new), 'C', 'the narrowest class';
is method-less(C.new), 'C', 'the narrowest class again';
is method-less(B.new), 'B', 'a middle class';
is method-less(A.new), 'A', 'the root class';
is method-less(D.new), 'R', 'a class that composes a role';
is method-less(E.new), 'R', 'a subclass of a composing class';
is method-less(U.new), 'mu', 'an unrelated class falls to Mu:D';

# A class declared after the first calls ranks against the cached relation.
EVAL 'class F is C { }; class G is A { }';
is method-less(::('F').new), 'C', 'a class declared later, under C';
is method-less(::('G').new), 'A', 'a class declared later, under A';

# A candidate whose type is an alias reads the alias, not the name.
{
    constant Alias = B;
    multi aliased(Alias $x) { 'alias' }
    multi aliased(A $x) { 'A' }
    is aliased(B.new), 'alias', 'an alias of B ranks as B';
    is aliased(C.new), 'alias', 'a subclass of the alias';
    is aliased(A.new), 'A', 'the root still takes the A candidate';
}

# A deep chain, called with two different argument types.
my $src = (0..39).map({ "class K$_ " ~ ($_ ?? "is K{$_ - 1} " !! '') ~ '{ }' }).join("\n")
    ~ "\n" ~ (0..39).map({ "multi chain(K$_ \$x) \{ $_ }" }).join("\n")
    ~ "\n[chain(K7.new), chain(K31.new)]";
is-deeply EVAL($src), [7, 31], 'a 40-class chain picks the narrowest for two argument types';

# The same relation for multi METHODS, whose ranking is the cached one.
class Disp {
    multi method pick(A:D $x) { 'A' }
    multi method pick(B:D $x) { 'B' }
    multi method pick(C:D $x) { 'C' }
    multi method pick(Mu:D $x) { 'mu' }
}
is Disp.pick(C.new), 'C', 'method: narrowest';
is Disp.pick(C.new), 'C', 'method: narrowest again';
is Disp.pick(B.new), 'B', 'method: a second argument type';
is Disp.pick(U.new), 'mu', 'method: an unrelated class';
