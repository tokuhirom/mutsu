use Test;

plan 30;

# A `my role` has declaration-site identity (ADR-0047 P1, #9894): two
# same-named lexical roles in different scopes are two different types. They
# used to share one registry entry keyed by the bare name, so the last
# declaration won everywhere.

my ($a, $b);
{ my role R { method go { "first" } }; $a = R }
{ my role R { method go { "second" } }; $b = R }
is $a.go, 'first', 'first sibling role keeps its own methods';
is $b.go, 'second', 'second sibling role has its own methods';
is $a.^name, 'R', '.^name shows the source name';
nok $a === $b, 'the two roles are different type objects';

my ($c1, $c2);
{ my role R { method go { "c-first" } }; my class C does R { }; $c1 = C }
nok $c1.new ~~ $b, 'a class does only the role visible where it was declared';
ok $c1.new ~~ $c1.^roles[0], 'and does that one';

my ($m1, $m2);
{ my role M { method hi { "m1" } }; $m1 = 42 but M }
{ my role M { method hi { "m2" } }; $m2 = 42 but M }
is "$m1.hi() $m2.hi()", 'm1 m2', '`but` mixes in the role visible in its own scope';

my ($p1, $p2);
{ my role P[$x] { method v { "p1-$x" } }; $p1 = P[1] }
{ my role P[$x] { method v { "p2-$x" } }; $p2 = P[2] }
is (1 but $p1).v, 'p1-1', 'first curried lexical role';
is (1 but $p2).v, 'p2-2', 'second curried lexical role';
is $p1.^name, 'P[Int]', 'a curried lexical role displays without its site key';

# Several candidates of one lexical role group in the same scope.
{
    my role G[$x] { method g { "one-$x" } }
    my role G[$x, $y] { method g { "two-$x-$y" } }
    is (1 but G[5]).g, 'one-5', 'one-argument candidate of a lexical group';
    is (1 but G[5, 6]).g, 'two-5-6', 'two-argument candidate of the same group';
    is G.^candidates.elems, 2, 'both candidates joined one group';
}

# A stub completed later in the same scope.
{
    my role S { ... }
    my role S { method s { "defined" } }
    is (1 but S).s, 'defined', 'a lexical role stub is completed in its scope';
}

# Lexical roles in class bodies, reached from the class's methods.
class P1 { my role Op { method o { "p1" } }; method m { (0 but Op).o } }
class P2 { my role Op { method o { "p2" } }; method m { (0 but Op).o } }
is P1.m, 'p1', 'first class body sees its own `my role Op`';
is P2.m, 'p2', 'second class body sees its own `my role Op`';

# The same declaration site keeps one identity across executions.
my @x;
for ^2 { my role LR { }; @x.push: (1 but LR) }
ok @x[0].WHAT === @x[1].WHAT, 'a loop body role keeps one identity';

# A role naming itself in a signature.
{
    my role A { method !foo(A:D:) { "success" }; method bar { self!foo } }
    my class CA does A { }
    is CA.new.bar, 'success', 'a lexical role names itself in an invocant constraint';
}

# Parameterized references to a lexical role.
{
    my role R1[::T] { method x { T } }
    my class C1 does R1[Int] { }
    my class C2 does R1[Str] { }
    lives-ok { my R1[Int] $v = C1.new }, 'a curried lexical role as a variable constraint';
    dies-ok { my R1[Int] $v = C2.new }, 'which rejects another concretization';
    lives-ok { my R1 of Int $v = C1.new }, '`of` spelling of the same constraint';
    ok C1.new ~~ R1[Int], 'smartmatch against a curried lexical role';
    nok C2.new ~~ R1[Int], 'and a non-match';
    sub f(R1[Int] $v) { "bound" }
    is f(C1.new), 'bound', 'a curried lexical role as a parameter constraint';
    is C1.^roles.map(*.^name).join(','), 'R1[Int]', '.^roles shows the source name';
}

# A lexical role forwarding its type parameter to another lexical role.
{
    my role Q1[::T] { method of-type { "Q1[" ~ T.^name ~ "]" } }
    my role Q1 { method of-type { "Q1" } }
    my role Q2[::T] does Q1[::T] { method x { self.Q1::of-type } }
    my class CQ does Q2[Num] { }
    is CQ.new.x, 'Q1[Num]', '`does Q1[::T]` names the lexical role group';
}

# A class that does a lexical role and inherits a class doing its parent role.
{
    my role R0 { }
    my role R1 does R0 { }
    my class C0 does R0 { }
    my class C1 does R1 is C0 { }
    is C1.^mro.map(*.^name).join(','), 'C1,C0,Any,Mu', 'the hierarchy is consistent';
    ok C1.new ~~ R0, 'the instance does the inherited lexical role';
}

# EVAL-local lexical roles.
is EVAL('my role Ev { method e { "ev" } }; (1 but Ev).e'), 'ev', 'a `my role` inside EVAL';

# An attribute `does` trait names the lexical role visible at the declaration.
{
    my role AttrRole { method bar { 42 } }
    my class WithAttr { has $.scalar does AttrRole }
    is WithAttr.new.scalar.bar, 42, 'an attribute `does` a lexical role';
}
