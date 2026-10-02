use Test;

# #10999: a frame that declares a class env-syncs only the lexicals the
# class registration (method bodies, signatures, attribute defaults,
# class-body statements, type names) reads by name, not every local. Each
# case declares the outer lexical, then a class reading it, then re-stores it
# in the same frame: the class must see the latest store.

plan 15;

my $x = 1;
class C1 { method m { $x } }
$x = 5;
is C1.m, 5, 'method body reads a re-stored outer scalar';

my $d = 3;
class D1 { has $.v = $d }
$d = 7;
is D1.new.v, 7, 'attribute default reads the outer scalar';

my $p = 1;
class F1 { method m($a = $p) { $a } }
$p = 9;
is F1.m, 9, 'method parameter default reads the outer scalar';

my $w = 10;
class W1 { method m($a where * < $w) { "ok $a" } }
$w = 100;
is W1.m(50), 'ok 50', 'method parameter where-constraint reads the outer scalar';

my $q = 2;
class Q1 { my $z = $q; method m { $z } }
is Q1.m, 2, 'class-body statement reads the outer scalar at registration';

my @arr = 1, 2;
class A1 { method m { @arr.elems } }
@arr = 1, 2, 3;
is A1.m, 3, 'method body reads a re-stored outer array';

my %h;
class H1 { method m { %h<a> } }
%h<a> = 4;
is H1.m, 4, 'method body reads the outer hash';

my $cnt = 0;
class K1 { method bump { $cnt++ } }
K1.bump; K1.bump;
is $cnt, 2, 'method writes accumulate into the outer scalar';

my $s = 'a';
class S1 { method m { "x{$s}y" } }
$s = 'b';
is S1.m, 'xby', 'interpolating string in a method reads the outer scalar';

my $r = 'b+';
class R1 { method m { 'abbb' ~~ /<$r>/ ?? ~$/ !! 'no' } }
$r = 'a';
is R1.m, 'a', 'interpolated regex in a method reads the outer scalar';

my $base = 100;
class B1 { sub inner { $base + 1 }; method m { inner() } }
$base = 200;
is B1.m, 201, 'class-body sub reads the outer scalar';

my $acc = '';
class M1 { method go { (1..3).map({ $acc ~= $_ }).eager; $acc } }
$acc = 'X';
is M1.go, 'X123', 'closure inside a method appends to the outer scalar';

my $inner-val = 'old';
class N1 { method m { my class In { method v { $inner-val } }; In.v } }
$inner-val = 'new';
is N1.m, 'new', 'class nested in a method reads the outer scalar';

# A bounded class declared before two subs shifts the declaration-plan
# indices away from the sub-plan indices; the unbounded sub must still keep
# the frame-wide fold.
class Z1 { method m { 1 } }
sub bounded-sub { 1 }
my $pat = 'x';
sub regex-sub { 'abc' ~~ /<$pat>/ ?? 'm' !! 'n' }
$pat = 'b';
is regex-sub(), 'm', 'unbounded sub after a bounded class reads the outer scalar';

my $t = 0;
my int $i = 0;
while $i < 100 { $t = C1.m + $i; $i = $i + 1; }
is $t, 104, 'loop storing through a class method call';
