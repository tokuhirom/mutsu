use Test;

# #11078: a frame that declares a role env-syncs only the lexicals the role's
# registration and compositions (method bodies, signatures, attribute
# defaults, type-parameter defaults, deferred body statements, type names)
# read by name, not every local. Each case declares the outer lexical, then a
# role reading it, then re-stores it in the same frame: the role must see the
# latest store.

plan 14;

my $x = 1;
role R1 { method m { $x } }
$x = 5;
is R1.m, 5, 'punned role method reads a re-stored outer scalar';

my $y = 1;
role R2 { method m { $y } }
class C2 does R2 { }
$y = 6;
is C2.m, 6, 'composed role method reads a re-stored outer scalar';

my $d = 3;
role R3 { has $.v = $d }
$d = 7;
is R3.new.v, 7, 'role attribute default reads the outer scalar';

my $p = 1;
role R4 { method m($a = $p) { $a } }
$p = 9;
is R4.m, 9, 'role method parameter default reads the outer scalar';

my $q = 2;
role R5 { my $z = $q; method m { $z } }
$q = 20;
is (1 but R5).m, 20, 'deferred role-body statement reads the outer scalar at composition';

my $tp = 4;
role R6[$n = $tp] { method m { $n } }
$tp = 40;
is (1 but R6).m, 40, 'type-parameter default reads the outer scalar';

my $cnt = 0;
role R7 { method bump { $cnt++ } }
R7.bump; R7.bump;
is $cnt, 2, 'role method writes accumulate into the outer scalar';

my $s = 'a';
role R8 { method m { "x{$s}y" } }
$s = 'b';
is R8.m, 'xby', 'interpolating string in a role method reads the outer scalar';

my $r = 'b+';
role R9 { method m { 'abbb' ~~ /<$r>/ ?? ~$/ !! 'no' } }
$r = 'a';
is R9.m, 'a', 'interpolated regex in a role method reads the outer scalar';

my $base = 100;
role R10 { sub inner { $base + 1 }; method m { inner() } }
$base = 200;
class C10 does R10 { }
is C10.m, 201, 'role-body sub reads the outer scalar';

my @arr = 1, 2;
role R11 { method m { @arr.elems } }
@arr = 1, 2, 3;
is R11.m, 3, 'role method reads a re-stored outer array';

my $arg = 'p';
role R12[$v] { method m { $v } }
$arg = 'q';
is (1 but R12[$arg]).m, 'q', 'parametric does argument reads the outer scalar';

# A bounded role declared before an unbounded sub must not let the sub skip
# the frame-wide fold.
role Z1 { method m { 1 } }
my $pat = 'x';
sub regex-sub { 'abc' ~~ /<$pat>/ ?? 'm' !! 'n' }
$pat = 'b';
is regex-sub(), 'm', 'unbounded sub after a bounded role reads the outer scalar';

my $t = 0;
my int $i = 0;
while $i < 100 { $t = R1.m + $i; $i = $i + 1; }
is $t, 104, 'loop storing through a role method call';
