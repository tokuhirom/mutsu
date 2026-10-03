use Test;

# #11116: the declarations #10999 left with the every-local env-sync fold —
# a class with a token/rule, a computed method name, a fallback trait
# argument, a hoisted forward-reference shell, and a class or role nested in
# a routine body — are bounded by what they read by name. Each case declares
# the outer lexical, then the type reading it, then re-stores it in the same
# frame: the type must see the latest store.

plan 14;

my $x = 1;
grammar T1 { token t { a <b> }; token b { b }; method m { $x } }
$x = 5;
is T1.m, 5, 'grammar with a static token: method reads a re-stored outer scalar';
ok T1.parse('ab', :rule<t>), 'static token still matches';

my $pat = 'x';
grammar T2 { token t { <$pat> }; method m { 1 } }
$pat = 'q';
ok T2.parse('q', :rule<t>), 'interpolating token reads the re-stored outer scalar';

my constant mname = 'dyn';
my $y = 1;
class N1 { method ::(mname) { $y } }
$y = 7;
is N1.dyn, 7, 'computed method name: body reads a re-stored outer scalar';

my $cy = 1;
class ::('CK') { method m { $cy } }
$cy = 4;
is ::('CK').m, 4, 'computed class name: method reads a re-stored outer scalar';

my $z = 1;
class F1 { method m { $z } }
say 'a runtime statement before the forward-referenced class';
is ::('F2').m, 2, 'forward reference reaches the hoisted class';
class F2 { method m { $z + 1 } }
$z = 10;
is F1.m, 10, 'class before a runtime statement reads the re-stored scalar';
is F2.m, 11, 'hoisted class reads the re-stored outer scalar';

my $w = 1;
role RF { method m { $w } }
say 'another runtime statement';
is ::('RH').m, 1, 'forward reference reaches the hoisted role';
role RH { method m { $w } }
$w = 3;
is RH.m, 3, 'hoisted role reads the re-stored outer scalar';

my $inner-val = 'old';
sub make { my class In { method v { $inner-val } }; In.v }
$inner-val = 'new';
is make(), 'new', 'class nested in a sub reads the re-stored outer scalar';

my $rv = 'r-old';
sub make-role { my role IR { method v { $rv } }; IR.v }
$rv = 'r-new';
is make-role(), 'r-new', 'role nested in a sub reads the re-stored outer scalar';

my $mv = 'm-old';
class Outer { method go { my class In2 { method v { $mv } }; In2.v } }
$mv = 'm-new';
is Outer.go, 'm-new', 'class nested in a method reads the re-stored outer scalar';

my $t = 0;
my int $i = 0;
while $i < 100 { $t = T1.m + $i; $i = $i + 1; }
is $t, 104, 'loop storing through a call on a grammar with a token';
