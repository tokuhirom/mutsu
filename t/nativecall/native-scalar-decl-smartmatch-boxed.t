use Test;

# Under RakuAST a natively declared variable is read boxed, so `$x ~~ int` is
# False; a native parameter and an element of a destructured declaration keep
# the native type. The table is the one measured on rakudo 2026.09.

plan 18;

my int $x = 5;
nok $x ~~ int, 'my int $x = 5';
my num $n = 1e0;
nok $n ~~ num, 'my num $n';
my str $s = "a";
nok $s ~~ str, 'my str $s';
my int $bare;
nok $bare ~~ int, 'my int $bare without an initializer';

my int @a = 1, 2;
nok @a[0] ~~ int, 'an element of a native array';

class C { has int $.n = 3; method m { $!n ~~ int } }
nok C.new.m, 'a native attribute read as $!n';

sub from-param(int $v) { my int $w = $v; $w ~~ int }
nok from-param(3), 'my int $w = $param';

sub param(int $v) { $v ~~ int }
ok param(3), 'a sub parameter';
my &pointy = -> int $v { $v ~~ int };
ok pointy(3), 'a pointy-block parameter';
sub copy(int $v is copy) { $v ~~ int }
ok copy(3), 'an is copy parameter';

my int ($p, $q) = 1, 2;
ok $p ~~ int, 'an element of my int ($p, $q) = ...';
my int ($r, $t);
ok $r ~~ int, 'an element of my int ($r, $t)';

for 1, 2 -> int $v {
    ok $v ~~ int, 'a for-loop parameter';
    last;
}
for 1, 2 -> int $v, int $w {
    ok $v ~~ int && $w ~~ int, 'both parameters of a two-parameter for loop';
    last;
}
for 1, 2 -> int $v is copy {
    ok $v ~~ int, 'an is copy for-loop parameter';
    last;
}
for 1, 2 -> Int $v {
    nok $v ~~ int, 'a boxed Int for-loop parameter is not native';
    last;
}
for 1, 2 -> int $v {
    my int $inner = 5;
    nok $inner ~~ int, 'a my int declared inside a loop body is read boxed';
    ok $v ~~ int, 'while the parameter next to it is native';
    last;
}
