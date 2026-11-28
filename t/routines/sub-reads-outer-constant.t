use Test;

# A sigil-less constant of an enclosing scope, read from a sub or a closure,
# keeps the call's first answer in a frame slot (#18274). Every read must
# answer what the constant is.

plan 7;

my constant VOID = <br hr img>.Set;
my constant LIMITS = (3, 5, 8);
my constant NAME = 'konst';
my constant TYPE = Int;

sub count-void(@tags) {
    my $n = 0;
    for @tags -> $t { $n++ if VOID{$t} }
    $n
}
is count-void(<br p img div hr br>), 4, 'a constant Set read in a loop';
is count-void(['p']), 0, 'and in a second call';

sub sum-limits() { my $s = 0; $s += LIMITS[$_] for ^LIMITS.elems; $s }
is sum-limits(), 16, 'a constant List read repeatedly';

sub outer() {
    sub inner($x) { NAME ~ $x }
    (^3).map({ inner($_) ~ NAME.chars }).join(',')
}
is outer(), 'konst05,konst15,konst25', 'nested subs and closures read the constant';

sub typed($v) { $v ~~ TYPE ?? TYPE.^name !! 'no' }
is typed(3) ~ typed('a'), 'Intno', 'a type-object constant';

{
    my constant NAME = 'inner';
    sub shadow() { NAME }
    is shadow(), 'inner', 'an inner constant of the same name shadows';
}

sub recur($n) { $n == 0 ?? NAME !! recur($n - 1) ~ LIMITS[$n % 3] }
is recur(3), 'konst583', 'recursion reads the constants in every frame';
