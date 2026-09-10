use Test;
plan 4;

my $x;
my $y = 42;

my $r1 = "";
with $x {
    $r1 = "x";
} orwith $y {
    $r1 = "y";
}
is $r1, "y", 'orwith runs when with fails';

my $a = 10;
my $b = 20;
my $r2 = "";
with $a {
    $r2 = "a";
} orwith $b {
    $r2 = "b";
}
is $r2, "a", 'orwith skipped when with succeeds';

my $r3 = "";
with $x {
    $r3 = "x";
} orwith $x {
    $r3 = "orwith";
} else {
    $r3 = "else";
}
is $r3, "else", 'else runs when both with and orwith fail';

my $r4 = "";
without $y {
    $r4 = "without";
}
is $r4, "", 'without skips defined value (sanity check)';
