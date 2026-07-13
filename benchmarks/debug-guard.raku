# Hot function with a compile-time-constant debug guard (common logging pattern).
constant DEBUG = False;

sub step(Int $x) {
    if DEBUG {
        note "step($x)";
    }
    my $y = $x * 2 + 1;
    if DEBUG {
        note "  -> $y";
    }
    $y
}

my $sum = 0;
for ^100000 -> $i {
    $sum += step($i) mod 1000;
}
say $sum;
