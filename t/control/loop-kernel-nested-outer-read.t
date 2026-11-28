use Test;

# An inner counted loop that only reads a slot its region does not declare --
# an enclosing loop's parameter, or a typed variable -- may run as a loop
# kernel; a slot it writes must still be untyped. Every shape must leave what
# the generic loop leaves.

plan 8;

sub outer-param() {
    my @backing = 0 xx 64;
    for ^4 -> $t {
        for ^7000 -> $i {
            @backing[$t * 16 + ($i +& 15)] = $t * 1000 + $i;
        }
    }
    [+] @backing
}
is outer-param(), 543456, 'an inner loop reads the outer loop parameter';

sub outer-sum() {
    my $s = 0;
    for ^3 -> $t {
        for ^100 -> $i { $s += $t * $i }
    }
    $s
}
is outer-sum(), 14850, 'the outer parameter in an accumulation';

sub typed-read() {
    my Int $k = 3;
    my $s = 0;
    for ^100 -> $i { $s += $k * $i }
    $s
}
is typed-read(), 14850, 'a typed variable that is only read';

sub typed-write() {
    my Int $s = 0;
    for ^100 -> $i { $s += $i }
    $s
}
is typed-write(), 4950, 'a typed variable that is written';

sub native-wrap() {
    my int8 $x = 0;
    for ^200 -> $i { $x = $x + 1 }
    $x
}
is native-wrap(), -56, 'a native int8 written in the loop still wraps';

sub subset-checked() {
    subset Small of Int where * < 50;
    my Small $x = 0;
    for ^100 -> $i { $x = $i }
    CATCH { default { return 'died' } }
    $x
}
is subset-checked(), 'died', 'a subset constraint still fires';

sub outer-topic() {
    my $s = 0;
    for ^3 {
        my $t = $_;
        for ^10 -> $i { $s += $t + $i }
    }
    $s
}
is outer-topic(), 165, 'an outer topic copied into a variable';

sub three-deep() {
    my $s = 0;
    for ^2 -> $a {
        for ^3 -> $b {
            for ^4 -> $c { $s += $a * 100 + $b * 10 + $c }
        }
    }
    $s
}
is three-deep(), 1476, 'three nested loops';
