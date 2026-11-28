# Shapes for the list loop kernel (t/control/loop-kernel-int-list.t). Each
# line is `name=answer`. The test runs this file with kernels on, with
# `MUTSU_KERNEL=off` and with `MUTSU_JIT=off`, and requires identical output.
# A hash's `.values` is the materialized list the kernel serves; every answer
# is independent of the hash's iteration order.

sub hash-of(@values) {
    my %h;
    my $i = 0;
    %h{"k" ~ $i++} = $_ for @values;
    %h
}

# The hash-access benchmark's second loop.
sub values-sum() {
    my %h;
    %h{"k$_"} = $_ * 2 for ^1000;
    my $sum = 0;
    for %h.values { $sum += $_ }
    $sum
}
say "values=", values-sum();

# The topic read twice, a declaration in the body.
{
    my %h = hash-of(1..500);
    my $s = 0;
    for %h.values { my $sq = $_ * $_; $s = $s + $sq - 1 }
    say "square=", $s;
}

# Overflow mid-list: the iteration that overflows reruns generically and the
# rest of the list continues there.
{
    my %h = hash-of(2 xx 80);
    my $p = 1;
    for %h.values { $p = $p * $_ }
    say "overflow=", $p;
}

# A Str among the Ints: the whole loop stays generic.
{
    my %h = hash-of((|(1..20), "x", |(21..40)));
    my $c = 0;
    for %h.values { $c = $c + 1 }
    say "mixed-kinds=", $c;
}

# A Num accumulator.
{
    my %h = hash-of(1..100);
    my $n = 0e0;
    for %h.values { $n = $n + $_ * 0.5e0 }
    say "num-acc=", $n;
}

# The enclosing topic after the loop.
{
    my %h = hash-of(1..50);
    $_ = 'outer';
    my $t = 0;
    for %h.values { $t += $_ }
    say "topic-after=", $_, ",", $t;
}

# A short hash (below the kernel's minimum) and an empty one.
{
    my %s = hash-of(1..3);
    my %e;
    my ($short, $empty) = 0, 0;
    for %s.values { $short += $_ }
    for %e.values { $empty += 1 }
    say "short=", $short, ",", $empty;
}

# A zero divisor somewhere in the list raises on the generic rerun.
{
    my %h = hash-of((|(1..30), 0, |(1..30)));
    my $d = 0;
    try { for %h.values { $d = $d + 100 div $_ } }
    say "div-zero=", ($! ?? $!.^name !! 'none');
}

# The same loop run again: the compiled kernel is reused.
{
    my $twice = values-sum() + values-sum();
    say "twice=", $twice;
}
