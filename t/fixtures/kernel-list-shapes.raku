# Shapes for the list loop kernel (t/control/loop-kernel-int-list.t). Each
# line is `name=answer`. The test runs this file with kernels on, with
# `MUTSU_KERNEL=off` and with `MUTSU_JIT=off`, and requires identical output.

sub values-sum() {
    my %h;
    %h{"k$_"} = $_ * 2 for ^1000;
    my $sum = 0;
    for %h.values { $sum += $_ }
    $sum
}
say "values=", values-sum();

# A list literal of Ints, read twice in the body.
my $s = 0;
for (1..500).list { $s = $s + $_ * $_ - 1 }
say "list=", $s;

# Overflow mid-list: the iteration that overflows reruns generically and the
# rest of the list continues there.
my $p = 1;
for (2 xx 80).list { $p = $p * $_ }
say "overflow=", $p;

# A Str among the Ints: the whole loop stays generic.
my $c = 0;
for (|(1..20), "x", |(1..20)).list { $c = $c + 1 }
say "mixed-kinds=", $c;

# A Num accumulator.
my $n = 0e0;
for (1..100).list { $n = $n + $_ * 0.5e0 }
say "num-acc=", $n;

# The enclosing topic after the loop.
$_ = 'outer';
my $t = 0;
for (1..50).list { $t += $_ }
say "topic-after=", $_, ",", $t;

# A short list (below the kernel's minimum) and an empty one.
my $short = 0;
for (1, 2, 3).list { $short += $_ }
my $empty = 0;
for ().list { $empty += 1 }
say "short=", $short, ",", $empty;

# A zero divisor mid-list raises at the right item.
my $d = 0;
my @div = (|(1..30), 0, |(1..30));
try { for @div.List { $d = $d + 100 div $_ } }
say "div-zero=", $d, ",", ($! ?? $!.^name !! 'none');
