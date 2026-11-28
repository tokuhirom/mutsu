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

# `%h{"prefix$i"} = v` in a counted loop: the key's text is built by the
# kernel's store helper (K::IntKey).
sub prefixed() {
    my %h;
    for ^1000 { %h{"key-$_"} = $_ * 2 }
    my $sum = 0;
    for %h.values { $sum += $_ }
    "{%h.elems},{%h<key-0>},{%h<key-999>},$sum"
}
say "prefixed=", prefixed();
{
    my %h;
    for -50..50 { %h{"n$_"} = $_ * 0.5e0 }
    say "prefixed-num=", %h<n-50>, ",", %h<n50>, ",", %h.elems;
}
{
    my %h;
    for ^300 { %h{"é$_"} = $_ }
    for ^300 { %h{"é$_"} = $_ + 1 }
    say "prefixed-overwrite=", %h.elems, ",", %h{"é299"}, ",", %h.keys.grep(*.starts-with("é")).elems;
}
{
    # A product that overflows mid-loop bails the kernel; the generic rerun
    # stores the big integer.
    my %h;
    for ^100 { %h{"p$_"} = $_ * 4611686018427387904 }
    say "prefixed-overflow=", %h<p99>, ",", %h.elems;
}
{
    # A bound entry: the kernel declines there and the generic store writes
    # through the binding.
    my %h;
    my $x = 0;
    %h<b5> := $x;
    for ^50 { %h{"b$_"} = $_ * 10 }
    say "prefixed-bound=", $x, ",", %h<b49>;
}
{
    my Int %t;
    for ^50 { %t{"t$_"} = $_ }
    say "prefixed-typed=", %t.values.sum;
}

# `%h{$w}++` over a list of Strs (a `Str` list kernel): the word-count shape,
# a named parameter and the topic, decrements and pre-forms.
sub word-count() {
    my @lines = ('a b c a b a' xx 10).list;
    my %counts;
    for ^20 {
        for @lines -> $line {
            for $line.split(' ') -> $w { %counts{$w}++ }
        }
    }
    %counts.sort.map({ .key ~ '=' ~ .value }).join(',')
}
say "str-words=", word-count();
{
    my %c;
    my @w = (<x y x> xx 10).flat;
    for @w.List { %c{$_}++ }
    for @w.List -> $k { --%c{$k} }
    say "str-topic=", %c<x>, ",", %c<y>;
}
{
    # A present element that is not an Int: the iteration reruns generically
    # (`Str.succ`).
    my %c = b => 'a9';
    for (<a b a c> xx 5).flat -> $k { %c{$k}++ }
    say "str-succ=", %c<a>, ",", %c<b>, ",", %c<c>;
}
{
    # The outer variable of the parameter's name is untouched.
    my $w = 'outer';
    my %c;
    for (<p q> xx 10).flat -> $w { %c{$w}++ }
    say "str-outer=", $w, ",", %c<p>;
}
