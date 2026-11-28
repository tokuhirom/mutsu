# Shapes for ADR-19288's loop kernels (t/vm/codegen/jit-loop-kernel.t). Each
# line is `name=answer`. The test runs this file with kernels on, with
# `MUTSU_KERNEL=off` and with `MUTSU_JIT=off`, and requires identical output: a
# loop kernel's result, or its bail and the generic rerun of the bailed
# iteration, must be exactly what the generic loop computes.

# The topic, a named parameter, an inclusive and an exclusive range.
my $sum = 0;
for ^1000 { $sum += $_ * 3 + 1 }
say "topic=", $sum;
my $s2 = 0;
for 1..1000 -> $i { $s2 = $s2 + $i * $i - 7 }
say "param=", $s2;

# Declarations in the body, `div` and `mod`.
my $total = 0;
for ^5000 -> $i {
    my $ts = $i * 97 + 13;
    my $days = $ts div 86400;
    my $hours = ($ts mod 86400) div 3600;
    my $secs = $ts mod 60;
    $total += $days + $hours * 2 + $secs * 5;
}
say "decls=", $total;

# Num arithmetic and mixed Int/Num.
my $x = 0.5e0;
for ^2000 { $x = 3.9e0 * $x * (1e0 - $x) }
say "num=", $x;
my $m = 0e0;
for 1..100 -> $i { $m = $m + $i * 0.5e0 }
say "mixed=", $m;

# Overflow mid-loop: the iteration that overflows is rerun generically and
# the loop finishes with a big integer.
my $p = 1;
for 1..80 { $p = $p * 2 }
say "overflow=", $p;
my $acc = 9223372036854775000;
for ^2000 { $acc = $acc + 1 }
say "overflow-add=", $acc;

# A zero divisor mid-loop: the exception comes from the generic rerun, with
# the sum of the iterations before it.
my $q = 0;
try { for -20..20 -> $i { $q = $q + 100 div $i } }
say "div-zero=", $q, ",", $!.^name;

# `while` with `&&`, Num and Int slots (the Mandelbrot kernel), entered often.
my $iters = 0;
for ^40 -> $py {
    for ^40 -> $px {
        my $cr = $px * 0.075e0 - 2.2e0;
        my $ci = $py * 0.075e0 - 1.5e0;
        my $zr = 0e0;
        my $zi = 0e0;
        my $n = 0;
        while $zr * $zr + $zi * $zi <= 4e0 && $n < 50 {
            my $t = $zr * $zr - $zi * $zi + $cr;
            $zi = 2e0 * $zr * $zi + $ci;
            $zr = $t;
            $n = $n + 1;
        }
        $iters = $iters + $n;
    }
}
say "mandel=", $iters;

# A long `while`, compiled mid-loop.
my $w = 0;
my $k = 0;
while $k < 3000 { $w = $w + $k mod 7; $k = $k + 1 }
say "while-long=", $w, ",", $k;

# NaN is true, and compares unequal to itself.
my $nan = NaN;
my $c = 0;
while $nan != $nan && $c < 500 { $c = $c + 1 }
say "nan=", $c;

# Slots a kernel must refuse: the generic loop answers.
my Int $typed = 0;
for ^100 { $typed += $_ }
say "typed=", $typed;
my UInt $u = 50;
try { for ^100 { $u = $u - 1 } }
say "uint=", $u, ",", ($! ?? 'died' !! 'lived');
my $b = True;
for ^20 { $b = $b + 1 }
say "bool=", $b;
my $str = "1";
for ^20 { $str = $str + 1 }
say "str=", $str;
my $kind = 1e0;
for ^20 { $kind = $kind div 1 }
say "kind-change=", $kind, ",", $kind.^name;

# The topic and the loop variable afterwards.
$_ = 'outer';
my $t2 = 0;
for ^100 { $t2 += $_ }
say "topic-after=", $_, ",", $t2;

# `last` / `next` keep the generic loop.
my $l = 0;
for ^100 -> $i { last if $i == 50; $l = $l + $i }
say "last=", $l;

# Nested loops in one kernel, `.Num` on an Int and on a Num.
my $nest = 0e0;
for ^30 -> $a {
    my $fa = $a.Num * 0.5e0;
    for ^30 -> $b {
        my $k2 = 0;
        while $k2 < 3 { $k2 = $k2 + 1 }
        $nest = $nest + $fa * $b.Num + $k2 + $fa.Num;
    }
}
say "nested=", $nest;

# An overflow in an inner loop reruns the outer iteration generically.
my $big = 1;
for 1..3 -> $o { for 1..40 -> $i { $big = $big * 2 } }
say "nested-overflow=", $big;

# A loop calling a sub kernel directly (from its 257th iteration, once the
# call cache knows the sub), and a callee overflow mid-loop.
sub sq3($x) { my $y = $x * $x + 3; $y }
my $cs = 0;
for ^2000 -> $i { $cs = $cs + sq3($i) mod 7 }
say "calls=", $cs;
sub dbl($x) { $x * 2 }
my $cb = 1;
for ^400 { $cb = dbl($cb) }
say "call-overflow=", $cb;

# Element reads and stores (campaign B's helpers), `+&`/`+|`/`+^`, `%`,
# `++`/`--`, and C-style loops.
my @ra = ^64;
my $rs = 0;
loop (my $ri = 0; $ri < 5000; $ri++) { $rs = $rs + @ra[$ri +& 15] }
say "elem-read-cloop=", $rs;
my @rn = (^32).map(* + 0.5e0);
my $rns = 0e0;
for ^3000 -> $i { $rns = $rns + @rn[$i % 32] }
say "elem-read-num=", $rns;
my @rmix = ^200;
@rmix[150] = "x";
my $rms = 0;
for ^200 -> $i { $rms = $rms + (@rmix[$i] ~~ Int ?? @rmix[$i] !! 1) }
say "elem-read-mixed=", $rms;
my @rk = ^100;
@rk[60] = 2.5e0;
my $rks = 0;
for ^100 -> $i { $rks = $rks + @rk[$i] }
say "elem-read-kind-change=", $rks;
my @sb;
for ^300 -> $i { @sb[$i] = $i * 2 }
say "elem-store-append=", @sb.elems, " ", @sb.sum;
my @sc = 0 xx 50;
loop (my $si = 0; $si < 500; $si++) { @sc[$si % 50] = $si }
say "elem-store-cloop=", @sc.sum;
my @sn;
for ^100 -> $i { @sn[$i] = $i * 0.25e0 }
say "elem-store-num=", @sn.sum;
my @sg;
for ^10 -> $i { @sg[$i * 2] = $i }
say "elem-store-gap=", @sg.elems, " ", @sg.grep(*.defined).sum;
my Int @st;
for ^50 -> $i { @st[$i] = $i }
say "elem-store-typed=", @st.sum;
my @rw = ^40;
for ^40 -> $i { @rw[$i] = @rw[$i] + 1 }
say "elem-read-write-same=", @rw.sum;
my @al = ^30;
my @al2 := @al;
for ^30 -> $i { @al2[$i] = @al[29 - $i] }
say "elem-alias=", @al.join(",");
my @rb;
my $rbig = 1;
for ^300 -> $i { @rb[$i] = $i; $rbig = $rbig * 3 }
say "elem-store-then-bail=", @rb.sum, " ", $rbig.chars;
my $bm = 0;
for ^2000 -> $i { $bm = $bm + ($i - 1000) % 7 + ($i +& 12) + ($i +| 3) + ($i +^ 5) + ($i - 1000) % -6 }
say "bitops-mod=", $bm;
my $pc = 0;
my $pd = 1000;
for ^3000 -> $i { $pc++; --$pd; $pc = $pc + $pd-- - ++$pd }
say "incdec=", $pc, " ", $pd;
my $px = 9223372036854775000;
for ^2000 { $px++ }
say "incdec-overflow=", $px;

# From several threads.
say "threads=", await((^4).map(-> $o { start { my $z = 0; for ^10000 -> $i { $z = $z + $i * $o }; $z } })).join(",");
