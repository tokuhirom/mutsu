# Shapes for ADR-19288's native int kernels (t/vm/codegen/jit-int-kernel.t).
# Each line is `name=answer`. The test runs this file with kernels compiled on
# the first call, with kernels off and with the JIT off, and requires the three
# transcripts to be identical: a kernel's answer, or its bail and generic rerun,
# must be exactly what the generic path answers.

sub fib($n) { if $n < 2 { return $n }; return fib($n - 1) + fib($n - 2) }
sub fib-typed(Int $n --> Int) { if $n <= 1 { return $n }; return fib-typed($n - 1) + fib-typed($n - 2) }
sub tak($x, $y, $z) {
    if $y < $x { return tak(tak($x - 1, $y, $z), tak($y - 1, $z, $x), tak($z - 1, $x, $y)) }
    return $z;
}
say "fib=", fib(20);
say "fib-typed=", fib-typed(20);
say "tak=", tak(12, 8, 4);

# Overflow: the kernel bails and the generic path promotes to a big integer.
sub pow2($n) { if $n == 0 { return 1 }; return 2 * pow2($n - 1) }
say "pow2-small=", pow2(10);
say "pow2-big=", pow2(70);
sub add1($x) { $x + 1 }
say "add1-max=", add1(9223372036854775807);
sub sub1($x) { $x - 1 }
say "sub1-min=", sub1(-9223372036854775807 - 1);

# Floored div / mod, a zero divisor and `MIN div -1`.
sub dv($a, $b) { $a div $b }
sub md($a, $b) { $a mod $b }
say "div=", (dv(7, 2), dv(-7, 2), dv(7, -2), dv(-7, -2)).join(",");
say "mod=", (md(7, 2), md(-7, 2), md(7, -2), md(-7, -2)).join(",");
say "div-zero=", (try { my $q = dv(7, 0); $q.Int }) // $!.^name;
say "mod-zero=", (try { my $q = md(7, 0); $q.Int }) // $!.^name;
say "div-min=", dv(-9223372036854775807 - 1, -1);

# Comparisons, a declaration, falling off the end.
sub cmp6($a, $b) {
    my $r = 0;
    if $a < $b { return 1 }
    if $a <= $b { return 2 }
    if $a > $b + 10 { return 3 }
    if $a >= $b + 5 { return 4 }
    if $a == $b + 1 { return 5 }
    if $a != $b + 2 { return 6 }
    $r
}
say "cmp6=", (cmp6(1, 2), cmp6(2, 2), cmp6(20, 2), cmp6(8, 2), cmp6(3, 2), cmp6(7, 2), cmp6(4, 2)).join(",");
sub step(Int $x) { my $y = $x * 2 + 1; $y }
say "step=", step(20);

# Arguments a kernel must refuse: the generic path answers.
sub id1($n) { if $n < 2 { return $n }; return id1($n - 1) }
say "bool-arg=", id1(True).raku;
say "intstr-arg=", id1(<1>).^name;
say "num-arg=", id1(1.5e0).raku;
say "rat-arg=", id1(1.5).raku;
say "big-arg=", add1(2 ** 70);
sub sq($x) { $x * $x }
say "num-sq=", sq(2.5e0);

# Deep recursion bails on the depth bound and the generic path finishes it.
sub depth($n) { if $n == 0 { return 0 }; return 1 + depth($n - 1) }
say "depth=", depth(3000);

# A package routine.
package P { our sub pf($n) { if $n < 2 { return $n }; return pf($n - 1) + pf($n - 2) } }
say "package=", P::pf(15);

# A routine wrapped after its kernel was built must reach its wrapper. (Not
# a recursive one: rakudo's optimizer stops a warmed-up recursive call from
# reaching a later wrapper, which is not something to pin.)
sub w($n) { $n * 3 }
my $before = 0;
$before += w(5) for ^200;
&w.wrap(-> $n { callsame() + 1000 });
say "wrap=", $before, ",", w(5);

# Calls from several threads.
say "threads=", await((^4).map({ start { fib(18) } })).join(",");
