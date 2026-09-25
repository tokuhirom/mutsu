use v6;
use Test;
use nqp;

# `nqp::repeat_while` / `nqp::repeat_until` -- the post-test loops -- were
# unsupported ("Unsupported nqp:: op: nqp::repeat_until"), which blocked the
# `are` distribution. Issue #9347. The body runs once before the condition is
# first tested. Both the bytecode compiler and the TRIR lowering (native-int
# subs) are covered.

plan 9;

{
    my $i = 0;
    nqp::repeat_until(nqp::isge_i($i, 3), $i++);
    is $i, 3, 'repeat_until loops until the condition holds';
}

{
    my $j = 0;
    nqp::repeat_while(nqp::islt_i($j, 3), $j++);
    is $j, 3, 'repeat_while loops while the condition holds';
}

{
    my $k = 5;
    nqp::repeat_while(nqp::islt_i($k, 3), $k++);
    is $k, 6, 'repeat_while runs the body once before a false condition';
}

{
    my $m = 5;
    nqp::repeat_until(1, $m++);
    is $m, 6, 'repeat_until runs the body once before a true condition';
}

{
    my @seen;
    my $n = 0;
    nqp::repeat_while(nqp::islt_i($n, 3), nqp::stmts(@seen.push($n), $n++));
    is-deeply @seen, [0, 1, 2], 'the body sees each iteration in order';
}

# Native-int subs lower through TRIR.
sub sum-below(int $n) {
    my int $c = 0;
    my int $s = 0;
    nqp::repeat_while(nqp::islt_i($c, $n), nqp::stmts(($s = $s + $c), ($c = $c + 1)));
    $s
}
sub step-past(int $n) {
    my int $c = 0;
    nqp::repeat_until(nqp::isge_i($c, $n), ($c = $c + 2));
    $c
}

is sum-below(10), 45, 'repeat_while in a native-int sub';
is sum-below(0), 0, 'repeat_while in a native-int sub runs the body once';
is step-past(7), 8, 'repeat_until in a native-int sub';
is step-past(0), 2, 'repeat_until in a native-int sub runs the body once';
