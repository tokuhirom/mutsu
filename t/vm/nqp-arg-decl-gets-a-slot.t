use v6;
use Test;

# `nqp::op((my int $x = EXPR), ...)` declares `$x` inside an argument. The
# declaration takes a real lexical slot like the statement form (#14774); its
# value, its later reads and writes, per-iteration re-declaration and scoping
# must be unchanged.

plan 8;

sub classify(int $n) {
    nqp::if(
      nqp::isle_i((my int $c = $n), 92) || nqp::isge_i($c, 128),
      $c,
      -1
    )
}
is classify(50), 50, 'the declared value is read back in a later operand';
is classify(100), -1, 'and the other branch sees the same value';
is classify(200), 200, 'a value past the upper bound is read back';

sub sum-codes(Str $s) {
    my $codes := $s.NFD;
    my int $i = -1;
    my int $sum = 0;
    nqp::while(
      nqp::islt_i(++$i, nqp::elems($codes)),
      nqp::stmts(
        (my int $code = nqp::atpos_i($codes, $i)),
        ($sum = nqp::add_i($sum, $code))
      )
    );
    $sum
}
is sum-codes('abc'), 294, 'a declaration re-evaluated on every loop iteration';
is sum-codes(''), 0, 'a loop that never runs';

sub shadow-test {
    my int $v = 7;
    my $r = nqp::add_i((my int $w = $v), 1);
    $w = nqp::add_i($w, 100);
    "$r $w $v"
}
is shadow-test(), '8 107 7', 'the declared variable is writable and independent';

sub scoped {
    my int $outer = 1;
    {
        my $t = nqp::add_i((my int $outer = 5), 0);
        return "$t $outer";
    }
}
is scoped(), '5 5', 'an inner declaration shadows an outer one';

my $x = 'out';
sub no-leak { nqp::add_i((my int $x = 3), 1) }
is no-leak() ~ $x, '4out', 'the declaration does not leak to the caller';
