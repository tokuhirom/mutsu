use nqp;

# Fixture for t/vm/codegen/adr0112-trir-short-circuit.t: `&&` / `||` yield an
# OPERAND, not its truth value. Each routine is called twice, so both the
# resolving call and a resolution-cache hit run it.

sub pred($s) { $s.contains('#') }

# Boxed operands: the untyped path's business (the routine declines).
sub boxed-and($s) { pred($s) && 'both' }
sub boxed-or($s)  { pred($s) || 'fallback' }

# Native int operands, including an int-returning op reached through the
# generic `nqp::` path: the int bank's own value is the result.
sub int-and(int $a, int $b) { nqp::isgt_i($a, 0) && nqp::add_i($a, $b) }
sub int-or(int $a, int $b)  { nqp::isgt_i($a, 0) || nqp::add_i($a, $b) }

for ^2 {
    say 'boxed-and => ', boxed-and('#x'), ' ', boxed-and('x');
    say 'boxed-or => ', boxed-or('#x'), ' ', boxed-or('x');
    say 'int-and => ', int-and(3, 4), ' ', int-and(0, 4);
    say 'int-or => ', int-or(3, 4), ' ', int-or(0, 4);
}
