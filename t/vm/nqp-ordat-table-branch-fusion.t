use Test;
use nqp;

# TRIR fuses a character-class table test (`nqp::atpos_i($table, $ord)` then a
# branch), a codepoint comparison read through an `is rw` position or a
# native slot, a boxed slot copy and an in-place unbox into single ops
# (#8673). Each shape here must answer exactly what the stack form did,
# including inside a callee that is inlined into its caller.

plan 10;

my $ws := nqp::list_i();
nqp::bindpos_i($ws, $_, 1) for 9, 10, 13, 32;
nqp::bindpos_i($ws, 200, 0);

sub skip-ws(str $text, int $pos is rw) {
    nqp::while(
      nqp::atpos_i($ws, nqp::ordat($text, $pos)),
      ($pos = nqp::add_i($pos, 1))
    );
}

sub quoted(str $text, int $pos is rw) {
    nqp::if(nqp::iseq_i(nqp::ordat($text, $pos), 34), 1, 0)
}

sub count-quotes(str $text --> int) {
    my int $n = 0;
    my int $i = 0;
    my int $end = nqp::chars($text);
    nqp::while(
      nqp::islt_i($i, $end),
      nqp::stmts(
        nqp::if(nqp::iseq_i(nqp::ordat($text, $i), 34), ($n = nqp::add_i($n, 1))),
        ($i = nqp::add_i($i, 1))
      )
    );
    $n
}

sub copy-and-unbox($v) {
    my $a = $v;
    my $b = $a;
    nqp::add_i(nqp::unbox_i($b), 1)
}

my int $p = 0;
skip-ws("  \t\n x", $p);
is $p, 5, 'a table test skips whitespace through an is rw position';

$p = 0;
skip-ws("x  ", $p);
is $p, 0, 'a table test that fails at once does not advance';

$p = 2;
is quoted('a "b"', $p), 1, 'a codepoint test through the reference';
$p = 1;
is quoted('a "b"', $p), 0, 'a failing codepoint test through the reference';

is count-quotes('"a" "b" c'), 4, 'a codepoint test on a native slot';
is count-quotes(''), 0, 'an empty string has no quotes';

is copy-and-unbox(41), 42, 'a boxed slot copy and an in-place unbox';

# Through a caller, so the inlined callee's reference becomes the caller's
# own variable and its table index is shifted past the caller's own.
sub scan(str $text) {
    my int $pos = 0;
    skip-ws($text, $pos);
    nqp::if(
      quoted($text, $pos),
      nqp::add_i(nqp::mul_i($pos, 10), 1),
      nqp::mul_i($pos, 10)
    )
}
is scan('   "x'), 31, 'the inlined table and codepoint tests see the caller position';
is scan('"x'), 1, 'no whitespace before the quote';

my int $sum = 0;
for ^30 { $sum = nqp::add_i($sum, scan(' "')) }
is $sum, 330, 'repeated inlined scans';
