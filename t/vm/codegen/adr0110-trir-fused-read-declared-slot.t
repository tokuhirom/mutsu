use nqp;
use Test;

plan 3;

# A TRIR body reads `nqp::atpos_i($c, $i)` / `nqp::ordat($s, $i)` straight out
# of the slot when `$c` is not reassigned after its declaration -- now also
# when the declaration's initializer is not an `nqp::` op (`my $c := text.NFD`,
# JSON::Fast's `str-escape`). Each routine is called repeatedly so the TRIR
# path is the one exercised; every answer is rakudo 2026.11's.

sub sum-codes(\text) {
    my $codes := text.NFD;
    my int $i = -1;
    my int $sum = 0;
    nqp::while(nqp::islt_i(++$i, nqp::elems($codes)), $sum = nqp::add_i($sum, nqp::atpos_i($codes, $i)));
    $sum
}
is (sum-codes('abc') for ^3).join(','), '294,294,294', 'atpos_i on a slot declared from a method call';

sub ords(Str $s) {
    my $t := $s.Str;
    my int $i = -1;
    my int $acc = 0;
    nqp::while(nqp::islt_i(++$i, nqp::chars($t)), $acc = nqp::add_i($acc, nqp::ordat($t, $i)));
    $acc
}
is (ords("ab") for ^3).join(","), "195,195,195", 'ordat on a slot declared from a method call';

# A slot reassigned later in a loop is read as it stands at each step.
sub later-write() {
    my $c := nqp::list_i(1, 2);
    my int $i = 0;
    my int $acc = 0;
    nqp::while(nqp::islt_i($i, 2), nqp::stmts(
        ($acc = nqp::add_i(nqp::mul_i($acc, 10), nqp::atpos_i($c, 0))),
        ($c := nqp::list_i(5)),
        ++$i));
    $acc
}
is (later-write() for ^3).join(','), '15,15,15', 'a later rebinding is seen by the next read';
