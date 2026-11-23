use nqp;

# `nqp::ordat` at a position read from a native slot, and through an
# `int $pos is rw` reference (the `nom-ws` shape). The reference form is
# also inlined into callers (ADR-8673 D2), where the reference becomes the
# caller's own slot. Positions past either end read -1.

my $ws := nqp::list_i;
nqp::bindpos_i($ws, 32, 1);
nqp::bindpos_i($ws, 9, 1);
nqp::push_i($ws, 0);

my sub skip-ws(str $text, int $pos is rw --> Nil) {
    nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ++$pos);
}

my sub first-word(str $text --> int) {
    my int $pos = 0;
    skip-ws($text, $pos);
    $pos
}

my sub codes(str $text, int $from, int $to) {
    my int $i = $from;
    my $out := nqp::list_i;
    nqp::while(
      nqp::isle_i($i, $to),
      nqp::stmts(nqp::push_i($out, nqp::ordat($text, $i)), ($i = nqp::add_i($i, 1)))
    );
    $out
}

my int $p = 0;
skip-ws("  \t x", $p);
say 'skipped=', $p;
say 'first-word=', first-word("   abc"), ' ', first-word("abc"), ' ', first-word("    ");
my $c := codes("héllo", -1, 5);
say 'codes=', (^nqp::elems($c)).map({ nqp::atpos_i($c, $_) }).join(',');
