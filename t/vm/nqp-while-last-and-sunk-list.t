use v6;
use Test;
use nqp;

# From Math::Combinatorics (Variations / Multicombinations iterators):
# `nqp::while(cond, nqp::if(test, last))` needs the loop registered for `last`,
# and an `nqp::while` that is an element of a statement-root comma list is
# sunk (it runs eagerly), as rakudo does.
plan 4;

{
    my $i = 5;
    nqp::while(
        nqp::isge_i(($i = nqp::sub_i($i, 1)), 0),
        nqp::if(nqp::isgt_i($i, 2), last)
    );
    is $i, 4, 'last in the body of a sunk nqp::while leaves the loop';
}

sub counted() {
    my $j = 4;
    my $v = 0;
    nqp::while(
        nqp::isge_i(($j = nqp::sub_i($j, 1)), 0),
        nqp::if(nqp::isle_i($j, 1), last, ($v = $v + 1))
    );
    "$j $v"
}
is counted(), '1 2', 'last in the else branch, with side effects before it';

sub in-list() {
    my $j := 0;
    my $m = 5;
    (my $k := 1),
    nqp::while(nqp::isle_i($j, $m),
        nqp::if(nqp::isle_i($j, 2), ($j := nqp::add_i($j, 1)), last)
    ),
    nqp::if(1, ($k := 7));
    "$j $k"
}
is in-list(), '3 7', 'an nqp::while element of a statement-root list runs';

{
    my $n = 0;
    nqp::while(nqp::islt_i($n, 3), ($n = nqp::add_i($n, 1)));
    is $n, 3, 'a loop without a loop control still runs as before';
}
