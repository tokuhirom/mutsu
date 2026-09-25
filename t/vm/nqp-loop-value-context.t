use v6;
use nqp;
use Test;

# The `nqp::` loop forms (`while`, `until`, `repeat_while`, `repeat_until`)
# used to compile to a plain jump loop yielding Nil everywhere. That is right
# where the loop is sunk (a statement, a block's tail), but where its value is
# used rakudo yields a lazy Seq of the body values (#9415).

plan 21;

{
    my $i = 0;
    is-deeply nqp::while(nqp::islt_i($i, 3), $i++), (0, 1, 2).Seq, 'while in value position is a Seq of body values';
    my $j = 0;
    is-deeply nqp::until(nqp::isge_i($j, 3), $j++), (0, 1, 2).Seq, 'until in value position';
    my $k = 0;
    is-deeply nqp::repeat_while(nqp::islt_i($k, 3), $k++), (0, 1, 2).Seq, 'repeat_while in value position';
    my $m = 0;
    is-deeply nqp::repeat_until(nqp::isge_i($m, 3), $m++), (0, 1, 2).Seq, 'repeat_until in value position';
    is-deeply nqp::while(0, 1), ().Seq, 'a loop that never runs its body is an empty Seq';
    my $r = 0;
    is-deeply nqp::repeat_while(0, $r++), (0,).Seq, 'a post-test loop runs its body once';
}

{
    my $a = 0;
    my @a = nqp::while(nqp::islt_i($a, 3), $a++);
    is-deeply @a, [0, 1, 2], 'list assignment reifies the loop';
    my $b = 0;
    is-deeply [nqp::while(nqp::islt_i($b, 3), $b++)], [0, 1, 2], 'array constructor';
    my $c = 0;
    is-deeply nqp::if(0, 1, nqp::while(nqp::islt_i($c, 3), $c++)), (0, 1, 2).Seq,
        'an nqp::if branch in value position';
}

{
    my $n = 0;
    my $s := nqp::while(nqp::islt_i($n, 3), $n++);
    is $n, 0, 'binding the Seq runs nothing yet';
    is-deeply $s.head(2).List, (0, 1), 'pulling two elements';
    is $n, 2, '... runs the body exactly twice';
}

{
    my $p = 0;
    nqp::while(nqp::islt_i($p, 3), $p++);
    is $p, 3, 'a sunk loop still runs eagerly';
    my $q = 0;
    is nqp::stmts(nqp::while(nqp::islt_i($q, 3), $q++), 5), 5, 'a non-final nqp::stmts operand is sunk';
    is $q, 3, '... and runs eagerly';
    my $o = 0;
    my $inner;
    nqp::while(nqp::islt_i($o, 2),
        nqp::stmts($o++, ($inner = 0), nqp::while(nqp::islt_i($inner, 2), $inner++)));
    is "$o $inner", '2 2', 'a loop nested in the body of a sunk loop is sunk';
}

{
    sub tail-loop() { my $i = 0; nqp::while(nqp::islt_i($i, 2), $i++) }
    is-deeply tail-loop(), Nil, 'a routine whose tail is a loop returns Nil';
    my $t = 0;
    is (-> { nqp::while(nqp::islt_i($t, 3), $t++) })(), Nil, 'a block whose tail is a loop returns Nil';
    is $t, 3, '... after running it';
}

# Hot enough for TRIR to take the routine: the tail stays Nil and a value
# position still yields the Seq.
{
    sub hot-tail(int $n) { my int $i = 0; nqp::while(nqp::islt_i($i, $n), ++$i) }
    sub count-it(\s) { s.elems }
    sub hot-value(int $n) { my int $i = 0; count-it(nqp::while(nqp::islt_i($i, $n), ++$i)) }
    my $nils = 0;
    for ^300 { $nils++ if hot-tail(3) === Nil }
    is $nils, 300, 'a hot routine with a loop tail keeps returning Nil';
    my $sum = 0;
    $sum += hot-value(3) for ^300;
    is $sum, 900, 'a hot routine using the loop value keeps getting the Seq';
}
