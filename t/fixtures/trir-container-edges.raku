use nqp;

# Fixture for t/vm/codegen/adr0112-trir-container-edges.t (#9122): the
# per-element work of JSON::Fast's `parse-array` -- a fresh IterationBuffer
# installed as an Array's `$!reified`, then filled -- and `nqp::ordat` reading
# one text repeatedly, then switching texts. Each routine is called twice (a
# TRIR bug gives a right first answer and a wrong later one).

# A fresh buffer installed into an empty array, filled afterwards: the array
# sees every push.
sub fresh-then-push(int $n) {
    my @result;
    nqp::bindattr(@result, List, '$!reified',
      my $buffer := nqp::create(IterationBuffer));
    my int $i = 0;
    nqp::while(
      nqp::islt_i($i, $n),
      nqp::stmts(
        nqp::push($buffer, nqp::p6scalarwithvalue(
          nqp::getattr(@result, Array, '$!descriptor'), $i * 10)),
        ($i = nqp::add_i($i, 1))
      )
    );
    @result
}

# A fresh buffer installed into an array that already had elements: the
# array is emptied, as the buffer it now shares has none. (Not a TRIR routine:
# it declares an initialized array.)
sub fresh-over-filled() {
    my @result = 1, 2, 3;
    nqp::bindattr(@result, List, '$!reified',
      my $buffer := nqp::create(IterationBuffer));
    nqp::push($buffer, 9);
    @result.elems ~ ':' ~ @result.join(',')
}

# A buffer that already holds elements: they are copied in, and the buffer
# stays shared with the array afterwards.
sub filled-then-bind() {
    my $buffer := nqp::create(IterationBuffer);
    nqp::push($buffer, 'a');
    nqp::push($buffer, 'b');
    my @result;
    nqp::bindattr(@result, List, '$!reified', $buffer);
    nqp::push($buffer, 'c');
    @result
}

# Codepoints of two texts read alternately, the second one non-ASCII.
sub alternate(str $a, str $b, int $n) {
    my int $i = 0;
    my $out := nqp::list_s;
    nqp::while(
      nqp::islt_i($i, $n),
      nqp::stmts(
        nqp::push_s($out, nqp::coerce_is(nqp::ordat($a, $i))),
        nqp::push_s($out, nqp::coerce_is(nqp::ordat($b, $i))),
        ($i = nqp::add_i($i, 1))
      )
    );
    nqp::join(',', $out)
}

# Past-the-end and negative positions.
sub edges(str $t) {
    nqp::ordat($t, 0) ~ ' ' ~ nqp::ordat($t, nqp::chars($t)) ~ ' '
      ~ nqp::ordat($t, -1)
}

for ^2 {
    sub show(@a) { @a.elems ~ ':' ~ @a.join(',') }
    say 'fresh-then-push => ', show fresh-then-push(4);
    say 'fresh-over-filled => ', fresh-over-filled();
    say 'filled-then-bind => ', show filled-then-bind();
    say 'alternate => ', alternate('abc', "é\x[1F600]z", 3);
    say 'alternate-again => ', alternate('xyz', 'abc', 2);
    say 'edges => ', edges('hi');
}
