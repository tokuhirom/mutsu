use nqp;

# Native-int list ops on a `Uni` from TRIR routines: one a routine built and
# returned, and one bound from `Uni.new`. Also the generic `nqp::` data ops a
# TRIR body with deferred frames runs without pushing them (`create` of a
# core type, `push` and `elems` on plain data).

my sub fill(int $k) {
    my $u := nqp::create(Uni);
    my int $i = 0;
    nqp::while(
      nqp::islt_i($i, $k),
      nqp::stmts(
        nqp::push_i($u, nqp::add_i(65, $i)),
        ($i = nqp::add_i($i, 1))
      )
    );
    $u
}

my sub drain(Uni:D \codes --> int) {
    my int $n = 0;
    nqp::while(
      nqp::elems(codes),
      ($n = nqp::add_i(nqp::mul_i($n, 31), nqp::shift_i(codes)))
    );
    $n
}

my sub count(Uni:D \codes --> int) { nqp::elems(codes) }

my sub buffer(int $k) {
    my $b := nqp::create(IterationBuffer);
    my int $i = 0;
    nqp::while(
      nqp::islt_i($i, $k),
      nqp::stmts(
        nqp::push($b, $i),
        ($i = nqp::add_i($i, 1))
      )
    );
    nqp::elems($b)
}

my $u := fill(5);
say 'filled=', count($u), ' ', $u.list.join(',');
say 'drained=', drain($u), ' left=', count($u);

my $bound := Uni.new(72, 105, 33);
say 'bound=', count($bound), ' ', drain($bound), ' left=', count($bound);

say 'buffer=', buffer(7);
