use nqp;

# Fixture for t/vm/codegen/adr0112-trir-first-call.t: every routine below is
# called exactly ONCE, so each call is the one that resolves its name. Those
# calls used to run the untyped body even for a TRIR routine; only later
# resolution-cache hits entered the chunk.

# A hot loop behind a single call.
sub spin(int $n) {
    my int $i = 0;
    my int $s = 0;
    nqp::while(nqp::islt_i($i, $n), nqp::stmts(
      ($s = nqp::add_i($s, $i)),
      ($i = nqp::add_i($i, 1))
    ));
    $s
}

# An `is rw` native written back on the resolving call.
sub advance(str $t, int $pos is rw) {
    nqp::while(nqp::iseq_i(nqp::ordat($t, $pos), 32), ($pos = nqp::add_i($pos, 1)));
    $pos
}

# A caller that stays untyped (it narrows a call result into a native), whose
# loop calls a TRIR routine: the first of those calls resolves the name.
sub depth(str $t, int $pos is rw) {
    my int $d = 0;
    nqp::while(nqp::iseq_i(nqp::ordat($t, $pos), 40), nqp::stmts(
      ($pos = nqp::add_i($pos, 1)),
      ($d = nqp::add_i($d, 1))
    ));
    $d
}
sub nest(str $t, int $pos is rw) {
    my int $sum = 0;
    nqp::while(nqp::islt_i($pos, nqp::chars($t)), nqp::stmts(
      (my int $d = depth($t, $pos)),
      ($sum = nqp::add_i($sum, $d)),
      ($pos = nqp::add_i($pos, 1))
    ));
    $sum
}

# A named argument is not the typed shape: the untyped path binds (and here
# rejects) it.
sub positional(int $a) { nqp::add_i($a, 1) }

# A wrapped routine must reach its wrapper on its first call too.
sub wrapped(Int $a) { $a * 2 }

say 'spin => ', spin(1000);
my int $p = 0;
say 'advance => ', advance('    x', $p), ' ', $p;
my int $q = 0;
say 'nest => ', nest('((( (( (', $q), ' ', $q;
say 'named => ', (try positional(:a(1))) // 'rejected';
&wrapped.wrap(sub (|c) { callsame() + 1 });
say 'wrapped => ', wrapped(20);
