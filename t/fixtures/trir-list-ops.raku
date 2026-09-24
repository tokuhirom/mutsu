use nqp;

# Fixture for t/vm/codegen/adr0112-trir-list-ops.t: one routine per shape of
# the typed list ops ADR-0112 Step 3 gives TRIR (`nqp::elems`,
# `nqp::shift_i`, `nqp::push_i` on a boxed operand), each called twice (a
# TRIR bug gives a right first answer and a wrong later one).

# `elems` of every list-ish kind, and of a hash, which the typed op hands on
# to the generic one.
sub counts(Uni:D \codes) {
    my $list := nqp::list(1, 2, 3);
    my $hash := nqp::hash('a', 1, 'b', 2);
    my $buffer := nqp::create(IterationBuffer);
    nqp::push($buffer, 7);
    nqp::elems(codes) ~ ' ' ~ nqp::elems($list) ~ ' ' ~ nqp::elems($hash)
      ~ ' ' ~ nqp::elems($buffer)
}

# `shift_i` into a native, a sized native (which wraps) and an expression.
sub drain-sized() {
    my $list := nqp::list_i(-1, 300, 5, 2);
    my uint32 $u = nqp::shift_i($list);
    my int8 $s = nqp::shift_i($list);
    my int $sum = nqp::add_i(nqp::shift_i($list), nqp::shift_i($list));
    $u ~ ' ' ~ $s ~ ' ' ~ $sum ~ ' ' ~ nqp::elems($list)
}

# `push_i` of a native and of a boxed value, reading the pushed value back.
sub build(int $n) {
    my $out := nqp::create(Uni);
    my int $i = 0;
    my $boxed = 70;
    nqp::while(
      nqp::islt_i($i, $n),
      nqp::stmts(
        nqp::push_i($out, nqp::add_i(97, $i)),
        ($i = nqp::add_i($i, 1))
      )
    );
    my $last := nqp::push_i($out, $boxed);
    nqp::strfromcodes($out) ~ ' ' ~ $last ~ ' ' ~ nqp::elems($out)
}

# The shape JSON::Fast's `unjsonify-string` has: consume a Uni from the
# front, copy what is kept onto a fresh one.
sub copy-except(Uni:D \codes, int $skip) {
    my $out := nqp::create(Uni);
    nqp::while(
      nqp::elems(codes),
      nqp::if(
        nqp::iseq_i((my uint32 $o = nqp::shift_i(codes)), $skip),
        nqp::null(),
        nqp::push_i($out, $o)
      )
    );
    nqp::strfromcodes($out) ~ ' ' ~ nqp::elems(codes)
}

# The operand-direct forms on a routine-local slot (not a parameter),
# consumed and refilled in the same loop.
sub rotate(int $n) {
    my $q := nqp::create(Uni);
    my int $i = 0;
    nqp::while(nqp::islt_i($i, 4), nqp::stmts(nqp::push_i($q, nqp::add_i(97, $i)), ($i = nqp::add_i($i, 1))));
    $i = 0;
    nqp::while(nqp::islt_i($i, $n), nqp::stmts(nqp::push_i($q, nqp::shift_i($q)), ($i = nqp::add_i($i, 1))));
    nqp::strfromcodes($q) ~ ' ' ~ nqp::elems($q)
}

for ^2 {
    say 'counts => ', counts(nqp::strtocodes('héllo', nqp::const::NORMALIZE_NFD, nqp::create(NFD)));
    say 'drain-sized => ', drain-sized();
    say 'build => ', build(3);
    say 'rotate => ', rotate(5);
    say 'copy-except => ', copy-except(nqp::strtocodes('a/b/c', nqp::const::NORMALIZE_NFC, nqp::create(NFC)), 47);
}
