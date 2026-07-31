use v6;
use nqp;
use Test;

# The nqp:: op subset CBOR::Simple's encoder/decoder is written in
# (todo/tickets/cbor-simple-nqp-buf-ops.md): value ops dispatched in
# runtime/nqp_ops.rs, control-flow special forms compiled in
# compiler/nqp_forms.rs, and the nqp::const::BINARY_* flag constants.
# Runs under raku too — which also pins that mutsu's constant values and
# flag decoding match MoarVM's.

plan 17;

# constants + bit ops (the CBOR flag idiom)
my int $be16 = nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT, BigEndian);
is $be16, 6, 'BINARY_SIZE_16_BIT +| BigEndian matches MoarVM value';
is nqp::bitshiftl_i(3, 5), 96, 'bitshiftl_i';
is nqp::bitand_i(0xE3, 0x1F), 3, 'bitand_i';

# int arith / comparisons
is nqp::add_i(40, 2), 42, 'add_i';
is nqp::islt_i(1, 2), 1, 'islt_i true is int 1';
is nqp::iseq_i(3, 4), 0, 'iseq_i false is int 0';
is nqp::add_I(2, 3, Int), 5, 'add_I';

# control-flow special forms: lazy branches, looping condition
my int $side = 0;
my $picked = nqp::if(nqp::islt_i(1, 2), "then", ($side = 1));
is $picked, 'then', 'nqp::if picks the then branch';
is $side, 0, 'nqp::if did NOT evaluate the else branch';
my int $i = 0;
my int $sum = 0;
nqp::while(nqp::islt_i($i, 5), nqp::stmts(($sum = nqp::add_i($sum, $i)), ($i = nqp::add_i($i, 1))));
is $sum, 10, 'nqp::while/nqp::stmts loop';

# istype
is nqp::istype("x", Str), 1, 'istype Str';
is nqp::istype(42, Str), 0, 'istype negative';

# buffer read/write (the CBOR hot path). NB: bound, not assigned — rakudo's
# nqp ops reject a Scalar-containerized buffer, and CBOR::Simple binds too.
my $buf := buf8.new;
nqp::writeuint($buf, 0, 0xA2, nqp::bitor_i(nqp::const::BINARY_SIZE_8_BIT, NativeEndian));
nqp::writeuint($buf, 1, 0x1234, $be16);
is-deeply $buf, buf8.new(0xA2, 0x12, 0x34), 'writeuint 8-bit and 16-bit big-endian';
is nqp::readuint($buf, 1, $be16), 0x1234, 'readuint round-trips';

# splice-append + elems (the encoder's string path)
my $utf8 := Encoding::Registry.find("utf8").encoder.encode-chars("ab");
is nqp::elems($utf8), 2, 'elems of encoded blob';
nqp::splice($buf, $utf8, 3, 2);
is-deeply $buf, buf8.new(0xA2, 0x12, 0x34, 0x61, 0x62), 'splice appends at end';

# The closure-position shape that corrupted CBOR encoding: a value-typed
# scalar captured and ++'d by a stored closure stays coherent with the
# owner's own reassignment (shared-cell capture, not a snapshot).
sub track(Int:D $pos is rw) {
    my &bump = -> { $pos++ };
    bump(); bump();
    $pos = $pos + 10;
    bump();
    $pos
}
is track(my $p = 0), 13, 'captured value-typed rw scalar stays coherent across owner reassignment';

done-testing;
