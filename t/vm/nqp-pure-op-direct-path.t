use v6;
use Test;
use nqp;

# The pure `nqp::` value ops (add_i, iseq_i, bitand_i, add_n, ...) run off the
# operand stack without building an argument list or matching the op's name
# (#8900, `src/runtime/nqp_pure.rs`). That direct path is taken only when the
# call site's arity matches the op's own AND every operand is already exactly
# an `Int` (or, for a `_n` op, an `Int` or a `Num`); every other operand shape
# keeps the general argument-list path, which is where the `VarRef` unwrap,
# the container deref, the `Proxy` FETCH and the coercions live.
#
# So this file's job is that BOTH paths answer what they answered before, and
# it is split accordingly: the first section is native operands (the direct
# path), the second is the operand shapes that decline it.

plan 36;

# ---------------------------------------------------------------------------
# Native operands -- the direct path. Rakudo agrees with every line here.
# ---------------------------------------------------------------------------

is nqp::add_i(3, 4), 7, 'add_i, native operands';
is nqp::sub_i(10, 4), 6, 'sub_i, native operands';
is nqp::mul_i(6, 7), 42, 'mul_i, native operands';
is nqp::neg_i(5), -5, 'neg_i, native operand';
is nqp::abs_i(-5), 5, 'abs_i, native operand';
is nqp::bitand_i(12, 10), 8, 'bitand_i, native operands';
is nqp::bitor_i(12, 10), 14, 'bitor_i, native operands';
is nqp::bitxor_i(12, 10), 6, 'bitxor_i, native operands';
is nqp::bitneg_i(0), -1, 'bitneg_i, native operand';
is nqp::bitshiftl_i(1, 10), 1024, 'bitshiftl_i, native operands';
is nqp::bitshiftr_i(1024, 10), 1, 'bitshiftr_i, native operands';
is nqp::iseq_i(3, 3), 1, 'iseq_i answers a native int, not a Bool';
is nqp::isne_i(3, 4), 1, 'isne_i, native operands';
is nqp::islt_i(3, 4), 1, 'islt_i, native operands';
is nqp::isle_i(4, 4), 1, 'isle_i, native operands';
is nqp::isgt_i(5, 4), 1, 'isgt_i, native operands';
is nqp::isge_i(4, 4), 1, 'isge_i, native operands';
is nqp::cmp_i(3, 9), -1, 'cmp_i, native operands';
is nqp::not_i(0), 1, 'not_i, native operand';
is nqp::add_n(1.5e0, 2.25e0), 3.75e0, 'add_n, native num operands';
is nqp::sub_n(5e0, 1.5e0), 3.5e0, 'sub_n, native num operands';
is nqp::mul_n(2e0, 4e0), 8e0, 'mul_n, native num operands';
is nqp::islt_n(1e0, 2e0), 1, 'islt_n, native num operands';
is nqp::isnanorinf(0e0), 0, 'isnanorinf on a finite num';
is nqp::isnanorinf(Inf), 1, 'isnanorinf on Inf';

# Native-int arithmetic wraps rather than trapping (it must not panic in a
# debug build either).
is nqp::add_i(9223372036854775807, 1), -9223372036854775808,
    'add_i wraps at the native-int edge';

# The partial ops are deliberately NOT on the direct path, so their errors are
# raised exactly where they always were.
dies-ok { nqp::div_i(1, 0) }, 'div_i by zero still dies';
is nqp::div_i(-7, 2), -4, 'div_i still floor-divides';

# ---------------------------------------------------------------------------
# Operand shapes that decline the direct path.
#
# Every assertion below pins mutsu's EXISTING general-path behaviour, which is
# more lenient than NQP's: rakudo rejects each of these operands outright
# ("This type cannot unbox to a native integer: P6opaque, Str"). Whether mutsu
# should become that strict is a separate question and NOT this change's --
# what matters here is that adding the direct path did not quietly move any of
# these answers, so the coercions must still be reached.
# ---------------------------------------------------------------------------

# A `Str` operand: only the general path coerces.
is nqp::add_i("3", "4"), 7, 'add_i coerces Str operands through the general path';
is nqp::iseq_i("5", 5), 1, 'iseq_i with one Str operand';

# A `Num` is not an `_i` operand, so `add_i` declines and truncates on the
# general path; an `Int` IS accepted as a `_n` operand, on either path.
is nqp::add_i(2.9e0, 1), 3, 'add_i truncates a Num operand through the general path';
is nqp::mul_n(2, 4e0), 8e0, 'mul_n accepts an Int operand';

# An `is rw` parameter reaches the op as a container, which the direct path
# declines and the general path dereferences.
sub bump(Int $n is rw) { nqp::add_i($n, 1) }
my $held = 41;
is bump($held), 42, 'add_i reads through an is rw container';

# A `Proxy` operand must still be FETCHed -- only the general path does that.
my $fetched = 0;
my $proxy := Proxy.new(FETCH => -> $ { $fetched++; 20 }, STORE => -> $, $v { $v });
is nqp::add_i($proxy, 22), 42, 'add_i FETCHes a Proxy operand';
ok $fetched > 0, 'the Proxy was actually FETCHed';

# A value too large for a native int saturates, as `to_int` has always done.
is nqp::add_i(2 ** 70, 0), 9223372036854775807,
    'a BigInt operand saturates through the general path';
