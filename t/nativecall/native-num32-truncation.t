use v6;
use nqp;
use Test;

# A native `num32` scalar must narrow to IEEE-754 single precision AT THE
# STORE, both in statement form (`my num32 $x = EXPR;`) and expression
# context (`f((my num32 $x = EXPR))`, e.g. the exact shape CBOR::Simple's
# float encoder uses: `nqp::iseq_n($_, (my num32 $num32 = $_))` to decide
# whether a Num safely round-trips through a 4-byte CBOR float). Previously
# mutsu stored the value untouched at full 64-bit precision in BOTH forms, so
# the round-trip check was always true and every double got wrongly encoded
# as a lossy 4-byte float.

plan 8;

# Statement-form declaration.
my $x = -4.1e0;
my num32 $y = $x;
isnt $y, $x, 'num32 statement-form declaration truncates precision';
is $y, -4.099999904632568e0, 'num32 truncated value matches float32 round-trip';

# Expression-context declaration (used as a call argument) — the shape
# CBOR::Simple actually uses.
is nqp::iseq_n($x, (my num32 $num32a = $x)), 0,
    'num32 expression-context declaration truncates (iseq_n sees the difference)';
is $num32a, -4.099999904632568e0,
    'num32 expression-context declared value matches float32 round-trip';

# A value that DOES round-trip losslessly through float32 stays unchanged.
my $exact = 1.5e0;
is nqp::iseq_n($exact, (my num32 $num32b = $exact)), 1,
    'num32 truncation is a no-op for a value already exact in float32';

# num64 needs no truncation — it is mutsu's native Num storage width already.
my num64 $z = $x;
is $z, $x, 'num64 does not truncate (already full precision)';

# Reassignment to an already-declared num32 variable also truncates.
my num32 $w = 1.5e0;
$w = 1.1e0;
isnt $w, 1.1e0, 'num32 reassignment (not just initial declaration) truncates';
is $w, 1.100000023841858e0, 'num32 reassigned value matches float32 round-trip';
