use Test;
use NativeCall;

plan 4;

# An unparameterized `CArray` parameter -- OpenSSL declares
# `RSA_sign(int32, Blob, int32, Blob, CArray, OpaquePointer)`, whose fifth
# argument is the `unsigned int*` output length. The element type is not in the
# signature, so it has to come from the argument, which `CArray[T].new` tags
# with its element type.
sub frexp(num64, CArray --> num64) is native { * }

my $exp = CArray[int32].new;
$exp[0] = 0;
my $mantissa = frexp(8e0, $exp);

is $mantissa, 0.5, 'frexp returned the mantissa';
is $exp[0], 4, 'the bare CArray out-parameter was written through';

$exp[0] = 0;
is frexp(0.75e0, $exp), 0.75, 'frexp returned the mantissa (second call)';
is $exp[0], 0, 'the bare CArray out-parameter was written through (second call)';
