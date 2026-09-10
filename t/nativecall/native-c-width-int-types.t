use Test;
use NativeCall;

# `NativeCall::Types` exports C-width integer aliases. The marshalling layer
# already mapped every one of these to a 64-bit C integer, but they were not
# declarable, so `has ulong $.length` / `our ulong constant zero = 0` — the shape
# DBDish::mysql::Native uses — died with "Type 'ulong' is not declared".

plan 12;

my ulong $u = 42;
is $u, 42, 'a ulong scalar holds its value';
my long $l = -42;
is $l, -42, 'a long scalar is signed';
my longlong $ll = -9223372036854775807;
is $ll, -9223372036854775807, 'longlong holds a 64-bit negative';
my ulonglong $ull = 18446744073709551615;
is $ull, 18446744073709551615, 'ulonglong holds the full unsigned range';
my size_t $s = 4096;
is $s, 4096, 'size_t holds its value';

# Signedness: an unsigned alias wraps rather than going negative.
my ulong $wrap = -1;
is $wrap, 18446744073709551615, 'ulong wraps -1 to the unsigned maximum';

# Attributes and signatures can name them.
class Field {
    has ulong $.length is rw;
    has long $.offset is rw;
    method span(ulong $extra --> ulong) { $!length + $extra }
}
my $f = Field.new(:length(10), :offset(-2));
is $f.length, 10, 'a ulong attribute';
is $f.offset, -2, 'a long attribute';
is $f.span(5), 15, 'a ulong parameter and return type';

# `our <native> constant` — the DBDish::mysql::Native declaration.
our ulong constant ULONG_ZERO = 0;
is ULONG_ZERO, 0, 'an `our ulong constant`';

# nativesizeof agrees with MoarVM: all of these are 8 bytes.
is nativesizeof(ulong), 8, 'nativesizeof(ulong) is 8';
is nativesizeof(size_t), 8, 'nativesizeof(size_t) is 8';
