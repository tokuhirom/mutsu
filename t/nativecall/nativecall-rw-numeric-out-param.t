use v6;
use Test;
use NativeCall;

# `is rw` NUMERIC parameters are out-parameters: C receives a `T*` and writes
# the result through it. libpq's `PQescapeByteaConn(..., size_t *to_length)`
# and `PQunescapeBytea(str, size_t is rw --> Pointer)` are declared this way;
# passing the value directly handed C a garbage pointer (a segfault, when
# lucky).
plan 6;

# frexp(3): x = mantissa * 2**exp, exp written through the int*.
# (No library named: frexp lives in the C runtime — naming 'm' would dlopen
# the libm.so linker script and fail under Rakudo on glibc systems.)
sub frexp(num64, int32 is rw --> num64) is native { * }
my int32 $exp;
my $mant = frexp(8e0, $exp);
is $exp, 4, 'C wrote the exponent through the int32* out-param';
is-approx $mant, 0.5e0, 'the return value is unaffected';

# The out-param also works for an inline declaration in the argument list —
# NativeHelpers::Blob writes `PQunescapeBytea($value, my size_t $elems)`.
my $m2 = frexp(4e0, my int32 $e2);
is $e2, 3, 'inline `my` declaration receives the written value';

# A definedness smiley on a parameter type is not part of the C type:
# `Blob:D $dest` marshals as `Blob` (the buffer's own storage pointer).
# Left attached it fell through to the opaque-handle branch and C wrote to
# NULL (NativeHelpers::Blob's `memcpy(Blob:D $dest, ...)`).
sub malloc(size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }
sub memset(Pointer, int32, size_t --> Pointer) is native { * }
sub memcpy(Blob:D $dest, Pointer $src, size_t $size --> Pointer) is native { * }

my \p = malloc(4);
memset(p, 65, 3);
my $buf = Buf.allocate(3);
memcpy($buf, p, 3);
is $buf, Buf.new(65, 65, 65), 'Blob:D destination received the bytes';
free(p);

# An uninitialized C-width-alias native scalar reads as 0, not Nil —
# `my size_t $sz;` is how DBDish::Pg declares its out-length slots.
my size_t $sz;
is $sz, 0, 'uninitialized size_t defaults to 0';
my ulong $ul;
is $ul, 0, 'uninitialized ulong defaults to 0';

done-testing;
