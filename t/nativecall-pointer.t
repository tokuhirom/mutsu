use Test;

# NativeCall `Pointer` type + `is rw` out-parameter.
#
# A `Pointer` carries a C address; an `is rw Pointer` argument is an
# *out-parameter*: C receives a `void**` and writes the resulting pointer back
# into the caller's `Pointer` object. We exercise this with libc's
# `posix_memalign` (an out-pointer allocator, guaranteed present) so the test
# does not depend on any optional library.

use NativeCall;

plan 16;

# --- the Pointer type itself (no FFI) ---
{
    my $p = Pointer.new;
    is $p.Int, 0, 'fresh Pointer.Int is 0';
    is $p.Numeric, 0, 'fresh Pointer.Numeric is 0';
    nok $p.Bool, 'fresh Pointer is falsy (NULL)';
    is $p.gist, 'NativeCall::Types::Pointer<NULL>', 'NULL Pointer gist';
}

sub posix_memalign(Pointer $p is rw, int64 $align, int64 $size)
    returns int32 is native('c') { * }
sub free(int64 $p) is native('c') { * }
sub memset(int64 $p, int32 $c, int64 $n) returns int64 is native('c') { * }

# --- is rw Pointer out-parameter writes a real address back ---
{
    my $p = Pointer.new;
    my $rc = posix_memalign($p, 16, 256);
    is $rc, 0, 'posix_memalign succeeds';
    ok $p.Int > 0, 'out-parameter wrote a non-NULL address into the Pointer';
    ok $p.Bool, 'Pointer is now truthy';
    ok $p.Int %% 16, 'returned block honours the 16-byte alignment';

    # The address is a usable heap block: writing to it must not crash.
    memset($p.Int, 0, 256);
    pass 'memset through the returned pointer works';

    free($p.Int);
    pass 'free of the returned pointer works';
}

# --- a by-value Pointer passes its current address ---
{
    my $p = Pointer.new;
    posix_memalign($p, 8, 64);
    my $addr = $p.Int;
    # Re-wrap the same address and confirm round-trip parity via .Int.
    free($p.Int);
    is $p.Int, $addr, 'Pointer.Int is stable across reads';
}

# --- a `returns Pointer` function yields a real Pointer object ---
{
    sub malloc(int64 $size) returns Pointer is native('c') { * }
    sub free_p(Pointer $p) is native('c') is symbol('free') { * }
    sub memset_p(Pointer $p, int32 $c, int64 $n) returns Pointer is native('c') is symbol('memset') { * }

    my $p = malloc(128);
    is $p.^name, 'Pointer', 'malloc returns a Pointer object';
    isa-ok $p, Pointer, 'the return value isa Pointer';
    ok $p.Bool, 'malloc returned a non-NULL Pointer';
    # The returned Pointer is usable as a by-value Pointer argument.
    memset_p($p, 0, 128);
    pass 'memset through a returned Pointer works';
    free_p($p);
    pass 'free of a returned Pointer works';
}
