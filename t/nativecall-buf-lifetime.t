use Test;
use NativeCall;

plan 7;

# A Blob passed to C must be backed by memory that lives as long as the Raku
# object, not just as long as the call: `open_memstream`-style APIs and
# OpenSSL's `BIO_new_mem_buf` retain the pointer they were handed.
#
# `memmem(haystack, hlen, needle, nlen)` returns an interior pointer into the
# haystack, so subtracting the haystack's own address gives the match offset --
# and it can only be checked against the haystack *after* the call, which is
# exactly what a per-call temporary buffer would make meaningless.
sub memmem(Blob, size_t, Blob, size_t --> Pointer) is native { * }
sub strlen(Pointer --> size_t) is native { * }
sub getenv(Str --> Pointer) is native { * }

my $hay = "hello wonderful world\0".encode;
my $needle = "wonderful".encode;

my $hit = memmem($hay, $hay.bytes, $needle, $needle.bytes);
ok $hit.defined, 'memmem found the needle';
# The returned interior pointer must still address live bytes.
is strlen($hit), "wonderful world".chars, 'the returned interior pointer is still valid';

# Passing the same Blob again must keep giving the same base address, so a C
# structure that captured the earlier pointer stays consistent.
my $again = memmem($hay, $hay.bytes, $needle, $needle.bytes);
is $again.Int, $hit.Int, 'the same Blob keeps the same C address across calls';

# A different Blob gets its own buffer.
my $other = "hello wonderful world\0".encode;
isnt memmem($other, $other.bytes, $needle, $needle.bytes).Int, $hit.Int,
    'a distinct Blob gets a distinct C address';

# A C function that returns NULL yields the type object, not a defined pointer
# holding 0 -- that is what makes the usual `unless defined(...)` guard work.
nok getenv('MUTSU_NO_SUCH_ENV_VAR_XYZZY').defined,
    'a NULL pointer return is undefined';
ok getenv('PATH').defined, 'a non-NULL pointer return is defined';

# An out-buffer still round-trips: the callee's writes land back in the Buf.
sub memcpy(Blob, Blob, size_t --> Pointer) is native { * }
my $dest = Buf.allocate(5, 0);
memcpy($dest, "abcde".encode, 5);
is $dest.decode, 'abcde', 'a callee-written out-buffer is copied back';
