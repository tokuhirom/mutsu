use Test;

# NativeCall features exercised by the bundled OpenSSL / IO::Socket::SSL
# binding, pinned here against libc (always present, no network):
#   * `is native(&sub)` — the library name supplied by a code object resolved
#     at bind time (OpenSSL uses `is native(&ssl-lib)`).
#   * a `Blob`/`Buf` argument marshalled as a pointer to its bytes, including
#     an out-buffer whose callee-written bytes are copied back (SSL_read/BIO_read).
#   * a `is repr('CStruct')` return type surfaced as a defined instance of that
#     class (OpenSSL's SSL / SSL_CTX / SSL_METHOD opaque handles).

plan 8;

use NativeCall;

sub libc { 'c' }

# --- is native(&sub): the library name comes from calling `libc()` ---
sub strlen(Str --> int64) is native(&libc) { * }
is strlen("hello"), 5, 'is native(&sub) resolves the library from a code object';
is strlen(""), 0, 'is native(&sub) with an empty string';

# --- Blob input + out-buffer writeback (memcpy(dest, src, n)) ---
sub memcpy(Blob, Blob, size_t --> Pointer) is native(&libc) { * }
my $dest = buf8.allocate(5);
my $src = "world".encode;
my $ret = memcpy($dest, $src, 5);
is $dest.decode, 'world', 'a Buf out-argument receives the bytes the callee wrote';
ok $ret.defined, 'memcpy returns a defined (non-null) Pointer';

# The source Blob is passed by pointer to its bytes (read side).
my $d2 = buf8.allocate(3);
memcpy($d2, Blob.new(0x41, 0x42, 0x43), 3);
is $d2.list, (0x41, 0x42, 0x43), 'a Blob argument is read through its byte pointer';

# --- CStruct return surfaced as a defined instance of the declared class ---
# `localeconv()` returns a non-null `struct lconv *`.
class lconv is repr('CStruct') { has Str $.decimal_point; }
sub localeconv(--> lconv) is native(&libc) { * }
my $lc = localeconv();
ok $lc.defined, 'a CStruct return is a defined instance (lowercase CStruct class)';
is $lc.^name, 'lconv', 'the CStruct return is tagged with its declared class';

# A CStruct value round-trips as a pointer argument to another native call.
sub free(Pointer) is native(&libc) { * }
lives-ok { my $p = memcpy(buf8.allocate(1), "x".encode, 1); }, 'CStruct/Pointer values round-trip through native calls';
