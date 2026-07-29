use v6;
use Test;
use NativeCall;

# A native method's invocant is its first C argument even when the signature
# does not spell it: `method strlen(--> size_t)` on a CPointer class is
# `strlen(char*)`. DBDish::Pg declares its whole libpq surface this way
# (`method PQstatus(--> int32)`), unlike DBDish::mysql which writes the
# invocant out (`::?CLASS:D:`).
plan 4;

class CStrHandle is repr('CPointer') {
    method strlen(--> size_t) is native { * }
    # A regular (non-native) method on a CPointer class must dispatch too.
    method double-len { self.strlen * 2 }
}

# `str` (the native string type) must marshal like `Str` — a NUL-terminated
# char*. DBDish::Pg declares `sub PQconnectdb(str --> PGconn)`.
sub strdup(str --> CStrHandle) is native { * }
sub free(CStrHandle) is native { * }

my $h = strdup("hello");
ok $h.defined, 'native sub returned a CPointer instance';
is $h.strlen, 5, 'implicit invocant passed as the first C argument';
is $h.double-len, 10, 'regular method on a CPointer class dispatches';

my $empty = strdup("");
is $empty.strlen, 0, 'empty native str round-trips';

free($_) for $h, $empty;

done-testing;
