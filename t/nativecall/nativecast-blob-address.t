use Test;
use NativeCall;

# `nativecast(Pointer, $blob)` is the address of the Blob's own storage (the
# same pointer a `Blob` parameter hands C), not a NULL Pointer (#11754).

plan 4;

sub memchr(Pointer, int32, size_t --> Pointer) is native { * }

my $blob = "abc".encode;
my $p = nativecast(Pointer, $blob);
ok $p.defined, 'nativecast(Pointer, $blob) is a defined Pointer';
ok +$p != 0, 'its address is non-NULL';
my $hit = memchr($p, 98, 3);
ok $hit.defined, 'memchr over the cast pointer finds the byte';
is +$hit - +$p, 1, 'the hit is one byte into the blob storage';
