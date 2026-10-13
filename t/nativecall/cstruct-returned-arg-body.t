use v6;
use NativeCall;
use Test;

# From Gnome::N's t/NativeLib.rakutest: `g_value_init(N-GValue.new, ...)` returns
# its first argument, so the handle it hands back points into the body of a
# temporary. The handle must keep that body alive (it used to dangle and the
# next native call segfaulted). libc's memset has the same shape.

plan 3;

class Blob16 is repr('CStruct') {
    has uint64 $.a;
    has uint64 $.b;
}

sub memset(Blob16 $s, int32 $c, size_t $n --> Blob16) is native { * }

my Blob16 $r = memset(Blob16.new, 0x41, 16);
# churn memory so a freed body would be reused
my @junk = (^2000).map({ "x" x 64 });
is $r.a, 0x4141414141414141, 'returned handle reads the temporary argument body';

my $again = memset($r, 0x42, 16);
is $again.b, 0x4242424242424242, 'a call through the returned handle works';
is $r.a, 0x4242424242424242, 'and writes through the shared body';
