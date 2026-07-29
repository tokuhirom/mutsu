use Test;
use NativeCall;

plan 8;

# A typed Buf element assignment must keep the full element width — the old
# path masked every element store with `& 0xff`, so `Buf[uint64]` lengths
# past 255 collapsed (DBDish::mysql binds its MYSQL_BIND length buffers as
# Buf[intptr] and stores values like 8192 into them).

my $b = Buf[uint64].allocate(4);
$b[0] = 258;
$b[1] = 8191;
$b[2] = 8192;
is $b[0], 258, 'Buf[uint64] element keeps a value past one byte';
is $b[1], 8191, 'Buf[uint64] element keeps 8191';
is $b[2], 8192, 'Buf[uint64] element keeps 8192';
is $b.bytes, 32, 'four uint64 elements are 32 bytes';

my $w = Buf[uint16].allocate(2);
$w[0] = 0x1234;
is $w[0], 0x1234, 'Buf[uint16] element keeps 16 bits';

# Slice assignment goes through the same chokepoint.
my $s = Buf[uint64].allocate(3);
$s[0, 2] = 300, 70000;
is $s[0], 300, 'slice-assigned wide element 0';
is $s[2], 70000, 'slice-assigned wide element 2';

# Element writes through a nativecast CArray view write native memory and
# stay a CArray handle (previously the assignment replaced the handle with a
# plain Raku Array, silently dropping the address).
sub calloc(size_t, size_t --> Pointer) is native(Str) { * }
my $p = calloc(1, 16);
my $c = nativecast(CArray[uint8], $p);
$c[0] = 77;
is nativecast(CArray[uint8], $p)[0], 77, 'CArray element assign writes native memory';
