use Test;
use NativeCall;

# A `nativecast(CArray[T], $blob)` result is an unmanaged CArray over the
# Blob's bytes; handed to a native `CArray[...]` parameter it passes that
# address as it is. Archive::SimpleZip computes its CRC this way
# (`nativecast CArray[uint8], $data` into zlib's `crc32`).

plan 4;

sub strlen(CArray[uint8] --> size_t) is native {*}
sub memchr(CArray[int8], int32, size_t --> Pointer) is native {*}

my $blob = "hello\0".encode;
my $view := nativecast CArray[uint8], $blob;

is $view[1], 101, 'the cast views the blob bytes';
is strlen($view), 5, 'a cast CArray passes its address to a CArray parameter';
is strlen(nativecast(CArray[uint8], "abc\0".encode)), 3, 'an inline cast too';

my $hit = memchr(nativecast(CArray[int8], $blob), ord('l'), 5);
is +$hit - +nativecast(Pointer, $blob), 2,
    'a parameter of another element type takes the same address';
