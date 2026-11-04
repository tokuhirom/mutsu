use lib 't/lib';
use Test;

# From the P5pack distribution: core's Blob.unpack is experimental, but it
# runs the `EXPERIMENTAL-PACK` routine visible from the calling scope when
# there is one, which a module installs with `BEGIN OUR::<EXPERIMENTAL-PACK> :=
# &f` (P5pack's `use P5pack` is what makes `$buf.unpack(...)` legal).

plan 3;

use PackProvider;

is Buf.new(1, 2, 3).unpack("C3 a*"), '3 bytes by C3|a*',
    'a template string is split into directives for the routine';
is Buf.new(1, 2).unpack(<C2>), '2 bytes by C2',
    'a directive list is passed through';
is Buf.new(7).unpack("a2 N*"), '1 bytes by a2|N*', 'a count and a star stay with their directive';
