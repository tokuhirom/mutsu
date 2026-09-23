use Test;
use nqp;

# The nqp:: buffer ops edit a Buf's storage in place rather than decoding and
# re-encoding the whole buffer per call (#9132). These pin the observable
# behaviour of that in-place path: growth, aliasing, copy-on-write of a shared
# node, and element width.

plan 19;

# writeuint / writenum grow the buffer and write only the covered bytes.
my $w := buf8.new;
nqp::writeuint($w, 0, 0x0102, 5);   # 5 = uint16 | little endian
nqp::writeuint($w, 4, 0xAB, 0);     # past the end: zero-filled gap
is-deeply $w, buf8.new(2, 1, 0, 0, 0xAB), 'writeuint grows and zero-fills';
nqp::writenum($w, 1, 1e0, 14);      # 14 = num64 | big endian
is $w.elems, 9, 'writenum grows to cover 8 bytes';
is nqp::readnum($w, 1, 14), 1e0, 'readnum reads back what writenum wrote';
is nqp::readuint($w, 0, 5), 0x3f02, 'readuint reads the overlapping bytes';

# A fill loop: every write lands, nothing is lost or reordered.
my $fill := buf8.new;
nqp::writeuint($fill, $_, $_ % 256, 0) for ^1000;
is $fill.elems, 1000, 'fill loop length';
ok (so (^1000).map({ $fill[$_] == $_ % 256 }).all), 'fill loop contents';

# Writes are visible through every alias of the buffer.
my $alias := $fill;
nqp::writeuint($fill, 0, 99, 0);
is $alias[0], 99, 'in-place write is alias-visible';

# ... but not through a buffer that merely shares storage (.Blob re-tag).
my $orig = Buf.new(1, 2, 3);
my $copy = $orig.Blob;
nqp::writeuint(nqp::decont($orig), 0, 42, 0);
is $orig[0], 42, 'write reaches the written buffer';
is $copy[0], 1, 'a re-tagged copy keeps its own bytes';

# bindpos_i encodes one element at the buffer's own width.
my $b16 := buf16.new(1, 2);
nqp::bindpos_i($b16, 1, 0xBEEF);
nqp::bindpos_i($b16, 3, 7);
is-deeply $b16, buf16.new(1, 0xBEEF, 0, 7), 'bindpos_i on buf16 grows by elements';
my $s8 := Buf[int8].new;
nqp::bindpos_i($s8, 0, -3);
is $s8[0], -3, 'bindpos_i keeps a signed element';

# splice replaces a range in place.
my $sp := buf8.new(1, 2, 3, 4, 5);
nqp::splice($sp, buf8.new(9, 9, 9), 1, 2);
is-deeply $sp, buf8.new(1, 9, 9, 9, 4, 5), 'splice replaces a range';
nqp::splice($sp, $sp, 0, 0);
is-deeply $sp, buf8.new(1, 9, 9, 9, 4, 5, 1, 9, 9, 9, 4, 5), 'splice a buffer into itself';

# slice is end-inclusive and borrows the source.
is-deeply nqp::slice(buf8.new(1, 2, 3, 4), 1, 2), buf8.new(2, 3), 'slice';
is-deeply nqp::slice(buf8.new(1, 2, 3, 4), 2, -1), buf8.new(3, 4), 'slice to a negative end';

# The queue ends on a buffer.
my $q := buf16.new(10, 20, 30);
is nqp::pop_i($q), 30, 'pop_i the last element';
is nqp::shift_i($q), 10, 'shift_i the first element';
nqp::push_i($q, 0x1234);
is-deeply $q, buf16.new(20, 0x1234), 'push_i at the element width';

# readuint out of range still errors.
throws-like { nqp::readuint(buf8.new(1), 0, 4) }, Exception, 'readuint past the end dies';
