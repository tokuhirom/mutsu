use Test;
use nqp;

# The nqp:: byte ops address a wide buffer's raw storage, as MoarVM's
# read_buf/write_buf do (byte position = offset * element width), and a Buf
# used as a queue keeps a head offset so shift/unshift do not move the rest
# (#9191). Every expectation here was measured against rakudo.

plan 22;

# writeuint on a buf32: the offset counts elements, the size counts bytes, and
# growth resizes to offset + size ELEMENTS (MoarVM mixes the units).
my $g := buf32.new;
nqp::writeuint($g, 0, 0x01020304, 4);   # 4 = uint16 | native endian
is-deeply $g, buf32.new(0x0304, 0), 'writeuint (16-bit) on an empty buf32';

my $c := buf32.new(1, 2, 3);
nqp::writeuint($c, 1, 0xAB, 0);
is-deeply $c, buf32.new(1, 0xAB, 3), 'writeuint lands at element 1, not byte 1';
is nqp::readuint($c, 1, 0), 0xAB, 'readuint reads element 1';
throws-like { nqp::readuint($c, 4, 0) }, Exception,
    message => 'MVMArray: read_buf out of bounds offset 4 start 0 elems 3 count 1',
    'readuint bounds-checks against the element count';
nqp::writeuint($c, 13, 5, 0);
is $c.elems, 14, 'writeuint past the end grows by elements';
is $c[13], 5, 'the grown write lands at element 13';

# writenum / readnum on a buf16: four raw bytes at byte 0.
my $d := buf16.new(0x1234);
nqp::writenum($d, 0, 1e0, 8);           # 8 = num32 | native endian
is-deeply $d, buf16.new(0, 0x3F80, 0, 0), 'writenum writes raw storage bytes';
is nqp::readnum($d, 0, 8), 1e0, 'readnum reads them back';

# decode reads the raw storage.
is nqp::decode(buf16.new(0x6261), 'utf8'), 'ab', 'decode of a buf16';

# slice and splice are element operations.
is-deeply nqp::slice(buf16.new(0x1234, 0x5678, 0x9abc), 1, 2),
    buf16.new(0x5678, 0x9abc), 'slice of a buf16 keeps whole elements';
my $t := buf16.new(1, 2, 3, 4);
nqp::splice($t, buf16.new(0x1111, 0x2222), 1, 1);
is-deeply $t, buf16.new(1, 0x1111, 0x2222, 3, 4), 'splice of buf16 into buf16';
my $t2 := buf16.new(1, 2, 3, 4);
nqp::splice($t2, buf8.new(0xff), 1, 1);
is-deeply $t2, buf16.new(1, 0xFF, 3, 4), 'a buf8 element widens into a buf16';
my $t3 := buf8.new(1, 2, 3, 4);
nqp::splice($t3, buf16.new(0x1234), 1, 1);
is-deeply $t3, buf8.new(1, 0x34, 3, 4), 'a buf16 element truncates into a buf8';

# readfh only fills a width-1 buffer.
my $fh := nqp::open($?FILE, 'r');
throws-like { nqp::readfh($fh, buf16.new, 4) }, Exception,
    message => 'read_fhb requires a native array of uint8 or int8',
    'readfh refuses a buf16';
is nqp::readfh($fh, buf8.new, 3).decode, 'use', 'the refused read consumed nothing';
nqp::closefh($fh);

# A Buf as a queue: shift from the front, push at the back, all in order.
my $q := buf8.new;
nqp::push_i($q, $_ % 256) for ^1000;
my @got;
@got.push(nqp::shift_i($q)) for ^700;
is-deeply @got, [(^700).map(* % 256)], 'shift_i drains the front in order';
is $q.elems, 300, 'the rest stays';
nqp::push_i($q, 7);
is-deeply ($q[0], $q[299], $q[300]), (700 % 256, 999 % 256, 7), 'the rest is intact';

# unshift_i fills the front repeatedly.
my $u := buf16.new(1);
nqp::unshift_i($u, $_) for 2..500;
is $u.elems, 500, 'unshift_i grows';
is-deeply ($u[0], $u[498], $u[499]), (500, 2, 1), 'unshift_i keeps order';

# A shifted buffer still reads, copies and compares like any other.
my $s := buf8.new(1, 2, 3, 4);
nqp::shift_i($s);
is-deeply $s, buf8.new(2, 3, 4), 'a shifted buffer compares by its live bytes';
is nqp::readuint($s, 0, 5), 0x0302, 'readuint starts at the new first element';
