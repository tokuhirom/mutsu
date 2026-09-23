use Test;
use nqp;

# nqp::atpos_i on a Buf must decode the whole element at the
# buffer's own width and signedness, not hand back its low byte (#9133).

plan 9;

my $b16 := buf16.new(0x0102, 65535);
is nqp::atpos_i($b16, 0), 258, 'buf16 element';
is nqp::atpos_i($b16, 1), 65535, 'buf16 max element';

my $b32 := buf32.new(258, 0xDEADBEEF);
is nqp::atpos_i($b32, 0), 258, 'buf32 element';
is nqp::atpos_i($b32, 1), 0xDEADBEEF, 'buf32 large element';

my $b64 := buf64.new(0x0102030405060708);
is nqp::atpos_i($b64, 0), 0x0102030405060708, 'buf64 element';

my $s8 := Buf[int8].new(-5, 7);
is nqp::atpos_i($s8, 0), -5, 'signed int8 buf keeps the sign';

my $s32 := Buf[int32].new(-100000);
is nqp::atpos_i($s32, 0), -100000, 'signed int32 buf keeps the sign';

my $u8 := buf8.new(200);
is nqp::atpos_i($u8, 0), 200, 'buf8 element unchanged';

is nqp::atpos_i($b16, 5), 0, 'out-of-range index is 0';
