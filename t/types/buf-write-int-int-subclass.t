use Test;

# `Buf.write-int*` / `write-uint*` given an instance of a user subclass of
# `Int` write its integer payload, not 0. Found via BSON::Simple, whose
# `class Int64 is Int` values encoded as eight zero bytes.

plan 4;

class I64 is Int { }

my $b = buf8.new(0 xx 8);
$b.write-int64(0, I64.new(-1), LittleEndian);
is $b.list, (255 xx 8).list, 'write-int64 of an Int subclass instance';

$b = buf8.new(0 xx 8);
$b.write-int64(0, I64.new(9223372036854775807), LittleEndian);
is $b.list, (255, 255, 255, 255, 255, 255, 255, 127).list, 'int64 max';

$b = buf8.new(0 xx 4);
$b.write-int32(0, I64.new(5), BigEndian);
is $b.list, (0, 0, 0, 5).list, 'write-int32 big-endian';

$b = buf8.new(0 xx 2);
$b.write-uint16(0, I64.new(258), LittleEndian);
is $b.list, (2, 1).list, 'write-uint16';
