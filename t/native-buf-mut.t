use Test;

# Pin for VM-native Buf write-method dispatch (ledger §1: native receiver
# dispatch -> VM-native). The byte transforms live as the single authoritative
# pure implementation in src/builtins/ (buf_bits / buf_write_int / buf_write_num)
# and are shared by both the bytecode VM (try_native_buf_mut) and the
# interpreter fallback, so these results must match exactly.

plan 23;

# --- write-uintN / read-uintN round trips ---
{
    my $b = Buf.new(0 xx 8);
    is $b.write-uint8(0, 200), Buf.new(200, 0, 0, 0, 0, 0, 0, 0), 'write-uint8 returns buf';
    is $b.read-uint8(0), 200, 'write-uint8 then read-uint8';
}
{
    my $b = Buf.new(0 xx 8);
    $b.write-uint16(0, 0xABCD, BigEndian);
    is $b.read-uint16(0, BigEndian), 0xABCD, 'write/read-uint16 big-endian';
    is $b.read-uint8(0), 0xAB, 'big-endian high byte first';
    is $b.read-uint8(1), 0xCD, 'big-endian low byte second';
}
{
    my $b = Buf.new(0 xx 8);
    $b.write-uint32(0, 0x01020304, LittleEndian);
    is $b.read-uint8(0), 0x04, 'little-endian low byte first';
    is $b.read-uint32(0, LittleEndian), 0x01020304, 'write/read-uint32 little-endian';
}

# --- signed write-intN ---
{
    my $b = Buf.new(0 xx 4);
    $b.write-int8(0, -5);
    is $b.read-int8(0), -5, 'write-int8 negative';
    is $b.read-uint8(0), 251, 'two-complement byte for -5';
}
{
    my $b = Buf.new(0 xx 4);
    $b.write-int16(0, -1, BigEndian);
    is $b.read-int16(0, BigEndian), -1, 'write-int16 negative';
    is $b.read-uint16(0, BigEndian), 0xFFFF, 'all bits set for -1';
}

# --- write-num32 / write-num64 ---
{
    my $b = Buf.new(0 xx 8);
    $b.write-num32(0, 1.5e0);
    is $b.read-num32(0), 1.5e0, 'write/read-num32';
    $b.write-num64(0, 3.25e0);
    is $b.read-num64(0), 3.25e0, 'write/read-num64';
}

# --- write-ubits / write-bits ---
{
    my $b = Buf.new(0, 0);
    $b.write-ubits(0, 4, 0xF);
    is $b.read-ubits(0, 4), 0xF, 'write-ubits low nibble';
    $b.write-bits(4, 4, -1);
    is $b.read-ubits(0, 8), 0xFF, 'write-bits fills nibble';
    is $b.read-bits(4, 4), -1, 'read-bits sign extends';
}

# --- aliasing: same buf object observed through two variables ---
{
    my $b = Buf.new(0 xx 4);
    my $c = $b;
    $b.write-uint8(0, 42);
    is $c.read-uint8(0), 42, 'alias sees write-uint8 mutation';
    $c.write-uint8(1, 99);
    is $b.read-uint8(1), 99, 'original sees alias mutation';
}

# --- buf grows when writing past current length ---
{
    my $b = Buf.new(1, 2);
    $b.write-uint8(5, 7);
    is $b.elems, 6, 'buf grew to fit write past end';
    is $b.read-uint8(5), 7, 'value written at extended offset';
    is $b.read-uint8(2), 0, 'gap zero-filled';
}

# --- immutable Blob still raises (interpreter owns the error) ---
{
    dies-ok { Blob.new(1, 2, 3).write-uint8(0, 9) }, 'write-uint8 on Blob dies';
}

# --- chained writes accumulate ---
{
    my $b = Buf.new(0 xx 4);
    $b.write-uint8(0, 1);
    $b.write-uint8(1, 2);
    $b.write-uint8(2, 3);
    is $b, Buf.new(1, 2, 3, 0), 'chained write-uint8 accumulate';
}
