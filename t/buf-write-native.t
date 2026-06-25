use Test;

# Pin for the ledger §D(b) slice that drains the Buf write-int/uint/num method
# family to VM-native dispatch in both the type-object form (`buf8.write-int32(...)`
# returns a fresh buf) and the instance form reached via a dynamic method name or a
# `\sigilless`/`:=` binding (mutate the shared cell, return the same buf). Previously
# these fell through to the interpreter slow path on every call.

plan 27;

# --- type-object form: fresh buf -----------------------------------------------
is buf8.write-int8(0, 0x2A).gist, 'Buf[uint8]:0x<2A>', 'type-object write-int8';
is buf8.write-int16(0, 0x1234).gist, 'Buf[uint8]:0x<34 12>', 'type-object write-int16 LE default';
is buf8.write-int16(0, 0x1234, BigEndian).gist, 'Buf[uint8]:0x<12 34>', 'type-object write-int16 BigEndian';
is buf8.write-int32(0, 7).elems, 4, 'type-object write-int32 size';
is buf8.write-int64(0, 1).elems, 8, 'type-object write-int64 size';
is buf8.write-int128(0, 1).elems, 16, 'type-object write-int128 size';
is buf8.write-uint8(0, 255).gist, 'Buf[uint8]:0x<FF>', 'type-object write-uint8';
is buf8.write-uint16(2, 0xABCD, BigEndian).gist, 'Buf[uint8]:0x<00 00 AB CD>', 'type-object write-uint16 offset+BE';
is buf8.write-num32(0, 1e0).elems, 4, 'type-object write-num32 size';
is buf8.write-num64(0, 1e0).elems, 8, 'type-object write-num64 size';

# offset extends the buffer
is buf8.write-int8(3, 9).gist, 'Buf[uint8]:0x<00 00 00 09>', 'type-object offset extends';

# --- scalar (=) instance: mutate-and-writeback ---------------------------------
my $b = buf8.new(0 xx 8);
my $r = $b.write-uint16(0, 0x1234);
is $b.gist, 'Buf[uint8]:0x<34 12 00 00 00 00 00 00>', 'scalar write mutates in place';
is $r.gist, $b.gist, 'scalar write returns the (mutated) buf value';
is $b.elems, 8, 'scalar write does not change size when in-bounds';

# --- sigilless (:=) instance via dynamic method name ---------------------------
my \e := buf8.new(0 xx 8);
for <write-int8 write-uint8> -> $w {
    e."$w"(2, 0x63);
    is e.gist, 'Buf[uint8]:0x<00 00 63 00 00 00 00 00>', "sigilless dynamic $w mutates shared cell";
}
my \g := buf8.new(0 xx 8);
my $write = 'write-int32';
my $ret := g."$write"(0, 0x04030201, LittleEndian);
is g.gist, 'Buf[uint8]:0x<01 02 03 04 00 00 00 00>', 'sigilless dynamic write-int32 LE';
ok $ret =:= g, 'sigilless dynamic write returns the same buf';

# --- scalar instance via dynamic method name -----------------------------------
my $sb = buf8.new(0 xx 4);
my $sw = 'write-uint8';
$sb."$sw"(1, 0xEE);
is $sb.gist, 'Buf[uint8]:0x<00 EE 00 00>', 'scalar dynamic write mutates in place';

# --- type-object via dynamic method name ---------------------------------------
my $tw = 'write-int16';
is buf8."$tw"(0, 0x0102, BigEndian).gist, 'Buf[uint8]:0x<01 02>', 'type-object dynamic write-int16 BE';

# --- error semantics preserved (fall through to interpreter) -------------------
dies-ok { buf8.write-int8(-1, 42) }, 'type-object negative offset dies';
dies-ok { buf8.new."$sw"(-1, 42) }, 'instance dynamic negative offset dies';
dies-ok { blob8.new(1, 2, 3).write-int8(0, 9) }, 'blob instance is immutable';

# --- round-trip read back ------------------------------------------------------
my $rt = buf8.new(0 xx 16);
$rt.write-int32(4, 0xDEADBEEF, BigEndian);
is $rt.read-uint32(4, BigEndian), 0xDEADBEEF, 'write then read-uint32 round-trips';
$rt.write-int64(8, 42, LittleEndian);
is $rt.read-int64(8, LittleEndian), 42, 'write then read-int64 round-trips';

# negative signed value
my $sv = buf8.new(0 xx 4);
$sv.write-int8(0, -1);
is $sv.read-int8(0), -1, 'signed -1 round-trips';
is $sv.read-uint8(0), 255, 'as unsigned reads 255';
