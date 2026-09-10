use v6;
use Test;
use experimental :pack;

# pack / unpack (experimental). Raku's template set is smaller than Perl's:
# A a Z (string), C (byte), H (hex), S v (LE u16), n (BE u16), L V (LE u32),
# N (BE u32), x (null byte). Numeric letters consume one item each; repetition
# is expressed by repeating the letter ("S" x $n).

plan 24;

# --- pack: integers ---
is pack("C", 65).list, (65), 'pack C — single byte';
is pack("CCC", 1, 2, 3).list, (1, 2, 3), 'pack CCC — three bytes';
is pack("S", 0x1234).list, (0x34, 0x12), 'pack S — little-endian u16';
is pack("v", 0x1234).list, (0x34, 0x12), 'pack v — little-endian u16 (alias)';
is pack("n", 0x1234).list, (0x12, 0x34), 'pack n — big-endian u16';
is pack("L", 0x01020304).list, (4, 3, 2, 1), 'pack L — little-endian u32';
is pack("V", 0x01020304).list, (4, 3, 2, 1), 'pack V — little-endian u32 (alias)';
is pack("N", 0x01020304).list, (1, 2, 3, 4), 'pack N — big-endian u32';

# --- pack: numeric count is a no-op (Rakudo quirk); repeat the letter ---
is pack("C3", 65, 66, 67).list, (65,), 'pack C3 — count ignored, one byte';
is pack("SS", 0x1234, 0x5678).list, (0x34, 0x12, 0x78, 0x56), 'pack SS — two units';

# --- pack: strings ---
is pack("A3", "Hi").list, (72, 105, 32), 'pack A3 — space-padded to width';
is pack("A*", "ABC").list, (65, 66, 67), 'pack A* — full string';
is pack("a3", "Hi").list, (72, 105, 0), 'pack a3 — null-padded to width';
is pack("H*", "deadbeef").list, (0xde, 0xad, 0xbe, 0xef), 'pack H* — hex string';

# --- pack: null bytes ---
is pack("x3").list, (0, 0, 0), 'pack x3 — null bytes';
is pack("Cx2C", 1, 2).list, (1, 0, 0, 2), 'pack with embedded null bytes';

# --- pack: the MIME::Base64 encoder pattern ---
{
    my @vals = (0x1234, 0x5678, 0x9abc);
    is pack("S" x @vals.elems, @vals).list, (0x34, 0x12, 0x78, 0x56, 0xbc, 0x9a),
        'pack "S" x N over a list (MIME pattern)';
}

# --- unpack: integers ---
is Blob.new(1, 2, 3, 4).unpack("C*").join(','), '1,2,3,4', 'unpack C* — all bytes';
is Blob.new(0x34, 0x12).unpack("S"), 4660, 'unpack S — collapses single value to Int';
is Blob.new(0x34, 0x12).unpack("S").^name, 'Int', 'unpack single value is an Int, not a List';
is Blob.new(0, 1, 0, 2).unpack("nn").join(','), '1,2', 'unpack nn — two big-endian u16';
is Blob.new(1, 2, 3, 4).unpack("N"), 0x01020304, 'unpack N — big-endian u32';

# --- unpack: strings ---
is Blob.new(72, 105).unpack("A*"), 'Hi', 'unpack A* — bytes to string';

# --- round trip ---
is Blob.new(pack("N", 0xdeadbeef).list).unpack("N"), 0xdeadbeef, 'pack/unpack N round trip';
