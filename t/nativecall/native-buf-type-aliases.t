use v6;
use Test;

# Lowercase native sized-buffer type aliases as variable type constraints:
#   blob8 == Blob[uint8], blob16 == Blob[uint16], buf8 == Buf[uint8], ...
# A `blobN` constraint (immutable Blob[uintN] role) accepts any buffer of
# matching element width — including a plain `Buf` (which is `Buf[uint8]`) and
# `utfN` encodings. A `bufN` constraint matches only the explicitly-sized
# mutable parameterized type.

plan 12;

# --- blobN accepts matching-width buffers ---
{
    my blob8 $b = Buf.new(1, 2, 3);
    is $b.list, (1, 2, 3), 'blob8 accepts a plain Buf (Buf is Buf[uint8])';
}
{
    my blob8 $b = "Hi".encode('utf8');
    is $b.list, (72, 105), 'blob8 accepts a utf8 Buf';
}
{
    my blob16 $u = "AB".encode('UTF-16');
    is $u.list, (65, 66), 'blob16 accepts a utf16 buffer (MIME::Base64 pattern)';
}
{
    my blob16 $b = Blob[uint16].new(7, 8);
    is $b.list, (7, 8), 'blob16 accepts an explicit Blob[uint16]';
}

# --- bufN accepts the explicitly-sized mutable type ---
{
    my buf16 $b = Buf[uint16].new(9, 10);
    is $b.list, (9, 10), 'buf16 accepts an explicit Buf[uint16]';
}

# --- the aliases are valid type names (no "not declared") ---
ok blob8 ~~ Blob, 'blob8 is a kind of Blob';
ok buf8 ~~ Buf, 'buf8 is a kind of Buf';

# --- rejections match raku ---
dies-ok { my blob16 $x = Buf.new(1, 2) }, 'blob16 rejects a uint8 Buf (width mismatch)';
dies-ok { my buf8 $x = Buf.new(1, 2) }, 'buf8 rejects a plain Buf (not Buf[uint8])';
dies-ok { my blob8 $x = Blob.new(1, 2) }, 'blob8 rejects a plain immutable Blob';

# --- as a sub parameter constraint ---
sub byte-sum(blob8 $b) { $b.list.sum }
is byte-sum(Buf.new(10, 20, 30)), 60, 'blob8 works as a sub parameter type';

# --- round-trip through a typed declaration ---
{
    my blob8 $data = "Raku".encode('utf8');
    is $data.decode('utf8'), 'Raku', 'blob8-typed encode/decode round trip';
}
