use v6;
use Test;

# `blob16` (the capitalized-class-name `Blob[uint16]` spelling) decoding a
# UTF-16 surrogate pair must combine the two 16-bit code units into the
# correct non-BMP codepoint, exactly like the lowercase-name `buf16` and
# `Buf[uint16]` spellings already did. Several `is_wide` checks (deciding
# whether to expand each stored element to 2 bytes before UTF-16 decoding)
# tested only `"utf16" | "buf16" | "Buf[uint16]"`, missing the `Blob[uint16]`
# class name that `blob16.new(...)` actually produces — so each 16-bit code
# unit silently truncated to its low BYTE before decoding, corrupting any
# surrogate pair. CBOR::Simple's own test suite hit this via
# `blob16.new(0xd800, 0xdd51).decode('utf-16')` (should decode to U+10151).

plan 4;

my $surrogate-pair = blob16.new(0xd800, 0xdd51);
my $decoded = $surrogate-pair.decode('utf-16');
is $decoded.ord, 0x10151, 'blob16 surrogate pair decodes to the correct non-BMP codepoint';
is-deeply $decoded.encode('utf8').list, (240, 144, 133, 145),
    'blob16 surrogate pair round-trips to the correct UTF-8 bytes';

# buf16 / Buf[uint16] (the pre-existing working spellings) must be unaffected.
my $buf16-pair = buf16.new(0xd800, 0xdd51);
is $buf16-pair.decode('utf-16').ord, 0x10151, 'buf16 surrogate pair still decodes correctly';

# A single BMP code unit (no surrogate pairing needed) still works on blob16.
my $bmp = blob16.new(0x0041);
is $bmp.decode('utf-16'), 'A', 'blob16 single BMP code unit decodes correctly';
