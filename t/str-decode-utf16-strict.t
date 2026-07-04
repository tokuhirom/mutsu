use Test;

plan 8;

# Decoding UTF-16 is strict: an odd byte length or an unpaired surrogate is
# malformed and throws (matching Rakudo, which rejects invalid UTF-16 rather
# than substituting U+FFFD). A :replacement makes it lenient again.
# Covers roast S32-str/encode.t (decode utf16 throws).

# Valid round-trips still decode.
is Buf.new(0x61, 0x00, 0x62, 0x00).decode('utf16'), 'ab',
    'valid little-endian UTF-16 decodes';
is 'abc'.encode('utf16').decode('utf16'), 'abc',
    'encode/decode UTF-16 round-trips';

# An odd byte length throws.
dies-ok { Buf.new(0x61, 0x00, 0x62).decode('utf16') },
    'odd byte length throws';

# An unpaired low surrogate (0xDC00-0xDFFF with no preceding high) throws.
dies-ok { Buf.new(0x00, 0xdc).decode('utf16') },
    'an unpaired surrogate throws';
dies-ok { 'aouÄÖÜ'.encode('latin1').decode('utf16') },
    'reinterpreting latin1 bytes as UTF-16 throws on the bad surrogate';

# The exception message mentions the malformation.
throws-like { Buf.new(0x00, 0xdc).decode('utf16') }, Exception,
    message => rx:i/malformed | surrogate | term/,
    'the error message describes the malformation';

# A :replacement makes decoding lenient (invalid unit -> replacement).
is Buf.new(0x00, 0xdc).decode('utf16', :replacement<?>), '?',
    ':replacement substitutes an invalid code unit';

# A valid surrogate pair (astral character) decodes.
is Buf.new(0x3d, 0xd8, 0x00, 0xde).decode('utf16'), "\x[1F600]",
    'a valid surrogate pair decodes to the astral character';
