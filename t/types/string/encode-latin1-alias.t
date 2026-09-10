use Test;

# The "latin1" encoding name (no hyphen) was not registered as an alias for
# iso-8859-1, so .encode('latin1') fell through to a windows-1252 codec that
# silently mapped out-of-range characters (€ -> 0x80) instead of throwing.

plan 12;

# In-range characters (U+0000..U+00FF) encode to their codepoint byte.
is 'A'.encode('latin1').list, (65,), 'latin1 encodes A';
is 'café'.encode('latin1').list, (99, 97, 102, 233), 'latin1 encodes café';
is 'ÿ'.encode('latin1').list, (255,), 'latin1 encodes U+00FF';

# Out-of-range characters throw (must NOT silently map via windows-1252).
dies-ok { '€'.encode('latin1') }, 'latin1 refuses € (U+20AC)';
dies-ok { 'Œ'.encode('latin1') }, 'latin1 refuses Œ (U+0152)';
dies-ok { "\x2018".encode('latin1') }, 'latin1 refuses left single quote';
dies-ok { "\x0100".encode('latin1') }, 'latin1 refuses U+0100';

# All spellings behave the same as iso-8859-1.
is 'ÿ'.encode('latin-1').list, (255,), 'latin-1 (hyphen) agrees';
is 'ÿ'.encode('iso-8859-1').list, (255,), 'iso-8859-1 agrees';
dies-ok { '€'.encode('iso-8859-1') }, 'iso-8859-1 refuses €';

# Decoding maps bytes 0x00..0xFF directly to U+0000..U+00FF (not windows-1252).
is Buf.new(0x80).decode('latin1').ord, 0x80, 'latin1 decodes 0x80 to U+0080';
is Buf.new(0xE9).decode('latin1'), 'é', 'latin1 decodes 0xE9 to é';
