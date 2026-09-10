use v6;
use Test;

# VM-native default construction now also covers `repr('CUnion')` classes
# (Track A ③ constructor). A CUnion lays all its native-int fields over the
# same little-endian bytes, so it constructs through the dedicated
# `construct_cunion_instance` byte-overlay helper. The interpreter's
# `dispatch_new` does nothing else for a CUnion class (no BUILD/TWEAK/required),
# so the native fast path runs that exact shared helper and skips the
# interpreter round-trip — byte-identical to the interpreter and to raku.

plan 11;

class U is repr('CUnion') {
    has uint8  $.byte;
    has uint16 $.word;
    has uint32 $.dword;
}

# --- the widest field sets the shared bytes; narrower fields read low bytes ---
my $a = U.new(dword => 0x12345678);
is $a.dword, 0x12345678, 'dword reads the full value';
is $a.word, 0x5678, 'word reads the low 16 bits';
is $a.byte, 0x78, 'byte reads the low 8 bits';

# --- setting the narrowest field overlays just the low byte ---
my $b = U.new(byte => 0xFF);
is $b.byte, 0xFF, 'byte field set';
is $b.word, 0xFF, 'word shares the low byte';
is $b.dword, 0xFF, 'dword shares the low byte';

# --- no argument: all fields are zero ---
my $z = U.new;
is $z.byte, 0, 'unset byte is 0';
is $z.word, 0, 'unset word is 0';
is $z.dword, 0, 'unset dword is 0';

# --- a 16-bit write leaves the high bytes of the 32-bit view clear ---
my $c = U.new(word => 0xABCD);
is $c.word, 0xABCD, 'word field set';
is $c.dword, 0xABCD, 'dword sees only the low 16 bits';
