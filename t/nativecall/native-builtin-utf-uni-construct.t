use v6;
use Test;

# VM-native construction for the byte/codepoint built-in types whose `.new(...)`
# is pure data assembly: `utf8`/`utf16` (code units, no masking) and `Uni`
# (codepoints). These join `Buf`/`Blob` under the shared
# `try_native_builtin_construct` entry, so the VM builds them directly instead
# of routing `.new` through the interpreter's generic `dispatch_new`. The
# interpreter's arms call the same per-type helpers, keeping them byte-identical.

plan 16;

# --- utf8.new ---
is utf8.new(72, 105).decode, 'Hi', 'utf8.new decodes to text';
is utf8.new(72, 105).elems, 2, 'utf8.new elem count';
is utf8.new(72, 105).list, (72, 105), 'utf8.new byte list';
is utf8.new(0xE2, 0x82, 0xAC).decode, '€', 'utf8.new multi-byte codepoint';
is utf8.new(65 .. 67).decode, 'ABC', 'utf8.new flattens a range';
my @a = 65, 66, 67;
is utf8.new(@a).decode, 'ABC', 'utf8.new flattens an array';
my $b = buf8.new(72, 73);
is utf8.new($b).decode, 'HI', 'utf8.new from a buf';
is utf8.new.elems, 0, 'utf8.new with no args is empty';

# --- utf16.new ---
is utf16.new(72, 105).elems, 2, 'utf16.new elem count';
is utf16.new(72, 105).list, (72, 105), 'utf16.new unit list';

# --- Uni.new ---
is Uni.new(72, 105).Str, 'Hi', 'Uni.new from codepoints';
is Uni.new(0x263A).Str, '☺', 'Uni.new from a single codepoint';
my @c = 72, 101, 108, 108, 111;
is Uni.new(@c).Str, 'Hello', 'Uni.new flattens an array';
is Uni.new.Str.chars, 0, 'Uni.new with no args is empty';

# --- Uni.new now flattens ranges too (was a pre-existing bug; the shared helper
#     fixes it to match raku and the sibling utf8 arm) ---
is Uni.new(65 .. 67).Str, 'ABC', 'Uni.new flattens an inclusive range';
is Uni.new(65 ..^ 68).Str, 'ABC', 'Uni.new flattens an exclusive range';
