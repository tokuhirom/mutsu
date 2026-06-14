use v6;
use Test;

# `Buf`/`Blob` construction (`Buf.new(...)`, `buf8.new(...)`, parametric
# `Buf[uintN].new(...)`) is a pure byte-overlay build — flatten the arguments to
# a byte sequence and mask each to the element width. The VM now builds these
# directly through the shared `build_native_buf_value` helper instead of routing
# `.new` through the interpreter's generic `dispatch_new`. The interpreter's
# Buf/Blob arm calls the same helper, so the two stay byte-identical.
# `utf8`/`utf16` keep their own (unmasked) construction arm on the interpreter.

plan 18;

# --- basic Buf/Blob ---
is Buf.new(1, 2, 3).elems, 3, 'Buf.new from a list';
is Buf.new(1, 2, 3)[1], 2, 'Buf element access';
is Blob.new(255, 256, 257).list, (255, 0, 1), 'Blob masks to uint8';

# --- aliases ---
is buf8.new(10, 20, 30).list, (10, 20, 30), 'buf8.new';
is blob8.new(0 xx 4).list, (0, 0, 0, 0), 'blob8.new with a repeat list';

# --- element-width masking ---
is buf16.new(0x1FFFF)[0], 0xFFFF, 'buf16 masks to uint16';
is buf32.new(0x1FFFFFFFF)[0], 0xFFFFFFFF, 'buf32 masks to uint32';

# --- parametric type names ---
is Buf[uint8].new(10, 20).list, (10, 20), 'Buf[uint8].new';
is Blob[uint16].new(70000)[0], 70000 +& 0xFFFF, 'Blob[uint16] masks';
is Buf[uint8].new(1, 2).^name, 'Buf[uint8]', 'parametric name preserved';

# --- ranges and arrays flatten ---
is Buf.new(1..3).list, (1, 2, 3), 'Buf.new from an inclusive range';
is Buf.new(^4).list, (0, 1, 2, 3), 'Buf.new from an exclusive range';
my @a = 10, 20, 30;
is buf8.new(@a).list, (10, 20, 30), 'buf8.new from an array';

# --- constructing from another buf copies its bytes ---
my $src = buf8.new(5, 6, 7);
is buf8.new($src).list, (5, 6, 7), 'buf8.new from another buf';
is Buf.new($src).list, (5, 6, 7), 'Buf.new from a buf8';

# --- empty ---
is Buf.new.elems, 0, 'Buf.new with no args is empty';
is buf8.new.elems, 0, 'buf8.new with no args is empty';

# --- canonical name normalization ---
is buf8.new(1).^name, 'Buf[uint8]', 'buf8 normalizes to Buf[uint8]';
