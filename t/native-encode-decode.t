use Test;

# Pin for VM-native `.encode` (Cool scalar -> Buf) and `.decode` (Buf/Blob -> Str)
# (ledger §D). The explicit-encoding forms (`"x".encode("utf-16")`, `$buf.decode`)
# used to bounce to the interpreter's `dispatch_method_by_name` catch-all; they are
# pure transformations over the VM-owned encoding registry (no io_handles), so the
# VM now dispatches them natively via `try_native_encode_decode` — the single shared
# `dispatch_encode`/`dispatch_decode` impl. Both literal and variable receivers
# (mut path) are exercised. The 0-arg `.encode` is already native via methods_0arg.

plan 23;

# --- .encode on a Str ---
is "abc".encode("utf-8").elems, 3,        '.encode("utf-8") yields 3 bytes';
is "abc".encode("utf-8")[0], 0x61,        '.encode first byte is "a"';
ok "abc".encode("utf-8") ~~ Blob,         '.encode returns a Blob';
is "héllo".encode("utf-8").elems, 6,      '.encode counts utf-8 bytes (é = 2)';

# utf-16 .elems counts 16-bit units (one per BMP codepoint), not bytes
is "xy".encode("utf-16").elems, 2,        '.encode("utf-16") counts 16-bit units';

# ascii / latin-1 single-byte encodings
is "abc".encode("ascii").elems, 3,        '.encode("ascii") is single-byte';
is "abc".encode("latin-1").elems, 3,      '.encode("latin-1") is single-byte';

# --- .encode on other Cool scalars ---
is 42.encode("utf-8").decode, "42",       'Int.encode stringifies then encodes';
is True.encode("utf-8").decode, "True",   'Bool.encode stringifies then encodes';
is (1/2).encode("utf-8").decode, "0.5",   'Rat.encode stringifies then encodes';

# --- .decode on a Buf/Blob ---
is Buf.new(0x61, 0x62, 0x63).decode("utf-8"), "abc", '.decode("utf-8") of ASCII bytes';
is Buf.new(0xc3, 0xa9).decode("utf-8"), "é",          '.decode("utf-8") of a 2-byte sequence';
is Buf.new(0x41, 0x42).decode, "AB",                  '.decode defaults to utf-8';
is Blob.new(0x68, 0x69).decode("ascii"), "hi",        '.decode("ascii") of a Blob';

# round-trip
is "round trip ☃".encode("utf-8").decode("utf-8"), "round trip ☃",
   '.encode -> .decode round-trips through utf-8';
is "wide ☃".encode("utf-16").decode("utf-16"), "wide ☃",
   '.encode -> .decode round-trips through utf-16';

# --- variable receivers (mut path / CallMethodMut) ---
my $s = "data";
is $s.encode("utf-8").elems, 4,           'variable Str.encode';
ok $s.encode("utf-8") ~~ Blob,            'variable Str.encode is a Blob';
my $b = Buf.new(0x6f, 0x6b);
is $b.decode("utf-8"), "ok",              'variable Buf.decode';
my $enc = "z";
is $enc.encode("utf-16").elems, 1,        'variable Str.encode("utf-16") in 16-bit units';

# receiver is not mutated by encode/decode (new value returned)
my $orig = "keep";
$orig.encode("utf-8");
is $orig, "keep",                         '.encode does not mutate the receiver';
my $bo = Buf.new(0x78);
$bo.decode;
is $bo.elems, 1,                          '.decode does not mutate the receiver';

# --- decode on a non-Buf falls through (still works via interpreter) ---
# (Str has no .decode; ensure we did not break the fall-through path.)
ok "abc".encode.elems == 3,               '0-arg .encode still works (methods_0arg)';
