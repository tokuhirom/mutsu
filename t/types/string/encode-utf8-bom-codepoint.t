use Test;

# U+FEFF (BOM / ZERO WIDTH NO-BREAK SPACE) is an ordinary encodable character
# in every Unicode encoding; encoding it must not be rejected, and a string
# decoded from a BOM-prefixed buffer must re-encode.
plan 9;

is "\x[FEFF]A".encode('utf8').bytes, 4, "leading U+FEFF encodes to 3 bytes + 1";
is "\x[FEFF]".encode.list, (0xEF, 0xBB, 0xBF), "bare U+FEFF";
is "A\x[FEFF]B".encode('utf8').list, (65, 0xEF, 0xBB, 0xBF, 66), "inner U+FEFF";
is "\x[FEFF]A".encode('utf-8').bytes, 4, "utf-8 spelling";
is "\x[FEFF]A".encode('utf16').bytes, 4, "utf16";
is "\x[FEFF]A".encode('utf8-c8').bytes, 4, "utf8-c8";

my $d = Blob.new(0xEF, 0xBB, 0xBF, 0x41).decode('utf8');
is $d.ords, (65,), "decode drops the leading BOM";
is $d.encode('utf8').bytes, 1, "the decoded text re-encodes";

dies-ok { "\x[FEFF]".encode('ascii') }, "ASCII still rejects it";
