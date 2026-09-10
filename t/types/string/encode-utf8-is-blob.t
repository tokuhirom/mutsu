use Test;

# The result of .encode (a utf8/utf16/utf32 buffer) is an immutable Blob, not a
# mutable Buf. mutsu previously classified utf8 as Buf-like, so `~~ Buf` was
# True and its elements could be mutated.

plan 16;

my $enc = "hi".encode;

# Type membership
nok $enc ~~ Buf, 'encode result is not a Buf';
ok $enc ~~ Blob, 'encode result is a Blob';
ok $enc ~~ Blob[uint8], 'encode result is a Blob[uint8]';
nok $enc ~~ Buf[uint8], 'encode result is not a Buf[uint8]';

# A Buf is both Buf and Blob
ok buf8.new(1) ~~ Buf, 'buf8 is a Buf';
ok buf8.new(1) ~~ Blob, 'buf8 is a Blob';

# Immutability: mutating an encode result throws
dies-ok { my $b = "hi".encode; $b[0] = 200 }, 'cannot mutate an encode result';

# A real Buf is mutable
lives-ok { my $b = buf8.new(1, 2); $b[0] = 9 }, 'buf8 element is mutable';

# All the read operations still work on the encoded Blob
is $enc.decode, 'hi', 'encode roundtrips through decode';
is $enc.elems, 2, 'elems';
is $enc.bytes, 2, 'bytes';
is $enc[0], 104, 'indexing';
is $enc.subbuf(1).list, (105,), 'subbuf';
is $enc.reverse.list, (105, 104), 'reverse';
is "café".encode.list, (99, 97, 102, 195, 169), 'utf-8 bytes';

# A Blob-typed parameter accepts an encode result
sub takes-blob(Blob $b) { $b.decode }
is takes-blob("hi".encode), 'hi', 'Blob parameter accepts an encode result';
