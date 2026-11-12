use Test;

# Calling a parameterized buffer type coerces, as calling `Buf` does:
# `Buf[uint8]($blob)` (Archive::SimpleZip's t/ZipTest.rakumod). A buffer
# that already is of the target type is the result itself.

plan 10;

is Buf[uint8]("aé".encode('utf8')).raku, 'Buf[uint8].new(97,195,169)', 'Buf[uint8]($utf8)';
isa-ok Buf[uint8]("ab".encode), Buf[uint8], 'it is a Buf[uint8]';
is Buf[uint16]((1, 2)).raku, 'Buf[uint16].new(1,2)', 'Buf[uint16] of a list';
is Blob[uint8](Blob.new(1, 2, 3)).raku, 'Blob[uint8].new(1,2,3)', 'a plain Blob is copied into a Blob[uint8]';

my $t = Buf[uint8];
is $t("ab".encode).raku, 'Buf[uint8].new(97,98)', 'through a variable';
my constant T = Buf[uint16];
is T((1, 2)).raku, 'Buf[uint16].new(1,2)', 'through a constant';

my $u = "a".encode;
ok Blob[uint8]($u) =:= $u, 'a utf8 already is a Blob[uint8]';
ok Blob($u) =:= $u, 'and a Blob';
my $b = buf8.new(1);
ok Buf[uint8]($b) =:= $b, 'a buf8 is kept by Buf[uint8]';
ok Blob[uint8]($b) =:= $b, 'and by Blob[uint8]';
