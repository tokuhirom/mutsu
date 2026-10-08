use Test;

plan 33;

# Blob / Buf / utf8 rows (ADR-11276 §8.3): the read-only methods.
my $blob = Blob.new(1, 2, 255);
my $buf = Buf.new(10, 20);
my $u = "hé".encode;
my $b16 = buf16.new(1, 258);
my $empty = Buf.new;

is $blob.elems, 3, 'Blob.elems';
is $buf.elems, 2, 'Buf.elems';
is $u.elems, 3, 'utf8.elems';
is $empty.elems, 0, 'empty Buf.elems';

is $blob.bytes, 3, 'Blob.bytes';
is $b16.bytes, 4, 'buf16.bytes is elems times the width';
is $u.bytes, 3, 'utf8.bytes';

is $blob.of.^name, 'uint8', 'Blob.of';
is $b16.of.^name, 'uint16', 'buf16.of';

is $blob.list.join(','), '1,2,255', 'Blob.list';
is $buf.contents.join(','), '10,20', 'Buf.contents';
isa-ok $blob.list, List, 'Blob.list is a List';

is $buf.reverse.list.join(','), '20,10', 'Buf.reverse';
isa-ok $buf.reverse, Buf, 'Buf.reverse keeps the class';
isa-ok $blob.reverse, Blob, 'Blob.reverse keeps the class';

ok $blob.Bool, 'non-empty Blob is true';
nok $empty.Bool, 'empty Buf is false';
ok ?$buf, 'prefix ? agrees';

is $blob.gist, 'Blob:0x<01 02 FF>', 'Blob.gist is hex';
is $buf.gist, 'Buf:0x<0A 14>', 'Buf.gist';
is $empty.gist, 'Buf:0x<>', 'empty Buf.gist';
is $b16.gist, 'Buf[uint16]:0x<0001 0102>', 'buf16.gist uses the element width';
is Buf.new(1..120).gist.chars, 'Buf:0x<'.chars + 100 * 3 - 1 + 4 + 1, 'gist stops at 100 elements';
is $blob.raku, 'Blob.new(1,2,255)', 'Blob.raku';
is $buf.raku, 'Buf.new(10,20)', 'Buf.raku';
is $b16.raku, 'Buf[uint16].new(1,258)', 'buf16.raku';

is $u.Str, 'hé', 'utf8.Str decodes';
throws-like { $blob.Str }, X::Buf::AsStr, 'Blob.Str dies';
throws-like { $buf.Str }, X::Buf::AsStr, 'Buf.Str dies';

my $as-buf = $u.Buf;
isa-ok $as-buf, Buf, 'utf8.Buf is a Buf';
is $as-buf.list.join(','), $u.list.join(','), 'utf8.Buf keeps the bytes';
isa-ok $buf.Blob, Blob, 'Buf.Blob is a Blob';
isa-ok $blob.Buf, Buf, 'Blob.Buf is a Buf';

done-testing;
