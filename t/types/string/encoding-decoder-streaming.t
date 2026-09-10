use Test;

plan 10;

# Streaming Encoding::Decoder API used by Cro::HTTP's request/response
# parsers: set-line-separators + consume-line-chars + consume-exactly-bytes.

my $d = Encoding::Registry.find('iso-8859-1').decoder();
$d.set-line-separators(["\r\n", "\n"]);
$d.add-bytes("a\r\nbb\ncc".encode('latin-1'));
is $d.consume-line-chars(:chomp), 'a', 'first line (CRLF separator, chomped)';
is $d.consume-line-chars(:chomp), 'bb', 'second line (LF separator, chomped)';
nok $d.consume-line-chars(:chomp).defined, 'incomplete line -> undefined Str';
is $d.consume-line-chars(:chomp, :eof), 'cc', ':eof drains the remainder';

my $e = Encoding::Registry.find('iso-8859-1').decoder();
$e.add-bytes("ab\r\n".encode('latin-1'));
is $e.consume-line-chars(), "ab\r\n", 'without :chomp the separator is kept';

my $b = Encoding::Registry.find('iso-8859-1').decoder();
$b.add-bytes("abcdef".encode('latin-1'));
my $got = $b.consume-exactly-bytes(3);
isa-ok $got, Buf, 'consume-exactly-bytes returns a Buf';
is $got.decode('latin-1'), 'abc', 'the exact bytes are consumed';
nok $b.consume-exactly-bytes(10).defined, 'insufficient bytes -> undefined Blob';
is $b.bytes-available, 3, 'buffer kept when insufficient';
is $b.consume-exactly-bytes(3).decode('latin-1'), 'def', 'remaining bytes still consumable';
