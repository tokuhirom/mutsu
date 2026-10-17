use Test;

# The built-in encodings' methods are rows (ADR-11276 §9.156):
# `Encoding::Builtin`, its encoder and its streaming decoder.

plan 14;

my $e = Encoding::Registry.find('utf8');
isa-ok $e, Encoding::Builtin, 'Registry.find answers an Encoding::Builtin';
ok $e.name, 'Encoding::Builtin.name';
isa-ok $e.alternative-names, Positional, 'Encoding::Builtin.alternative-names';
like $e.raku, /^ 'Encoding::Builtin.new(name => ' /, 'Encoding::Builtin.raku is Mu.raku';

my $enc = $e.encoder;
isa-ok $enc, Encoding::Encoder::Builtin, 'Encoding::Builtin.encoder';
is $enc.encode-chars('hé').^name, 'utf8', 'a UTF-8 encoder answers a utf8 buffer';
is $enc.encode-chars('hé').list, (0x68, 0xC3, 0xA9), 'encode-chars';
is Encoding::Registry.find('ascii').encoder(:replacement('?')).encode-chars('é!').list,
    (0x3F, 0x21), 'an ASCII encoder replaces what it cannot encode';

my $d = $e.decoder;
isa-ok $d, Encoding::Decoder::Builtin, 'Encoding::Builtin.decoder';
$d.add-bytes("abc\ndef".encode);
is $d.bytes-available, 7, 'add-bytes and bytes-available';
is $d.consume-line-chars(:chomp), 'abc', 'consume-line-chars(:chomp)';
nok $d.is-empty, 'is-empty with bytes left';
is $d.consume-all-chars, 'def', 'consume-all-chars';
ok $d.is-empty, 'is-empty once consumed';
