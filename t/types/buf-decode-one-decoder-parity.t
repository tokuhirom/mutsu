use v6;
use nqp;
use Test;

# `nqp::decode`, `Blob.decode` (with and without `:replacement`) and a
# slurped file are ONE decoder (ADR-0118 §2.4): the same encoding names,
# strict ASCII, BOM handling, error text and NFC normalization. They used to
# be four copies that disagreed. Expected values were measured with rakudo.

plan 11;

sub dies-like(&code, $prefix, $desc) {
    try { sink code() }
    ok ($! && $!.message.starts-with($prefix)), $desc;
}

my $ohm := blob8.new(0xE2, 0x84, 0xA6);    # U+2126 OHM SIGN, NFC is U+03A9
is $ohm.decode.ord, 0x3A9, '.decode normalizes to NFC';
is nqp::decode($ohm, 'utf8').ord, 0x3A9, 'nqp::decode normalizes too (it kept U+2126)';
is $ohm.decode('utf8', :replacement<?>).ord, 0x3A9, '.decode(:replacement) normalizes too';

my $path = $*TMPDIR.add("mutsu-decode-parity-{$*PID}.txt");
$path.spurt($ohm, :bin);
is $path.slurp.ord, 0x3A9, 'a slurped file normalizes too';
$path.unlink;

my $bom := blob8.new(0xEF, 0xBB, 0xBF, 0x41);
is $bom.decode.chars, 1, '.decode drops a UTF-8 BOM';
is nqp::decode($bom, 'utf8').chars, 1, 'nqp::decode drops it too (it kept it)';

dies-like { blob8.new(200).decode('ascii') },
    'Will not decode invalid ASCII', 'strict ASCII in .decode';
dies-like { nqp::decode(blob8.new(200), 'ascii') },
    'Will not decode invalid ASCII', '... and in nqp::decode (it let it through)';

is nqp::decode(blob8.new(0xE9), 'iso-8859-1'), 'é', 'nqp::decode knows latin-1';
is blob8.new(0xE9).decode('latin-1'), 'é', '... as .decode does';
is nqp::decode(blob8.new(0xE3, 0x81, 0x82), 'utf8'), 'あ', 'nqp::decode UTF-8';
