use Test;

plan 4;

my Blob $buf = buf8.new(128, 1);
enum (:Marker(0x80), :Ascii(1), :Binary(2));
constant PFB-Header = (Marker, Ascii);

ok $buf[0..1] ~~ PFB-Header,
    'a byte-buffer slice smartmatches a list of enum values';
ok $buf[0..1] ~~ (Marker, Ascii),
    'a byte-buffer slice smartmatches the matching enum tuple';
nok $buf[0..1] ~~ (Marker, Marker),
    'a byte-buffer slice rejects a tuple with a nonmatching enum value';

subset PFB-Buf of buf8 where { .[0..1] ~~ PFB-Header };
multi sub handle(PFB-Buf $b) { 'specific' }
multi sub handle(buf8 $b) { 'fallback' }
is handle($buf), 'specific',
    'a where subset over the byte-buffer slice selects its multi candidate';
