use Test;
use NativeCall;

plan 6;

class TS is repr('CStruct') { has int64 $.tv_sec; has int64 $.tv_nsec; }

sub raw-bytes(TS:D $t --> List) {
    my $view = nativecast(CArray[uint8], $t);
    (^nativesizeof(TS)).map({ $view[$_] }).List
}

my $t = TS.CREATE;
is-deeply raw-bytes($t), (0 xx 16).List, '.CREATE gives a zeroed native body';

TS.^attributes.first(*.name eq '$!tv_sec').set_value($t, 0x5A);
is $t.tv_sec, 0x5A, 'set_value is visible through the accessor';
is raw-bytes($t)[0], 0x5A, 'set_value writes tv_sec into the native bytes';
is raw-bytes($t)[8], 0, 'and leaves tv_nsec untouched';

TS.^attributes.first(*.name eq '$!tv_nsec').set_value($t, 0x7);
is raw-bytes($t)[8], 7, 'set_value on the second field lands at offset 8';
is-deeply raw-bytes(TS.new(:tv_sec(0x5A), :tv_nsec(7))), raw-bytes($t), '.new builds the same bytes';
