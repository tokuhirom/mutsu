use Test;
use NativeCall;

# A method call boxes every argument, so a `multi method` candidate with a
# boxed parameter (`Int:D`) is preferred to one with a native parameter
# (`int`, `uint`) and a native candidate never makes the call ambiguous
# (#14670). Expected values are the `raku` output.

plan 11;

class C {
    multi method a(int $pos, int $value) { "ii" }
    multi method a(Int:D $pos, int $value) { "Ii" }
    multi method a(int $pos, uint $value) { "iu" }
    multi method a(Int:D $pos, uint $value) { "Iu" }
    multi method a(Int:D $pos, Int:D $value) { "II" }
    multi method a(Any:D $pos, Int:D $value) { "AI" }
}
my $c = C.new;
is $c.a(0, 5), 'II', 'a small value picks the boxed candidate';
is $c.a(0, 0xAAAAAAAAAAAAAAAA), 'II', 'a value above int64 picks it too';
is $c.a(0, 2**70), 'II', 'and so does a value above uint64';
my int $p = 0;
is $c.a($p, 5), 'II', 'a native variable argument is boxed by the call';

class D {
    multi method m(int $v) { "i" }
    multi method m(Int:D $v) { "I" }
}
is D.new.m(5), 'I', 'int versus Int:D';
class E {
    multi method m(Int:D $v) { "I" }
    multi method m(int $v) { "i" }
}
is E.new.m(5), 'I', 'whatever the declaration order';
my $arr = CArray[uint64].new;
$arr[0] = 0xAAAAAAAAAAAAAAAA;
is $arr[0], 12297829382473034410, 'a CArray[uint64] stores a value above int64';
my $bytes = nativecast(CArray[uint8], $arr);
is (^8).map({ $bytes[$_] }).join(','), '170,170,170,170,170,170,170,170', 'and its bytes are intact';
my $i64 = CArray[int64].new;
$i64[0] = -1;
is $i64[0], -1, 'a negative CArray[int64] element still works';

class CS is repr('CStruct') {
    has uint64 $.channels;
    has uint32 $.x;
}
my $attr = CS.^attributes.first(*.name eq '$!channels');
my $cs = CS.CREATE;
$attr.set_value($cs, 2**64 - 1);
my $raw = nativecast(CArray[uint8], $cs);
is (^8).map({ $raw[$_] }).join(','), '255,255,255,255,255,255,255,255', 'a uint64 CStruct field keeps all 64 bits';
is $attr.get_value($cs), 18446744073709551615, 'and reads them back';
